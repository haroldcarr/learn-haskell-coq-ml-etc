// ==============================================================================

#[no_mangle]
pub extern fn fact(x: u64) -> u64 {
    match x {
        0 => 1,
        _ => x * fact(x-1),
    }
}

// ==============================================================================
// https://gist.github.com/iskakaushik/1c5b8aa75c77479c33c4320913eebef6

use std::ffi::CString;
use std::os::raw::{c_char, c_int};
use std::{/*HCptr,*/ mem};

#[no_mangle]
unsafe extern "C" fn get_strings(/*HC outlen: *mut c_int*/) -> *mut *mut c_char {
    // fill vector with null-terminated strings
    let v = vec![
        CString::new("Hello").unwrap(),
        CString::new("World").unwrap(),
        CString::new("!").unwrap()
    ];

    // Turn each null-terminated string into a pointer.
    // `into_raw` takes ownershop, gives us the pointer and does NOT drop the data.
    let mut out = v
        .into_iter()
        .map(|s| s.into_raw())
        .collect::<Vec<_>>();

    // Make sure space is not wasted.
    out.shrink_to_fit();
    assert!(out.len() == out.capacity());

    // Get pointer to vector.
    //HC let len = out.len();
    let ptr = out.as_mut_ptr();
    mem::forget(out);

    // write back the length the caller can expect
    //HC ptr::write(outlen, len as c_int);

    // return the data
    ptr
}

#[no_mangle]
unsafe extern "C" fn free_string_array(ptr: *mut *mut c_char, len: c_int) {
    let len = len as usize;

    // Get vector back.
    // It was previously shrank, so capacity == length.
    let v = Vec::from_raw_parts(ptr, len, len);

    // drop one string at a time.
    for elem in v {
        let s = CString::from_raw(elem);
        mem::drop(s);
    }
    // Afterwards the vector will be dropped and thus freed.
}
/*
// The C code calling into our Rust part:
#include <stdio.h>

char** get_strings(int* outlen);
void free_string_array(char **ptr, int len);


int main(int argc, char** argv)
{
  int len;
  char** s = get_strings(&len);

  for (int i=0; i<len; i++) {
    printf("String %d: %s\n", i, s[i]);
  }

  free_string_array(s, len);

  return 0;
}
*/
// ==============================================================================

extern crate libc;

use std::collections::HashMap;
use std::ffi::CStr;

pub struct ZipCodeDatabase {
    population: HashMap<String, u32>,
}

impl ZipCodeDatabase {
    fn new() -> ZipCodeDatabase {
        ZipCodeDatabase {
            population: HashMap::new(),
        }
    }

    fn populate(&mut self) {
        for i in 0..100_000 {
            let zip = format!("{:05}", i);
            self.population.insert(zip, i);
        }
    }

    fn population_of(&self, zip: &str) -> u32 {
        self.population.get(zip).cloned().unwrap_or(0)
    }
}

#[no_mangle]
pub extern "C" fn zip_code_database_new() -> *mut ZipCodeDatabase {
    Box::into_raw(Box::new(ZipCodeDatabase::new()))
}

#[no_mangle]
/// # Safety
pub unsafe extern "C" fn zip_code_database_free(ptr: *mut ZipCodeDatabase) {
    if ptr.is_null() {
        return;
    }
    drop(Box::from_raw(ptr))
    //std::ptr::read(ptr);
}

#[no_mangle]
/// # Safety
pub unsafe extern "C" fn zip_code_database_populate(ptr: *mut ZipCodeDatabase) {
    let database = {
        assert!(!ptr.is_null());
        &mut *ptr
    };
    database.populate();
}

#[no_mangle]
/// # Safety
pub unsafe extern "C" fn zip_code_database_population_of(
    ptr: *const ZipCodeDatabase,
    zip: *const c_char,
) -> u32 {
    let database = {
        assert!(!ptr.is_null());
        &*ptr
    };
    let zip = {
        assert!(!zip.is_null());
        CStr::from_ptr(zip)
    };
    let zip_str = zip.to_str().unwrap();
    database.population_of(zip_str)
}


