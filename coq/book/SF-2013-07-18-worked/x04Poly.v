(** * Poly: Polymorphism and Higher-Order Functions *)

(**
/Applications/CoqIdE_8.4.app/Contents/Resources/bin/coqc x03Lists.v
*)

(** This chapter
    - _polymorphism_ (abstracting functions over the types of the data they manipulate) and
    -  _higher-order functions_ (treating functions as data).
*)

Require Export x03Lists.

(* ###################################################### *)
(** * Polymorphism *)
(* ###################################################### *)
(** ** Polymorphic Lists *)

(** So far, only lists of numbers. But want lists of strings, booleans,
    lists of lists, etc.  Bad: define new inductive datatype for each: *)

Inductive boollist : Type :=
  | bool_nil  : boollist
  | bool_cons : bool -> boollist -> boollist.

(** Bad: would need new versions of list functions ([length], [rev], etc.)
    for each new type. *)

(** Instead, use _polymorphic_ inductive type definitions: *)

Inductive list (X:Type) : Type :=
  | nil  : list X
  | cons : X -> list X -> list X.

(** Exactly like [natlist] definition except
    - [nat] arg to [cons] replaced by arbitrary type [X],
    - binding for [X] added to header,
    - occurrences of [natlist] in constructor types replaced by [list X]. *)

(** [list] is a _function_ from [Type]s to [Inductive] definitions, or
    [list] is a  function  from [Type]s to [Type]s.

    For any type [X], the type [list X] is an [Inductive]ly defined
    set of lists whose elements are of type [X]. *)

(** Observe types of these constructors: *)

Check nil.
(* ===> nil : forall X : Type, list X *)
Check cons.
(* ===> cons : forall X : Type, X -> list X -> list X *)

(** "[forall X]" means : additional arg to constructors that
    determines expected types of args that follow.
    Type args supplied in same way as other args. *)

(** When using constructors [nil] / [cons] must pass type. *)

Check (cons nat 2 (cons nat 1 (nil nat))).

(** Polymorphic (aka "generic") [length]. *)

Fixpoint length (X:Type) (l:list X) : nat :=
  match l with
  |      nil => 0
  | cons h t => S (length X t)
  end.


(** [nil] and [cons] in [match] do not need type annotations:
    already know that list contains elements of type [X].

    Type [X] is parameter of whole definition of [list],
    not individual constructors.

    Use [length] by giving type then list arg: *)

Example test_length1 :
    length nat (cons nat 1 (cons nat 2 (nil nat))) = 2.
Proof. reflexivity.  Qed.

Example test_length2 :
    length bool (cons bool true (nil bool)) = 1.
Proof. reflexivity.  Qed.

(** More polymorphic list functions: *)

Fixpoint app (X : Type) (l1 l2 : list X) : (list X) :=
  match l1 with
  |      nil => l2
  | cons h t => cons X h (app X t l2)
  end.

Fixpoint snoc (X:Type) (l:list X) (v:X) : (list X) :=
  match l with
  |      nil => cons X v (nil X)
  | cons h t => cons X h (snoc X t v)
  end.

Fixpoint rev (X:Type) (l:list X) : list X :=
  match l with
  |      nil => nil X
  | cons h t => snoc X (rev X t) h
  end.


Example test_rev1 :
    rev nat (cons nat 1 (cons nat 2 (nil nat))) =
            (cons nat 2 (cons nat 1 (nil nat))).
Proof. reflexivity.  Qed.

Example test_rev2:
  rev bool (nil bool) = nil bool.
Proof. reflexivity.  Qed.

Module MumbleBaz.
(** **** Exercise: 2 stars (mumble_grumble) *)
(** Consider: *)
(* TODO mumble_grumble type (not proof) *)
Inductive mumble : Type :=
  | a : mumble
  | b : mumble -> nat -> mumble
  | c : mumble.
Inductive grumble (X:Type) : Type :=
  | d : mumble -> grumble X
  | e : X      -> grumble X.

(** Which of the following are well-typed elements of [grumble X] for
    some type [X]?
      - [d (b a 5)]
      - [d mumble (b a 5)]
      - [d bool (b a 5)]
      - [e bool true]
      - [e mumble (b c 0)]
      - [e bool (b c 0)]
      - [c]
(* FILL IN HERE *)
[] *)


(** **** Exercise: 2 stars (baz_num_elts) *)
(** Consider the following inductive definition: *)
(* TODO baz_num_elts - how many elements does type have (not a proof)? *)
Inductive baz : Type :=
   | x : baz -> baz
   | y : baz -> bool -> baz.

(** How _many_ elements does the type [baz] have?
(* FILL IN HERE *)
[] *)

End MumbleBaz.

(* ###################################################### *)
(** *** Type Annotation Inference *)

(** [app] without specifying types of args to [app]. *)

Fixpoint app' X l1 l2 : list X :=
  match l1 with
  |      nil => l2
  | cons h t => cons X h (app' X t l2)
  end.

(** Indeed it will.  Let's see what type Coq has assigned to [app']: *)

Check app'.
(* ===> forall X : Type, list X -> list X -> list X *)
Check app.
(* ===> forall X : Type, list X -> list X -> list X *)

(** [app'] same type as [app].

    Uses _type inference_.

    Since [X] is an argument to [cons], it must be a [Type]
    since [cons] expects a [Type] as its first arg;

    Matching [l1] with [nil] and [cons] means [l1] must be a [list]; etc.

    No need to write explicit type annotations.
    But useful as documentation.
    Find balance to avoid readers doing type inference in their heads. *)


(* ###################################################### *)
(** *** Type Argument Synthesis *)

(** Since second arg to poly [length] is list of [X]s then first arg
    can only be [X].

    In place of type argument can write "implicit argument" [_].

    [_] uses _unification_ to determine concrete type
    using all locally available information:
    - type of function applied,
    - types of other args,
    - and type expected by context in which application appears

    Type inference of function args:
      app' (X : _) (l1 l2 : _) : list X :=

    [length] with implicit arguments: *)

Fixpoint length' (X:Type) (l:list X) : nat :=
  match l with
  |      nil => 0
  | cons h t => S (length' _ t)
  end.

(** [_] instead of [X] : not much saving.
    But can be significant: *)

Definition list123  := cons nat 1 (cons nat 2 (cons nat 3 (nil nat))).
Definition list123' := cons _   1 (cons _   2 (cons _   3 (nil _  ))).


(* ###################################################### *)
(** *** Implicit Arguments *)

(** Avoid [_].
    To _always_ infer type arg(s) of given function: *)

Arguments nil {X}.
Arguments cons {X} _ _.  (* use underscore for argument position that has no name *)
Arguments length {X} l.
Arguments app {X} l1 l2.
Arguments rev {X} l.
Arguments snoc {X} l v.

(* note: no _ arguments required... *)
Definition list123'' := cons 1 (cons 2 (cons 3 nil)).
Check (length list123'').

(** Alternate syntax:
    Declare implicit argument by surrounding argument in curly braces.
    Then not necessary to provide type arg to recursive call. *)

Fixpoint length'' {X:Type} (l:list X) : nat :=
  match l with
  |      nil => 0
  | cons h t => S (length'' t)
  end.


(** Style:
    - curlies whenever possible;
    - explicit [Implicit Argument] declarations for [Inductive] constructors. *)

(** Problem with declaring arguments [Implicit] :
    When not enough local information, give arg explicitly "this" time,
    even though declared globally to be [Implicit]: *)

(* Definition mynil := nil.  *)

(** Uncomment [mynil] def and see error, since not known what type
    arg to supply to [nil].  So give explicit type declaration to be
    used during "application" of [nil]): *)

Definition mynil : list nat := nil.

(** Alternate: force implicit arguments to be explicit by prefixing
   function name with [@]. *)

Check @nil.
(* ===> @nil : forall X : Type, list X *)

Definition mynil' := @nil nat.

(** Via arg synthesis + implicit args, can now define notation for lists.
    Since constructor type arguments implicit, automatically infer types when notations used. *)

Notation "x :: y" := (cons x y)
                     (at level 60, right associativity).
Notation "[ ]" := nil.
Notation "[ x ; .. ; y ]" := (cons x .. (cons y []) ..).
Notation "x ++ y" := (app x y)
                     (at level 60, right associativity).

(** Now lists can be written just the way we'd hope: *)

Definition list123''' := [1; 2; 3].


(* ###################################################### *)
(** *** Exercises: Polymorphic Lists *)

(** **** Exercise: 2 stars, optional (poly_exercises) *)
(** Fill in definitions and complete proofs. *)

Fixpoint repeat {X : Type} (n : X) (count : nat) : list X :=
  match count with
  |        0 => []
  | S count' => n :: (repeat n count')
  end.

Example test_repeat1:
  repeat true 2 = cons true (cons true nil).
Proof. reflexivity. Qed.

Theorem nil_app : forall X:Type, forall l:list X,
  app [] l = l.
Proof.
  intros X l.  (* [] ++ l = l *)
  simpl.       (*       l = l *)
  reflexivity.
Qed.


Theorem rev_snoc : forall X : Type,
                   forall v : X,
                   forall s : list X,
  rev (snoc s v) = v :: (rev s).
Proof.
  intros X v s. induction s as [| v' s'].
  Case "s is []".     (* rev (snoc [] v) = v :: rev [] *)
    simpl.            (*             [v] = [v]         *)
    reflexivity.
  Case "s is v'::s'". (* IHs' :      rev (snoc        s'  v)     = v :: rev              s' *)
                      (*             rev (snoc (v' :: s') v)     = v ::       rev (v' :: s')   *)
    simpl.            (*       snoc (rev (snoc        s'  v)) v' = v :: snoc (rev        s') v' *)
    rewrite -> IHs'.  (*       snoc (v :: rev s')             v' = v :: snoc (rev        s') v' *)
    simpl.            (*  v :: snoc      (rev s')             v' = v :: snoc (rev        s') v' *)
    reflexivity.
Qed.

Theorem rev_involutive : forall (X : Type) (l : list X),
  rev (rev l) = l.
Proof.
  intros X l. induction l as [|n l'].
  Case "l is []". simpl. reflexivity.
  Case "l is n::l'".     (* IHl' : rev (rev       l')    =      l' *)
                         (*        rev (rev (n :: l'))   = n :: l' *)
    simpl.               (*        rev (snoc (rev l') n) = n :: l' *)
    rewrite -> rev_snoc. (*   n :: rev (rev       l')    = n :: l' *)
    rewrite -> IHl'.     (*   n ::                l'     = n :: l' *)
    reflexivity.
Qed.

Theorem snoc_with_append : forall (X : Type) (l1 l2 : list X) (v : X),
  snoc (l1 ++ l2) v = l1 ++ (snoc l2 v).
Proof.
  intros X l1 l2 v. induction l1 as [| v' l1'].
  Case "l1 is []". simpl. reflexivity.
  Case "l1 is v'::l1'". (* IHl1' : snoc        (l1'  ++      l2) v =        l1'  ++ snoc l2 v *)
                        (*         snoc ((v' :: l1') ++      l2) v = (v' :: l1') ++ snoc l2 v *)
    simpl.              (*   v' :: snoc        (l1'  ++      l2) v =  v' :: l1'  ++ snoc l2 v *)
    rewrite -> IHl1'.   (*   v' ::              l1'  ++ snoc l2  v =  v' :: l1'  ++ snoc l2 v *)
    reflexivity.
Qed.
(** [] *)


(* ###################################################### *)
(** ** Polymorphic Pairs *)

Inductive prod (X Y : Type) : Type :=
  pair : X -> Y -> prod X Y.

Arguments pair {X} {Y} _ _.

Notation "( x , y )" := (pair x y).

(** Standard notation for pair _types_: *)

Notation "X * Y" := (prod X Y) : type_scope.

(** [: type_scope] says:  use abbrev when parsing types
    (avoids clash with multiplication symbol). *)

(** Caution: can confuse [(x,y)] and [X*Y].
    [(x,y)] is _value_ built from two other values;
    [X*Y]   is _type_  built from two other types.
    Given [x : X] and [y : Y], [(x,y)] has type [X*Y]. *)

Definition fst {X Y : Type} (p : X * Y) : X :=
  match p with (x,y) => x end.

Definition snd {X Y : Type} (p : X * Y) : Y :=
  match p with (x,y) => y end.

(** [zip] : called [combine] for consistency with Coq's standard library. *)
(** Note pair notation used in exprs and patterns. *)

Fixpoint combine {X Y : Type} (lx : list X) (ly : list Y) : list (X*Y) :=
  match (lx,ly) with
  | (   [],     _) => []
  | (    _,    []) => []
  | (x::tx, y::ty) => (x,y) :: (combine tx ty)
  end.

(** Can drop parens when no ambiguity: *)

Fixpoint combine' {X Y : Type} (lx : list X) (ly : list Y) : list (X*Y) :=
  match lx,ly with
  |    [],     _ => []
  |     _,    [] => []
  | x::tx, y::ty => (x,y) :: (combine' tx ty)
  end.

(** **** Exercise: 1 star, optional (combine_checks) *)
(** Answering on paper, check with coq:
    - What is the type of [combine]
      i.e., what does [Check @combine] print?
      Paper:
        combine  : forall X : Type, forall Y : Type, list X -> list Y -> list(X*Y)
*)
Check @combine.
(* ===> combine : forall X Y : Type, list X -> list Y -> list (X * Y) *)
(**
    - What does the following print?
      Paper: [(1,false), (2,false)]
*)
Eval simpl in (combine [1;2] [false;false;true;true]).
(* ===> = [(1, false); (2, false)] : list (nat * bool)  *)

(** **** Exercise: 2 stars (split) *)
(** Define/test [split] (aka [unzip]) :
    right inverse of combine: takes list of pairs, returns pair of lists. *)

Fixpoint hc_split' {X Y : Type} (lxy : list (X*Y)) (lx : list X) (ly : list Y) : list X*list Y :=
  match lxy with
  |         []  => pair           (rev lx) (rev ly)
  | (x,y)::lxy' => hc_split' lxy'  (x::lx)  (y::ly)
  end.

Definition hc_split {X Y : Type} (lxy : list (X*Y)) : list X*list Y := hc_split' lxy [] [].

Check @hc_split'.
Check @hc_split.

Eval compute in (hc_split [(1,false);(2,false);(3,true)]).
(* ===> = ([1; 2; 3], [false; false; true]) : list nat * list bool *)

Example test_hc_split:
  hc_split [(1,false);(2,false)] = ([1;2],[false;false]).
Proof. reflexivity. Qed.
(** [] *)

(* HC: Definition of split from coq stdlib (except A B type variables defined in Section/Variables).
   Avoids the two accumulators and reverses
   http://coq.inria.fr/V8.2pl1/stdlib/Coq.Lists.List.html
*)
Fixpoint split {A B : Type} (l:list (A*B)) { struct l }: list A * list B :=
  match l with
  |          [] => ([], [])
  | (x,y) :: tl => let (g,d) := split tl in (x::g, y::d)
  end.

Example test_split:       split [(1,false);(2,false)] = ([1;2],[false;false]).
Proof. reflexivity.  Qed.


(* ###################################################### *)
(** ** Polymorphic Options *)

Inductive option (X:Type) : Type :=
  | Some : X -> option X
  | None : option X.

Arguments Some {X} _.
Arguments None {X}.

Fixpoint index {X : Type} (n : nat) (l : list X) : option X :=
  match l with
  |     []  => None
  | a :: l' => if beq_nat n O then Some a else index (pred n) l'
  end.

Example test_index1 :    index 0 [4;5;6;7]  = Some 4.
Proof. reflexivity.  Qed.
Example test_index2 :    index  1 [[1];[2]]  = Some [2].
Proof. reflexivity.  Qed.
Example test_index3 :    index  2 [true]  = None.
Proof. reflexivity.  Qed.

(** **** Exercise: 1 star, optional (hd_opt_poly) *)
(** Write/test polymorphic version of [hd_opt] function. *)

Definition hd_opt {X : Type} (l : list X)  : option X :=
  match l with
  | h::t => Some h
  |    _ => None
  end.

(** Reminder: to force implicit arguments to be explicit, use [@]: *)
Check @hd_opt.

Example test_hd_opt1 :  hd_opt [1;2] = Some 1.
Proof. reflexivity. Qed.
Example test_hd_opt2 :   hd_opt  [[1];[2]]  = Some [1].
Proof. reflexivity. Qed.
Example test_hd_opt3 :        hd_opt  mynil'     = None.
Proof. reflexivity. Qed.
(** [] *)

(* ###################################################### *)
(** * Functions as Data *)
(* ###################################################### *)
(** ** Higher-Order Functions *)

(** _higher-order_ functions:
    - functions passed as arguments,
    - returned as results,
    - stored in data structures,
    -etc. *)

Definition doit3times {X:Type} (f:X->X) (n:X) : X :=
  f (f (f n)).

Check @doit3times.
(* ===> doit3times : forall X : Type, (X -> X) -> X -> X *)

Example test_doit3times: doit3times minustwo 9 = 3.
Proof. reflexivity.  Qed.

Example test_doit3times': doit3times negb true = false.
Proof. reflexivity.  Qed.

(* ###################################################### *)
(** ** Partial Application *)

(** Multi-arg functions are examples of passing functions as data.
    See type of [plus]: *)

Check plus.
(* ==> nat -> nat -> nat *)

(** Each [->] is a _binary_ operator on types.
    Coq only supports one-arg functions.

    [->] is _right-associative_, so [plus] is really
    [nat -> (nat -> nat)],
    read as : [plus] is one-arg function that takes a [nat] and
    returns a one-arg function that takes a [nat] and returns a [nat].

    _partial application_ : supply only the first arg: *)

Definition plus3 := plus 3.
Check plus3.

Example test_plus3 :    plus3 4 = 7.
Proof. reflexivity.  Qed.
Example test_plus3' :   doit3times plus3 0 = 9.
Proof. reflexivity.  Qed.
Example test_plus3'' :  doit3times (plus 3) 0 = 9.
Proof. reflexivity.  Qed.

(* ###################################################### *)
(** ** Digression: Currying *)

(** **** Exercise: 2 stars, advanced (currying) *)
(** Processing list of args with functions that return functions is
    called _currying_, for logician Haskell Curry.

    [f : A -> B -> C] has type [A -> (B -> C)].

    [f]  given a value of type [A] returns function [f' : B -> C].
    [f'] given a value of type [B] returns a value of type [C].

    Enables partial application.

    _Uncurrying_. reinterpret type [A -> B -> C] as [(A * B) -> C].

    With uncurried binary function, both args given at once as a pair;
    there is no partial application. *)

(** Can define currying as: *)

Definition prod_curry {X Y Z : Type}
                      (f : X * Y -> Z)
                      (x : X)
                      (y : Y) : Z := f (x, y).

(** Define inverse, [prod_uncurry].
    Prove theorems to show the two are inverses. *)

Definition prod_uncurry {X Y Z : Type}
                        (f : X -> Y -> Z)
                        (p : X * Y) : Z := f (fst p) (snd p).

(** Thought exercise: before using coq,
    calculate types of [prod_curry] and [prod_uncurry]. *)

(* HC: prod_curry : forall X Y Z : Type, f : X*Y->Z -> x:X -> y:Y -> Z *)
Check @prod_curry.
(*     prod_curry : forall X Y Z : Type, (X * Y -> Z) -> X -> Y -> Z *)

(* HC: prod_uncurry : forall X Y Z : Type, (X->Y->Z) -> (X*Y) -> Z *)
Check @prod_uncurry.
(*     prod_uncurry : forall X Y Z : Type, (X -> Y -> Z) -> X * Y -> Z *)

Theorem uncurry_curry : forall (X Y Z : Type) (f : X -> Y -> Z) x y,
  prod_curry (prod_uncurry f) x y = f x y.
Proof.
  intros.               (* prod_curry (prod_uncurry f) x  y   = f x y *)
  unfold prod_curry.    (*             prod_uncurry f (x, y)  = f x y *)
  unfold prod_uncurry.  (*        f (fst (x, y)) (snd (x, y)) = f x y *)
  simpl.                (*                           f x  y   = f x y *)
  reflexivity.
Qed.

Theorem curry_uncurry : forall (X Y Z : Type)
                               (f : (X * Y) -> Z) (p : X * Y),
  prod_uncurry (prod_curry f) p = f p.
Proof.
  intros.               (*                    prod_uncurry (prod_curry f) p = f p      *)
  unfold prod_curry.    (* prod_uncurry (fun (x : X) (y : Y) => f (x, y)) p = f p      *)
  unfold prod_uncurry.  (*                                 f (fst p, snd p) = f p      *)
  destruct p as (n, m). (*                       f (fst (n, m), snd (n, m)) = f (n, m) *)
  simpl.                (*                                         f (n, m) = f (n, m) *)
  reflexivity.
Qed.
(** [] *)

(* ###################################################### *)
(** ** Filter *)

(** higher-order function: given list of [X]s and _predicate_ on [X].
    Returns new list containing elements for which predicate is [true]. *)

Fixpoint filter {X:Type} (test: X->bool) (l:list X)
                : (list X) :=
  match l with
  |     [] => []
  | h :: t => if test h then h :: (filter test t)
                        else       filter test t
  end.

Example test_filter1: filter evenb [1;2;3;4] = [2;4].
Proof. reflexivity.  Qed.

Definition length_is_1 {X : Type} (l : list X) : bool :=
  beq_nat (length l) 1.

Example test_filter2:
    filter length_is_1
           [ [1; 2]; [3]; [4]; [5;6;7]; []; [8] ]
  = [ [3]; [4]; [8] ].
Proof. reflexivity.  Qed.

(** [filter] version of [countoddmembers] from [Lists] chapter. *)

Definition countoddmembers' (l:list nat) : nat :=
  length (filter oddb l).

Example test_countoddmembers'1:   countoddmembers' [1;0;3;1;4;5] = 4.
Proof. reflexivity.  Qed.
Example test_countoddmembers'2:   countoddmembers' [0;2;4] = 0.
Proof. reflexivity.  Qed.
Example test_countoddmembers'3:   countoddmembers' nil = 0.
Proof. reflexivity.  Qed.

(* ###################################################### *)
(** ** Anonymous Functions *)

Example test_anon_fun':
  doit3times (fun n => n * n) 2 = 256.
Proof. reflexivity.  Qed.

Example test_filter2':
    filter (fun l => beq_nat (length l) 1)
           [ [1; 2]; [3]; [4]; [5;6;7]; []; [8] ]
  = [ [3]; [4]; [8] ].
Proof. reflexivity.  Qed.

(** **** Exercise: 2 stars (filter_even_gt7) *)

(** Write [filter_even_gt7] (using [filter]) :
    given list of nats
    returns list of nats > 7 and even. *)

Definition filter_even_gt7 (l : list nat) : list nat :=
  filter (fun x => andb (ble_nat 7 x) (evenb x)) l.

Example test_filter_even_gt7_1 :
  filter_even_gt7 [1;2;6;9;10;3;12;8] = [10;12;8].
Proof. reflexivity. Qed.

Example test_filter_even_gt7_2 :
  filter_even_gt7 [5;2;6;19;129] = [].
Proof. reflexivity. Qed.
(** [] *)

(** **** Exercise: 3 stars (partition) *)
(** Using [filter], write:
    partition : forall X : Type,
                (X -> bool) -> list X -> list X * list X
    Given type [X], test predicate [X -> bool], and [list X].
    Returns a pair of lists.
    Fst has elements that satisfy test, Snd those that don't.
    Order of elements in returned lists should be same as original.
*)

(* O(2n) version *)
Definition partition {X : Type} (test : X -> bool) (l : list X)
                     : list X * list X :=
  (filter test l, filter (fun x => negb (test x)) l).

Example test_partition1: partition oddb [1;2;3;4;5] = ([1;3;5], [2;4]).
Proof. reflexivity. Qed.
Example test_partition2: partition (fun x => false) [5;9;0] = ([], [5;9;0]).
Proof. reflexivity. Qed.
(** [] *)

(* HC: O(n) version *)
Fixpoint  hc_partition {X : Type} (test : X -> bool) (l : list X)
                       : list X * list X :=
  match l with
  |     [] => ([], [])
  | h :: t => match test h with
              | true  => let (p,f) := hc_partition test t in (h::p,    f)
              | false => let (p,f) := hc_partition test t in (   p, h::f)
              end
  end.

Example test_hc_partition1: partition oddb [1;2;3;4;5] = ([1;3;5], [2;4]).
Proof. reflexivity. Qed.
Example test_hc_partition2: partition (fun x => false) [5;9;0] = ([], [5;9;0]).
Proof. reflexivity. Qed.


(* ###################################################### *)
(** ** Map *)

(** Given [f] and [ l = [n1, n2, n3, ...] ]
    returns [ [f n1, f n2, f n3,...] ]. *)

Fixpoint map {X Y:Type} (f:X->Y) (l:list X)
             : (list Y) :=
  match l with
  |     [] => []
  | h :: t => (f h) :: (map f t)
  end.

Example test_map1: map (plus 3) [2;0;2] = [5;3;5].
Proof. reflexivity.  Qed.

(** Types of input/output need not be same (i.e., [map] has _two_ type arguments). *)

Example test_map2: map oddb [2;1;2;5] = [false;true;false;true].
Proof. reflexivity.  Qed.

Example test_map3:
    map (fun n => [evenb n;oddb n]) [2;1;2;5]
  = [[true;false];[false;true];[true;false];[false;true]].
Proof. reflexivity.  Qed.

(** **** Exercise: 3 stars (map_rev) *)
(** Prove [map] and [rev] commute (using auxiliary lemma). *)
(**
  map o rev: [1, 2, 3] / [f1, f2, f3] / [f3, f2, f1]
  rev o map: [1, 2, 3] / [ 3,  2,  1] / [f3, f2, f1]
*)

Lemma hc_map_lemma1_not_used : forall (X Y : Type) (f : X -> Y) (l : list X) (x : X),
  (f x)::map f l = map f (x::l).
Proof.
  intros. simpl. reflexivity. Qed.

(** from
    https://github.com/joshcough/software-foundations/blob/master/Poly.v
*)

Lemma map_rev_helper : forall (X Y : Type) (f : X -> Y) (l : list X)(x : X),
  map f (snoc l x) = snoc (map f l) (f x).
Proof.
  intros X Y f l x. induction l as [| x' l'].
  Case "l is []". simpl. reflexivity.
  Case "l is x'::l'". (* IHl' :  map f (snoc        l'     x) =         snoc (map f        l')  (f x) *)
                      (*         map f (snoc (x' :: l')    x) =         snoc (map f (x' :: l')) (f x) *)
  simpl.              (* f x' :: map f (snoc        l'     x) = f x' :: snoc (map f        l')  (f x) *)
  rewrite -> IHl'.    (* f x' ::        snoc (map f l') (f x) = f x' :: snoc (map f        l')  (f x) *)
  reflexivity.
Qed.

(** Given above, I did this myself. *)
Theorem map_rev : forall (X Y : Type) (f : X -> Y) (l : list X),
  map f (rev l) = rev (map f l).
Proof.
  intros. induction l as [|n l'].
  Case "l is []". simpl. reflexivity.
  Case "l is n::l'".              (* IHl' : map      f       (rev       l')        =       rev (map f       l')        *)
                                  (*        map      f       (rev (n :: l'))       =       rev (map f (n :: l'))       *)
    simpl.                        (*        map      f (snoc (rev       l')     n) = snoc (rev (map f       l')) (f n) *)
    rewrite -> map_rev_helper.    (*  snoc (map      f       (rev       l')) (f n) = snoc (rev (map f       l')) (f n) *)
    rewrite -> IHl'.              (*  snoc (rev (map f                  l')) (f n) = snoc (rev (map f       l')) (f n) *)
    reflexivity.
Qed.
(** [] *)

(** **** Exercise: 2 stars (flat_map) *)
(** Write [flat_map]:
    maps a [list X] to a [list Y] using [f] of type [X -> list Y].

    Definition should flatten results of [f]:

    flat_map (fun n => [n,n+1,n+2]) [1,5,10] = [1, 2, 3, 5, 6, 7, 10, 11, 12].
*)

Fixpoint flat_map {X Y:Type} (f:X -> list Y) (l:list X)
                   : (list Y) :=
  match l with
  |     [] => []
  | h :: t => (f h) ++ flat_map f t
  end.

Example test_flat_map1:
  flat_map (fun n => [n;n;n]) [1;5;4]
  = [1; 1; 1; 5; 5; 5; 4; 4; 4].
Proof. reflexivity. Qed.
(** [] *)

Definition option_map {X Y : Type} (f : X -> Y) (xo : option X)
                      : option Y :=
  match xo with
    |   None => None
    | Some x => Some (f x)
  end.

(** **** Exercise: 2 stars, optional (implicit_args) *)
(* TODO remove implicit and fill in explicit *)
(** Above definitions/uses of [filter]/[map] use implicit arguments in many places.
    - Replace the curly braces around the implicit arguments with parentheses.
    - Fill in explicit type parameters where necessary.
    - Use Coq to check that you've done so correctly.
    Do it on a _copy_ of this file. *)

(* ###################################################### *)
(** ** Fold *)
(* tactic:fold *)
(** [fold].  Inspiration for "[reduce]" at heart of Google's map/reduce. *)

(** [fold] inserts binary operator [f] between every pair of elements.
    [ fold plus [1,2,3,4] ] = [1+2+3+4].
    Need starting element to be a second input to [f] at end of list.

    fold plus [1,2,3,4] 0 = 1 + (2 + (3 + (4 + 0))).
*)

Fixpoint fold {X Y:Type} (f: X->Y->Y) (l:list X) (b:Y) : Y :=
  match l with
  |     [] => b
  | h :: t => f h (fold f t b)
  end.

Check (fold andb).
(* ===> fold andb : list bool -> bool -> bool *)

Example fold_example1 : fold mult [1;2;3;4] 1 = 24.
Proof. reflexivity. Qed.

Example fold_example2 : fold andb [true;true;false;true] true = false.
Proof. reflexivity. Qed.

Example fold_example3 : fold app  [[1];[];[2;3];[4]] [] = [1;2;3;4].
Proof. reflexivity. Qed.


(** **** Exercise: 1 star, advanced (fold_types_different) *)
(** [fold] has _two_ type variables: operator [f] takes [X], [Y] returns [Y].
    Describe use-case for [X] and [Y] to be different. *)

(**
    fold f [x0; x1; x2] b = f x0 (f x1 (f x2 b))
    where f = plus

                         f  0    (f  1   (f  2   [[0;0;0]]))
                         f  0    (f  1   [[2;0;2];[0;0;0]])
                         f  0    [[1;2;3];[2;0;2];[0;0;0]]
                         [[0;1;1];[1;2;3];[2;0;2];[0;0;0]]

    TODO: the following should really use list nat*nat*nat.
*)

Definition hc_f_t_d (x : nat) (y : list (list nat)) : list (list nat) :=
  match y with
  | [y'; _; _]::t => [x;y';x+y']::y
  |             _ => []
  end.

Example test_hc_f_t_d1 : hc_f_t_d 2                 [[0;0;0]] =                 [[2;0;2];[0;0;0]].
Proof. simpl. reflexivity. Qed.
Example test_hc_f_t_d2 : hc_f_t_d 1         [[2;0;2];[0;0;0]] =         [[1;2;3];[2;0;2];[0;0;0]].
Proof. simpl. reflexivity. Qed.
Example test_hc_f_t_d3 : hc_f_t_d 0 [[1;2;3];[2;0;2];[0;0;0]] = [[0;1;1];[1;2;3];[2;0;2];[0;0;0]].
Proof. simpl. reflexivity. Qed.

Example hc_fold_types_different :
    fold (fun x y => (hc_f_t_d x y))
         [0;1;2]
         [[0;0;0]] = [[0;1;1];[1;2;3];[2;0;2];[0;0;0]].
Proof. unfold fold. simpl. reflexivity. Qed.

Definition hc_f_t_d_g (X Y : Type) (f : X -> X -> X) (x : X) (y : list (list X))
                      : list (list X) :=
  match y with
  | [y'; _; _]::t => [x;y';f x y']::y
  |             _ => []
  end.

Example test_hc_f_t_d_g1 : hc_f_t_d_g nat (list (list nat)) plus 2                 [[0;0;0]] =                 [[2;0;2];[0;0;0]].
Proof. simpl. reflexivity. Qed.
Example test_hc_f_t_d_g2 : hc_f_t_d_g nat (list (list nat)) plus 1         [[2;0;2];[0;0;0]] =         [[1;2;3];[2;0;2];[0;0;0]].
Proof. simpl. reflexivity. Qed.
Example test_hc_f_t_d_g3 : hc_f_t_d_g nat (list (list nat)) plus 0 [[1;2;3];[2;0;2];[0;0;0]] = [[0;1;1];[1;2;3];[2;0;2];[0;0;0]].
Proof. simpl. reflexivity. Qed.

Example hc_fold_types_different_g1 :
    fold (fun x y => (hc_f_t_d_g nat (list (list nat)) plus x y))
         [0;1;2]
         [[0;0;0]] = [[0;1;1];[1;2;3];[2;0;2];[0;0;0]].
Proof. unfold fold. simpl. reflexivity. Qed.

Example hc_fold_types_different_g3 :
    fold (fun x y => (hc_f_t_d_g nat (list (list nat)) mult x y))
         [1;2;3]
         [[10;0;0]] = [[1;2;2];[2;3;6];[3;10;30];[10;0;0]].
Proof. unfold fold. unfold hc_f_t_d_g. simpl. reflexivity. Qed.

Example hc_fold_types_different_g4 :
    fold (fun x y => (hc_f_t_d_g nat (list (list nat)) exp x y))
         [1;2;3]
         [[2;0;0]] = [[1;2;1];[2;3;8];[3;2;9];[2;0;0]].
Proof. unfold fold. unfold hc_f_t_d_g. simpl. reflexivity. Qed.

(* ###################################################### *)
(** ** Functions For Constructing Functions *)

(** Above: functions as _arguments_.
    Now: _returning_ functions as results.

    E.G.: given [x : X] returns function [nat->X].
    Returned functions ignores its arg and always returns [x]. *)

Definition constfun {X: Type} (x: X) : nat->X :=
  fun (k:nat) => x.

Definition ftrue := constfun true.

Example constfun_example1 : ftrue 0 = true.
Proof. reflexivity. Qed.

Example constfun_example2 : (constfun 5) 99 = 5.
Proof. reflexivity. Qed.

(** Following returns function that behaves like [f]
    except when called with arg [k] it returns [x]. *)

Definition override {X: Type} (f: nat->X) (k:nat) (x:X) : nat->X:=
  fun (k':nat) => if beq_nat k k' then x else f k'.

(** E.G.: apply [override] twice gives function that
    returns [false] on [1] and [3],
            [true] otherwise. *)

Definition fmostlytrue := override (override ftrue 1 false) 3 false.

Example override_example1 : fmostlytrue 0 = true.
Proof. reflexivity. Qed.

Example override_example2 : fmostlytrue 1 = false.
Proof. reflexivity. Qed.

Example override_example3 : fmostlytrue 2 = true.
Proof. reflexivity. Qed.

Example override_example4 : fmostlytrue 3 = false.
Proof. reflexivity. Qed.

(** **** Exercise: 1 star (override_example) *)
(** Paraphrase following theorem then prove.

    Function constructed from [constfun] :
      forall (b:bool) returns b.
    Function constructed from [override] :
      forall (n:nat) (b:bool) returns b except given 3 returns true.
    Application : forall (b:bool) returns b because applied to 2 not equal to 3.
*)

Theorem override_example : forall (b:bool),
  (override (constfun b) 3 true) 2 = b.
Proof.
  intros. destruct b. reflexivity. reflexivity. Qed.
(** [] *)

Theorem override_example_applied_to_3 : forall (b:bool),
  (override (constfun b) 3 true) 3 = true.
Proof.
  intros. destruct b. reflexivity. reflexivity. Qed.

(** Use overriding alot.
    Need to know its properties.
    To prove properties need to know more Coq tactics.
    Main topic of the next chapter. *)

(* ##################################################### *)
(* ##################################################### *)
(** * Optional Material *)
(** ** Non-Uniform Inductive Families (GADTs) *)
(* TODO: study GADTs *)
(** Recall definition:
Inductive boollist : Type :=
  boolnil  : boollist
| boolcons : bool -> boollist -> boollist.
*)

(** That was generalized above to "polymorphic lists".
    Another way of generalizing:
    inductive family of "length-indexed" lists of booleans
    (Note: names add additional 'l'): *)

Inductive boolllist : nat -> Type :=
  boollnil  : boolllist O
| boollcons : forall n, bool -> boolllist n -> boolllist (S n).

Implicit Arguments boollcons [[n]].

Check (boollcons true (boollcons false (boollcons true boollnil))).

Fixpoint blapp {n1} (l1: boolllist n1)
               {n2} (l2: boolllist n2)
             : boolllist (n1 + n2) :=
  match l1 with
  | boollnil        => l2
  | boollcons _ h t => boollcons h (blapp t l2)
  end.

(** Generalizions can be combined: length-indexed polymorphic version: *)

Inductive llist (X:Type) : nat -> Type :=
  lnil  : llist X O
| lcons : forall n, X -> llist X n -> llist X (S n).

Implicit Arguments lnil [[X]].
Implicit Arguments lcons [[X] [n]].

Check (lcons true (lcons false (lcons true lnil))).

Fixpoint lapp (X:Type)
              {n1} (l1: llist X n1)
              {n2} (l2: llist X n2)
            : llist X (n1 + n2) :=
  match l1 with
  | lnil        => l2
  | lcons _ h t => lcons h (lapp X t l2)
  end.


(* ###################################################### *)
(** * The [unfold] Tactic *)
(* tactic:unfold
   tactic:fold *)

(** In proof process, not all function calls expanded into definitions
    (so proofs do not become unwieldy and slow). *)

Theorem unfold_example_bad : forall m n,
  3 + n = m ->
  plus3 n + 1 = m + 1.
Proof.
  intros m n H.
  (* Want [rewrite -> H] here since [plus3 n] is definitionally equal to [3 + n].
     But [plus3 n] not expanded automatically. *)
Abort.

(** [unfold] tactic replaces defined name with definition.  *)

Theorem unfold_example : forall m n,
  3 + n = m ->
  plus3 n + 1 = m + 1.
Proof.
  intros m n H. (* plus3 n + 1 = m + 1 *)
  unfold plus3. (*   3 + n + 1 = m + 1 *)
  rewrite -> H. (*       m + 1 = m + 1 *)
  reflexivity.
Qed.

(** Proof (using [unfold]) of property of [override]:
    If we override a function at some argument [k] to return [x]
    and then look up [k],
    we get back [x]. *)

Theorem override_eq : forall {X:Type} x k (f:nat->X),
  (override f k x) k = x.
Proof.
  intros X x k f.          (*                 override f k x k = x *)
  unfold override.         (* (if beq_nat k k then x else f k) = x *)
  rewrite <- beq_nat_refl. (*                                x = x *)
  reflexivity.
Qed.

(** **** Exercise: 2 stars (override_neq) *)
(** When the function returned from override is given a [k] that is
    NOT the overriden number it returns [f] applied to [k]. *)

Theorem override_neq : forall {X:Type} x1 x2 k1 k2 (f : nat->X),
  f k1 = x1 ->
  beq_nat k2 k1 = false ->
  (override f k2 x2) k1 = x1.
Proof.
  intros.           (*                  override f k2 x2 k1 = x1 *)
  unfold override.  (* (if beq_nat k2 k1 then x2 else f k1) = x1 *)
  rewrite -> H.     (*   (if beq_nat k2 k1 then x2 else x1) = x1 *)
  rewrite -> H0.    (*                                   x1 = x1 *)
  reflexivity.
Qed.
(** [] *)

(** Inverse of [unfold] is [fold].  Used less often. *)


(* ##################################################### *)
(** * Additional Exercises *)

(** **** Exercise: 2 stars (fold_length) *)
(** Many common functions on lists implemented in terms of [fold].
    E.G., : *)

Definition fold_length {X : Type} (l : list X) : nat :=
  fold (fun _ n => S n) l 0.

Example test_fold_length1 : fold_length [4;7;0] = 3.
Proof. reflexivity. Qed.

(** Prove correctness of [fold_length]. *)

Theorem fold_length_correct : forall X (l : list X),
  fold_length l = length l.
Proof.
  intros X l. induction l as [| n l' IH].
  Case "l is []".
    simpl.
    unfold fold_length.
    simpl.
    reflexivity.
  Case "l is n::l'".
    simpl.
    rewrite <- IH.
    unfold fold_length.
    simpl.
    reflexivity.
Qed.
(** [] *)

(** **** Exercise: 3 stars (fold_map) *)
(** Define [map] in terms of [fold]. *)

Definition fold_map {X Y:Type} (f : X -> Y) (l : list X) : list Y :=
  fold (fun x acc => f x :: acc) l [] .

Example fold_map_double :  (map double [1;2;3;4]) = [2; 4; 6; 8].
Proof. reflexivity. Qed.
Example fold_map_le_3 : (map (fun x => ble_nat x 2) [1;2;3;4]) = [true; true; false; false].
Proof. reflexivity. Qed.

(** Prove [fold_map] is correct *)
Theorem fold_map_correct : forall X Y (f : X -> Y) (l : list X),
  map f l = fold_map f l.
Proof.
  intros. induction l as [| n l' IH].
  Case "l is []".
    simpl.
    unfold fold_map.
    simpl.
    reflexivity.
  Case "l is n::l'".
    simpl.
    rewrite -> IH.
    unfold fold_map.
    simpl.
    reflexivity.
Qed.
(** [] *)

(* $Date: 2013-07-17 16:19:11 -0400 (Wed, 17 Jul 2013) $ *)

