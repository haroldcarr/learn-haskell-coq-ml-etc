(** * Poly: Polymorphism and Higher-Order Functions *)

(* $Date: 2012-09-08 20:51:57 -0400 (Sat, 08 Sep 2012) $ *)

Require Export x02Lists.

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
(* ===> cons nat 2 (cons nat 1 (nil nat)) : list nat *)

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

Example test_length1 :        length nat  (cons nat  1    (cons nat 2 (nil nat ))) = 2.
Proof. reflexivity.  Qed.

Example test_length2 :        length bool (cons bool true             (nil bool))  = 1.
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

Example test_rev2:            rev bool (nil bool) = nil bool.
Proof. reflexivity.  Qed.

(* ###################################################### *)
(** *** Type Annotation Inference *)

(** [app] without specifying types of args to [app]. *)

Fixpoint app' X l1 l2 : list X :=
  match l1 with
  |      nil => l2
  | cons h t => cons X h (app' X t l2)
  end.

Check app'.
(* ===> app' : forall X : Type, list X -> list X -> list X *)
Check app.
(* ===> app  : forall X : Type, list X -> list X -> list X *)

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

Implicit Arguments nil    [[X]].
Implicit Arguments cons   [[X]].
Implicit Arguments length [[X]].
Implicit Arguments app    [[X]].
Implicit Arguments rev    [[X]].
Implicit Arguments snoc   [[X]].

Definition list123'' := cons 1 (cons 2 (cons 3 nil)).
Check list123''.
(* ===> list123'' : list nat *)
Check (length list123'').
(* ===> length list123'' : nat *)

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

(* Definition mynil := nil. *)

(** Uncomment [mynil] def and see error, since not known what type
    arg to supply to [nil].  So give explicit type declaration to be
    used during "application" of [nil]): *)

Definition mynil : list nat := nil.

(** Alternate: force implicit arguments to be explicit by prefixing
   function name with [@]. *)

Check @nil.
(* ===> @nil : forall X : Type, list X *)

Definition mynil' := @nil nat.

Check mynil'.
(* ===> mynil' : list nat *)

(** Via arg synthesis + implicit args, can now define notation for lists.
    Since constructor type arguments implicit, automatically infer types when notations used. *)

Notation "x :: y" := (cons x y)
                     (at level 60, right associativity).
Notation "[ ]" := nil.
Notation "[ x , .. , y ]" := (cons x .. (cons y []) ..).
Notation "x ++ y" := (app x y)
                     (at level 60, right associativity).

Definition list123''' := [1, 2, 3].

(* ###################################################### *)
(** *** Exercises: Polymorphic Lists *)

(** **** Exercise: 2 stars, optional (poly_exercises) *)
(** Fill in definitions and complete proofs. *)

Fixpoint repeat (X : Type) (n : X) (count : nat) : list X :=
  match count with
  |        0 => nil
  | S count' => n :: (repeat X n count')
  end.

Example test_repeat1:         repeat     bool true 2 = cons true (cons true nil).
Proof. reflexivity. Qed.

Fixpoint hc_repeat' {X : Type} (n : X) (count : nat) : list X :=
  match count with
  |        0 => []
  | S count' => n :: (hc_repeat' n count')
  end.

Example test_hc_repeat2:      hc_repeat'      true 2 = [true, true].
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

(* ###################################################### *)
(** ** Polymorphic Pairs *)

Inductive prod (X Y : Type) : Type :=
  pair : X -> Y -> prod X Y.

Implicit Arguments pair [[X] [Y]].

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
    - What does
        Eval simpl in (combine [1,2] [false,false,true,true]).
      print?
      Paper: [(1,false), (2,false)]
*)
Eval simpl in (combine [1,2] [false,false,true,true]).
(* ===> = [(1, false), (2, false)] : list (nat * bool)  *)

(** **** Exercise: 2 stars, recommended (split) *)
(** Define/test [split] (aka [unzip]) :
    right inverse of combine: takes list of pairs, returns pair of lists. *)

Fixpoint split' {X Y : Type} (lxy : list (X*Y)) (lx : list X) (ly : list Y) : list X*list Y :=
  match lxy with
  |         []  => pair        (rev lx) (rev ly)
  | (x,y)::lxy' => split' lxy'  (x::lx)  (y::ly)
  end.

Definition split {X Y : Type} (lxy : list (X*Y)) : list X*list Y := split' lxy [] [].

Check @split'.
Check @split.

Eval compute in (split [(1,false),(2,false),(3,true)]).
(* ===>  = ([1, 2, 3], [false, false, true]) : list nat * list bool *)

Example test_split:           split [(1,false),(2,false)] = ([1,2],[false,false]).
Proof. reflexivity.  Qed.

(* HC: Definition of split from coq stdlib (except A B type variables defined in Section/Variables).
   Avoids the two accumulators and reverses
   http://coq.inria.fr/V8.2pl1/stdlib/Coq.Lists.List.html
*)
Fixpoint coq_split {A B : Type} (l:list (A*B)) { struct l }: list A * list B :=
  match l with
  |          [] => ([], [])
  | (x,y) :: tl => let (g,d) := coq_split tl in (x::g, y::d)
  end.

Example test_coq_split:       coq_split [(1,false),(2,false)] = ([1,2],[false,false]).
Proof. reflexivity.  Qed.

(* ###################################################### *)
(** ** Polymorphic Options *)

Inductive option (X:Type) : Type :=
  | Some : X -> option X
  | None : option X.

Implicit Arguments Some [[X]].
Implicit Arguments None [[X]].

Fixpoint index {X : Type} (n : nat) (l : list X) : option X :=
  match l with
  |     []  => None
  | a :: l' => if beq_nat n O then Some a else index (pred n) l'
  end.

Example test_index1 :         index 0 [4,5,6,7]  = Some 4.
Proof. reflexivity.  Qed.
Example test_index2 :         index  1 [[1],[2]]  = Some [2].
Proof. reflexivity.  Qed.
Example test_index3 :         index  2 [true]  = None.
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
(* ===> hd_opt : forall X : Type, list X -> option X *)

Example test_hd_opt1 :        hd_opt [1,2]       = Some 1.
Proof. reflexivity. Qed.
Example test_hd_opt2 :        hd_opt  [[1],[2]]  = Some [1].
Proof. reflexivity. Qed.
Example test_hd_opt3 :        hd_opt  mynil'     = None.
Proof. reflexivity. Qed.

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

(** **** Exercise: 2 stars, optional (currying) *)
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

Example test_filter1: filter evenb [1,2,3,4] = [2,4].
Proof. reflexivity.  Qed.

Definition length_is_1 {X : Type} (l : list X) : bool :=
  beq_nat (length l) 1.

Example test_filter2:
    filter length_is_1
           [ [1, 2], [3], [4], [5,6,7], [], [8] ]
  = [ [3], [4], [8] ].
Proof. reflexivity.  Qed.

(** [filter] version of [countoddmembers] from [Lists] chapter. *)

Definition countoddmembers' (l:list nat) : nat :=
  length (filter oddb l).

Example test_countoddmembers'1:   countoddmembers' [1,0,3,1,4,5] = 4.
Proof. reflexivity.  Qed.
Example test_countoddmembers'2:   countoddmembers' [0,2,4] = 0.
Proof. reflexivity.  Qed.
Example test_countoddmembers'3:   countoddmembers' nil = 0.
Proof. reflexivity.  Qed.

(* ###################################################### *)
(** ** Anonymous Functions *)

Example test_anon_fun':       doit3times (fun n => n * n) 2 = 256.
Proof. reflexivity.  Qed.

Example test_filter2':
    filter (fun l => beq_nat (length l) 1)
           [ [1, 2], [3], [4], [5,6,7], [], [8] ]
  = [ [3], [4], [8] ].
Proof. reflexivity.  Qed.

(** **** Exercise: 2 stars (filter_even_gt7) *)

(** Write [filter_even_gt7] (using [filter]) :
    given list of nats
    returns list of nats > 7 and even. *)

Definition filter_even_gt7 (l : list nat) : list nat :=
  filter (fun x => andb (ble_nat 7 x) (evenb x)) l.

Eval compute in (filter_even_gt7 [1,2,6,9,10,3,12,8]).

Example test_filter_even_gt7_1 :
  filter_even_gt7 [1,2,6,9,10,3,12,8] = [10,12,8].
Proof. reflexivity. Qed.

Example test_filter_even_gt7_2 :
  filter_even_gt7 [5,2,6,19,129] = [].
Proof. reflexivity. Qed.

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

Example test_partition1: partition oddb [1,2,3,4,5] = ([1,3,5], [2,4]).
Proof. reflexivity. Qed.
Example test_partition2: partition (fun x => false) [5,9,0] = ([], [5,9,0]).
Proof. reflexivity. Qed.

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

Example test_hc_partition1: hc_partition oddb [1,2,3,4,5] = ([1,3,5], [2,4]).
Proof. reflexivity. Qed.
Example test_hc_partition2: hc_partition (fun x => false) [5,9,0] = ([], [5,9,0]).
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

Example test_map1: map (plus 3) [2,0,2] = [5,3,5].
Proof. reflexivity.  Qed.

(** Types of input/output need not be same (i.e., [map] has _two_ type arguments). *)

Example test_map2: map oddb [2,1,2,5] = [false,true,false,true].
Proof. reflexivity.  Qed.

Example test_map3:
    map (fun n => [evenb n,oddb n]) [2,1,2,5]
  = [[true,false],[false,true],[true,false],[false,true]].
Proof. reflexivity.  Qed.

(** **** Exercise: 3 stars, optional (map_rev) *)
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

(** **** Exercise: 2 stars, recommended (flat_map) *)
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
  flat_map (fun n => [n,n,n]) [1,5,4]
  = [1, 1, 1, 5, 5, 5, 4, 4, 4].
Proof. reflexivity. Qed.

Definition option_map {X Y : Type} (f : X -> Y) (xo : option X)
                      : option Y :=
  match xo with
    |   None => None
    | Some x => Some (f x)
  end.

(** **** Exercise: 2 stars, optional (implicit_args) *)
(* TODO *)
(** Above definitions/uses of [filter]/[map] use implicit arguments in many places.
    - Replace the curly braces around the implicit arguments with parentheses.
    - Fill in explicit type parameters where necessary.
    - Use Coq to check that you've done so correctly.
    Do it on a _copy_ of this file. *)

(* ###################################################### *)
(** ** Fold *)

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

Check (fold plus).                      (* fold plus : list nat -> nat -> nat *)
Eval simpl in (fold plus [1,2,3,4] 0).  (* 10 : n *)

Example fold_example1 : fold mult [1,2,3,4] 1 = 24.
Proof. reflexivity. Qed.

Example fold_example2 : fold andb [true,true,false,true] true = false.
Proof. reflexivity. Qed.

Example fold_example3 : fold app  [[1],[],[2,3],[4]] [] = [1,2,3,4].
Proof. reflexivity. Qed.

(** **** Exercise: 1 star, optional (fold_types_different) *)
(** [fold] has _two_ type variables: operator [f] takes [X], [Y] returns [Y].
    Describe use-case for [X] and [Y] to be different. *)

(**
    fold f [x0, x1, x2] b = f x0 (f x1 (f x2 b))
    where f = plus

                         f  0    (f  1   (f  2   [[0,0,0]]))
                         f  0    (f  1   [[2,0,2],[0,0,0]])
                         f  0    [[1,2,3],[2,0,2],[0,0,0]]
                         [[0,1,1],[1,2,3],[2,0,2],[0,0,0]]

    TODO: the following should really use list nat*nat*nat.
*)

Definition hc_f_t_d (x : nat) (y : list (list nat)) : list (list nat) :=
  match y with
  | [y', _, _]::t => [x,y',x+y']::y
  |             _ => []
  end.

Example test_hc_f_t_d1 : hc_f_t_d 2                 [[0,0,0]] =                 [[2,0,2],[0,0,0]].
Proof. simpl. reflexivity. Qed.
Example test_hc_f_t_d2 : hc_f_t_d 1         [[2,0,2],[0,0,0]] =         [[1,2,3],[2,0,2],[0,0,0]].
Proof. simpl. reflexivity. Qed.
Example test_hc_f_t_d3 : hc_f_t_d 0 [[1,2,3],[2,0,2],[0,0,0]] = [[0,1,1],[1,2,3],[2,0,2],[0,0,0]].
Proof. simpl. reflexivity. Qed.

Example hc_fold_types_different :
    fold (fun x y => (hc_f_t_d x y))
         [0,1,2]
         [[0,0,0]] = [[0,1,1],[1,2,3],[2,0,2],[0,0,0]].
Proof. unfold fold. simpl. reflexivity. Qed.

Definition hc_f_t_d_g (X Y : Type) (f : X -> X -> X) (x : X) (y : list (list X))
                      : list (list X) :=
  match y with
  | [y', _, _]::t => [x,y',f x y']::y
  |             _ => []
  end.

Example test_hc_f_t_d_g1 : hc_f_t_d_g nat (list (list nat)) plus 2                 [[0,0,0]] =                 [[2,0,2],[0,0,0]].
Proof. simpl. reflexivity. Qed.
Example test_hc_f_t_d_g2 : hc_f_t_d_g nat (list (list nat)) plus 1         [[2,0,2],[0,0,0]] =         [[1,2,3],[2,0,2],[0,0,0]].
Proof. simpl. reflexivity. Qed.
Example test_hc_f_t_d_g3 : hc_f_t_d_g nat (list (list nat)) plus 0 [[1,2,3],[2,0,2],[0,0,0]] = [[0,1,1],[1,2,3],[2,0,2],[0,0,0]].
Proof. simpl. reflexivity. Qed.

Example hc_fold_types_different_g1 :
    fold (fun x y => (hc_f_t_d_g nat (list (list nat)) plus x y))
         [0,1,2]
         [[0,0,0]] = [[0,1,1],[1,2,3],[2,0,2],[0,0,0]].
Proof. unfold fold. simpl. reflexivity. Qed.

Example hc_fold_types_different_g3 :
    fold (fun x y => (hc_f_t_d_g nat (list (list nat)) mult x y))
         [1,2,3]
         [[10,0,0]] = [[1,2,2],[2,3,6],[3,10,30],[10,0,0]].
Proof. unfold fold. unfold hc_f_t_d_g. simpl. reflexivity. Qed.

Example hc_fold_types_different_g4 :
    fold (fun x y => (hc_f_t_d_g nat (list (list nat)) exp x y))
         [1,2,3]
         [[2,0,0]] = [[1,2,1],[2,3,8],[3,2,9],[2,0,0]].
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

(** Returns function that behaves like [f]
    except when called with arg [k] it returns [x]. *)

Definition override {X: Type} (f: nat->X) (k:nat) (x:X) : nat->X :=
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

Theorem override_example_applied_to_3 : forall (b:bool),
  (override (constfun b) 3 true) 3 = true.
Proof.
  intros. destruct b. reflexivity. reflexivity. Qed.

(** Use overriding alot.
    Need to know its properties.
    To prove properties need to know more Coq tactics.
    Main topic of the rest of chapter. *)

(* ##################################################### *)
(* ##################################################### *)
(** * Optional Material *)
(** ** Non-Uniform Inductive Families (GADTs) *)

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
(** * More About Coq *)

(* ###################################################### *)
(** ** The [apply] Tactic *)

(** When goal to be proved is same as a hypothesis in context
    or previously proved lemma. *)

Theorem silly1 : forall (n m o p : nat),
     n = m  ->
     [n,o] = [n,p] ->
     [n,o] = [m,p].
Proof.
  intros n m o p eq1 eq2.  (* [n, o] = [m, p] *)
  rewrite <- eq1.          (* [n, o] = [n, p] *)
  (* Could finish by: *)
  (* rewrite -> eq2.          [n, p] = [n, p]
     reflexivity.
     Same effect in single step using [apply]: *)
  apply eq2.
Qed.

(** When [apply] used with _conditional_ hypotheses and lemmas,
    premises added as subgoals. *)

Theorem silly2 : forall (n m o p : nat),
  n = m  ->
  (forall (q r : nat), q = r -> [q,o] = [r,p]) ->
  [n,o] = [m,p].
Proof.
  intros n m o p eq1 eq2. (* [n, o] = [m, p] *) (* QUESTION *)
  apply eq2.              (*      n = m      *)
  apply eq1.
Qed.

(** Version with [rewrite]: *)
(** TODO: is it possible?
Theorem hc_silly2 : forall (n m o p : nat),
  n = m  ->
  (forall (q r : nat), q = r -> [q,o] = [r,p]) ->
  [n,o] = [m,p].
Proof.
  intros n m o p eq1 eq2.
  rewrite -> eq1.
*)

(** Often in [apply H], [H] begins with [forall].
    To match current goal with conclusion of [H]
    must find values for quantified variables.

    E.G: [apply eq2] in following,
    [q] in [eq2] gets instantiated with [n] and
    [r]          gets instantiated with [m]. *)

Theorem silly2a : forall (n m : nat),
     (n,n) = (m,m)  ->
     (forall (q r : nat), (q,q) = (r,r) -> [q] = [r]) ->
     [n] = [m].
Proof.
  intros n m eq1 eq2.   (*    [n] = [m]    *)
  apply eq2.            (* (n, n) = (m, m) *)
  apply eq1.
Qed.

(** **** Exercise: 2 stars, optional (silly_ex) *)
(** Complete without using [simpl]: *)

Theorem silly_ex :
     (forall n, evenb n = true -> oddb (S n) = true) ->
     evenb 3 = true ->
     oddb 4 = true.
Proof.
  intros eq1 eq2. (*  oddb 4 = true *)
  apply eq1.      (* evenb 3 = true *)
  apply eq2.
Qed.

(** To use [apply], conclusion of the fact being applied must match the goal.
    E.G.: [apply] no good if left and right sides of equality are swapped: *)

Theorem silly3_firsttry : forall (n : nat),
     true = beq_nat n 5  ->
     beq_nat (S (S n)) 7 = true.
Proof.
  intros n H.   (* beq_nat (S (S n)) 7 = true *)
  simpl.        (*         beq_nat n 5 = true *)
  (* Here we cannot use [apply] directly *)
Admitted.

(** Use [symmetry] tactic to switch left/right sides of an equality in goal. *)

Theorem silly3 : forall (n : nat),
     true = beq_nat n 5  ->
     beq_nat (S (S n)) 7 = true.
Proof.
  intros n H. (* beq_nat (S (S n)) 7 = true                *)
  symmetry.   (*                true = beq_nat (S (S n)) 7 *)
  (* [simpl] not necessary. [apply] will do a [simpl] step first. *)
  simpl.      (*                true = beq_nat n 5         *)
  apply H.
Qed.

(** **** Exercise: 3 stars, recommended (apply_exercise1) *)

(* Hint: use [apply] with previously defined lemmas besides
   hypotheses in the context.  Remember [SearchAbout]. *)
Theorem rev_exercise1 : forall (l l' : list nat),
  l = rev l' -> l' = rev l.
Proof.
   intros l l' H.             (*          l'  = rev l  *)
   symmetry.                  (*      rev l   =     l' *)
   rewrite -> H.              (* rev (rev l') =     l' *)
(* rewrite -> rev_involutive.             l'  =     l' *)
   apply rev_involutive.
Qed.

(** **** Exercise: 1 star (apply_rewrite) *)
(** Explain the difference between [apply] and [rewrite].

    HC:
    - [apply] does several steps: [simpl], [rewrite], [reflexivity]
    - [rewrite] does not involve other steps

    Are there situations where both can usefully be applied?

    Yes: see rev_exercise1 above.
*)

(* ###################################################### *)
(** ** The [unfold] Tactic *)
(* RIGHT HERE JULY *)
(** In proof process, not all function calls expanded into definitions
    (so proofs do not become unwieldy and slow). *)

Theorem unfold_example_bad : forall m n,
  3 + n = m ->
  plus3 n + 1 = m + 1.
Proof.
  intros m n H.
  (* Want [rewrite -> H] here since [plus3 n] is definitionally equal to [3 + n].
     But [plus3 n] not expanded automatically. *)
Admitted.

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

(** Inverse of [unfold] is [fold].  Used less often. *)

(* ###################################################### *)
(** ** Inversion *)

(** Recall the definition of natural numbers:
     Inductive nat : Type :=
       | O : nat
       | S : nat -> nat.
    Every number has one of two forms.
    Implicit in definition are two other facts:

    - Constructor [S] is _injective_: [S n = S m] iff [n = m].

    - Constructors [O] and [S] are _disjoint_: [O] not equal to [S n] for any [n]. *)

(** Principles apply to all inductively defined types:
    - all constructors are injective
    - values built from distinct constructors are never equal.

    E.G.:
    [cons] is injective and [nil] is different from every non-empty list.
    [true] and [false] are unequal. (Since neither [true] nor [false]
    take any arguments, their injectivity is not an issue.) *)

(** [inversion] tactic exploits these principles.

    Suppose [H] is hypothesis in context or a previously proven lemma
    of the form
      c a1 a2 ... an = d b1 b2 ... bm
    for constructors [c], [d] and args [a1 ... an], [b1 ... bm].

    Then [inversion H] says "invert" this equality to extract
    information it contains about terms:

    - If [c], [d] same constructor, then, by injectivity of constructor,
      [a1 = b1], [a2 = b2], etc.;
      [inversion H] adds these facts to context, and tries to use them
      to rewrite goal.

    - If [c], [d] different constructors, then hypothesis [H] is contradictory.
      A false assumption has crept into context, meaning any goal provable!
      In this case, [inversion H] marks current goal as completed
      and pops it off goal stack. *)

Theorem eq_add_S : forall (n m : nat),
     S n = S m ->
     n = m.
Proof.
  intros n m eq. (* n = m *)
  inversion eq.  (* m = m ; adds and uses H0 : n = m *)
  reflexivity.
Qed.

Theorem silly4 : forall (n m : nat),
     [n] = [m] ->
     n = m.
Proof.
  intros n o eq. (* n = o *)
  inversion eq.  (* o = o ; adds and uses H0 : n = o *)
  reflexivity.
Qed.

(** [inversion] can also destruct equalities between complex values,
    binding multiple variables as it goes. *)

Theorem silly5 : forall (n m o : nat),
     [n,m] = [o,o] ->
     [n] = [m].
Proof.
  intros n m o eq. (* [n] = [m] *)
  inversion eq.    (* [o] = [o] ; adds and uses H0 : n = o   H1 : m = o *)
  reflexivity.
Qed.

(** **** Exercise: 1 star (sillyex1) *)
Example sillyex1 : forall (X : Type) (x y z : X) (l j : list X),
     x :: y :: l = z :: j ->
          y :: l = x :: j ->
                       x = y.
Proof.
  intros.        (* x = y *)
  inversion H0.  (* x = x ; add and uses  H2 : y = x   H3 : l = j  *)
  reflexivity.
Qed.

(* TODO: since this is proved, you could accidently use it? *)
Theorem silly6 : forall (n : nat),
     S n = O ->
     2 + 2 = 5.
Proof.
  intros n contra. inversion contra. Qed.

Theorem silly7 : forall (n m : nat),
     false = true ->
     [n] = [m].
Proof.
  intros n m contra. inversion contra.  Qed.

Eval simpl in (@nil nat :: @nil nat :: []).
(* ===>      = [[], []] : list (list nat) *)

(** **** Exercise: 1 star (sillyex2) *)
Example sillyex2 : forall (X : Type) (x y z : X) (l j : list X),
     x :: y :: l = [] ->
          y :: l = z :: j ->
               x = z.
Proof.
  intros X x y z l j contra eq2.
  inversion contra.
Qed.

(** Injectivity of constructors proves [forall (n m : nat), S n = S m -> n = m].
    Reverse direction (provable by standard equational reasoning) is a useful fact. *)

Lemma eq_remove_S : forall n m,
  n = m -> S n = S m.
Proof.
  intros n m eq. (* S n = S m *)
  rewrite -> eq. (* S m = S m *)
  reflexivity.
Qed.

(** Another way of proving length_snoc (from Lists).
    Extra equalities force more equational reasoning and use of tactics. *)
(** TODO: understand this better *)
Theorem length_snoc' : forall (X : Type)   (v : X)
                              (l : list X) (n : nat),
     length l = n ->
     length (snoc l v) = S n.
Proof.
  intros X v l. induction l as [| v' l']. (* IHl' : forall n : nat, length l' = n -> length (snoc l' v) = S n *)
  Case "l = []".    (* forall n : nat, length [] = n -> length (snoc [] v) = S n           *)
    intros n eq.    (*                                  length (snoc [] v) = S n           *)
    rewrite <- eq.  (*                                  length (snoc [] v) = S (length []) *)
    simpl.          (*                                                   1 = 1             *)
    reflexivity.
  Case "l = v' :: l'".
    intros n eq.          (* length (snoc (v' :: l') v) = S n *)
    simpl.                (*     S (length (snoc l' v)) = S n *)
    destruct n as [| n'].
    SCase "n = 0".        (* S (length (snoc l' v)) = 1 *)
      inversion eq.
    SCase "n = S n'".     (* S (length (snoc l' v)) = S (S n')  *)
      apply eq_remove_S.  (*     length (snoc l' v) = S n'      *)
      apply IHl'.         (*              length l' = n'        *)
      inversion eq.       (*              length l' = length l' *)
      reflexivity.
Qed.

(* ###################################################### *)
(** ** Varying the Induction Hypothesis *)

(** Use of inversion useful in many places: *)

Theorem beq_nat_eq_FAILED : forall n m,
  true = beq_nat n m -> n = m.
Proof.
  intros n m H.          (* This is the BAD step - too much intros *)
  induction n as [| n'].
  Case "n = 0".          (* 0 = m *)
    destruct m as [| m'].
    SCase "m = 0".       (* 0 = 0 *)
      reflexivity.
    SCase "m = S m'".    (* 0 = S m' *)
      simpl in H.        (* H : true = beq_nat 0 (S m') changes to: H : true = false *)
      inversion H.
  Case "n = S n'".       (* S n' = m *)
    destruct m as [| m'].
    SCase "m = 0".       (* S n' = 0 *)
      simpl in H.        (* H : true = beq_nat (S n') 0 changes to: H : true = false *)
      inversion H.
    SCase "m = S m'".    (* S n' = S m' *)
      apply eq_remove_S. (*   n' = m'   *)
      (* stuck here because the induction hypothesis
         talks about an extremely specific m *)
Admitted.

(** Inductive proof above fails because proof set up
    induction hypothesis as

       [ true = beq_nat n' m -> n' = m ].

     Hypothesis talks about [n'] and _particular_ natural number [m] --
     the number [m] is "held constant" in the induction hypothesis.  This
     induction hypothesis not strong enough to work.

     Solution: set up proof differently: introduce just [n].
     Then we get stronger induction hypothesis :

      [ forall m : nat, true = beq_nat n' m -> n' = m ]

     Now proof of [beq_nat_eq] goes through: *)

Theorem beq_nat_eq : forall n m,
  true = beq_nat n m -> n = m.
Proof.
  intros n.              (* This is the GOOD step - only intro [n] now, [m] later *)
  induction n as [| n']. (* IHn' : forall m : nat, true = beq_nat n' m -> n' = m  *)
  Case "n = 0".          (* forall m : nat, true = beq_nat 0 m           ->    0 = m *)
    intros m.            (*                 true = beq_nat 0 m           ->    0 = m *)
    destruct m as [| m'].
    SCase "m = 0".       (*                 true = beq_nat 0 0           ->    0 = 0 *)
      simpl.             (*                 true = true                  ->    0 = 0 *)
      reflexivity.
    SCase "m = S m'".    (*                 true = beq_nat 0 (S m')      ->    0 = S m' *)
      simpl.             (*                 true = false                 ->    0 = S m' *)
      intros contra.     (*                                                    0 = S m' ; contra: true = false *)
      inversion contra.
  Case "n = S n'".       (* forall m : nat, true = beq_nat (S n') m      -> S n' = m *)
    intros m.            (*                 true = beq_nat (S n') m      -> S n' = m *)
    destruct m as [| m'].
    SCase "m = 0".       (*                 true = beq_nat (S n') 0      -> S n' = 0 *)
      simpl.             (*                 true = false                 -> S n' = 0 *)
      intros contra.     (*                                                 S n' = 0 ; contra: true = false *)
      inversion contra.
    SCase "m = S m'".    (*                 true = beq_nat (S n') (S m') -> S n' = S m' *)
      simpl.             (*                 true = beq_nat n'     m'     -> S n' = S m' *)
      intros H.          (*                                                 S n' = S m' ; H : true = beq_nat n' m' *)
      apply eq_remove_S. (*                                                   n' = m'   *)
      apply IHn'.        (*                 true = beq_nat n' m'                        *)
      apply H.
Qed.

(** Similar issues in _many_ of proofs below.
    When in situation where induction hypothesis is insufficient to establish goal,
    consider going back and doing fewer [intros] to make the IH stronger. *)
(* TODO *)
(** **** Exercise: 2 stars (beq_nat_eq_informal) *)
(** Give an informal proof of [beq_nat_eq]. *)

(* FILL IN HERE *)
(** [] *)

(** **** Exercise: 3 stars (beq_nat_eq') *)
(** Prove beq_nat_eq by induction on [m].
    Be careful about when and order of [intro] of variables to get a general enough induction hypothesis.
    Try to prove without looking back at one above. *)

Theorem beq_nat_eq' : forall m n,
  beq_nat n m = true -> n = m.
Proof.
  intros m.               (* forall n : nat, beq_nat n m = true -> n = m *)
  induction m as [| m'].
  Case "m is 0".          (* forall n : nat, beq_nat n 0 = true -> n = 0 *)
    intros n.             (*                 beq_nat n 0 = true -> n = 0 *)
    destruct n as [| n'].
    SCase "n is 0".       (*                 beq_nat 0 0 = true -> 0 = 0 *)
      reflexivity.
    SCase "n is S n'".    (*            beq_nat (S n') 0 = true -> S n' = 0 *)
      simpl.              (*                       false = true -> S n' = 0 *)
      intro contra.       (*                                       S n' = 0 ; contra : false = true *)
      inversion contra.
  Case "m is S m'".       (* forall n : nat, beq_nat n (S m') = true -> n = S m' *)
    intros n.             (*                 beq_nat n (S m') = true -> n = S m' *)
    destruct n as [| n'].
    SCase "n is 0".       (*                 beq_nat 0 (S m') = true -> 0 = S m' *)
      simpl.              (*                            false = true -> 0 = S m' *)
      intro contra.       (*                                            0 = S m' ; contra : false = true *)
      inversion contra.
    SCase "n is S n'".    (*            beq_nat (S n') (S m') = true -> S n' = S m' *)
      simpl.              (*                    beq_nat n' m' = true -> S n' = S m' *)
      intros H.           (*                                            S n' = S m' ; H : beq_nat n' m' = true *)
      apply eq_remove_S.  (*                                              n' = m'   *)
      apply IHm'.         (*                    beq_nat n' m' = true                *)
      apply H.
Qed.

(* ###################################################### *)
(** *** Practice Session *)

(** **** Exercise: 2 stars, optional (practice) *)
(** May involve applying lemmas from earlier lectures or homeworks. *)

Theorem beq_nat_0_l : forall n,
  true = beq_nat 0 n -> 0 = n.
Proof.
  intros n H. destruct n as [| n'].
  Case "n is 0". reflexivity.
  Case "n is S n'". (* 0 = S n'  ; H : true = beq_nat 0 (S n') *)
    simpl in H.     (* 0 = S n'  ; H : true = false            *)
    inversion H.
Qed.

Theorem beq_nat_0_r : forall n,
  (* true = beq_nat 0 n -> 0 = n. file originally contained this, but probably means: *)
  true = beq_nat n 0 -> n = 0.
Proof.
  intros n H. destruct n as [| n'].
  Case "n is 0". reflexivity.
  Case "n is S n'". (* S n' = 0 ;   H : true = beq_nat (S n') 0 *)
    simpl in H.     (* S n' = 0 ;   H : true = false            *)
    inversion H.
Qed.

(** **** Exercise: 3 stars (apply_exercise2) *)
(** Next don't introduce [m] before [induction].
    This makes IH general : does not specify a particular [m], pick later.
    Finish proof. *)

Theorem beq_nat_sym : forall (n m : nat),
  beq_nat n m = beq_nat m n.
Proof.
  intros n. induction n as [| n']. (* IHn' : forall m : nat, beq_nat n' m = beq_nat m n' *)
  Case "n is 0".       (*  forall m : nat, beq_nat 0 m = beq_nat m 0 *)
    intro m.           (*                  beq_nat 0 m = beq_nat m 0 *)
    destruct m as [| m'].
    SCase "m is 0". reflexivity.
    SCase "m is S m'". (*              beq_nat 0 (S m') = beq_nat (S m') 0 *)
      simpl.           (*                         false = false *)
      reflexivity.
  Case "n is S n'". (* forall m : nat, beq_nat (S n') m = beq_nat m (S n') *)
    intro m.        (*                 beq_nat (S n') m = beq_nat m (S n') *)
    destruct m as [| m'].
    SCase "m is 0".    (*              beq_nat (S n') 0 = beq_nat 0 (S n') *)
      simpl.           (*                         false = false *)
      reflexivity.
    SCase "m is S m'". (*         beq_nat (S n') (S m') = beq_nat (S m') (S n') *)
      simpl.           (*                 beq_nat n' m' = beq_nat m' n'         *)
      rewrite IHn'.    (*                 beq_nat m' n' = beq_nat m' n'         *)
      reflexivity.
Qed.

(** **** Exercise: 3 stars (beq_nat_sym_informal) *)
(* TODO *)
(** Give informal proof following (corresponds to formal proof above):

   Theorem: For any [nat]s [n] [m], [beq_nat n m = beq_nat m n].

   Proof:
   (* FILL IN HERE *)
[]
 *)

(* ###################################################### *)
(** ** Using Tactics on Hypotheses *)

(** Default: most tactics work on goal.
    Most tactics have variant that work on statement in the context.

    E.G., tactic [simpl in H] does simplification of hypothesis named
    [H] in context. *)

Theorem S_inj : forall (n m : nat) (b : bool),
     beq_nat (S n) (S m) = b  ->
     beq_nat n m = b.
Proof.
  intros n m b H. (* beq_nat n m = b ; H : beq_nat (S n) (S m) = b *)
  simpl in H.     (* beq_nat n m = b ; H :         beq_nat n m = b *)
  apply H.
Qed.

(** Tactic [apply L in H] matches conditional statement [L]
    (of the form [L1 -> L2], say) against [H] in the context.

    [apply L in H] matches [H] against [L1] and replaces it [L2] if successful.

    [apply L in H] gives form of "forward reasoning":
    from [L1 -> L2] and hypothesis matching [L1]
    gives hypothesis matching [L2].

    Ordinary [apply] rewrites a goal matching [L2] into a subgoal [L1].

    [apply L] is "backward reasoning":
    if [L1->L2] known and trying to prove [L2], suffices to prove [L1].

    Variant of previous proof using forward reasoning throughout: *)

Theorem silly3' : forall (n : nat),
  (beq_nat n 5 = true -> beq_nat (S (S n)) 7 = true) ->
     true = beq_nat n 5  ->
     true = beq_nat (S (S n)) 7.
Proof.
  intros n eq H.  (* true = beq_nat (S (S n)) 7 ; H :                true = beq_nat n 5         *)
  symmetry in H.  (* true = beq_nat (S (S n)) 7 ; H :         beq_nat n 5 = true                *)
  apply eq in H.  (* true = beq_nat (S (S n)) 7 ; H : beq_nat (S (S n)) 7 = true                *)
  symmetry in H.  (* true = beq_nat (S (S n)) 7 ; H :                true = beq_nat (S (S n)) 7 *)
  apply H.
Qed.

(** Forward reasoning starts from what is _given_ (premises,
    previously proven theorems) and iteratively draws conclusions from
    them until the goal is reached.

    Backward reasoning starts from the _goal_, and iteratively reasons
    about what would imply the goal, until premises or previously
    proven theorems are reached.

    Most informal proofs in math or CS class use forward reasoning.

    Coq tends to favor backward reasoning, but sometimes forward style
    easier to use/think about.  *)

(** **** Exercise: 3 stars, recommended (plus_n_n_injective) *)
(** You can practice using the "in" variants in this exercise.
    Hint: use [plus_n_Sm: forall n m : nat, S (n + m) = n + S m].  *)
(* TODO  *)
Theorem plus_n_n_injective : forall n m,
     n + n = m + m ->
     n = m.
Proof.
  intros n. induction n as [| n'].
  Case "n is 0". (* forall m : nat, 0 + 0 = m + m -> 0 = m *)
    intros m.
    simpl.
    destruct m as [| m'].
    SCase "m is 0". reflexivity.
    SCase "m is S m'". (* 0 = S m' + S m' -> 0 = S m' *)
      intro contra.
      simpl in contra.
      inversion contra.
  Case "n is S n'". (* forall m : nat, S n' + S n' = m + m -> S n' = m *)
    intros m.
    destruct m as [| m'].
    SCase "m is 0".
      intros contra.
      simpl in contra.
      inversion contra.
    SCase "m is S m'". (* S n' + S n' = S m' + S m' -> S n' = S m' *)
      intros H.
      rewrite <- IHn'.
      rewrite <- plus_n_Sm in H.
      rewrite <- plus_n_Sm in H at 1.
      apply eq_remove_S in H.
      simpl in H.
Admitted.

(* ###################################################### *)
(** ** Using [destruct] on Compound Expressions *)

(** [destruct] used above for case analysis of value of a variable.
    Can use [destruct] to reason by cases on result of an _expression_ too. *)

Definition sillyfun (n : nat) : bool :=
  if beq_nat n 3 then false
  else if beq_nat n 5 then false
  else false.

Theorem sillyfun_false : forall (n : nat),
  sillyfun n = false.
Proof.
  intros n. unfold sillyfun.
  destruct (beq_nat n 3).
  Case "beq_nat n 3 = true". reflexivity.
  Case "beq_nat n 3 = false".
    destruct (beq_nat n 5).
    SCase "beq_nat n 5 = true". reflexivity.
    SCase "beq_nat n 5 = false". reflexivity.
Qed.

(** Above, unfold [sillyfun] resulting in [if (beq_nat n 3) then ... else ...].
    Use [destruct (beq_nat n 3)] to reason about two cases:
    [n] is equal to [3]  or not equal. *)

(** **** Exercise: 1 star (override_shadow) *)
Theorem override_shadow : forall {X:Type} x1 x2 k1 k2 (f : nat->X),
  (override (override f k1 x2) k1 x1) k2 = (override f k1 x1) k2.
Proof.
  intros. unfold override.
  destruct (beq_nat k1 k2).
  Case "beq_nat k1 k2 = true". reflexivity.
  Case "beq_nat k1 k2 = false". reflexivity.
Qed.

(** **** Exercise: 3 stars, recommended (combine_split) *)
(* RIGHT HERE *)
(* combine == zip; split == unzip *)
Theorem combine_split : forall X Y (l : list (X * Y)) l1 l2,
  split l = (l1, l2) ->
  combine l1 l2 = l.
Proof.
  intros X Y l. induction l as [| [x y] l'].
  Case "l is []".
    destruct l1.
    SCase "l1 is []".
      simpl.
      destruct l2.
      SSCase "l2 is []".
        reflexivity.
      SSCase "l2 is y::l2".


Admitted.

(** [] *)

(** **** Exercise: 3 stars, optional (split_combine) *)
(** Thought exercise: We have just proven that for all lists of pairs,
    [combine] is the inverse of [split].  How would you state the
    theorem showing that [split] is the inverse of [combine]?

    Hint: what property do you need of [l1] and [l2] for [split]
    [combine l1 l2 = (l1,l2)] to be true?

    State this theorem in Coq, and prove it. (Be sure to leave your
    induction hypothesis general by not doing [intros] on more things
    than necessary.) *)

(* FILL IN HERE *)
(** [] *)

(* ###################################################### *)
(** ** The [remember] Tactic *)

(** (Note: the [remember] tactic is not strictly needed until a
    bit later, so if necessary this section can be skipped and
    returned to when needed.) *)

(** We have seen how the [destruct] tactic can be used to
    perform case analysis of the results of arbitrary computations.
    If [e] is an expression whose type is some inductively defined
    type [T], then, for each constructor [c] of [T], [destruct e]
    generates a subgoal in which all occurrences of [e] (in the goal
    and in the context) are replaced by [c].

    Sometimes, however, this substitution process loses information
    that we need in order to complete the proof.  For example, suppose
    we define a function [sillyfun1] like this: *)

Definition sillyfun1 (n : nat) : bool :=
  if beq_nat n 3 then true
  else if beq_nat n 5 then true
  else false.

(** And suppose that we want to convince Coq of the rather
    obvious observation that [sillyfun1 n] yields [true] only when [n]
    is odd.  By analogy with the proofs we did with [sillyfun] above,
    it is natural to start the proof like this: *)

Theorem sillyfun1_odd_FAILED : forall (n : nat),
     sillyfun1 n = true ->
     oddb n = true.
Proof.
  intros n eq. unfold sillyfun1 in eq.
  destruct (beq_nat n 3).
  (* stuck... *)
Admitted.

(** We get stuck at this point because the context does not
    contain enough information to prove the goal!  The problem is that
    the substitution peformed by [destruct] is too brutal -- it threw
    away every occurrence of [beq_nat n 3], but we need to keep at
    least one of these because we need to be able to reason that
    since, in this branch of the case analysis, [beq_nat n 3 = true],
    it must be that [n = 3], from which it follows that [n] is odd.

    What we would really like is not to use [destruct] directly on
    [beq_nat n 3] and substitute away all occurrences of this
    expression, but rather to use [destruct] on something else that is
    _equal_ to [beq_nat n 3].  For example, if we had a variable that
    we knew was equal to [beq_nat n 3], we could [destruct] this
    variable instead.

    The [remember] tactic allows us to introduce such a variable. *)

Theorem sillyfun1_odd : forall (n : nat),
     sillyfun1 n = true ->
     oddb n = true.
Proof.
  intros n eq. unfold sillyfun1 in eq.
   remember (beq_nat n 3) as e3.
   (* At this point, the context has been enriched with a new
      variable [e3] and an assumption that [e3 = beq_nat n 3].
      Now if we do [destruct e3]... *)
   destruct e3.
   (* ... the variable [e3] gets substituted away (it
     disappears completely) and we are left with the same
      state as at the point where we got stuck above, except
      that the context still contains the extra equality
      assumption -- now with [true] substituted for [e3] --
      which is exactly what we need to make progress. *)
     Case "e3 = true". apply beq_nat_eq in Heqe3.
       rewrite -> Heqe3. reflexivity.
     Case "e3 = false".
      (* When we come to the second equality test in the
        body of the function we are reasoning about, we can
         use [remember] again in the same way, allowing us
         to finish the proof. *)
       remember (beq_nat n 5) as e5. destruct e5.
         SCase "e5 = true".
           apply beq_nat_eq in Heqe5.
           rewrite -> Heqe5. reflexivity.
         SCase "e5 = false". inversion eq.  Qed.

(** **** Exercise: 2 stars (override_same) *)
Theorem override_same : forall {X:Type} x1 k1 k2 (f : nat->X),
  f k1 = x1 ->
  (override f k1 x1) k2 = f k2.
Proof.
  (* FILL IN HERE *) Admitted.
(** [] *)

(** **** Exercise: 3 stars, optional (filter_exercise) *)
(** This one is a bit challenging.  Be sure your initial [intros] go
    only up through the parameter on which you want to do
    induction! *)

Theorem filter_exercise : forall (X : Type) (test : X -> bool)
                             (x : X) (l lf : list X),
     filter test l = x :: lf ->
     test x = true.
Proof.
  (* FILL IN HERE *) Admitted.
(** [] *)

(* ###################################################### *)
(** ** The [apply ... with ...] Tactic *)

(** The following silly example uses two rewrites in a row to
    get from [[a,b]] to [[e,f]]. *)

Example trans_eq_example : forall (a b c d e f : nat),
     [a,b] = [c,d] ->
     [c,d] = [e,f] ->
     [a,b] = [e,f].
Proof.
  intros a b c d e f eq1 eq2.
  rewrite -> eq1. rewrite -> eq2. reflexivity.  Qed.

(** Since this is a common pattern, we might
    abstract it out as a lemma recording once and for all
    the fact that equality is transitive. *)

Theorem trans_eq : forall {X:Type} (n m o : X),
  n = m -> m = o -> n = o.
Proof.
  intros X n m o eq1 eq2. rewrite -> eq1. rewrite -> eq2.
  reflexivity.  Qed.

(** Now, we should be able to use [trans_eq] to
    prove the above example.  However, to do this we need
    a slight refinement of the [apply] tactic. *)

Example trans_eq_example' : forall (a b c d e f : nat),
     [a,b] = [c,d] ->
     [c,d] = [e,f] ->
     [a,b] = [e,f].
Proof.
  intros a b c d e f eq1 eq2.
  (* If we simply tell Coq [apply trans_eq] at this point,
     it can tell (by matching the goal against the
     conclusion of the lemma) that it should instantiate [X]
     with [[nat]], [n] with [[a,b]], and [o] with [[e,f]].
     However, the matching process doesn't determine an
     instantiation for [m]: we have to supply one explicitly
     by adding [with (m:=[c,d])] to the invocation of
     [apply]. *)
  apply trans_eq with (m:=[c,d]). apply eq1. apply eq2.   Qed.

(**  Actually, we usually don't have to include the name [m]
    in the [with] clause; Coq is often smart enough to
    figure out which instantiation we're giving. We could
    instead write: apply trans_eq with [c,d]. *)

(** **** Exercise: 3 stars, recommended (apply_exercises) *)
Example trans_eq_exercise : forall (n m o p : nat),
     m = (minustwo o) ->
     (n + p) = m ->
     (n + p) = (minustwo o).
Proof.
  (* FILL IN HERE *) Admitted.

Theorem beq_nat_trans : forall n m p,
  true = beq_nat n m ->
  true = beq_nat m p ->
  true = beq_nat n p.
Proof.
  (* FILL IN HERE *) Admitted.

Theorem override_permute : forall {X:Type} x1 x2 k1 k2 k3 (f : nat->X),
  false = beq_nat k2 k1 ->
  (override (override f k2 x2) k1 x1) k3 = (override (override f k1 x1) k2 x2) k3.
Proof.
  (* FILL IN HERE *) Admitted.
(** [] *)

(* ################################################################## *)
(** * Review *)

(** We've now seen a bunch of Coq's fundamental tactics -- enough to
    do pretty much everything we'll want for a while.  We'll introduce
    one or two more as we go along through the next few lectures, and
    later in the course we'll introduce some more powerful
    _automation_ tactics that make Coq do more of the low-level work
    in many cases.  But basically we've got what we need to get work
    done.

    Here are the ones we've seen:

      - [intros]:
        move hypotheses/variables from goal to context

      - [reflexivity]:
        finish the proof (when the goal looks like [e = e])

      - [apply]:
        prove goal using a hypothesis, lemma, or constructor

      - [apply... in H]:
        apply a hypothesis, lemma, or constructor to a hypothesis in
        the context (forward reasoning)

      - [apply... with...]:
        explicitly specify values for variables that cannot be
        determined by pattern matching

      - [simpl]:
        simplify computations in the goal

      - [simpl in H]:
        ... or a hypothesis

      - [rewrite]:
        use an equality hypothesis (or lemma) to rewrite the goal

      - [rewrite ... in H]:
        ... or a hypothesis

      - [symmetry]:
        changes a goal of the form [t=u] into [u=t]

      - [symmetry in H]:
        changes a hypothesis of the form [t=u] into [u=t]

      - [unfold]:
        replace a defined constant by its right-hand side in the goal

      - [unfold... in H]:
        ... or a hypothesis

      - [destruct... as...]:
        case analysis on values of inductively defined types

      - [induction... as...]:
        induction on values of inductively defined types

      - [inversion]:
        reason by injectivity and distinctness of constructors

      - [remember (e) as x]:
        give a name ([x]) to an expression ([e]) so that we can
        destruct [x] without "losing" [e]

      - [assert (e) as H]:
        introduce a "local lemma" [e] and call it [H]
*)

(* ###################################################### *)
(** * Additional Exercises *)

(** **** Exercise: 2 stars, optional (fold_length) *)
(** Many common functions on lists can be implemented in terms of
   [fold].  For example, here is an alternate definition of [length]: *)

Definition fold_length {X : Type} (l : list X) : nat :=
  fold (fun _ n => S n) l 0.

Example test_fold_length1 : fold_length [4,7,0] = 3.
Proof. reflexivity. Qed.

(** Prove the correctness of [fold_length]. *)

Theorem fold_length_correct : forall X (l : list X),
  fold_length l = length l.
(* FILL IN HERE *) Admitted.
(** [] *)

(** **** Exercise: 3 stars, recommended (fold_map) *)
(** We can also define [map] in terms of [fold].  Finish [fold_map]
    below. *)

Definition fold_map {X Y:Type} (f : X -> Y) (l : list X) : list Y :=
(* FILL IN HERE *) admit.

(** Write down a theorem in Coq stating that [fold_map] is correct,
    and prove it. *)

(* FILL IN HERE *)
(** [] *)

Module MumbleBaz.
(** **** Exercise: 2 stars, optional (mumble_grumble) *)
(** Consider the following two inductively defined types. *)

Inductive mumble : Type :=
  | a : mumble
  | b : mumble -> nat -> mumble
  | c : mumble.
Inductive grumble (X:Type) : Type :=
  | d : mumble -> grumble X
  | e : X -> grumble X.

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

(** **** Exercise: 2 stars, optional (baz_num_elts) *)
(** Consider the following inductive definition: *)

Inductive baz : Type :=
   | x : baz -> baz
   | y : baz -> bool -> baz.

(** How _many_ elements does the type [baz] have?
(* FILL IN HERE *)
[] *)

End MumbleBaz.

(** **** Exercise: 4 stars, recommended (forall_exists_challenge) *)
(** Challenge problem: Define two recursive [Fixpoints],
    [forallb] and [existsb].  The first checks whether every
    element in a list satisfies a given predicate:
      forallb oddb [1,3,5,7,9] = true

      forallb negb [false,false] = true

      forallb evenb [0,2,4,5] = false

      forallb (beq_nat 5) [] = true
    The function [existsb] checks whether there exists an element in
    the list that satisfies a given predicate:
      existsb (beq_nat 5) [0,2,3,6] = false

      existsb (andb true) [true,true,false] = true

      existsb oddb [1,0,0,0,0,3] = true

      existsb evenb [] = false
    Next, create a _nonrecursive_ [Definition], [existsb'], using
    [forallb] and [negb].

    Prove that [existsb'] and [existsb] have the same behavior.
*)

(* FILL IN HERE *)
(** [] *)

(** **** Exercise: 2 stars, optional (index_informal) *)
(** Recall the definition of the [index] function:
   Fixpoint index {X : Type} (n : nat) (l : list X) : option X :=
     match l with
     | [] => None
     | a :: l' => if beq_nat n O then Some a else index (pred n) l'
     end.
   Write an informal proof of the following theorem:
   forall X n l, length l = n -> @index X (S n) l = None.
(* FILL IN HERE *)
*)
(** [] *)

