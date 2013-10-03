(** * Basics: Functional Programming in Coq *)

(* This library definition is included here temporarily
   for backward compatibility with Coq 8.3.
   Please ignore. *)
Definition admit {T: Type} : T.  Admitted.

(* ###################################################################### *)
(** * Introduction *)

(** Functional programming style brings programming closer to mathematics.

    Emphasizes the use of functions as _first-class_ values
    - i.e., pass, return, store.

    Common features:
    - _algebraic data types_
    - _pattern matching_
    - _polymorphic type systems_
*)


(* ###################################################################### *)
(** * Enumerated Types *)

(** enumerated types: explicitly enumerate finite set of elements. *)

(* ###################################################################### *)
(** ** Days of the Week *)

Inductive day : Type :=
  | monday    : day
  | tuesday   : day
  | wednesday : day
  | thursday  : day
  | friday    : day
  | saturday  : day
  | sunday    : day.

Definition next_weekday (d:day) : day :=
  match d with
  | monday    => tuesday
  | tuesday   => wednesday
  | wednesday => thursday
  | thursday  => friday
  | friday    => monday
  | saturday  => monday
  | sunday    => monday
  end.

Eval compute in (next_weekday friday).
   (* ==> monday : day *)
Eval compute in (next_weekday (next_weekday saturday)).
   (* ==> tuesday : day *)

(** Note: Square brackets delimit fragments of Coq code in comments. *)
(** keyword [compute], or [simpl] ("simplify") says how to evaluate the given expression. *)
(* tactic:simpl
   tactic:compute *)

(** Record expected the result via: *)

Example test_next_weekday:
  (next_weekday (next_weekday saturday)) = tuesday.

(** Does two things:
    1. Makes an assertion (i.e., second weekday after [saturday] is [tuesday]),
    2. Gives assertion a name for later reference *)

(* Try this:
Print test_next_weekday.
   now it says: "Error: test_next_weekday not a defined object."
   because it has not been proved yet. *)

(** Now verify/prove assertion: *)
Proof. simpl. reflexivity.  Qed.

(** Says: "Assertion proved by observing both sides of equality are
    same after simplification." *)

Print test_next_weekday.

(** Coq can "extract" a program into OCaml, Scheme, or Haskell.
    Gives way to construct _fully certified_ programs.
    See: Coq'Art book by Bertot and Casteran, and Coq reference manual. *)


(* ###################################################################### *)
(** ** Booleans *)

(** Coq boolean library: [Coq.Init.Datatypes].
    But defined below for pedagogy. *)

Inductive bool : Type :=
  | true  : bool
  | false : bool.

Definition negb (b:bool) : bool :=
  match b with
  | true  => false
  | false => true
  end.

Definition andb (b1:bool) (b2:bool) : bool :=
  match b1 with
  | true  => b2
  | false => false
  end.

Definition orb (b1:bool) (b2:bool) : bool :=
  match b1 with
  | true  => true
  | false => b2
  end.

(** Note: multi-argument function definitions above. *)

Example test_orb1:  (orb true  false) = true.
Proof. reflexivity.  Qed.
Example test_orb2:  (orb false false) = false.
Proof. reflexivity.  Qed.
Example test_orb3:  (orb false true)  = true.
Proof. reflexivity.  Qed.
Example test_orb4:  (orb true  true)  = true.
Proof. reflexivity.  Qed.

(** Note: dropped [simpl].  [reflexivity] automatically does simplification. *)

(** Values [Admitted] and [admit] used to fill a hole in an incomplete definition or proof.

    Says "Don't feel like proving now---trust me."  Useful
    for developing longer proofs, to state subsidiary facts believed
    to be useful in larger argument.  Use [Admitted] to accept on
    faith for moment, continue with larger argument until sure it
    makes sense; then go back and fill in skipped proofs.  Be careful:
    when you say [admit] or [Admitted] you are leaving a door open for
    total nonsense.  *)

(** **** Exercise: 1 star (nandb) *)

Definition nandb (b1:bool) (b2:bool) : bool :=
  match b1 with
  | true  => negb b2
  | false => true
  end.

Example test_nandb1:               (nandb true false)  = true.
Proof. simpl. reflexivity.  Qed.
Example test_nandb2:               (nandb false false) = true.
Proof. simpl. reflexivity.  Qed.
Example test_nandb3:               (nandb false true)  = true.
Proof. simpl. reflexivity.  Qed.
Example test_nandb4:               (nandb true true)   = false.
Proof. simpl. reflexivity.  Qed.

(** [] *)

(** **** Exercise: 1 star (andb3) *)
Definition andb3 (b1:bool) (b2:bool) (b3:bool) : bool :=
  match b1 with
  | false => false
  | true  => andb b2 b3
  end.

Example test_andb31:                 (andb3 true true true)  = true.
Proof. simpl. reflexivity.  Qed.
Example test_andb32:                 (andb3 false true true) = false.
Proof. simpl. reflexivity.  Qed.
Example test_andb33:                 (andb3 true false true) = false.
Proof. simpl. reflexivity.  Qed.
Example test_andb34:                 (andb3 true true false) = false.
Proof. simpl. reflexivity.  Qed.
(** [] *)

(* ###################################################################### *)
(** ** Function Types *)

(** [Check] : print the type of an expression.  *)

Check true.
(* ===> true : bool *)
Check (negb true).
(* ===> negb true : bool *)
Check negb.
(* ===> negb : bool -> bool *)
(** [bool -> bool] pronounced "[bool] arrow [bool]" *)
Check andb3.
(* ===> andb3 : bool -> bool -> bool -> bool *)


(* ###################################################################### *)
(** ** Numbers *)

(** Note: Module system.
    Declarations between [Module X] and [End  X] markers.
    From outside module refer via [X.foo]. *)

Module Playground1.

(** Besides enumerated types (above) : can also define type via
    collection of "inductive rules" describing its elements. *)

Inductive nat : Type :=
  | O : nat          (* 0-arg constructor *)
  | S : nat -> nat.  (* 1-arg constructor *)

(** Says:
    - [O] is a natural number (note: the letter "[O]," not numeral "[0]").
    - [S] is a "constructor" : takes natural number and yields another one,
      i.e., if [n] is a natural number, then [S n] is too.

    Inductively defined sets ([weekday], [nat], [bool], etc.) are a
    set of _expressions_.  Definition of [nat] says how expressions in
    the set [nat] can be constructed:

    - expression [O] belongs to set [nat];
    - if [n] is expression belonging to set [nat], then [S n] is too;
    - expressions formed in these two ways are the _only_ ones belonging
      to the set [nat]. *)

(** Implies: exprs [O], [S O], [S (S O)], [S (S (S O))], ...,
    belong to set [nat].
    Other exprs (e.g., [true], [andb true false], [S (S false)]) do not. *)

(** [Inductive] definition of [nat] does not say what [O] and [S] "mean".
    Just gives structure.
    Interpretation of [O] as zero and [S] as successor (or plus one)
    comes from way we use [nat] values in functions and proofs. *)

Definition pred (n : nat) : nat :=
  match n with
    | O    => O  (* there is no predecessor of this natural number *)
    | S n' => n' (* if [n] has form [S n'] for some [n'], then return [n'] *)
  end.

End Playground1.

Definition minustwo (n : nat) : nat :=
  match n with
    |      O   => O  (*  0     - 2 = 0  for n  < 2 *)
    |    S O   => O  (*  1     - 2 = 0  for n  < 2 *)
    | S (S n') => n' (* (n'+2) - 2 = n' for n >= 2 *)
  end.

(** Coq parsing/printing lets you use arabic numerals instead of [S ...].
    Prints arabic by default. *)

Check (S (S (S (S O)))).
Eval simpl in (minustwo 4).

Check (S (S (S (S O)))).
(* ===> 4 : nat *)
Eval simpl in (minustwo 4).
(* ===> 2 : nat *)

Check S.         (* ===> S : nat -> nat *)
Check O.         (* ===> 0 : nat *)
Check pred.      (* ===> pred : nat -> nat *)
Check minustwo.  (* ===> minustwo : nat -> nat *)

(** [O] is constructor/function of 0 arg.  Produces element of nat. No behavior.
    [S] is constructor/function of 1 arg.  Produces element of nat. No behavior.
    [pred], [minustwo], etc., are functions with _computation rules_ that define behavior of [nat]. **)

(** Pattern matching and recusion needed for infinite structures like numbers.
    E.G., to check a number [n] is even, recursively check if [n-2] is even.
    Define recursive functions with [Fixpoint]. *)

Fixpoint evenb (n:nat) : bool :=
  match n with
  |      O   => true
  |    S O   => false
  | S (S n') => evenb n'
  end.

(** [evenb] does _structural recursion_ over arg [n].
    Recurses only on strictly smaller values of [n], impling all calls to [evenb] terminate.
    Coq requires some argument of [Fixpoint] definition is decreasing. *)

Definition oddb (n:nat) : bool   :=   negb (evenb n).

Example test_oddb1:    (oddb (S O)) = true.
Proof. reflexivity.  Qed.
Example test_oddb2:    (oddb (S (S (S (S O))))) = false.
Proof. reflexivity.  Qed.

Module Playground2.

(** Multi-arg recursive functions. *)

Fixpoint plus (n : nat) (m : nat) : nat :=
  match n with
    |   O  => m
    | S n' => S (plus n' m)
  end.

Eval simpl in (plus (S (S (S O))) (S (S O))).
   (* ==> 5 : nat *)
Eval simpl in (plus 3 2).
   (* ==> 5 : nat *)


Eval simpl in (plus (S (S (S O))) (S (S O))).
   (* ==> 5 : nat *)
Eval simpl in (plus 3 2).
   (* ==> 5 : nat *)

(** Simplification of above visualized as: *)

(*  [plus (S (S (S O))) (S (S O))   ] 2nd clause applies
==> [S (plus (S (S O))  (S (S O)))  ] 2nd clause applies
==> [S (S (plus (S O)   (S (S O)))) ] 2nd clause applies
==> [S (S (S (plus O    (S (S O)))))] 1st clause applies
==> [S (S (S            (S (S O)))) ] result
*)

(** Notation: [(n m : nat)] same as [(n : nat) (m : nat)]. *)

Fixpoint mult (n m : nat) : nat :=
  match n with
    |   O  => O
    | S n' => plus m (mult n' m)
  end.

Example test_mult1: (mult 3 3) = 9.
Proof. reflexivity.  Qed.

(** Match two expressions at once by putting a comma between them.
    [_] avoids inventing an unused variable name. *)

Fixpoint minus (n m:nat) : nat :=
  match n, m with
  |   O ,   _  => O
  | S _ ,   O  => n
  | S n', S m' => minus n' m'
  end.

(*  [minus (S (S (S O))) (S (S O))] 3rd applies
==> [minus    (S (S O))     (S O) ] 3rd applies
==> [minus       (S O)         O  ] 2nd applies
==> [            (S O)            ] result
*)

(*  [minus (S (S O)) (S (S (S O)))] 3rd applies
==> [minus    (S O)     (S (S O)) ] 3rd applies
==> [minus       O         (S O)  ] 1st applies
==> [            O                ] result
*)

(* BEGIN HC EXTRA *)

Fixpoint hc_div_by_2' (n a : nat) : nat :=   (* [a] is accumulator *)
  match n with
    |      O   => a
    |    S O   => a
    | S (S n') => hc_div_by_2' n' (plus a 1) (* count how many times you can subtract 2 *)
  end.

(* Cannot define third case as : div (minus (S n') 2) m (plus a 1)
   because "Error: Cannot guess decreasing argument of fix." *)

Definition hc_div_by_2 (n : nat) : nat := hc_div_by_2' n 0.

Example hc_div_2_17: (hc_div_by_2 17) = 8.
Proof. simpl.   reflexivity.  Qed.
Example hc_div_2_10: (hc_div_by_2 10) = 5.
Proof. simpl.   reflexivity.  Qed.
Example hc_div_2_9: (hc_div_by_2 9) = 4.
Proof. simpl.   reflexivity.  Qed.
Example hc_div_2_8: (hc_div_by_2 8) = 4.
Proof. simpl.   reflexivity.  Qed.
Example hc_div_2_7: (hc_div_by_2 7) = 3.
Proof. simpl.   reflexivity.  Qed.
Example hc_div_2_2: (hc_div_by_2 2) = 1.
Proof. simpl.   reflexivity.  Qed.
Example hc_div_2_1: (hc_div_by_2 1) = 0.
Proof. simpl.   reflexivity.  Qed.
Example hc_div_2_0: (hc_div_by_2 0) = 0.
Proof. simpl.   reflexivity.  Qed.

(* END HC EXTRA *)

End Playground2.

(* same pattern as mult definition *)
Fixpoint exp (base power : nat) : nat :=
  match power with
    |   O => S O
    | S p => mult base (exp base p)
  end.

Example test_exp1:             (exp 3 3) = 27.
Proof. reflexivity.  Qed.

(** **** Exercise: 1 star (factorial) *)

Fixpoint factorial (n:nat) : nat :=
  match n with
  |   O  => 1
  | S n' => mult n (factorial n')
  end.

Example test_factorial1:          (factorial 3) = 6.
Proof. reflexivity. Qed.
Example test_factorial2:          (factorial 5) = (mult 10 12).
Proof. reflexivity. Qed.
(** [] *)

(** Notation: Instruct parser/printer to accept/display
    [x + y] for [plus x y]. *)

Notation "x + y" := (plus x y)
                       (at level 50, left associativity)
                       : nat_scope.
Notation "x - y" := (minus x y)
                       (at level 50, left associativity)
                       : nat_scope.
Notation "x * y" := (mult x y)
                       (at level 40, left associativity)
                       : nat_scope.

Check ((0 + 1) + 1).
(* ===> 0 + 1 + 1 : nat *)

(** Each notation-symbol is active in a _notation scope_.
    Coq tries to guess.
    For [S(O*O)] it guesses [nat_scope].
    For the cartesian product (tuple) type [bool*bool] it guesses [type_scope].
    To help Coq, use "percent-notation": [(x*y)%nat].
    Coq's feedback will use [%nat] to indicate notation scope.

    Apply to numeral notation (3,4,5, etc.).
    [0%nat] means [O],
    [0%Z] means Integer zero. *)


(** Nothing built-in, even equality testing for numbers. *)
(** Note use of nested [match]es.
    Could have used simultaneous match, like in [minus] above.)  *)

(** equality *)

(* Subtract one from both until one arg is zero, then check other. *)

Fixpoint beq_nat (n m : nat) : bool :=
  match n with
  |   O  => match m with
            |   O  => true
            | S m' => false
            end
  | S n' => match m with
            |   O  => false
            | S m' => beq_nat n' m' (* i.e., minus *)
            end
  end.

(** [l]ess-or-[e]qual *)

Fixpoint ble_nat (n m : nat) : bool :=
  match n with
  |   O  => true  (* m can only be zero or greater *)
  | S n' => match m with
            |   O  => false
            | S m' => ble_nat n' m'
            end
  end.

Example test_ble_nat1:             (ble_nat 2 2) = true.
Proof. simpl. reflexivity.  Qed.
Example test_ble_nat2:             (ble_nat 2 4) = true.
Proof. simpl. reflexivity.  Qed.
Example test_ble_nat3:             (ble_nat 4 2) = false.
Proof. simpl. reflexivity.  Qed.


(** **** Exercise: 2 stars (blt_nat) *)
(** [l]ess-[t]han.  Don't use [Fixpoint]. Use previously defined function.

    Note: If [simpl] tactic does not work, use [compute].
    Elegant definition only needs [simpl]. *)

Definition blt_nat (n m : nat) : bool :=
  andb (ble_nat n m) (negb (beq_nat n m))
  .
(* Note: [simpl] works but does not show reduction whereas [compute] does. *)
Example test_blt_nat1:             (blt_nat 2 2) = false.
Proof. simpl.   reflexivity.  Qed.
Example test_blt_nat1':            (blt_nat 2 2) = false.
Proof. compute. reflexivity.  Qed.
Example test_blt_nat2:             (blt_nat 2 4) = true.
Proof. compute. reflexivity.  Qed.
Example test_blt_nat3:             (blt_nat 4 2) = false.
Proof. compute. reflexivity.  Qed.
(** [] *)

(* BEGIN HC EXTRA *)

Fixpoint hc_bge_nat (n m : nat) : bool :=
  match m with
  |   O  => true
  | S m' => match n with
            |   O  => false
            | S n' => hc_bge_nat n' m'
            end
  end.

Example test_hc_bge_nat1:             (hc_bge_nat 2 2) = true.
Proof. compute. reflexivity. Qed.
Example test_hc_bge_nat2:             (hc_bge_nat 2 4) = false.
Proof. compute. reflexivity. Qed.
Example test_hc_bge_nat3:             (hc_bge_nat 4 2) = true.
Proof. compute. reflexivity. Qed.

Theorem hc_gt_suc_0 : forall n : nat,
  hc_bge_nat (S n) 0 = true.
Proof.
  simpl.
  reflexivity.
Qed.

(* HC/TODO *)
Theorem hc_lt_n_suc_n : forall n : nat,
  blt_nat n (S n) = true.
Proof.
  intros n. destruct n as [| n'].
  (* n is 0 *)
  simpl. reflexivity.
  (* n is S n' *)
Admitted.

(* HC/TODO *)
Theorem hc_ge_suc_n : forall n : nat,
  hc_bge_nat (S n) n = true.
Proof.
  intros n. destruct n as [| n'].
  (* n is 0 *)
  simpl. reflexivity.
  (* n is S n' *)
Admitted.

(* general div n m *)

(*
0->9/2
1->7/2
2->5/2
3->3/2
4->1/2

0->4/2
1->2/2
2->0/2
*)

(* k stands for "kluge".
   It only exists to get a recursive decreasing argument. *)

Fixpoint hc_div' (n m a k : nat) : nat :=
  match (blt_nat n m) with
  | true  => a
  | false => match k with
             |   0  => 1234
             | S k' => hc_div' (minus n m) m (S a) k'
             end
  end.

Definition hc_div (n m : nat) : nat :=
  match (orb (orb (beq_nat n 0) (beq_nat m 0)) (blt_nat n m)) with
  | true  => 0
  | false => match (beq_nat n m) with
             | true  => 1
             | false => hc_div' n m 0 n
             end
  end.

Example test_hc_div_3_0: (hc_div 3 0) = 0.
Proof. compute. reflexivity. Qed.
Example test_hc_div_0_3: (hc_div 0 3) = 0.
Proof. compute. reflexivity. Qed.
Example test_hc_div_1_2: (hc_div 1 2) = 0.
Proof. compute. reflexivity. Qed.
Example test_hc_div_2_2: (hc_div 2 2) = 1.
Proof. compute. reflexivity. Qed.
Example test_hc_div_3_2: (hc_div 3 2) = 1.
Proof. compute. reflexivity. Qed.
Example test_hc_div_4_2: (hc_div 4 2) = 2.
Proof. compute. reflexivity. Qed.
Example test_hc_div_5_2: (hc_div 5 2) = 2.
Proof. compute. reflexivity. Qed.
Example test_hc_div_6_2: (hc_div 6 2) = 3.
Proof. compute. reflexivity. Qed.
Example test_hc_div_7_2: (hc_div 7 2) = 3.
Proof. compute. reflexivity. Qed.
Example test_hc_div_8_2: (hc_div 8 2) = 4.
Proof. compute. reflexivity. Qed.
Example test_hc_div_9_2: (hc_div 9 2) = 4.
Proof. compute. reflexivity. Qed.

Example test_hc_div_2_3: (hc_div 2 3) = 0.
Proof. compute. reflexivity. Qed.
Example test_hc_div_3_3: (hc_div 3 3) = 1.
Proof. compute. reflexivity. Qed.
Example test_hc_div_4_3: (hc_div 4 3) = 1.
Proof. compute. reflexivity. Qed.
Example test_hc_div_5_3: (hc_div 5 3) = 1.
Proof. compute. reflexivity. Qed.
Example test_hc_div_6_3: (hc_div 6 3) = 2.
Proof. compute. reflexivity. Qed.
Example test_hc_div_7_3: (hc_div 7 3) = 2.
Proof. compute. reflexivity. Qed.
Example test_hc_div_8_3: (hc_div 8 3) = 2.
Proof. compute. reflexivity. Qed.
Example test_hc_div_9_3: (hc_div 9 3) = 3.
Proof. compute. reflexivity. Qed.
Example test_hc_div_10_3: (hc_div 10 3) = 3.
Proof. compute. reflexivity. Qed.

(* HC/TODO - but probably can't given klugey definition of div above.
Theorem mult_div_inverse : forall n m : nat,
  (hc_div (mult n m) m) = n.
*)

(* END HC EXTRA *)


(* ###################################################################### *)
(** * Proof by Simplification *)

(** How to state and prove properties of datatypes and functions.

    [Example] makes claim about behavior of function on particular inputs.

    Proofs, so far, use function's definition to simplify exprs on both
    sides of [=] then see if they become identical.

    "Proof by simplification" does more.

    E.G.,
    Fact : [0] is a "neutral element" for [+] on the left.
    Proved by observing that [0 + n] reduces to [n] for all [n],
    _since definition of [+] is recursive in its first argument_. *)

Theorem plus_O_n : forall n:nat,
  0 + n = n.
Proof.
  simpl.       (* uses def of plus that does structure matching on left arg *)
  reflexivity. (* true since forall n : nat, n = n *)
Qed.

(* tactic:simpl
   tactic:reflexivity *)

(** [reflexivity] implicitly simplifies both sides of [=] before
    testing to see if same, so can omit [simpl.].
    But then simplifiation step in not visible interaction. *)

Theorem plus_O_n' : forall n:nat,
  0 + n = n.
Proof.
  reflexivity.
Qed.

(** [reflexivity] does more than [simpl] -- it tries "unfolding"
    defined terms, replacing them with their right-hand sides.

    Reason for difference: when reflexivity succeeds, whole goal is
    finished - need to look at whatever expanded expressions
    [reflexivity] has found.

    [simpl] is used in situations where we may have to read and
    understand the new goal, so we do not want it expanding
    definitions. *)

(** Keywords [Example], [Theorem], [Lemma], [Fact], [Remark], ...,
    mean the same thing.

    Also added quantifier [forall n:nat].

    Keywords [simpl] and [reflexivity] are _tactics_.

    Tactic is command used between [Proof] and [Qed] to check
    correctness of claim. *)

(* ###################################################################### *)
(** * The [intros] Tactic *)
(* tactic:intros *)

(** Unit tests (i.e., [Example]) apply functions to particular arguments.

    Most properties used in proving will begin
    - with quantifiers (e.g.,"for all numbers [n], ...")
    - and/or hypothesis ("assuming [m=n], ...").
    By _assuming the hypothesis_ we start by saying
    "suppose [n] is some arbitrary number," or "suppose [m=n]."

    [intros] tactic enables moving one or more quantifiers or
    hypotheses from the goal to a "context" of current assumptions.

    E.G., slightly different proof of same theorem: *)

Theorem plus_O_n'' : forall n:nat,
  0 + n = n.
Proof.
  intros n. simpl. reflexivity. Qed.

(** Step through above and notice how the goal and context change. *)
(* HC: I added [simpl] so interaction shows reduction. *)

Theorem plus_1_l : forall n:nat,
  1 + n = S n.
Proof.
  intros n. simpl. reflexivity. Qed.

Theorem mult_0_l : forall n:nat,
  0 * n = 0.
Proof.
  intros n. simpl. reflexivity. Qed.

(** [_l] suffix is pronounced "on the left." *)

(** **** Exercise: 1 star, optional (simpl_plus) *)
(** What prints for the following? *)

Eval simpl in (forall n:nat, n + 0 = n).
(** ===>
     = forall n : nat, n + 0 = n
     : Prop
*)

Eval simpl in (forall n:nat, 0 + n = n).
(** ===>
     = forall n : nat, n = n
     : Prop
*)

(** Explain the difference. *)
(** The second uses the definition of [plus] (proved in [plus_0_n]) to
    simplify the left-hand side.  The first has no corresponding
    definition the does matching on right side, so just returns what
    it is given. *)


(* ###################################################################### *)
(** * Proof by Rewriting *)
(* tactic:rewrite *)
(** Theorem using implication: *)

Theorem plus_id_example : forall n m:nat,
  n = m ->
  n + n = m + m.

(** Arrow symbol ([->]) pronounced "implies."

    Does not make universal claim about all numbers [n], [m].

    Only talks about specialized property that only holds when [n = m].

    Since [n] and [m] are arbitrary numbers, can't use simplification
    to prove this theorem (i.e., no structure to match on).

    Instead, prove by observing:

    assume [n = m],
    then replace [n] with [m] in the goal statement
    and obtain equality with same expr on both sides.

    Use [rewrite] tactic to perform replacement. *)

Proof.
  intros n m.   (* move universally quantified variables [n], [m] into context *)
  intros H.     (* move hypothesis [n = m] into context and give it the name [H] *)
  rewrite -> H. (*  Third rewrites current goal, [n + n = m + m] assuming H:
                    replaces left side of equality hypothesis [H] with right side
                    (i.e., replaces [n] with [m]) leaving: [m + m = m + m]. *)
  reflexivity.
Qed.


(** Diff between two versions of proof:
    The first gives  m + m = m + m
    the second gives n + n = n + n
*)

(** **** Exercise: 1 star (plus_id_exercise) *)

Theorem plus_id_exercise : forall n m o : nat,
  n = m   ->   m = o   ->   n + m = m + o.
Proof.
  intros n m o. (* n = m -> m = o -> n + m = m + o                    *)
  intros H.     (*          m = o -> n + m = m + o ; assume H : n = m *)
  rewrite -> H. (*          m = o -> m + m = m + o ; changes all n to m via applying assumption H *)
  intros J.     (*                   m + m = m + o ; assume J : m = o *)
  rewrite -> J. (*                   o + o = o + o ; changes all m to o via applying assumption J *)
  reflexivity.
Qed.
(** [] *)

(** Can use [rewrite] tactic with previously proved
    theorem instead of a hypothesis from the context. *)

Theorem mult_0_plus : forall n m : nat,
  (0 + n) * m = n * m.
Proof.
  intros n m.
  simpl.     (* changes [(0 + n) * m] to [n * m] by def of [plus] *)
  (* rewrite -> plus_O_n. could also do this instead of simpl *)
  reflexivity.
Qed.


(** **** Exercise: 2 stars (mult_S_1) *)
Theorem mult_S_1 : forall n m : nat,
  m = S n ->
  m * (1 + n) = m * m.
Proof.
  intros n m H.
  simpl.
  rewrite H.
  reflexivity.
Qed.
(** [] *)

(** **** Exercise: 2 stars, recommended (mult_1_plus) *)
Theorem mult_1_plus : forall n m : nat,
  (1 + n) * m = m + (n * m).
Proof.
  intros n m.
  simpl.
  (* rewrite -> plus_1_l. can also do this *)
  simpl.
  reflexivity.
Qed.



(* ###################################################################### *)
(** * Proof by Case Analysis *)
(* tactic:destruct *)
(** Proving by simple calculation is often not enough.  Unknown,
    hypothetical values (arbitrary numbers, booleans, lists, etc.) can
    show up in the "head position" of functions we want to reason
    about, blocking simplification.  E.G., trying [simpl] tactic
    leads to deadend : *)

Theorem plus_1_neq_0_firsttry : forall n : nat,
  beq_nat (n + 1) 0 = false.
Proof.
  intros n.
  simpl.  (* does nothing! *)
Abort.

(** ... because defs of [beq_nat] and [+] pattern [match] on
    first arg.  Here, first arg to [+] is unknown [n] and first arg to
    [beq_nat] is compound expr [n + 1]; neither can be simplified.

    Instead, consider possible forms of [n] separately.

    1. If [n] is [O] : we can calculate final result of [beq_nat (n + 1) 0]
    and check that it is [false].

    2. If [n = S n'] for some [n']:
    we don't know what number [n + 1] yields,
    but we can calculate that it begins with one [S].
    Enough to calculate that [beq_nat (n + 1) 0] yields [false].

    [destruct] : tactic to separately consider cases
    where [n = O] and where [n = S n']. *)

Theorem plus_1_neq_0 : forall n : nat,
  beq_nat (n + 1) 0 = false.
Proof.
  intros n. destruct n as [| n'].
  simpl. reflexivity. (* by def of plus 0 on left *)
  simpl. reflexivity. (* by def of plus and beq_nat *)
Qed.


(** [destruct] generates _two_ subgoals, each proved separately.

    When first subgoal proved, it disappears, then other "in focus."

    "[as [| n']]" called an "intro pattern."

    INTRO says what variable names to introduce in each subgoal.
    Square brackets contains _list_ of lists of names, separated by [|].

    Here, first component empty, since [O] constructor is
    nullary (it doesn't carry any data).  Second component gives a
    single name, [n'], since [S] is a unary constructor.

    [destruct] tactic can be used with any inductively defined datatype.

    E.G., to prove that boolean negation is involutive (i.e, negation
    is its own inverse): *)

Theorem negb_involutive : forall b : bool,
  negb (negb b) = b.
Proof.
  intros b. destruct b.
  simpl. reflexivity. (* double neg of true  is true  *)
  simpl. reflexivity. (* double neg of false is true *)
Qed.

(** Here [destruct] has no [as] clause because no subcases of
    [destruct] need to bind any variables.

    Could have written "[as [|]]", or "[as []]". *)

(** **** Exercise: 1 star (zero_nbeq_plus_1) *)
Theorem zero_nbeq_plus_1 : forall n : nat,
  beq_nat 0 (n + 1) = false.
Proof.
  intros b. destruct b.
  reflexivity.
  reflexivity.
Qed.

Theorem zero_nbeq_plus_1' : forall n : nat,
  beq_nat 0 (n + 1) = false.
Proof.
  intros n. destruct n as [| n'].
  simpl. reflexivity. (* by def of 0 on left and beq *)
  simpl. reflexivity. (* by def of beq *)
Qed.
(** [] *)

(* ###################################################################### *)
(** * More Exercises *)

(** **** Exercise: 2 stars (boolean functions) *)
(** Prove using tactics from above. *)

Theorem identity_fn_applied_twice :
  forall (f : bool -> bool),
  (forall (x : bool), f x = x) ->
  forall (b : bool), f (f b) = b.
Proof.
  intros fbb fx ffb. destruct ffb.
  (* true *)   (* fbb (fbb true)  = true  *)
  rewrite fx.  (*      fbb true   = true  *)
  rewrite fx.  (*          true   = true  *)
  reflexivity.
  (* false *)  (* fbb (fbb false) = false *)
  rewrite fx.  (*      fbb false  = false *)
  rewrite fx.  (*          false  = false *)
  reflexivity.
Qed.

(** State/prove theorem similar to previous,
    but second hypothesis says [f] has property [f x = negb x].*)

Theorem negation_fn_applied_twice :
  forall (f : bool -> bool),
  (forall (x : bool), f x = negb x) ->
  forall (b : bool), f (f b) = b.
Proof.
  intros fbb fx ffb. destruct ffb.
  (* true *)                (*  fbb (fbb  true) = true  *)
  rewrite fx.               (* negb (fbb  true) = true *)
  rewrite fx.               (* negb (negb true) = true  *)
  rewrite negb_involutive.  (*            true  = true  *)
  reflexivity.
  (* false *)
  rewrite fx.
  rewrite fx.
  rewrite negb_involutive.
  reflexivity.
Qed.

(** **** Exercise: 2 stars (andb_eq_orb) *)
(** Prove the following theorem.  (You may want to first prove a
    subsidiary lemma or two.) *)

Theorem andb_eq_orb :
  forall (b c : bool),
  (andb b c = orb b c) ->
  b = c.
Proof.
  intros b c.
  destruct b.
  (* b true *)       (*    andb true c = orb true c -> true = c *)
    destruct c.
      (* c true *)   (*    andb true true = orb true true -> true = true *)
      reflexivity.
      (* c false *)  (*    andb true false = orb true false -> true = false *)
      simpl.         (*    false = true -> true = false *)
      intros.        (*    true = false *)
      rewrite H.     (*    true = true *)
      reflexivity.
  (* b false *)
    simpl.           (*    andb false c = orb false c -> false = c *)
    intros.          (*    false = c *)
    rewrite H.       (*    c = c *)
    reflexivity.
Qed.

(** **** Exercise: 3 stars (binary) *)
(** Different (more efficient) representation of natural numbers.
    Use binary (rather than above unary) : A number is:

      - zero,
      - twice a binary number, or
      - one more than twice a binary number.

    (a) Write inductive definition of type [bin] for this description.

    Hint:
    Inductive nat : Type :=
      | O : nat
      | S : nat -> nat.
    does not say what [O] and [S] "mean".  Just gives structure.
    Interpretation of [O] as zero and [S] as successor/plus one
    comes from the way we use nat values in functions and proofs. *)
(* Took type def from:
   http://staff.ustc.edu.cn/~xyfeng/teaching/TOPL/assignments/Answer_AS1.txt
   http://staff.ustc.edu.cn/~bjhua/courses/theory/2012/slides/lec1notes.html
*)
Inductive bin : Type :=
  | Z : bin
  | T : bin -> bin
  | M : bin -> bin.


(** (b) 1. Write increment function for bin. *)
(* Took incr def from:
   http://staff.ustc.edu.cn/~xyfeng/teaching/TOPL/assignments/Answer_AS1.txt
   http://staff.ustc.edu.cn/~bjhua/courses/theory/2012/slides/lec1notes.html
*)
Fixpoint incr (b: bin) : bin :=
  match b with
  | Z    => M Z
  | T b' => M b'
  | M b' => T (incr b')
  end.

Example binz0:                                      Z      =         Z.   (*   0 *)
Proof. reflexivity. Qed.
Example incr1:                                (incr Z)     =       M Z.   (*   1 *)
Proof. reflexivity. Qed.
Example incr2:                           incr (incr Z)     =    T (M Z).  (*  10 *)
Proof. reflexivity. Qed.
Example incr3:                     incr (incr (incr Z))    =    M (M Z).  (*  11 *)
Proof. reflexivity. Qed.
Example incr4:               incr (incr (incr (incr Z)))   = T (T (M Z)). (* 100 *)
Proof. reflexivity. Qed.
Example incr5:         incr (incr (incr (incr (incr Z))))  = M (T (M Z)). (* 101 *)
Proof. reflexivity. Qed.
Example incr6:   incr (incr (incr (incr (incr (incr Z))))) = T (M (M Z)). (* 110 *)
Proof. reflexivity. Qed.

(** (b) 2. Write function to convert bin to nat. *)
(* HC: did rest on my own. *)
Fixpoint b2n (b: bin) : nat :=
  match b with
  | Z    => 0
  | T b' => 2 * (b2n b')
  | M b' => 2 * (b2n b') + 1
  end.

Example b2n0:             b2n          Z    = 0. (*   0 *)
Proof. reflexivity. Qed.
Example b2n1:             b2n       (M Z)   = 1. (*   1 *)
Proof. reflexivity. Qed.
Example b2n2:             b2n    (T (M Z))  = 2. (*  10 *)
Proof. reflexivity. Qed.
Example b2n3:             b2n    (M (M Z))  = 3. (*  11 *)
Proof. reflexivity. Qed.
Example b2n4:             b2n (T (T (M Z))) = 4. (* 100 *)
Proof. reflexivity. Qed.
Example b2n5:             b2n (M (T (M Z))) = 5. (* 101 *)
Proof. reflexivity. Qed.
Example b2n6:             b2n (T (M (M Z))) = 6. (* 110 *)
Proof. reflexivity. Qed.

(** (c) Write unit tests showing that incrementing a binary number and
        then converting it to unary should yield the same result as first
        converting it to unary and then incrementing.
*)


(* ###################################################################### *)
(** * Optional Material *)

(** ** More on Notation *)

Notation "x + y" := (plus x y)
                       (at level 50, left associativity)
                       : nat_scope.
Notation "x * y" := (mult x y)
                       (at level 40, left associativity)
                       : nat_scope.

(** For each notation-symbol can specify its _precedence level_ (via [at level n])
    and its _associativity_.

    Helpful to disambiguate expressions containing different symbols.

    Associativity helpful to disambiguate expressions containing more
    occurrences of same symbol.

    E.G., parameters for [+] and [*] say [1+2*3*4] is shorthand for [(1+((2*3)*4))].

    Coq uses precedence levels from 0 to 100, and _left_, _right_, or
    _no_ associativity.

    Each notation-symbol active in a _notation scope_.
    Coq guesses what scope you mean:
    For [S(O*O)] it guesses [nat_scope].
    For cartesian product (tuple) type [bool*bool] it guesses [type_scope].
    To help Coq, write [(x*y)%nat].
    Sometimes Coq prints [%nat] to indicate what scope a notation is in.

    Notation scopes also apply to numeral notation (3,4,5, etc.)
    Therefore  may sometimes see
    - [0%nat] : means [O]
    - [0%Z]   : means Integer zero.
*)

(** ** [Fixpoint]s and Structural Recursion *)

Fixpoint plus' (n : nat) (m : nat) : nat :=
  match n with
    |   O  => m
    | S n' => S (plus' n' m)
  end.

(** Coq checks this:
    - notes [plus'] is "decreasing on 1st argument."
    - means performing _structural recursion_ over argument [n]
      - i.e., recursive calls only on strictly smaller values of [n].
    - implies all calls [plus'] will terminate.
    - Coq requires some argument of _every_ [Fixpoint] definition is "decreasing".

    Because Coq's "decreasing analysis" is not sophisticated, sometimes
    necessary to write functions in unnatural ways. *)

(** **** Exercise: 2 stars, optional (decreasing) *)
(** Write a sensible [Fixpoint] definition (e.g., on numbers) that
    _does_ terminate on all inputs, but Coq will _not_ accept because
    of restriction. *)

(*
Fixpoint factorialNO (n:nat) : nat :=
  match n with
  | O => 1
  | _ => mult n (factorialNO (minus n 1))
  end.
*)
(*
Fixpoint factorialNO' (n:nat) : nat :=
  match n with
  | O => 1
  | _ => mult n (factorialNO' (pred n))
  end.
*)

(** [] *)

(* $Date: 2013-07-17 16:19:11 -0400 (Wed, 17 Jul 2013) $ *)

