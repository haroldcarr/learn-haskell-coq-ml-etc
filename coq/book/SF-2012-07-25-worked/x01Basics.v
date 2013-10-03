(** Summary:

    Tactics: [simpl], [reflexivity], [intros], [rewrite],
             [destruct], [Case], [induction], [assert], [replace]

    Theorems: [hc_gt_suc_0], [plus_0_n], [plus_1_l], [mult_0_1], [mult_0_plus], [mult_1_plus], [plus_1_neq_0],
              [negb_involutive], [zero_nbeq_plus_1], [andb_true_elim1], [andb_true_elim2], [plus_0_r], [minus_diag],
              [mult_0_r], [plus_n_Sm], [plus_comm], [double_plus], [plus_assoc], [beq_nat_refl], [plus_swap], [hc_plus_1_r], [hc_mult_1_r],
              [hc_mult_n_Sm], [mult_comm], [evenb_n__oddb_Sn], [ble_nat_refl], [zero_nbeq_S], [andb_false_r],
              [plus_ble_compat_l], [S_nbeq_0], [mult_1_l], [mult_plus_distr_r], [mult_assoc]
*)


(** enumerated types: explicitly enumerate finite set of elements. *)

(** Inductive : from particular to general.
    Dedcutive : from general to particular. *)

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

Eval simpl in (next_weekday friday).
   (* ==> monday : day *)
Eval simpl in (next_weekday (next_weekday saturday)).
   (* ==> tuesday : day *)

(** Note: Square brackets delimit fragments of Coq code in comments. *)
(** keyword [simpl] ("simplify") says how to evaluate the given expression. *)

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

Proof.
  simpl.       (* reduction step uses definition of [next_weekday] *)
  reflexivity. (* relation ~ on set S is reflexive when x~x is true for all x : S *)
Qed.

(** Says: "Assertion proved by observing both sides of equality are
    same after simplification." *)

Print test_next_weekday.

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

Example test_orb1:  (orb true  false) = true.
Proof. simpl. reflexivity.  Qed.
Example test_orb2:  (orb false false) = false.
Proof. simpl. reflexivity.  Qed.
Example test_orb3:  (orb false true)  = true.
Proof. simpl. reflexivity.  Qed.
Example test_orb4:  (orb true  true)  = true.
Proof. simpl. reflexivity.  Qed.

(** "magic": [admit] to plug a hole in an incomplete definition or
    proof.  In exercises, replace [admit] or [Admitted] with real
    definitions or proofs. *)

Definition admit {T: Type} : T.  Admitted.

(** Says "Don't feel like proving now---trust me."  Useful
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

(* ###################################################################### *)
(** ** Function Types *)

(** [Check] : print the type of an expression.  *)

Check true.
(* ===> true : bool *)
Check (negb true).
(* ===> negb true : bool *)
Check negb.
(* ===> negb : bool -> bool *)
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
    - expressions formed in these two ways are the only ones belonging
      to the set [nat]. *)

(** Implies: exprs [O], [S O], [S (S O)], [S (S (S O))], ...,
    belong to set [nat].
    Other exprs (e.g., [true], [andb true false], [S (S false)]) do not. *)

(** [Inductive] definition of [nat] does not say what [O] and [S] "mean".
    Just gives structure.
    Interpretation of [O] as zero and [S] as successor (or plus one)
    comes from the way we use [nat] values in functions and proofs. *)

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

(** Pattern matching + recusion needed for infinite structures like numbers.
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

(** Define [oddb] with [Fixpoint] or with previous functions: *)

Definition oddb (n:nat) : bool   :=   negb (evenb n).

(* [simpl] does not show reduction.  [compute] does show reduction. *)
Example test_oddb1:    (oddb          (S O))    = true.
Proof. simpl.   reflexivity.  Qed.
Example test_oddb1':   (oddb          (S O))    = true.
Proof. compute. reflexivity.  Qed.
Example test_oddb2:    (oddb (S (S (S (S O))))) = false.
Proof. simpl.   reflexivity.  Qed.

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

(* same pattern as definition of plus *)
Fixpoint exp (base power : nat) : nat :=
  match power with
    |   O => S O
    | S p => mult base (exp base p)
  end.

Example test_mult1:             (mult 3 3) = 9.
Proof. reflexivity.  Qed.

(** **** Exercise: 1 star (factorial) *)

Fixpoint factorial (n:nat) : nat :=
  match n with
  |   O  => 1
  | S n' => mult n (factorial n')
  end.

Example test_factorial1:          (factorial 3) = 6.
Proof. simpl. reflexivity.  Qed.
Example test_factorial2:          (factorial 5) = (mult 10 12).
Proof. simpl. reflexivity.  Qed.

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
  simpl. reflexivity.
Admitted.

(* HC/TODO *)
Theorem hc_ge_suc_n : forall n : nat,
  hc_bge_nat (S n) n = true.
Proof.
  intros n. destruct n as [| n'].
  simpl. reflexivity.
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
(** * Proof By Simplification *)

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
  simpl. (* uses definition of plus that does structure matching on left arg *)
  reflexivity.  (* true since forall n : nat, n = n *)
Qed.

(** [reflexivity] implicitly simplifies both sides of [=] before
    testing to see if same, so can omit [simpl.]. But then
    simplifiation step in not visible interaction. *)

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
(** * The [intros] Tactic *)

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

(* ###################################################################### *)
(** * Proof by Rewriting *)

(** Theorem using implication: *)

Theorem plus_id_example : forall n m:nat,
  n = m   ->   n + n = m + m.

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

(** The arrow symbol in [rewrite] is NOT implication.
    Says to apply rewrite from left to right.
    Use [rewrite <-] to rewrite right to left:  *)

Theorem plus_id_example' : forall n m:nat,
  n = m    ->   n + n = m + m.
Proof.
  intros n m.
  intros H.
  rewrite <- H. (* [n + n = n + n]. *)
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
(** * Case Analysis *)

(** Proving by simple calculation is often not enough.  Unknown,
    hypothetical values (arbitrary numbers, booleans, lists, etc.) can
    show up in the "head position" of functions we want to reason
    about, blocking simplification.  E.G., trying [simpl] tactic
    leads to deadend : *)

Theorem plus_1_neq_0_firsttry : forall n : nat,
  beq_nat (n + 1) 0 = false.
Proof.
  intros n. simpl.  (* does nothing! *)
Admitted.

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
  intros n. destruct n as [| n'].
  simpl. reflexivity. (* by def of 0 on left and beq *)
  simpl. reflexivity. (* by def of beq *)
Qed.

(* ###################################################################### *)
(** * Naming Cases *)

(** No command for moving from one branch of a case analysis to the next.
    Indentation and comments help.  Better to use [Case] tactic.

    [Case] not built in.No need to understand how it works.
    Skip definition.  Go to example. *)

Require String. Open Scope string_scope.

Ltac move_to_top x :=
  match reverse goal with
  | H : _ |- _ => try move x after H
  end.

Tactic Notation "assert_eq" ident(x) constr(v) :=
  let H := fresh in
  assert (x = v) as H by reflexivity;
  clear H.

Tactic Notation "Case_aux" ident(x) constr(name) :=
  first [
    set (x := name); move_to_top x
  | assert_eq x name; move_to_top x
  | fail 1 "because we are working on a different case" ].

Tactic Notation "Case" constr(name) := Case_aux Case name.
Tactic Notation "SCase" constr(name) := Case_aux SCase name.
Tactic Notation "SSCase" constr(name) := Case_aux SSCase name.
Tactic Notation "SSSCase" constr(name) := Case_aux SSSCase name.
Tactic Notation "SSSSCase" constr(name) := Case_aux SSSSCase name.
Tactic Notation "SSSSSCase" constr(name) := Case_aux SSSSSCase name.
Tactic Notation "SSSSSSCase" constr(name) := Case_aux SSSSSSCase name.
Tactic Notation "SSSSSSSCase" constr(name) := Case_aux SSSSSSSCase name.

(** Example of how [Case] is used.
    Step through and observe how the context changes. *)

Theorem andb_true_elim1 : forall b c : bool,
  andb b c = true -> b = true.
Proof.
  intros b c H. destruct b.
  Case "b = true".  (* true  = true *)
    reflexivity.
  Case "b = false". (* false = true *)
                    (* QUESTION: what is the state of this step of the goal? *)
    rewrite <- H.   (* false = andb false c *)
    simpl.          (* false = false *)
    reflexivity.
Qed.

(** [Case] just adds string to the context for the current goal.
    When subgoals generated, string is carried over into their contexts.

    When last of subgoals is proved and next top-level goal (a sibling
    of the current one) becomes active, string will disappear from
    context so we see case that introduced it is complete.

    Gives error message if try to execute a new [Case] tactic while
    string left by previous one is still in the context.

    [SCase] ("subcase") tactic for nested case analyses: when using
    a [destruct] to solve a goal that has itself been generated
    by a [destruct]). *)

(** **** Exercise: 2 stars (andb_true_elim2) *)
(** Prove [andb_true_elim2], marking cases (and subcases) when
    you use [destruct]. *)

Theorem andb_true_elim2 : forall b c : bool,
  andb b c = true -> c = true.
Proof.
  intros b c H.
  destruct b.
  Case "b = true".
    destruct c.
    SCase "c = true".
      reflexivity.
    SCase "c = false".
      rewrite <- H. simpl. reflexivity.
  Case "b = false".
    destruct c.
    SCase "c = true".
      reflexivity.
    SCase "c = false".
      rewrite <- H. simpl. reflexivity.
Qed.

(** No formatting rules in Coq.
    [Case] tactics help make proof readable. *)

(* ###################################################################### *)
(** * Induction *)

(** Proved above : [0] is neutral element for [+] on left -
    using [simpl] partial evaluation argument.

    To prove [0] is also a neutral element on _right_ : *)

Theorem plus_0_r_firsttry : forall n:nat,
  n + 0 = n.

(** needs different proof because [n] in [n + 0] is an unknown number,
    so "left" [match] in [+] definition can't be simplified (like
    handling andb_true_elim2 above). *)

Proof.
  intros n.
  simpl. (* Does nothing! *)
Admitted.

(** Reasoning by cases using [destruct n] does not work.
    Case [n = 0] works.
    But next case [n = S n'] for some [n']
    we end up needing infinite [destruct n']. *)

Theorem plus_0_r_secondtry : forall n:nat,
  n + 0 = n.
Proof.
  intros n. destruct n as [| n'].
  Case "n = 0".
    reflexivity. (* so far so good... *)
  Case "n = S n'".
    simpl.       (* stuck again *)
Admitted.

(** Use _induction_ to prove most interesting properties of inductive
    sets (numbers, lists, etc.).

    Principle of induction over natural numbers:

    [P(n)] is some proposition involving a natural number [n].

    Want to show that P holds for _all_ numbers [n].

    1. Show [P(O)] holds.

    2. Show for any [n'], if [P(n')] holds, then so does [P(S n')].

    3. Conclude that [P(n)] holds for all [n].

    In Coq the order is reversed:

    1. Prove [P(n)] for all [n].

    2. use [induction] tactic to break into two separate subgoals:

    3. [P(O)]

    4. [P(n') -> P(S n')].

    E.G.,: *)

Theorem plus_0_r : forall n:nat,
  n + 0 = n.
Proof.
  intros n.
  induction n as [| n'].
  Case "n = 0".
    reflexivity.
  Case "n = S n'".   (* IHn' : n' + 0  =   n' *)
                     (*      S n' + 0  = S n' *)
    simpl.           (*     S (n' + 0) = S n' by def of + *)
    rewrite -> IHn'. (*           S n' = S n' by replacing n' + 0 with n' *)
    reflexivity.
Qed.

(** [induction] tactic uses [as] to specify subgoal var names.

    1st case: [n] replaced by [0]
    goal becomes [0 + 0 = 0]
    which follows by simplification.

    2nd case: [n] replaced by [S n'] and
    assumption [n' + 0 = n'] added to the context
    with the name [IHn'], : the Induction Hypothesis for [n'].

    Goal becomes  [(S n') + 0 = S n']
    simplifies to [S (n' + 0) = S n']
    induction hypothesis replaces [n' + 0] with [n']
    then both sides the same. *)

Theorem minus_diag : forall n,
  minus n n = 0.
Proof.
  intros n. induction n as [| n'].
  Case "n = 0".    simpl. reflexivity.
  Case "n = S n'".   (* IHn' : n' -   n' = 0 *)
                     (*      S n' - S n' = 0 *)
    simpl.           (*        n' -   n' = 0 *)
    rewrite -> IHn'. (*               0  = 0 *)
    reflexivity.
Qed.

(** **** Exercise: 2 stars, recommended (basic_induction) *)

Theorem mult_0_r : forall n:nat,
  n * 0 = 0.
Proof.
  intros n. induction n as [| n'].
  Case "n = 0".    simpl. reflexivity.
  Case "n = S n'".   (* IHn' :      n' * 0  = 0 *)
                     (*           S n' * 0  = 0 *)
    simpl.           (*        0 + (n' * 0) = 0 by mult
                                    n' * 0  = 0 by plus *)
    rewrite -> IHn'. (*                  0  = 0 *)
    reflexivity.
Qed.

Theorem plus_n_Sm : forall n m : nat,
  S (n + m) = n + (S m).
Proof.
  intros n m. induction n as [| n'].
  Case "n = O".    simpl. reflexivity.
  Case "n = S n'".    (* IHn' : S (n' +   m)  =    n' + S m *)
                      (*      S (S n' +   m)  =  S n' + S m *)
    simpl.            (*     S (S (n' +   m)) = S (n' + S m) by def of + *)
    rewrite -> IHn'.  (*        S (n' + S m)  = S (n' + S m) *)
    reflexivity.
Qed.

Theorem plus_comm : forall n m : nat,
  n + m = m + n.
Proof.
  intros n m. induction n as [| n'].
  Case "n = 0".            (*           0 + m = m + 0         *)
    rewrite -> plus_0_r.   (*           0 + m = m             *)
    simpl.                 (*               m = m by def of + *)
    reflexivity.
  Case "n = S n'".         (* IHn' : n' + m   =    m +   n'   *)
                           (*      S n' + m   =    m + S n'   *)
    simpl.                 (*     S (n' + m)  =    m + S n'   *)
    rewrite <- plus_n_Sm.  (*     S (n' + m)  = S (m +   n')  *)
    rewrite -> IHn'.       (*     S (m +  n') = S (m +   n')  *)
    reflexivity.
Qed.

Fixpoint double (n:nat) :=
  match n with
  | O    => O
  | S n' => S (S (double n'))
  end.

Eval simpl in (double 16).
  (* ===> 32 : nat *)

(** **** Exercise: 2 stars (double_plus) *)
Lemma double_plus : forall n,
  double n = n + n .
Proof.
  intros n. induction n as [| n'].
  Case "n = 0".
    simpl. reflexivity.
  Case "n = S n'".        (*  IHn' : double    n'   =       n' +   n'   *)
                          (*         double (S n')  =     S n' + S n'   *)
    simpl.                (*   S (S (double    n')) =    S (n' + S n') left by def of double, right by def of + *)
    rewrite <- plus_n_Sm. (*   S (S (double    n')) = S (S (n' +   n')) *)
    rewrite -> IHn'.      (*   S (S (n'      + n')) = S (S (n' +   n')) *)
    reflexivity.
Qed.

(** **** Exercise: 1 star (destruct_induction) *)
(** Briefly explain the difference between the tactics
    [destruct] and [induction].

    [destruct] splits into two cases: [n = 0] and [n = S n'], each
    case is then proved separately.

    [induction] also splits into same two cases, but also adds the
    assumption to the context, with a name so you can use it in the
    proof of the general case. *)

(* ###################################################################### *)
(** * Formal vs. Informal Proof *)

(** "Informal proofs are algorithms; formal proofs are code." *)

(** What constitutes a "proof" of a mathematical claim has challenged philosophers for millenia.

    Rough definition: a proof of mathematical proposition [P] is written/spoken text that gives
    reader/hearer certainty that [P] is true.

    Proof is an act of communication.

    Reader can be human being, with proof written a natural language, thus _informal_.

    Different sorts of readers (e.g., Coq).

    "Belief" is a mechanical check that [P] can be derived from set of logical rules.

    Proof is recipe that guides program in performing check.
    Such recipes are _formal_ proofs. *)

(** Formal proof that addition is associative: *)

Theorem plus_assoc : forall n m p : nat,
  n + (m + p) = (n + m) + p.
Proof.
  intros n m p. induction n as [| n'].
  Case "n = 0". reflexivity.
  Case "n = S n'".   (* IHn' : n' + (m + p)  =    n' + m + p  *)
                     (*      S n' + (m + p)  =  S n' + m + p  *)
    simpl.           (*     S (n' + (m + p)) = S (n' + m + p) by def of + *)
    rewrite -> IHn'. (*     S (n' +  m + p)  = S (n' + m + p) *)
    reflexivity.
Qed.

(** This formal proof hard for human to understand.
    A mathematician mighty write it like this:

    -
    _Theorem_: For any [n], [m] and [p],
      n + (m + p) = (n + m) + p.
    _Proof_: By induction on [n].

    - First, suppose [n = 0].  We must show
        0 + (m + p) = (0 + m) + p.
      This follows directly from the definition of [+].

    - Next, suppose [n = S n'], where
        n' + (m + p) = (n' + m) + p.
      We must show
        (S n') + (m + p) = ((S n') + m) + p.
      By the definition of [+], this follows from
        S (n' + (m + p)) = S ((n' + m) + p),
      which is immediate from the induction hypothesis. []

    Overall form similar.  Differences of detail: formal proof more
    explicit in some ways (e.g., use of [reflexivity]) but much less
    explicit in others; in particular, the "proof state" at any given
    point in Coq proof is completely implicit, whereas informal proof
    reminds reader several times where things stand. *)

(** **** Exercise: 2 stars (plus_comm_informal) *)
(** Translate your solution for [plus_comm] into an informal proof. *)

(* TODO *)

(** Theorem: Addition is commutative.
    Proof: (* FILL IN HERE *)
[]
*)

(** **** Exercise: 2 stars, optional (beq_nat_refl_informal) *)
(* TODO *)
(** Write an informal proof of the following theorem, using the
    informal proof of [plus_assoc] as a model.  Don't just
    paraphrase the Coq tactics into English!

    Theorem: [true = beq_nat n n] for any [n].

    Proof: (* FILL IN HERE *)
[]
 *)

(** **** Exercise: 1 star, optional (beq_nat_refl) *)
Theorem beq_nat_refl : forall n : nat,
  true = beq_nat n n.
Proof.
  intros n. induction n as [| n'].
  Case "n is 0".    simpl. reflexivity.
  Case "n is S n'".  (*   IHn' : true = beq_nat    n'     n'  *)
                     (*          true = beq_nat (S n') (S n') *)
    simpl.           (*          true = beq_nat    n'     n' by def of beq_nat *)
    rewrite -> IHn'. (* beq_nat n' n' = beq_nat    n'     n' *)
    reflexivity.
Qed.

(* ###################################################################### *)
(** * Proofs Within Proofs *)

(** Large proofs often broken into sequence of theorems, with later
    proofs referring to earlier theorems.  Sometimes proof will need
    some misc fact too simple (and of too little general interest) to
    give it a name.  Convenient to state/prove the needed
    "sub-theorem" right at point of use (i.e., "anonymous theorem").

    [assert] tactic: to state/prove "sub-theorem" at point of use
    without giving it a name.

    E.G., earlier proof of [mult_0_plus] theorem referred theorem
    named [plus_O_n].  Use [assert] to state and prove [plus_O_n]
    in-line: *)

Theorem mult_0_plus' : forall n m : nat,
  (0 + n) * m = n * m.
Proof.
  intros n m.
  assert (H: 0 + n = n).
    Case "Proof of assertion". (*       0 + n = n     *)
      simpl.                   (*           n = n     *)
      reflexivity.             (*   H : 0 + n = n now in context *)
                               (* (0 + n) * m = n * m *)
  rewrite -> H.                (*       n * m = n * m *)
  reflexivity.
Qed.

(** [assert] tactic introduces two sub-goals.

    First is assertion itself; prefixed with name (e.g., [H:]).

    Note: could name assertion via [as]: [assert (0 + n = n) as H].

    Note: use [Case], so can see assertion proved because ["Proof of
    assertion"] string disappears from context.

    Second goal : same as one at point where we invoke [assert],
    except assumption [H] that [0 + n = n] is in context.

    [assert] generates one subgoal where we must prove asserted fact
    and second subgoal where we use asserted fact. *)

(** [assert] useful in many situations.
    E.G., to prove

        [(n + m) + (p + q) = (m + n) + (p + q)]

    Only difference : [m] and [n] swapped.

    Seems we should use commutativity of addition ([plus_comm]) to rewrite.
    But [rewrite] tactic doesn't know _where_ to apply the rewrite.
    Because: three uses of [+] above.  [rewrite -> plus_comm]
    only does _outer_ one: *)

Theorem plus_rearrange_firsttry : forall n m p q : nat,
  (n + m) + (p + q) = (m + n) + (p + q).
Proof.
  intros n m p q.        (* n + m + (p + q) = m + n + (p + q) *)
  (* Need to swap (n + m) for (m + n)... seems like plus_comm is what to use *)
  rewrite -> plus_comm.  (* p + q + (n + m) = m + n + (p + q) *)
  (* rewrote the wrong plus *)
Admitted.

(** To apply [plus_comm] at needed point,
    introduce local lemma [n + m = m + n]
    (_for particular [m] and [n] in case_),
    prove lemma using [plus_comm],
    then use lemma to do rewrite. *)

Theorem plus_rearrange : forall n m p q : nat,
  (n + m) + (p + q) = (m + n) + (p + q).
Proof.
  intros n m p q.
  assert (H: n + m = m + n).
    Case "Proof of assertion". (*     n + m = m + n *)
      rewrite -> plus_comm.    (*     m + n = m + n *)
      reflexivity.             (* H : n + m = m + n now in context *)

                               (* n + m + (p + q) = m + n + (p + q) *)
  rewrite -> H.                (* m + n + (p + q) = m + n + (p + q) *)
  reflexivity.
Qed.

(** **** Exercise: 4 stars, recommended (mult_comm) *)
(** Use [assert] in proof (and NO induction): *)

Theorem plus_swap : forall n m p : nat,
  n + (m + p) = m + (n + p).
Proof.
  intros n m p.             (*  n + (m + p) = m + (n + p) *)
  rewrite -> plus_comm.     (*  m +  p + n  = m + (n + p) *)
  assert (H: p + n = n + p).
    Case "Proof of assertion". (* p + n = n + p *)
      rewrite -> plus_comm.    (* n + p = n + p *)
      reflexivity.
                            (*  m +  p + n = m + (n + p) *)
  rewrite <- H.             (*  m +  p + n = m + (p + n) *)
  rewrite -> plus_assoc.    (*  m +  p + n = m +  p + n  *)
  reflexivity.
Qed.

(** Prove commutativity of multiplication.
    Define/prove separate theorem to use in proof of this one.
    [plus_swap] comes in handy. *)

(* HC: I PROVED THESE TWO THINKING I COULD USE THEM. *)

Theorem hc_plus_1_r : forall n:nat,
  S n = 1 + n.
Proof.
  intros n. simpl. reflexivity. Qed.

Theorem hc_mult_1_r : forall n:nat,
  n * 1 = n.
Proof.
  intros n. induction n as [| n'].
  simpl. reflexivity.
  simpl. rewrite -> IHn'. reflexivity.
Qed.

(* After several hours I googled and found
   http://boilerpl.blogspot.com/2011_08_01_archive.html
   I only peaked to see the following theorem,
   not its proof - I proved it myself. *)

Theorem hc_mult_n_Sm : forall n m : nat,
  n * (S m) = n + n * m.
Proof.
  intros n m. induction n as [| n'].
  Case "n is 0".
    simpl. reflexivity.
  Case "n is S n'".         (*        IHn' : n'  * S m   =    n' +        n' * m *)
                            (*             S n'  * S m   =  S n' +      S n' * m *)
    simpl.                  (*       S (m +  n'  * S m)  = S (n' +  (m +  n' * m)) by def of * and + *)
    rewrite <- plus_swap.   (*       S (m +  n'  * S m)  = S (m  +  (n' + n' * m)) *)
    rewrite -> plus_assoc.  (*       S (m +  n'  * S m)  = S (m  +   n' + n' * m) *)
    rewrite -> IHn'.        (*  S (m + (n' + n'  *   m)) = S (m  +   n' + n' * m) *)
    rewrite -> plus_assoc.  (*  S (m +  n' + n'  *   m)  = S (m  +   n' + n' * m) *)
    reflexivity.
Qed.

(* version from the link: *)
Theorem hc_mult_n_Sm' : forall n m : nat,
 n * (S m) = n + n * m.
Proof.
  intros n m. induction n as [| n'].
  Case "n = O".
    simpl. reflexivity.
  Case "n = S n'".        (*        IHn' : n' * S m   =    n' +      n' * m *)
                          (*             S n' * S m   =  S n' +    S n' * m *)
    simpl.                (*       S (m +  n' * S m)  = S (n' + (m + n' * m)) *)
    rewrite -> IHn'.      (* S (m +  (n' + n' *   m)) = S (n' + (m + n' * m)) *)
    rewrite -> plus_swap. (* S (n' + (m +  n' *   m)) = S (n' + (m + n' * m)) *)
    reflexivity.
Qed.

(* Then I was able to proceed quickly in this proof: *)
Theorem mult_comm : forall m n : nat,
  m * n = n * m.
Proof.
  intros m n. induction n as [| n'].
  Case "n is 0".
    simpl.               (* mult on left is zero by definition *)
    rewrite -> mult_0_r. (* mult on right *)
    reflexivity.
  Case "n is S n'".          (* IHn' : m  *   n' =     n' * m *)
                             (*        m  * S n' =   S n' * m *)
    rewrite -> hc_mult_n_Sm. (*    m + m  *   n' =   S n' * m *)
    rewrite -> IHn'.         (*    m + n' *   m  =   S n' * m *)
    rewrite <- mult_1_plus.  (*  (1 + n') *   m  =   S n' * m : discovered unnecessary in review *)
    simpl.                   (*   m + n'  *   m  = m + n' * m *)
    reflexivity.
Qed.

(* version from the link *)
Theorem mult_comm' : forall m n : nat,
 m * n = n * m.
Proof.
  intros m n.
  induction m as [|m'].
  Case "m = O".
    simpl. rewrite -> mult_0_r. reflexivity.
  Case "m = S m'".            (*   IHm' : m' *  n  = n *   m' *)
                              (*        S m' *  n  = n * S m' *)
    simpl.                    (*      n + m' *  n  = n * S m' *)
    rewrite -> IHm'.          (*      n + n  *  m' = n * S m' *)
    rewrite <- hc_mult_n_Sm'. (*          n * S m' = n * S m' *)
    reflexivity.
Qed.

(** **** Exercise: 2 stars, optional (evenb_n__oddb_Sn) *)
(* I could not do this myself.  Got answer from:
   https://github.com/debasishg/sf/blob/master/Basics.v
   This definitely NOT 2 stars. *)
Theorem evenb_n__oddb_Sn : forall n : nat,
  evenb n = negb (evenb (S n)).
Proof.
  intros n. induction n as [| n'].
  Case "n = 0".
    simpl. reflexivity.
  Case "n = S n'".                (*  IHn' : evenb n'  = negb (evenb       (S n'))   *)
                                  (*      evenb (S n') = negb (evenb    (S (S n')))  *)
    assert (evenb n' = evenb (S (S n'))) as H.
      SCase "Proof of assertion". (*         evenb n'  = evenb (S (S n'))            *)
        simpl.                    (*         evenb n'  = evenb       n' by def of evenb *)
        reflexivity.
                                  (*      evenb (S n') = negb (evenb    (S (S n')))  *)
    rewrite <- H.                 (*      evenb (S n') = negb (evenb          n')    *)
    rewrite IHn'.                 (*      evenb (S n') = negb (negb (evenb (S n')))  *)
    rewrite -> negb_involutive.   (*      evenb (S n') =             evenb (S n')    *)
    reflexivity.
Qed.

(* ###################################################################### *)
(** * More Exercises *)

(** **** Exercise: 3 stars, optional (more_exercises) *)
(** For each theorem, first predict whether
    - can be proved using only simplification and rewriting,
    - also requires case analysis ([destruct])
    - also requires induction.
    Then do proof.
    Goal: think before hack. *)

(* HC: simpl *)
Theorem ble_nat_refl : forall n:nat,
  true = ble_nat n n.
Proof.
  intros n. induction n as [| n'].
  Case "n is 0".
    simpl. reflexivity.
  Case "n is S n'".  (*   IHn' : true = ble_nat    n'     n'  *)
                     (*          true = ble_nat (S n') (S n') *)
    simpl.           (*          true = ble_nat    n'     n'  *)
    rewrite -> IHn'. (* ble_nat n' n' = ble_nat    n'     n'  *)
    reflexivity.
Qed.

(* HC: simpl  *)
Theorem zero_nbeq_S : forall n:nat,
  beq_nat 0 (S n) = false.
Proof.
  intros n.
  simpl. reflexivity.
Qed.

(* HC: destruct *)
Theorem andb_false_r : forall b : bool,
  andb b false = false.
Proof.
  intros b. destruct b.
  simpl. reflexivity.
  simpl. reflexivity.
Qed.

(* HC: destruct  *)
Theorem plus_ble_compat_l : forall n m p : nat,
  ble_nat n m = true -> ble_nat (p + n) (p + m) = true.
Proof.
  intros n m p H. induction p as [|p'].
  Case "p is 0".      (* ble_nat (0 + n) (0 + m) = true *)
    simpl.            (* ble_nat      n       m  = true *)
    rewrite -> H.     (*                    true = true *)
    reflexivity.
  Case "p is S n".    (* IHp' : ble_nat (  p' + n)   (p' + m) = true *)
                      (*        ble_nat (S p' + n) (S p' + m) = true *)
    simpl.            (*        ble_nat   (p' + n)   (p' + m) = true by def of ble_nat and + *)
    rewrite -> IHp'.  (*                                 true = true *)
    reflexivity.
Qed.

(* HC: destruct *)
Theorem S_nbeq_0 : forall n:nat,
  beq_nat (S n) 0 = false.
Proof.
  intros n. destruct n as [|n'].
  Case "n is 0".
    simpl. reflexivity.
  Case "n is S n'".
    simpl. reflexivity.
Qed.

(* HC: destruct *)
Theorem mult_1_l : forall n:nat,
  1 * n = n.
Proof.
  intros n. destruct n as [| n'].
  Case "n is 0".    simpl. reflexivity.
  Case "n is S n'".      (*   1 * S n' = S n' *)
    simpl.               (* S (n' + 0) = S n' *)
    rewrite -> plus_0_r. (*       S n' = S n' *)
    reflexivity.
Qed.

(* HC: destruct *)
Theorem all3_spec : forall b c : bool,
  orb
    (andb b c)
    (orb (negb b)
         (negb c))
  = true.
Proof.
  intros b c. destruct b.
  Case "B is true".
    simpl.
    destruct c.
    SCase "C is true".  simpl. reflexivity.
    SCase "C is false". simpl. reflexivity.
  Case "B is false".
    simpl. reflexivity.
Qed.

(* HC: induction. *)
Theorem mult_plus_distr_r : forall n m p : nat,
  (n + m) * p = (n * p) + (m * p).
Proof.
  intros n m p. induction n as [|n'].
  Case "n is 0".
    simpl. reflexivity.
  Case "n is S n'".        (*   IHn' : (n' + m) * p  =     n' * p + m * p *)
                           (*        (S n' + m) * p  =   S n' * p + m * p *)
    simpl.                 (*  p +     (n' + m) * p  = p + n' * p + m * p *)
    rewrite -> IHn'.       (*  p + (n' * p + m  * p) = p + n' * p + m * p *)
    rewrite -> plus_assoc. (*  p +  n' * p + m  * p  = p + n' * p + m * p *)
    reflexivity.
Qed.

(* HC: induction *)
Theorem mult_assoc : forall n m p : nat,
  n * (m * p) = (n * m) * p.
Proof.
  intros n m p. induction n as [|n'].
  Case "n is 0".
    simpl. reflexivity.
  Case "n is S n'".               (*    IHn' : n' * (m * p) =         n' * m  * p *)
                                  (*         S n' * (m * p) =       S n' * m  * p *)
    simpl.                        (*   m * p + n' * (m * p) =    (m + n' * m) * p *)
    rewrite -> mult_plus_distr_r. (*   m * p + n' * (m * p) = m * p + n' * m  * p *)
    rewrite -> IHn'.              (*   m * p + n' *  m * p  = m * p + n' * m  * p *)
    reflexivity.
Qed.

(** **** Exercise: 2 stars, optional (plus_swap') *)
(** [replace] tactic enables specifying particular subterm to
    rewrite and its rewritten form:
    [replace (t) with (u)] replaces
    all copies of expression [t] in goal by expression [u],
    and generates [t = u] as an additional subgoal.

    Useful when [rewrite] acts on wrong part of goal.

    Use [replace] to prove [plus_swap'], like
    [plus_swap] without needing [assert (n + m = m + n)].  *)

Theorem plus_swap' : forall n m p : nat,
  n + (m + p) = m + (n + p).
Proof.
  intros n m p.
  Case "main goal".                  (* n + (m + p) = m + (n + p) *)
    rewrite -> plus_comm.            (* m +  p + n  = m + (n + p) *)
    rewrite <- plus_assoc.           (* m + (p + n) = m + (n + p) *)
    replace (p + n) with (n + p).    (* m + (n + p) = m + (n + p) *)
    reflexivity.
  SCase "replacement goal".          (*      n + p  =      p + n  *)
    rewrite <- plus_comm.            (*      p + n  =      p + n  *)
    reflexivity.
Qed.

(** **** Exercise: 3 stars, optional *)
(** QUESTION: what is this? *)
Theorem bool_fn_applied_thrice : forall (f : bool -> bool) (b : bool),
  f (f (f b)) = f b.
Proof.
  intros f b.
  destruct b.
  Case "b = true".
  remember (f true) as ftrue.
    destruct ftrue.
    SCase "f true = true".
      rewrite <- Heqftrue.
      symmetry.
     apply Heqftrue.
    SCase "f true = false".
      remember (f false) as ffalse.
      destruct ffalse.
      SSCase "f false = true".
        symmetry.
        apply Heqftrue.
      SSCase "f false = false".
        symmetry.
        apply Heqffalse.
  remember (f false) as ffalse.
    destruct ffalse.
    SCase "f false = true".
      remember (f true) as ftrue.
      destruct ftrue.
      SSCase "f true = true".
        symmetry.
        apply Heqftrue.
      SSCase "f true = false".
        symmetry.
        apply Heqffalse.
    SCase "f false = false".
      rewrite <- Heqffalse.
      symmetry.
      apply Heqffalse.
Qed.
(** [] *)

(** **** Exercise: 4 stars, recommended (binary) *)
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

(** (c) Prove incrementing bin and then converting yields same result
        as converting then incrementing. *)
(* HC: did this myself *)
Theorem bin_comm: forall b : bin,
  b2n (incr b) = 1 + (b2n b).
Proof.
  intros b.  induction b as [|b'|b''].
  Case "b is Z". simpl. reflexivity.
  Case "b is T b'".                     (*      IHb'  : b2n (incr    b')  =         1 +  b2n    b'       *)

                                        (*              b2n (incr (T b')) =         1 +  b2n (T b')      *)
    simpl.                              (*      b2n b' + (b2n b' + 0) + 1 = S (b2n b' + (b2n    b' + 0))
                                           right:  incr (T b') -> M b';
                                                    b2n (M b') -> 2 * (b2n b') + 1;
                                                  2 * (b2n b') -> b2n b' + (b2n b' + 0) TODO: where does 0 come from?
                                           left:    b2n (T b') -> 2 * (b2n b')
                                                  2 * (b2n b') -> b2n b' + (b2n b' + 0) TODO: 0
                                                         1 + _ -> S _                                    *)
    replace (b2n b' + 0) with (b2n b'). (*      b2n b' +  b2n b'      + 1 = S (b2n b' +  b2n    b')      *)
    rewrite -> plus_comm.               (* 1 + (b2n b' +  b2n b')         = S (b2n b' +  b2n    b')      *)
    simpl.                              (*   S (b2n b' +  b2n b')         = S (b2n b' +  b2n    b')      *)
    reflexivity.
  SCase "prove replacement".            (* b2n b' = b2n b' + 0 *)
    rewrite -> plus_0_r.                (* b2n b' = b2n b'     *)
    reflexivity.
  Case "b is M b''".                           (* IHb'' : b2n (incr b'') = 1 + b2n b'' *)

                                               (*                    b2n (incr (M b''))  = 1 + b2n (M b'') *)
    simpl.                                     (* b2n (incr b'') + (b2n (incr b'') + 0)  = S (b2n b'' + (b2n b'' + 0) + 1) *)
    rewrite -> IHb''.                          (*       1 + b2n b'' + (1 + b2n b'' + 0)  = S (b2n b'' + (b2n b'' + 0) + 1) *)
    simpl.                                     (*        S (b2n b'' +   S (b2n b'' + 0)) = S (b2n b'' + (b2n b'' + 0) + 1) *)
    rewrite -> plus_0_r.                       (*        S (b2n b'' +   S (b2n b''))     = S (b2n b'' + b2n b'' + 1) *)
    replace (S (b2n b'')) with (b2n b'' + 1).  (*        S (b2n b'' +     (b2n b'' + 1)) = S (b2n b'' + b2n b'' + 1) *)
    rewrite -> plus_assoc.                     (*        S (b2n b'' +      b2n b'' + 1)  = S (b2n b'' + b2n b'' + 1) *)
    reflexivity.
  SCase "prove 2nd replacement". (* b2n b'' + 1 = S (b2n b'') *)
    rewrite -> plus_comm.        (* 1 + b2n b'' = S (b2n b'') *)
    simpl.                       (* S (b2n b'') = S (b2n b'') *)
    reflexivity.
Qed.

(** **** Exercise: 5 stars (binary_inverse) *)
(** More binary numbers.  Needs previous defs/theorems.

    (a) 1. Write function to convert nat to bin. *)

Fixpoint n2b (n : nat) : bin :=
  match n with
  | O    => Z
  | S n' => incr (n2b n')
  end.

Eval simpl in           (n2b 0).
Example n2b0:            n2b 0 =                  Z.   (*   0 *)
Proof. reflexivity. Qed.
Eval simpl in           (n2b 1).
Example n2b1:            n2b 1 =                M Z.   (*   1 *)
Proof. reflexivity. Qed.
Eval simpl in           (n2b 2).
Example n2b2:            n2b 2 =             T (M Z).  (*  10 *)
Proof. reflexivity. Qed.
Eval simpl in           (n2b 3).
Example n2b3:            n2b 3 =             M (M Z).  (*  11 *)
Proof. reflexivity. Qed.
Eval simpl in           (n2b 4).
Example n2b4:            n2b 4 =          T (T (M Z)). (* 100 *)
Proof. reflexivity. Qed.
Eval simpl in           (n2b 5).
Example n2b5:            n2b 5 =          M (T (M Z)). (* 101 *)
Proof. reflexivity. Qed.
Eval simpl in           (n2b 6).
Example n2b6:            n2b 6 =          T (M (M Z)). (* 110 *)
Proof. reflexivity. Qed.
Eval simpl in           (n2b 7).    (*    M (M (M Z))     111 *)
Eval simpl in           (n2b 8).    (* T (T (T (M Z)))   1000 *)

Eval simpl in (b2n (n2b 3)).
Eval simpl in (b2n (n2b 6)).
Eval simpl in (b2n (n2b 123)).

(** (a) 2. Prove nat -> bin -> nat yields the same starting number. *)

Theorem n2b2n_eq: forall n : nat,
  (b2n (n2b n)) = n.
Proof.
  intros n. induction n as [|n'].
  Case "n is 0".  simpl. reflexivity.
  Case "n is S n'".       (*    IHn' : b2n (n2b n') = n' *)

                          (*    b2n    (n2b (S n')) = S n' *)
    simpl.                (*    b2n (incr (n2b n')) = S n' *)
    rewrite -> bin_comm.  (*      1 + b2n (n2b n')  = S n' *)
    simpl.                (*       S (b2n (n2b n')) = S n' *)
    rewrite -> IHn'.      (*                 S n'   = S n' *)
    reflexivity.
Qed.

(** (b) bin -> nat -> bin yields same starting number but can't prove
        it with what we have so far. Explain why. *)

Eval simpl in (n2b (b2n    (M (T (M Z))))).
Eval simpl in (n2b (b2n    (T (M (M Z))))).
Eval simpl in (n2b (b2n    (M (M (M Z))))).
Eval simpl in (n2b (b2n (T (T (T (M Z)))))).
(* TODO *)
Theorem b2n2b_eq: forall b : bin,
  (n2b (b2n b)) = b.
Proof.
  intros b. induction b as [|b'|b''].
  Case "b is Z".    simpl. reflexivity.
  Case "b is T b'". (* n2b (b2n (T b'))            = T b' *)
    simpl.          (* n2b (b2n b' + (b2n b' + 0)) = T b' *)
    assert (H: b2n b' + 0 = b2n b').
      SSCase "Proof of Assertion".
        rewrite -> plus_0_r. reflexivity.
    rewrite -> H.   (* n2b (b2n b' + b2n b')       = T b' *)
Admitted.

Eval simpl in (b2n (T (T (M Z)))).
(* TODO *)
(** (c) 1. Define [normalize] function bin -> bin such that for any
           binary number b, converting to a natural and then back to
           binary yields [(normalize b)]. *)
(** (c) 2. Prove it. *)

(* FILL IN HERE *)

(** **** Exercise: 2 stars, optional (decreasing) *)
(** Decreasing arg to function guarantees termination on all inputs.

    Coq's "Decreasing analysis" not sophisticated.
    Sometimes necessary to write functions in unnatural ways.

    Write a sensible [Fixpoint] definition (e.g., on numbers) that
    _does_ terminate on all inputs, but Coq will _not_ accept because
    of restriction.
*)
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

(* End of file. *)


