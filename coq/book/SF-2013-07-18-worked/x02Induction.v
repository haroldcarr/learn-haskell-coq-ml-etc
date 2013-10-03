(** * Induction: Proof by Induction *)

(**
/Applications/CoqIdE_8.4.app/Contents/Resources/bin/coqc x01Basics.v
*)

(** Next line imports definitions from previous chapter. *)

Require Export x01Basics.

(* ###################################################################### *)
(** * Naming Cases *)
(* tactic:Case *)
(** No command for moving from one branch of a case analysis to the next.
    Indentation and comments help.  Better to use [Case] tactic.

    [Case] not built in.No need to understand how it works.
    Skip definition.  Go to example.

    Kudos to Aaron Bohannon for this nice hack! *)

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
(** [] *)

(** No formatting rules in Coq.
    [Case] tactics help make proof readable. *)


(* ###################################################################### *)
(** * Proof by Induction *)
(* tactic:induction *)
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
Abort.

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
    simpl.       (* ...but stuck again *)
Abort.

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

(** **** Exercise: 2 stars (basic_induction) *)

(** Prove the following lemmas using induction. You might need
    previously proven results. *)

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
(** [] *)

(** **** Exercise: 2 stars (double_plus) *)

Fixpoint double (n:nat) :=
  match n with
  | O => O
  | S n' => S (S (double n'))
  end.

Eval simpl in (double 16).
  (* ===> 32 : nat *)

(** Use induction to prove: *)

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
(** [] *)


(** **** Exercise: 1 star (destruct_induction) *)
(** Briefly explain the difference between the tactics
    [destruct] and [induction].

    [destruct] splits into two cases: [n = 0] and [n = S n'], each
    case is then proved separately.

    [induction] also splits into same two cases, but also adds the
    assumption to the context, with a name so you can use it in the
    proof of the general case. *)
(** [] *)


(* ###################################################################### *)
(** * Proofs Within Proofs *)
(* tactic:assert *)

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

(** **** Exercise: 4 stars (mult_comm) *)
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
(** [] *)

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
  Case "n = S n'".                (*  IHn' : evenb    n'  = negb (evenb       (S n'))   *)
                                  (*         evenb (S n') = negb (evenb    (S (S n')))  *)
    assert (evenb n' = evenb (S (S n'))) as H.
      SCase "Proof of assertion". (*         evenb    n'  = evenb (S (S n'))            *)
        simpl.                    (*         evenb    n'  = evenb       n' ; by def of evenb *)
        reflexivity.
                                  (*         evenb (S n') = negb (evenb    (S (S n')))  *)
    rewrite <- H.                 (*         evenb (S n') = negb (evenb          n')    *)
    rewrite IHn'.                 (*         evenb (S n') = negb (negb (evenb (S n')))  *)
    rewrite -> negb_involutive.   (*         evenb (S n') =             evenb (S n')    *)
    reflexivity.
Qed.
(** [] *)

(* ###################################################################### *)
(** * More Exercises *)

(** **** Exercise: 3 stars, optional (more_exercises) *)
(** For each theorem, first predict whether
    - can be proved using only simplification and rewriting,
    - also requires case analysis ([destruct])
    - also requires induction.
    Then do proof.
    Goal: think before hack. *)

(* HC 1st: simpl *)
(* HC 2nd: destruct *)
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


(* HC 1st: simpl  *)
(* HC 2nd: destruct *)
Theorem zero_nbeq_S : forall n:nat,
  beq_nat 0 (S n) = false.
Proof.
  intros n. destruct n as [| n'].
  reflexivity.
  reflexivity.
Qed.

(* HC 1st: destruct *)
(* HC 2nd: destruct *)
Theorem andb_false_r : forall b : bool,
  andb b false = false.
Proof.
  intros b. destruct b.
  reflexivity.
  reflexivity.
Qed.

(* HC 1st: destruct  *)
(* HC 2nd: induction *)
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

(* HC 1st: destruct *)
(* HC 2nd: destruct *)
Theorem S_nbeq_0 : forall n:nat,
  beq_nat (S n) 0 = false.
Proof.
  intros n. destruct n as [| n'].
  reflexivity.
  reflexivity.
Qed.

(* HC 1st: destruct *)
(* HC 2nd: induction *)
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

(* HC 1st: destruct *)
(* HC 2nd: destruct *)
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

(* HC 1st: induction. *)
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

(* HC 1st: induction *)
(* HC 2nd: induction *)
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
(** [] *)

(** **** Exercise: 2 stars, optional (beq_nat_refl) *)
(** [true] on left may seem odd (same as stated in standard library).
    Rewriting works in either direction, so no problem using theorem. *)

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
(** [] *)

(** **** Exercise: 2 stars, optional (plus_swap') *)
(* tactic:replace *)
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
(** [] *)


(** **** Exercise: 3 stars (binary_commute) *)
(** Prove binary [increment] and [binary-to-unary] (from [Basics] chapter)
    incrementing bin and then converting yields same result as converting then incrementing,
    i.e., that they commute. *)
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
   Case "b is M b''".                          (* IHb'' : b2n (incr b'') = 1 + b2n b'' *)

                                               (*                    b2n (incr (M b''))  = 1 + b2n (M b'') *)
    simpl.                                     (* b2n (incr b'') + (b2n (incr b'') + 0)  = S (b2n b'' + (b2n b'' + 0) + 1) *)
    rewrite -> IHb''.                          (*       1 + b2n b'' + (1 + b2n b'' + 0)  = S (b2n b'' + (b2n b'' + 0) + 1) *)
    simpl.                                     (*        S (b2n b'' +   S (b2n b'' + 0)) = S (b2n b'' + (b2n b'' + 0) + 1) *)
    rewrite -> plus_0_r.                       (*        S (b2n b'' +   S (b2n b''))     = S (b2n b'' + b2n b'' + 1) *)
    replace (S (b2n b'')) with (b2n b'' + 1).  (*        S (b2n b'' +     (b2n b'' + 1)) = S (b2n b'' + b2n b'' + 1) *)
    rewrite -> plus_assoc.                     (*        S (b2n b'' +      b2n b'' + 1)  = S (b2n b'' + b2n b'' + 1) *)
    reflexivity.
    SCase "prove 2nd replacement". (* b2n b'' + 1 = S (b2n b'') *)
    rewrite -> plus_comm.          (* 1 + b2n b'' = S (b2n b'') *)
    simpl.                         (* S (b2n b'') = S (b2n b'') *)
    reflexivity.
Qed.
(** [] *)


(** **** Exercise: 5 stars, advanced (binary_inverse) *)
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
  Case "n is 0".   simpl. reflexivity.
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
(* TODO b2n2b_eq *)
Theorem b2n2b_eq: forall b : bin,
  (n2b (b2n b)) = b.
Proof.
  intros b. induction b as [|b'|b''].
  Case "b is Z".     simpl. reflexivity.
  Case "b is T b'". (* n2b (b2n (T b'))            = T b' *)
    simpl.          (* n2b (b2n b' + (b2n b' + 0)) = T b' *)
    assert (H: b2n b' + 0 = b2n b').
      (* SSCase "Proof of Assertion". *)
        rewrite -> plus_0_r. reflexivity.
    rewrite -> H.   (* n2b (b2n b' + b2n b')       = T b' *)
Admitted.

Eval simpl in (b2n (T (T (M Z)))).
(* TODO binary normalize *)
(** (c) 1. Define [normalize] function bin -> bin such that for any
           binary number b, converting to a natural and then back to
           binary yields [(normalize b)]. *)
(** (c) 2. Prove it. *)
(** [] *)

(* ###################################################################### *)
(** * Advanced Material *)

(** ** Formal vs. Informal Proof *)

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

Theorem plus_assoc' : forall n m p : nat,
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
    A mathematician mighty write it like this: *)

(** - _Theorem_: For any [n], [m] and [p],
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
      which is immediate from the induction hypothesis. [] *)

(** Overall form similar.  Differences of detail: formal proof more
    explicit in some ways (e.g., use of [reflexivity]) but much less
    explicit in others; in particular, the "proof state" at any given
    point in Coq proof is completely implicit, whereas informal proof
    reminds reader several times where things stand. *)

(** Here is a formal proof that shows the structure more
    clearly: *)

Theorem plus_assoc'' : forall n m p : nat,
  n + (m + p) = (n + m) + p.
Proof.
  intros n m p. induction n as [| n'].
  Case "n = 0".
    reflexivity.
  Case "n = S n'".
    simpl. rewrite -> IHn'. reflexivity.   Qed.

(** **** Exercise: 2 stars, advanced (plus_comm_informal) *)
(** Translate your solution for [plus_comm] into an informal proof. *)

(** Theorem: Addition is commutative.

    Proof: (* FILL IN HERE *)
[]
*)

(** **** Exercise: 2 stars, optional (beq_nat_refl_informal) *)
(** Write an informal proof of the following theorem, using the
    informal proof of [plus_assoc] as a model.  Don't just
    paraphrase the Coq tactics into English!

    Theorem: [true = beq_nat n n] for any [n].

    Proof: (* FILL IN HERE *)
[]
 *)

(* $Date: 2013-07-17 16:19:11 -0400 (Wed, 17 Jul 2013) $ *)
