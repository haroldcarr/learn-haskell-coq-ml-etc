(** * MoreCoq: More About Coq *)

(**
/Applications/CoqIdE_8.4.app/Contents/Resources/bin/coqc x04Poly.v
*)

Require Export x04Poly.

(* ###################################################### *)
(** * The [apply] Tactic *)
(* tactic:apply *)
(** When goal to be proved is same as
    - a hypothesis in context, or
    - previously proved lemma. *)

Theorem silly1 : forall (n m o p : nat),
     n = m  ->
     [n;o] = [n;p] ->
     [n;o] = [m;p].
Proof.
  intros n m o p eq1 eq2.  (*       [n, o] = [m, p] *)
  rewrite <- eq1.          (* eq1 :  n     =  m
                              eq2 : [n; o] = [n; p]
                              ============================
                                    [n; o] = [n; p] *)
  (* Could finish by: *)
  (* rewrite -> eq2.                [n, p] = [n, p]
     reflexivity.
     Same effect in single step using [apply]: *)
  apply eq2.
Qed.

(** When [apply] used with _conditional_ hypotheses and lemmas,
    premises added as subgoals. *)

Theorem silly2 : forall (n m o p : nat),
     n = m  ->
     (forall (q r : nat), q = r -> [q;o] = [r;p]) ->
     [n;o] = [m;p].
Proof.
  intros n m o p eq1 eq2. (* eq1 : n = m
                             eq2 : forall q r : nat, q = r -> [q; o] = [r; p]
                             ================================================
                                                              [n; o] = [m; p] *)
  apply eq2.              (*                                   n     =  m     *)
                          (* eq2 matches the o and p leaving the assumption with n=m substituted for q=r
                             See comment for silly2a.                         *)
  apply eq1.
Qed.

(** Complete following
    - using just [rewrite]
    - instead of [apply].
    - Is it possible? *)

Theorem hc_silly2 : forall (n m o p : nat),
  n = m  ->
  (forall (q r : nat), q = r -> [q;o] = [r;p]) ->
  [n;o] = [m;p].
Proof.
  intros n m o p eq1 eq2.
  apply eq2.
  apply eq1.
Qed.

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
  intros n m eq1 eq2.   (* eq1 : (n, n) = (m, m)
                           eq2 : forall q r : nat, (q, q) = (r, r) -> [q] = [r]
                           ============================
                                                                      [n] = [m] *)
  apply eq2.            (*                         (n, n) = (m, m)              *)
  apply eq1.
Qed.

(** **** Exercise: 2 stars, optional (silly_ex) *)
(** Complete without using [simpl]. *)
(* TODO : do not understand: see following hc version *)
Theorem silly_ex :
     (forall n, evenb n = true -> oddb (S n) = true) ->
                evenb 3 = true ->
                                  oddb 4     = true.
Proof.
  intros eq1 eq2. (* eq1 : forall n : nat, evenb n = true -> oddb (S n) = true
                     eq2 :                 evenb 3 = true
                     ====================================
                                                             oddb    4  = true *)
  apply eq1.      (*                       evenb 3 = true                      *)
  apply eq2.
Qed.

Theorem hc_silly_ex :
     (forall n, evenb n = true -> oddb (S n) = true) ->
                evenb 3 = true ->
                                  oddb 4     = true.
Proof.
  intros eq1 eq2.     (* eq1 : forall n : nat, evenb n = true -> oddb (S n) = true
                         eq2 :                 evenb 3 = true
                         =========================================================
                                                                 oddb 4     = true  *)
  unfold oddb.        (*                                  negb (evenb 4)    = true  *)
  unfold evenb.       (*                                  negb true         = true  *)
  unfold negb.        (*                                  false             = true  *)
  simpl in eq2.       (* eq2 :                 false   = true                       *)
(* rewrite <- eq2. *) (*                                  false             = false *)
  rewrite -> eq2. (*                                      true              = true  *)
  reflexivity.
Qed.
(** [] *)

(** To use [apply], conclusion of the fact being applied must match the goal.
    E.G.: [apply] no good if left and right sides of equality are swapped: *)

Theorem silly3_firsttry : forall (n : nat),
     true = beq_nat n 5  ->
     beq_nat (S (S n)) 7 = true.
Proof.
  intros n H.   (* beq_nat (S (S n)) 7 = true *)
  simpl.        (*         beq_nat n 5 = true *)
  (* Here we cannot use [apply] directly *)
Abort.

(* tactic:symmetry *)
(** Use [symmetry] tactic to swap left/right sides of an equality in goal. *)

Theorem silly3 : forall (n : nat),
     true = beq_nat n 5  ->
     beq_nat (S (S n)) 7 = true.
Proof.
  intros n H.   (* beq_nat (S (S n)) 7 = true                *)
  symmetry.     (*                true = beq_nat (S (S n)) 7 *)
(* [simpl] not necessary. [apply] will do a [simpl] step first. *)
  simpl.        (*                true = beq_nat n 5         *)
  apply H.
Qed.

(** **** Exercise: 3 stars (apply_exercise1) *)
(* Hint: use [apply] with previously defined lemmas besides
   hypotheses in the context.  Remember [SearchAbout]. *)

Theorem rev_exercise1 : forall (l l' : list nat),
     l  = rev l' ->
     l' = rev l.
Proof.
   intros l l' H.             (* H :      l   = rev l'
                                 ============================
                                          l'  = rev l  *)
   symmetry.                  (*      rev l   =     l' *)
   rewrite -> H.              (* rev (rev l') =     l' *)
(* rewrite -> rev_involutive.             l'  =     l'
   reflexivity.                                        *)
   apply rev_involutive.
Qed.
(** [] *)

(** **** Exercise: 1 star, optional (apply_rewrite) *)
(** Explain the difference between [apply] and [rewrite].

    HC:
    - [apply] does several steps: [simpl], [rewrite], [reflexivity] and possibly introduces subgoals
    - [rewrite] does not involve other steps

    Are there situations where both can usefully be applied?

    Yes: see rev_exercise1 above.
*)
(** [] *)


(* ###################################################### *)
(** * The [apply ... with ...] Tactic *)
(* tactic:apply...with... *)
(** Transitivity: get from [[a,b]] to [[e,f]]: *)

Example trans_eq_example : forall (a b c d e f : nat),
     [a;b] = [c;d] ->
     [c;d] = [e;f] ->
     [a;b] = [e;f].
Proof.
  intros a b c d e f eq1 eq2. (* eq1 : [a; b] = [c; d]
                                 eq2 : [c; d] = [e; f]
                                 ============================
                                       [a; b] = [e; f] *)
  rewrite -> eq1.             (*       [c; d] = [e; f] *)
  rewrite -> eq2.             (*       [e; f] = [e; f] *)
  reflexivity.
Qed.

(** Equality is transitive: common pattern, so make lemma. *)

Theorem trans_eq : forall (X:Type) (n m o : X),
  n = m -> m = o -> n = o.
Proof.
  intros X n m o eq1 eq2. (* eq1 : n = m
                             eq2 : m = o
                             ============================
                                   n = o *)
  rewrite -> eq1.         (*       m = o *)
  rewrite -> eq2.         (*       o = o *)
  reflexivity.
Qed.

(** Use [trans_eq] to prove previous example using refinement of [apply] tactic. *)

Example trans_eq_example' : forall (a b c d e f : nat),
     [a;b] = [c;d] ->
     [c;d] = [e;f] ->
     [a;b] = [e;f].
Proof.
  intros a b c d e f eq1 eq2.      (* eq1 : [a; b] = [c; d]
                                      eq2 : [c; d] = [e; f]
                                      ============================
                                            [a; b] = [e; f] *)
  (* if use [apply trans_eq] here
     Coq instantiates
         [X] with [[nat]]
         [n] with [[a,b]]
         [o] with [[e,f]]
     by matching goal against conclusion of lemma.
     But conclusion in terms of  [n] and [o].
     Can't determine instantiation for [m].
     Supply one explicitly: *)
  apply trans_eq with (m:=[c;d]).
  Case "prove initial goal".       (*       [a; b] = [c; d] ; and introduces subgoal:  [c; d] = [e; f] *)
    apply eq1.
  Case "prove subgoal".            (*       [c; d] = [e; f] *)
  apply eq2.
Qed.

(** Do not need to include name [m] in [with] clause.
    Coq usually can figure out.
    Instead write: [apply trans_eq with [c,d]]. *)

(** **** Exercise: 3 stars, optional (apply_with_exercise) *)
Example trans_eq_exercise : forall (n m o p : nat),
               m = (minustwo o) ->
     (n + p) = m                ->
     (n + p) =     (minustwo o).
Proof.
  intros n m o p eq1 eq2.     (* eq1 :         m = minustwo o
                                 eq2 : n + p = m
                                 ==================
                                       n + p =     minustwo o *)
  apply trans_eq with (m:=m). (*       n + p = m  - adds subgoal:  m = minustwo o *)
  Case "prove mainline".
    apply eq2.
  Case "prove subgoal".       (*               m = minustwo o *)
    apply eq1.
Qed.
(** [] *)


(* ###################################################### *)
(** * The [inversion] tactic *)
(* tactic:inversion *)
(** Recall definition of natural numbers:
     Inductive nat : Type :=
       | O : nat
       | S : nat -> nat.
    Every number has one of two forms.
    Two other facts implicit:
    - Constructor  [S] is _injective_ (one-to-one): [S n = S m] iff [n = m].
    - Constructors [O] and [S] are _disjoint_: [O] never equal to [S n] for any [n]. *)

(** For all inductively defined types:
    - all constructors are injective
    - values built from distinct constructors are never equal.

    E.G.:
    [cons] is injective and [nil] is different from every non-empty list.
    [true] and [false] are unequal. (Since neither [true] nor [false]
    take any arguments, their injectivity is not an issue.) *)

(** [inversion] tactic exploits these principles.

    Suppose [H] is hypothesis in context or a previously proven lemma with form
      c a1 a2 ... an = d b1 b2 ... bm
    for constructors [c], [d] and args [a1 ... an], [b1 ... bm].

    [inversion H] says "invert" this equality to extract info it
    contains about terms:

    - If [c], [d] same constructor, then, by injectivity of constructor,
      [a1 = b1], [a2 = b2], etc.;
      [inversion H]
      - adds these facts to context, and
      - tries to use them to rewrite goal.

    - If [c], [d] different constructors, then hypothesis [H] is contradictory.
      A false assumption has crept into context, meaning any goal provable!
      In this case, [inversion H] marks current goal as completed
      and pops it off goal stack. *)

Theorem eq_add_S : forall (n m : nat),
     S n = S m ->
       n =   m.
Proof.
  intros n m eq. (* eq : S n = S m
                    ==============
                           n =   m *)
  inversion eq.  (* H0 :   n =   m ; adds and uses
                    ==============
                           m =   m *)
  reflexivity.
Qed.

Theorem silly4 : forall (n m : nat),
     [n] = [m] ->
      n  =  m.
Proof.
  intros n m eq. (* eq : [n] = [m]
                    ==============
                          n  =  m *)
  inversion eq.  (* H0 :  n  =  m ; adds and uses
                    ==============
                          m  =  m *)
  reflexivity.
Qed.

(** [inversion] can destruct equalities between complex values,
    binding multiple variables as it goes. *)

Theorem silly5 : forall (n m o : nat),
     [n;    m] = [o;o] ->
     [n] = [m].
Proof.
  intros n m o eq. (* eq : [n; m] = [o; o]
                      ====================
                           [n]    = [m]    *)
  inversion eq.    (* H0 :  n     =  o
                      H1 :     m  =     o
                      ====================
                           [o]    = [o]    *)
  reflexivity.
Qed.

(** **** Exercise: 1 star (sillyex1) *)
Example sillyex1 : forall (X : Type) (x y z : X) (l j : list X),
     x :: y :: l = z :: j ->
     y :: l = x :: j ->
     x = y.
Proof.
  intros X x y z l j H0 H1. (* H1 : y :: l = x :: j
                               ====================
                                    x      = y      *)
  inversion H1.             (* H2 : y      = x       ; adds/uses
                               H3 :      l =      j  ; adds/uses
                               ====================
                                    x      = x      *)
  reflexivity.
Qed.
(** [] *)


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

(** **** Exercise: 1 star (sillyex2) *)
Example sillyex2 : forall (X : Type) (x y z : X) (l j : list X),
     x :: y :: l = [] ->
     y :: l = z :: j ->
     x = z.
Proof.
  intros X x y z l j contra eq2. (* contra : x :: y :: l =     [] ; this case has different
                                    eq2    :      y :: l = z :: j ; constructor from this case
                                    =============================
                                             x           = z      *)
  inversion contra.
Qed.
(** [] *)


(** Injectivity of constructors proves [forall (n m : nat), S n = S m -> n = m].
    Reverse direction (provable by standard equational reasoning) is a useful fact. *)

(** HC: this is from the SF-2012-07-25.
    It is a specific lemma used in a previous proof of length_snoc' below. *)

Lemma eq_remove_S : forall n m,
  n = m -> S n = S m.
Proof.
  intros n m eq. (* S n = S m *)
  rewrite -> eq. (* S m = S m *)
  reflexivity.
Qed.

(** HC: this is a more general theorem that is now (SF-2013-07-18) used for length_snoc' *)

Theorem f_equal : forall (A B : Type) (f: A -> B) (x y: A),
    x = y -> f x = f y.
Proof.
  intros A B f x y eq. (* f x = f y *)
  rewrite eq.          (* f y = f y *)
  reflexivity.
Qed.

(** Another way of proving length_snoc (from Lists).
    Extra equalities force more equational reasoning and use of tactics. *)

Theorem length_snoc' : forall (X : Type) (v : X)
                              (l : list X) (n : nat),
     length       l    =   n ->
     length (snoc l v) = S n.
Proof.
  intros X v l. induction l as [| v' l'].
  Case "l = []".          (* forall n : nat,
                                     length []                   =      n ->
                                     length (snoc []         v)  =    S n           *)
    intros n eq.          (*         length (snoc []         v)  =    S n           *)
    simpl.                (*    eq : length []                   =      n
                             ============================
                                     1                           =    S n           *)
    rewrite <- eq.        (*         1                           =    S (length []) *)
    simpl.                (*         1                           =    1             *)
    reflexivity.
  Case "l = v' :: l'".
    intros n eq.          (*         length (snoc (v' :: l') v)  =    S n           *)
    simpl.                (*      S (length (snoc        l'  v)) =    S n           *)
    destruct n as [| n'].
    SCase "n = 0".        (*    eq : length (      v' :: l')     =      0
                             ============================
                                  S (length (snoc        l'  v)) =      1           *)
      inversion eq.
    SCase "n = S n'".     (*  IHl' : forall n : nat,
                                     length              l'      =      n ->
                                     length (snoc        l'  v)  =    S n
                                eq : length (      v' :: l')     =    S n'
                             ============================
                                  S (length (snoc        l'  v)) = S (S n')         *)
      apply f_equal    .  (*         length (snoc        l'  v)  =    S n'          *)
(* following apply does these two steps in one step
      rewrite <- IHl'.               length (snoc        l'  v)  = length (snoc l' v) ; adds subgoal:  length l' = n'
      reflexivity.                   length              l'      =      n'
*)
      apply IHl'.         (*         length              l'      =      n'          *)
      inversion eq.       (*    H0 : length              l'      =      n' ; adds then uses
                             ============================
                                     length l'                   = length l'        *)
      reflexivity.
Qed.

(** **** Exercise: 2 stars, optional (practice) *)
(** May involve applying lemmas from earlier lectures or homeworks. *)

Theorem beq_nat_0_l : forall n,
   beq_nat 0 n = true ->
             n = 0.
Proof.
  intros n H. destruct n as [| n'].
  Case "n is 0". reflexivity.
  Case "n is S n'". (* H : beq_nat 0 (S n') = true
                       ===========================
                                      S n'  = 0     *)
    inversion H.
Qed.

Theorem beq_nat_0_r : forall n,
   beq_nat n 0 = true ->
           n   = 0.
Proof.
  intros n H. destruct n as [| n'].
  Case "n is 0". reflexivity.
  Case "n is S n'". (* H : beq_nat (S n') 0 = true
                       ===========================
                                    S n'    = 0    *)
    inversion H.
Qed.
(** [] *)


(* ###################################################### *)
(** * Using Tactics on Hypotheses *)
(* tactic:using tactics on hypotheses *)

(** Default: tactics work on goal.
    Some have variant that work on statement in context.

    E.G., tactic [simpl in H] does simplification of hypothesis named
    [H] in context. *)

Theorem S_inj : forall (n m : nat) (b : bool),
     beq_nat (S n) (S m) = b  ->
     beq_nat    n     m  = b.
Proof.
  intros n m b H. (* H : beq_nat (S n) (S m) = b
                     ============================
                         beq_nat    n     m  = b  *)
  simpl in H.     (* H : beq_nat    n     m  = b  *)
  apply H.
Qed.

(** BACKWARD REASONING:

    Ordinary [apply] rewrites a goal matching [L2] into a subgoal [L1].

    [apply L] is "backward reasoning":
    if [L1->L2] known and trying to prove [L2], suffices to prove [L1].

    FORWARD REASONING:

    [apply L in H] gives form of "forward reasoning":
    from [L1 -> L2] and hypothesis matching [L1]
    gives hypothesis matching [L2].

    [apply L in H] matches conditional statement [L]
    (of the form [L1 -> L2], say) against [H] in the context.

    [apply L in H] matches [H] against [L1] and replaces it with [L2] if successful.

    Variant of previous proof using forward reasoning throughout: *)

Theorem silly3' : forall (n : nat),
           (beq_nat n 5 = true -> beq_nat (S (S n)) 7 = true) ->
     true = beq_nat n 5  ->
                           true = beq_nat (S (S n)) 7.
Proof.
  intros n eq H.  (* eq : ...
                     H  : true = beq_nat n 5
                     ======================================================
                                                true = beq_nat (S (S n)) 7 *)
  symmetry in H.  (* eq : beq_nat n 5 = true -> beq_nat (S (S n)) 7 = true
                     H  : beq_nat n 5 = true                               *)
  apply eq in H.  (* H  :                       beq_nat (S (S n)) 7 = true *)
  symmetry in H.  (* H  :                       true = beq_nat (S (S n)) 7
                     ======================================================
                                                true = beq_nat (S (S n)) 7 *)
  apply H.
Qed.

(** Forward reasoning starts from what is _given_ (premises, previously proven theorems)
    and iteratively draws conclusions from them until goal is reached.

    Backward reasoning starts from the _goal_
    and iteratively reasons about what would imply the goal
    until premises or previously proven theorems are reached.

    Most informal proofs in math or CS class use forward reasoning.

    Coq favors backward reasoning, but sometimes forward style useful. *)

(** **** Exercise: 3 stars (plus_n_n_injective) *)
(** Practice using "in" variants in this exercise.
    Hint: use the plus_n_Sm lemma *)
Theorem plus_n_n_injective : forall n m,
     n + n = m + m ->
         n = m.
Proof.
  intros n. induction n as [| n'].
  Case "n is 0".
    intros m eq. destruct m as [| m'].
    SCase "m is 0". reflexivity.
    SCase "m is S m'". inversion eq.
  Case "n is S n'".               (*   IHn' : forall m : nat,
                                             n' +   n'  =   m +     m ->
                                                    n'  =   m            *)
    intros m eq. destruct m as [| m'].
    SCase "m is 0". inversion eq.
    (* rest from https://github.com/flavioc/coq/blob/master/Poly.v *)
    SCase "m is S m'".            (* eq :  S n' + S n'  = S m' +  S m'
                                     ============================
                                                  S n'  =         S m'   *)
      inversion eq.               (* eq : ...
                                     H0 :    n' + S n'  =    m' + S m'   *)
      rewrite <- plus_n_Sm in H0. (* H0 : S (n' +   n') =    m' + S m'   *)
      rewrite <- plus_n_Sm in H0. (* H0 : S (n' +   n') = S (m' +   m')  *)
      inversion H0.               (* H1 :    n' +   n'  =    m' +   m'   *)
      apply IHn' in H1.           (* H1 :           n'  =           m'   *)
      rewrite -> H1.              (*              S m'  =         S m'   *)
      reflexivity.
Qed.
(** [] *)

(* ###################################################### *)
(** * Varying the Induction Hypothesis *)
(* tactic:intros - varying the induction hypothesis *)

(** Sometimes need to control the form of induction hypothesis.

    Need to be careful about which assumptions we move (using [intros])
    from goal to context _before_ invoking the [induction] tactic.

    E.G., Prove [double] function is injective
    -- i.e., always maps different args to different results:

    _Start_ proof carefully: if begin with
      [intros n. induction n.]
    then good.

    But begin with
      [intros n m. induction n.]
    then get stuck in middle of inductive case: *)

Theorem double_injective_FAILED : forall n m,
     double n = double m ->
            n =        m.
Proof.
  intros n m. induction n as [| n'].
  Case "n = O". simpl. intros eq. destruct m as [| m'].
    SCase "m = O". reflexivity.
    SCase "m = S m'". inversion eq.
  Case "n = S n'". intros eq. destruct m as [| m'].
    SCase "m = O". inversion eq.
    SCase "m = S m'".  apply f_equal.
      (* Stuck.
         Induction hypothesis, [IHn'], does not give us [n' = m']
         -- there is an extra [S] in the way
         -- so the goal is not provable. *)
Abort.

(** Problem: at point we invoke induction hypothesis,
    we have already introduced [m] into the context.
    [intros n m] above said:
      "Let's consider some _particular_ [n] and [m]..."
    so need to prove
      if [double n = double m] for _this particular_ [n] and [m],
      then [n = m].

    Then following [induction n] says: show goal by induction on [n] : going to prove:

      - [P n]  =  "if [double n = double m], then [n = m]"

    holds for all [n] by showing

      - [P O]

         (i.e., "if [double O = double m] then [O = m]")

      - [P n -> P (S n)]

        (i.e., "if [double    n  = double m] then   [n = m]" implies
               "if [double (S n) = double m] then [S n = m]").

    But second statement says for a _particular_ [m], if we know

      - "if [double n = double m] then [n = m]"

    then we can prove

       - "if [double (S n) = double m] then [S n = m]".

    Say [m] is [5].  Then statement says, if we know

      - [Q] = "if [double    n  = 10] then   [n = 5]"

    then we can prove

      - [R] = "if [double (S n) = 10] then [S n = 5]".

    But knowing [Q] doesn't give us any help with proving [R]!

    (To try proving [R] from [Q], say
      "Suppose [double (S n) = 10]..."
    but stuck: knowing [double (S n)] is [10] tells us nothing
    about whether [double n] is [10], so [Q] is useless at this point.) *)

(** Summary:
    Trying to prove by induction on [n]
    when [m] is already in context
    doesn't work because trying to prove a relation involving _every_ [n]
    but just a _single_ [m]. *)

(** Good proof of [double_injective] leaves [m] in goal statement
    at point where [induction] tactic invoked on [n]: *)

Theorem double_injective : forall n m,
     double n = double m ->
            n =        m.
Proof.
  intros n. induction n as [| n'].
  Case "n = O".                       (* forall m : nat,    double 0   = double    m ->   0  =   m  *)
    simpl.                            (* forall m : nat,           0   = double    m ->   0  =   m  *)
    intros m eq. destruct m as [| m'].
    SCase "m = O".                    (*                                                  0  =   0  *)
      reflexivity.
    SCase "m = S m'".                 (*                                                  0  = S m' *)
      inversion eq.
  Case "n = S n'".                    (* IHn' :
                                         forall m : nat, double    n'  = double    m ->   n' =   m
                                         ======================================================
                                         forall m : nat, double (S n') = double    m -> S n' =   m  *)
    (* Note: both goal and induction hypothesis have changed from last proof:
       - goal more general: prove for _every_ [m]
       - IH  more flexible: can choose any [m] when we apply the IH.  *)
    intros m eq.                      (*                                                S n' =   m  *)
    (* Now choose particular [m] and introduce assumption [double n = double m]. *)
    destruct m as [| m'].
    SCase "m = O".                    (*                                                S n' =   0  *)
      inversion eq.
    SCase "m = S m'".                 (*                                                S n' = S m' *)
      apply f_equal.                  (*                                                  n' =   m' *)
      (* Since in second branch of [destruct m],
         the [m'] in context here is predecessor of one we started with via [intros].
         Since also in [S] branch of the induction, this is perfect:
         if we instantiate generic [m] in IH with the [m'] we have right now
         (this instantiation is performed automatically by [apply]),
         then [IHn'] gives exactly what we need. *)
      apply IHn'.                     (*                 double    n'  = double    m'               *)
      inversion eq.                   (* eq :            double (S n') = double (S m')
                                         H0 :            double    n'  = double    m'
                                         ========================================================
                                                         double n'     = double    n'               *)
      reflexivity.
Qed.

(** Lesson: be careful about using induction to try to prove something too specific.
    If proving property of [n] and [m] by induction on [n],
    may need to leave [m] generic. *)

(** HC: BEGIN Material from SF-2012-07-25 *)

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

(** Inductive proof above fails because proof set up with
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
  intros n.              (* This is the GOOD step - only intro [n] now, [m] later       *)
  induction n as [| n'].
  Case "n = 0".          (* forall m : nat, true = beq_nat 0         m   ->   0  = m    *)
    intros m.            (*                 true = beq_nat 0         m   ->   0  = m    *)
    destruct m as [| m'].
    SCase "m = 0".       (*                 true = beq_nat 0         0   ->   0  = 0    *)
      simpl.             (*                 true = true                  ->   0  = 0    *)
      reflexivity.
    SCase "m = S m'".    (*                 true = beq_nat 0      (S m') ->   0  = S m' *)
      simpl.             (*                 true = false                 ->   0  = S m' *)
      intros contra.     (*                                                   0  = S m' ; contra: true = false *)
      inversion contra.
  Case "n = S n'".       (* IHn' :
                            forall m : nat, true = beq_nat n'        m   ->   n' = m    *)
                         (* forall m : nat, true = beq_nat (S n')    m   -> S n' = m    *)
    intros m.            (*                 true = beq_nat (S n')    m   -> S n' = m    *)
    destruct m as [| m'].
    SCase "m = 0".       (*                 true = beq_nat (S n')    0   -> S n' = 0    *)
      simpl.             (*                 true = false                 -> S n' = 0    *)
      intros contra.     (*                                                 S n' = 0 ; contra: true = false *)
      inversion contra.
    SCase "m = S m'".    (*                 true = beq_nat (S n') (S m') -> S n' = S m' *)
      simpl.             (*                 true = beq_nat    n'     m'  -> S n' = S m' *)
      intros H.          (*                                                 S n' = S m' ; H : true = beq_nat n' m' *)
      apply eq_remove_S. (*                                                   n' = m'   *)
      apply IHn'.        (*                 true = beq_nat    n'     m'                 *)
      apply H.
Qed.

(** HC: END Material from SF-2012-07-25 *)

(** Following needs similar proof: *)

(** **** Exercise: 2 stars (beq_nat_true) *)

Theorem beq_nat_true : forall n m,
    beq_nat n   m = true ->
            n = m.
Proof.
  intros n.              (* This is the GOOD step - only intro [n] now, [m] later        *)
  induction n as [| n'].
  Case "n = 0".          (* forall m : nat, beq_nat    0      m   = true ->    0  =   m  *)
    intros m.            (*                 beq_nat    0      m   = true ->    0  =   m  *)
    destruct m as [| m'].
    SCase "m = 0".       (*                 beq_nat    0      0   = true ->    0  =   0  *)
      simpl.             (*                 true                  = true ->    0  =   0  *)
      reflexivity.
    SCase "m = S m'".    (*                 beq_nat    0   (S m') = true ->    0  = S m' *)
      simpl.             (*                 false                 = true ->    0  = S m' *)
      intros contra.     (* contra :        false                 = true
                            ============================
                                                                               0  = S m' *)
      inversion contra.
  Case "n = S n'".       (* forall m : nat, beq_nat (S n')    m   = true -> S n'  =   m  *)
    intros m.            (*                 beq_nat (S n')    m   = true -> S n'  =   m  *)
    destruct m as [| m'].
    SCase "m = 0".       (*                 beq_nat (S n')    0   = true -> S n'  =   0  *)
      simpl.             (*                 false                 = true -> S n'  =   0  *)
      intros contra.     (*   contra :      false                 = true
                            ============================
                                                                            S n'  =   0  *)
      inversion contra.
    SCase "m = S m'".    (* IHn' :
                            forall m : nat, beq_nat    n'     m   = true ->   n'  =   m
                            ===========================================================
                                            beq_nat (S n') (S m') = true -> S n'  = S m' *)
      simpl.             (*                 beq_nat    n'     m'  = true -> S n'  = S m' *)
      intros H.          (*    H :          beq_nat    n'     m'  = true
                            ===========================================================
                                                                            S n'  = S m' *)
      apply f_equal.     (*                                                   n'  =   m' *)
      apply IHn'.        (*                 beq_nat    n'     m'  = true                 *)
      apply H.
Qed.
(** [] *)

(** **** Exercise: 2 stars, advanced (beq_nat_true_informal) *)
(** Give a careful informal proof of [beq_nat_true], being as explicit
    as possible about quantifiers. *)

(* FILL IN HERE *)
(** TODO/informal *)
(** [] *)

(* tactic:generalize dependent *)
(** Doing fewer [intros] before [induction] does not always work directly.
    Sometimes need to _rearrange_ quantified variables.
    E.G., prove [double_injective] by induction on [m] instead of [n]. *)

Theorem double_injective_take2_FAILED : forall n m,
     double n = double m ->
     n = m.
Proof.
  intros n m. induction m as [| m'].
  Case "m = O".
    simpl.                           (*        double n = double    0   -> n  =   0 *)
    intros eq. destruct n as [| n'].
    SCase "n = O".                   (*                                    0  =   0 *)
      reflexivity.
    SCase "n = S n'".                (*                                  S n' =   0 *)
      inversion eq.
  Case "m = S m'".                   (* IHm' : double n = double    m'  -> n  =   m' *)
                                     (*        double n = double (S m') -> n  = S m' *)
    intros eq. destruct n as [| n'].
    SCase "n = O".                   (*                                    0  = S m' *)
      inversion eq.
    SCase "n = S n'".                (*                                  S n' = S m' *)
      apply f_equal.                 (*                                    n' =   m' *)
      (* Stuck just like before. *)
Abort.

(** Problem: to do induction on [m], must first introduce [n].
    (note: [induction m] without introducing [n], Coq auto introduces [n])   *)

(** Potential solution: rewrite lemma statement so [m] is quantified before [n].
    But, do not want to mangle statements of lemmas to fit needs
    of particular proof strategy. *)

(** Solution: [generalize dependent] tactic:
    First introduce all quantified variables then _re-generalize_ one or more of them,
    taking them out of context, putting back at beginning of goal. *)

Theorem double_injective_take2 : forall n m,
     double n = double m ->
            n =        m.
Proof.
  intros n m.             (* [n] [m] both in context *)
  generalize dependent n. (* [n] back in goal, now can do induction on [m] with sufficiently general IH. *)
  induction m as [| m'].
  Case "m = O".           (*    forall n : nat, double n  = double    0   ->   n =    0 *)
    simpl.                (*    forall n : nat, double n  =           0   ->   n =    0 *)
    intros n eq.
    destruct n as [| n'].
    SCase "n = O".        (*                                                   0  =   0 *)
      reflexivity.
    SCase "n = S n'".     (*                                                 S n' =   0 *)
      inversion eq.
  Case "m = S m'".        (* IHm' :
                                forall n : nat, double n  = double    m'  ->   n  =   m'
                                ========================================================
                                forall n : nat, double n  = double (S m') ->   n  = S m' *)
    intros n eq. destruct n as [| n'].
    SCase "n = O".        (*                                                   0 =  S m' *)
      inversion eq.
    SCase "n = S n'".     (*                                                 S n' = S m' *)
      apply f_equal.      (*                                                   n' =   m' *)
      apply IHm'.         (*                    double n' = double    m'                 *)
      inversion eq.       (*                    double n' = double    n'                 *)
      reflexivity.
Qed.

(** Informal proof of above:
    Note: proposition proved by induction leaves [n] quantified,
    corresponding to use of generalize dependent in formal proof.

_Theorem_: For any nats [n] and [m], if [double n = double m], then
  [n = m].

_Proof_: Let [m] be a [nat]. We prove by induction on [m] that, for
  any [n], if [double n = double m] then [n = m].

  - First, suppose [m = 0], and suppose [n] is a number such
    that [double n = double m].  We must show that [n = 0].

    Since [m = 0], by the definition of [double] we have [double n =
    0].  There are two cases to consider for [n].  If [n = 0] we are
    done, since this is what we wanted to show.  Otherwise, if [n = S
    n'] for some [n'], we derive a contradiction: by the definition of
    [double] we would have [double n = S (S (double n'))], but this
    contradicts the assumption that [double n = 0].

  - Otherwise, suppose [m = S m'] and that [n] is again a number such
    that [double n = double m].  We must show that [n = S m'], with
    the induction hypothesis that for every number [s], if [double s =
    double m'] then [s = m'].

    By the fact that [m = S m'] and the definition of [double], we
    have [double n = S (S (double m'))].  There are two cases to
    consider for [n].

    If [n = 0], then by definition [double n = 0], a contradiction.
    Thus, we may assume that [n = S n'] for some [n'], and again by
    the definition of [double] we have [S (S (double n')) = S (S
    (double m'))], which implies by inversion that [double n' = double
    m'].

    Instantiating the induction hypothesis with [n'] thus allows us to
    conclude that [n' = m'], and it follows immediately that [S n' = S
    m'].  Since [S n' = n] and [S m' = m], this is just what we wanted
    to show. [] *)

(** **** Exercise: 3 stars (gen_dep_practice) *)

(** Prove by induction on [l]: *)
(* NO: https://bitbucket.org/lunaticas/sf/src/d63733c062ac/Gen.v *)
(* https://github.com/flavioc/coq/blob/master/Ind.v *)
Theorem index_after_last: forall (n : nat) (X : Type) (l : list X),
     length  l = n ->
     index n l = None.
Proof.
  intros n X l.            (*                    length       l   =   n -> index    n                     l   = None *)
  generalize dependent n.  (*    forall n : nat, length       l   =   n -> index    n                     l   = None *)
  induction l as [| e l'].
  Case "l is []".          (*    forall n : nat, length       []  =   n -> index    n                     []  = None *)
    simpl.                 (*    forall n : nat, 0                =   n -> None                               = None *)
    reflexivity.
  Case "l is e::'l".       (*    forall n : nat, length (e :: l') =   n -> index    n               (e :: l') = None *)
    intros n eq.           (*   eq :             length (e :: l') =   n
                                ===============================================================================
                                                                           index    n               (e :: l') = None *)
    destruct n as [| n'].
    SCase "n is 0".        (*                                              index    0               (e :: l') = None *)
      simpl.               (*                                                                       Some e = None    *)
      inversion eq.
    SCase "n is S n'".     (*  IHl' :
                                 forall n : nat, length       l'  =   n -> index    n                     l'  = None
                                 eq :            length (e :: l') = S n'
                                 ==============================================================================
                                                                           index (S n')             (e :: l') = None *)
      inversion eq.        (*    H0 :            length       l'  =   n'
                                 ==============================================================================
                                                                           index (S (length l'))    (e :: l') = None *)
      (* proof A:  *)
      simpl.               (*                                              index    (length l')           l'  = None *)
      rewrite H0.          (*                                              index    n'                    l'  = None *)
      apply IHl'.          (*                    length       l' =    n'                                             *)
      apply H0.
      (* proof B:  *)
  (*  apply IHl'.  *)      (*                    length       l' = pred  (S (length l'))                             *) (* TODO - understand *)
  (*  simpl.       *)      (*                    length       l' =           length l'                               *)
  (*  reflexivity. *)
Qed.
(** [] *)

(** **** Exercise: 3 stars, advanced, optional (index_after_last_informal) *)
(* TODO/informal *)
(** Write an informal proof corresponding to your Coq proof
    of [index_after_last]:

     _Theorem_: For all sets [X], lists [l : list X], and numbers
      [n], if [length l = n] then [index n l = None].

     _Proof_:
     (* FILL IN HERE *)
[]
*)

(** **** Exercise: 3 stars, optional (gen_dep_practice_more) *)
(** Prove this by induction on [l]. *)
(* HC: this is exactly like the proof in 2012-07-25 x03Poly except adding
   [generalize dependent n] *)
Theorem length_snoc''' : forall (n : nat) (X : Type)
                                (v : X)   (l : list X),
     length l = n ->
     length (snoc l v) = S n.
Proof.
  intros n X v l.
  generalize dependent n.
  induction l as [| v' l'].
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
(** [] *)


(** **** Exercise: 3 stars, optional (app_length_cons) *)
(** Prove by induction on [l1], without using [app_length]. *)

Theorem hc_app_len_cons_l_eq_len_cons_r : forall (X : Type) (l1 l2 : list X) (x : X),
    length ((x :: l1) ++ l2) = length (l1 ++ (x :: l2)).
Proof.
  intros.
  induction l1 as [| n l1'].
  Case "l1 is []".
    simpl. reflexivity.
  Case "l1 is n::l1'".
    destruct l2 as [| e l2'].
    SCase "l2 is []".
      simpl. rewrite <- IHl1'. simpl. reflexivity.
    SCase "l2 is e::l2'".
      simpl. rewrite <- IHl1'. simpl. reflexivity.
Qed.

Theorem app_length_cons : forall (X : Type) (l1 l2 : list X) (x : X) (n : nat),
       length (l1 ++ (x :: l2)) = n ->
    S (length (l1 ++       l2)) = n.
Proof.
  intros X l1.
  destruct l1 as [| e l1'].
  Case "l1 is []".      (*  forall (l2 : list X) (x : X) (n : nat),
                                  length ([] ++         x :: l2) = n -> S (length ([] ++          l2)) = n *)
    simpl.              (*  forall l2 : list X, X ->
                            forall n : nat,
                               S (length                     l2) = n -> S (length                 l2)  = n *)
    intros.             (* H : S (length                     l2) = n
                           ============================================================================
                                                                        S (length                 l2)  = n *)
    apply H.
  Case "l1 is e::l1'".  (* forall (l2 : list X) (x : X) (n : nat),
                                  length ((e :: l1') ++ x :: l2) = n -> S (length ((e :: l1') ++ l2))  = n *)
    intros l2 x n H.    (* H :    length ((e :: l1') ++ x :: l2) = n
                           ============================================================================
                                                                        S (length ((e :: l1') ++ l2))  = n *)
    rewrite <- H.       (*                                              S (length ((e :: l1') ++ l2))  =       length (     (e :: l1') ++ x :: l2)   *)
    rewrite <- hc_app_len_cons_l_eq_len_cons_r. (*                      S (length ((e :: l1') ++ l2))  =       length ((x :: e :: l1') ++      l2)   *)
    simpl.              (*                                           S (S (length       (l1' ++  l2))) = S (S (length            (l1'  ++      l2))) *)
    reflexivity.
Qed.
(** [] *)


(** **** Exercise: 4 stars, optional (app_length_twice) *)
(** Prove by induction on [l], without using app_length. *)

Lemma hc_add_succ_eq_succ_add : forall (n m :nat),
    S (n + m) = n + S m.
Proof.
  intros.
  induction n as [| n'].
  Case "n is 0".      (*         S (0 + m) =        0  + S m   *)
    simpl.            (*              S m  =             S m   *)
    reflexivity.
  Case "n is S n'".   (* IHn' : S (n' + m) =        n' + S m
                         ===================================
                              S (S n' + m)  =     S n' + S m   *)
    simpl.            (*     S (S (n' + m)) =    S (n' + S m)  *)
    rewrite IHn'.     (*     S (S (n' + m)) = S (S (n' +   m)) *)
    reflexivity.
Qed.

(* Under the guidance of David Darais (above too). *)
Theorem app_length_twice : forall (X:Type) (n:nat) (l:list X),
     length  l       = n      ->
     length (l ++ l) = n + n.
Proof.
  intros.
  generalize dependent n.  (* do not lock down 'n' at this point *)
  induction l as [| e l']. (* ; intros.*)  (* does intros on all subcases *)
  Case "l is []".
    intros n H.                         (* now choose a specific 'n'
                                              H : length []            =   n
                                           ============================
                                                                                      length (     []    ++      []) =      n  +   n   *)
    rewrite <- H.                       (*                                            length (     []    ++      []) = length [] + length [] *)
    simpl.                              (*                                                                         0 = 0               *)
    reflexivity.
  Case "l is e::l'".
    intros n H.                         (* IHl' : forall n : nat,
                                                     length        l'  =   n ->       length (      l'   ++      l')   =    n  +   n
                                              H :    length  (e :: l') =   n
                                           ============================
                                                                                      length ((e :: l')  ++ e :: l')   =    n  +   n   *)
    simpl.                              (*                                         S (length (      l'   ++ e :: l'))  =    n  +   n   *)
    destruct n as [| n'].
    SCase "n is 0".                     (*    H :    length (e :: l')  =   0
                                           ============================
                                                                                   S (length (       l'  ++ e :: l'))  =    0  +   0   *)
      simpl.                            (*                                         S (length (       l'  ++ e :: l'))  =    0          *)
      simpl in H.                       (*    H : S (length       l')  =   0                                                           *)
      inversion H.
    SCase "n is S n'".                  (*    H :    length (e :: l')  = S n'
                                           ============================
                                                                                   S (length (       l'  ++ e :: l'))  =  S n' + S n'  *)
      rewrite  <- hc_app_len_cons_l_eq_len_cons_r.
                                        (*                                         S (length ((e ::  l') ++      l'))  =  S n' + S n'  *)
      simpl.                            (*                                      S (S (length (       l'  ++      l'))) = S (n' + S n') *)
      simpl in H.                       (*    H : S (length       l') = S n'                                                           *)
      injection H.                      (*           length       l'  =   n' -> S (S (length (       l'  ++      l'))) = S (n' + S n') *)
      intros H1.                        (*   H1 :    length       l'  =   n'
                                           ============================
                                                                                S (S (length (       l'  ++      l'))) = S (n' + S n') *)
      assert (HA: length (l' ++ l') = n' + n').  (*   PAY ATTENTION  *)
      SSCase ("proof of assertion"). (*                                               length (       l'  ++      l')   =    n' +   n'  *)
        rewrite <- IHl'.             (*                                               length (       l'  ++      l')   = length (l' ++ l')
                                         subgoal 2 (ID 1977) is:
                                                     length       l' =    n'                                                           *)
        reflexivity.
        SSSCase ("proof of subgoal 2").
          apply H1.

                                        (*   HA :                                     length (       l'  ++      l')   =    n' +   n'
                                            ============================
                                                                                S (S (length (       l'  ++      l'))) = S (n' + S n') *)
      rewrite HA.                       (*                                      S (S (n' +   n'))                      = S (n' + S n') *)
      rewrite hc_add_succ_eq_succ_add.  (*                                         S (n' + S n')                       = S (n' + S n') *)
      reflexivity.
Qed.
(** [] *)

(* ###################################################### *)
(** * Using [destruct] on Compound Expressions *)
(* tactic:destruct on compound expressions *)

(** [destruct] used above for case analysis of value of a variable.
    Can also use [destruct] to reason by cases on result of an _expression_. *)

Definition sillyfun (n : nat) : bool :=
       if beq_nat n 3 then false
  else if beq_nat n 5 then false
  else                     false.

Theorem sillyfun_false : forall (n : nat),
  sillyfun n = false.
Proof.
  intros n.                      (* sillyfun n = false *)
  unfold sillyfun.               (* (if beq_nat n 3 then false else if beq_nat n 5 then false else false) = false *)
  destruct (beq_nat n 3).
  Case "beq_nat n 3 = true".     (* false = false *)
    reflexivity.
  Case "beq_nat n 3 = false".    (* (if beq_nat n 5 then false else false) = false *)
    destruct (beq_nat n 5).
    SCase "beq_nat n 5 = true".  (* false = false *)
      reflexivity.
    SCase "beq_nat n 5 = false". (* false = false *)
      reflexivity.
Qed.

(** Above, unfold [sillyfun] resulted in [if (beq_nat n 3) then ... else ...].
    Use [destruct (beq_nat n 3)] to reason about two cases:
    [n] is equal to [3] or not. *)

(** [destruct] tactic can be used to perform case analysis
    of results of arbitrary computations.

    If [e] is expression whose type is some inductively defined type [T], then,
    for each constructor [c] of [T], [destruct e] generates a subgoal
    in which all occurrences of [e] (in the goal and in the context)
    are replaced by [c].
*)

(** **** Exercise: 1 star (override_shadow) *)
Theorem override_shadow : forall (X:Type) x1 x2 k1 k2 (f : nat->X),
  (override (override f k1 x2) k1 x1) k2 = (override f k1 x1) k2.
Proof.
  intros. unfold override.
  destruct (beq_nat k1 k2).
  Case "beq_nat k1 k2 = true".  reflexivity.
  Case "beq_nat k1 k2 = false". reflexivity.
Qed.
(** [] *)

Print split.

(** **** Exercise: 3 stars, optional (combine_split) *)
(** HC: illustrates not doing intro on everything at beginning and destruct of an expression. *)
(* https://github.com/joshcough/software-foundations/blob/master/Poly.v *)
Theorem combine_split : forall X Y (l : list (X * Y)) l1 l2,
    split   l = (l1, l2) ->
    combine      l1  l2     = l.
Proof.
  intros X Y l.                         (* do NOT assume specific l1 l2 at this point                                                    *)
  induction l as [| (x,y) l'].          (* l is a list or pairs                                                                          *)
  Case "l is [] ".                      (*        forall (l1 : list X) (l2 : list Y),
                                                  split []                      = (l1, l2) -> combine l1        l2        =           [] *)
    intros.                             (* now assume specific l1 l2 as output of split, input to combine
                                           l1 : list X  l2 : list Y
                                           H :    split []                      = (l1, l2)
                                           ============================
                                                                                              combine l1        l2        =           [] *)
    simpl in H.                         (* since l1 l2 are specific, now simplified output of split determines their value
                                           H    : ([], [])                      = (l1, l2)                                               *)
    inversion H.                        (* inversion "sets" the values of l1 l2 in H then carries values forward to combine goal
                                           H    : ([], [])                      = (l1, l2)
                                           H1   :  []                           =  l1
                                           H2   :      []                       =      l2
                                           ============================
                                                                                              combine []        []        =           [] *)
    reflexivity.
  Case "l is (x,y) :: l'".              (* IHl' : forall (l1 : list X) (l2 : list Y),
                                                  split l'                      = (l1, l2) -> combine l1        l2        =           l'
                                           ============================
                                                  forall (l1 : list X) (l2 : list Y),
                                                  split ((x, y) :: l')          = (l1, l2) -> combine l1        l2        = (x, y) :: l' *)
    simpl.                              (* apply split once : this "splits" the pair, putting it at head of l1 l2
                                                  forall (l1 : list X) (l2 : list Y),
                                  (let (g, d) := split l' in (x :: g,  y :: d)) = (l1, l2) -> combine l1        l2        = (x, y) :: l' *)
    destruct (split l') as (xs,ys).     (* structure the output of split
                                           IHl' : forall (l1 : list X) (l2 : list Y),
                                                         (xs,               ys) = (l1, l2) -> combine l1        l2        =           l'
                                           ============================
                                                  forall (l1 : list X) (l2 : list Y),
                                                    (x :: xs,          y :: ys) = (l1, l2) -> combine l1        l2        = (x, y) :: l' *)
    intros.                             (* pick specific (l1, l2) pair as the now structured output of split
                                           l1 : list X  l2 : list Y
                                           H    :   (x :: xs,          y :: ys) = (l1, l2)                                               *)
    inversion H.                        (* "set" l1 l2 from pair, carry forward into goal as input to combine
                                           H1 :      x :: xs                    =  l1
                                           H2 :                        y :: ys  =      l2
                                           ============================
                                                                                              combine (x :: xs) (y :: ys) = (x, y) :: l' *)
    simpl.                              (* do one application of combine
                                                                                    (x, y) :: combine       xs        ys  = (x, y) :: l' *)


    assert (IHuse: combine xs ys = l'). (* prove output of combine via induction
      SCase "assertion".                                                                      combine       xs        ys  =           l' *)
      apply IHl'.                       (*                                                                 (xs,       ys) = (xs, ys)     *)
      reflexivity.


                                        (*                                          (x, y) :: combine       xs        ys  = (x, y) :: l' *)
    rewrite IHuse.                      (* use output of combine
                                                                                    (x, y) :: l'                          = (x, y) :: l' *)
    reflexivity.
Qed.
(** [] *)

(* tactic:destruct:eqn *)
(** Sometimes, [destruct] on compound expression (a non-variable)
    erases info needed to complete a proof: *)

Definition sillyfun1 (n : nat) : bool :=
  if beq_nat n 3 then true
  else if beq_nat n 5 then true
  else false.

(** Then prove [sillyfun1 n] is [true] only when [n] is odd.
    Try starting like [sillyfun] above: *)

Theorem sillyfun1_odd_FAILED : forall (n : nat),
     sillyfun1 n = true ->
     oddb n = true.
Proof.
  intros n eq. unfold sillyfun1 in eq.
  destruct (beq_nat n 3).
  (* stuck... *)
Abort.

(** Stuck. Context does not contain enough info to prove goal.
    Problem: the [destruct] substitution threw away all occurrence of [beq_nat n 3]
    but need to keep memory of this expr and how it was destructed
    because we need to reason that in this branch of case analysis,
      [beq_nat n 3 = true],
    says
      [n = 3],
    therefore [n] is odd.

    Want to substitute away all occurences of [beq_nat n 3]
    but also add equation to context that records which the current case.

    [eqn:] qualifier introduces such an equation with given name: *)

Theorem sillyfun1_odd : forall (n : nat),
     sillyfun1 n = true ->
     oddb n = true.
Proof.
  intros n eq. unfold sillyfun1 in eq.
  destruct (beq_nat n 3) eqn:Heqe3.
  (* Now we have the same state as at the point where we got stuck
    above, except that the context contains an extra equality
    assumption, which is exactly what we need to make progress. *)
    Case "e3 = true". apply beq_nat_true in Heqe3.
      rewrite -> Heqe3. reflexivity.
    Case "e3 = false".
     (* When we come to the second equality test in the body of the
       function we are reasoning about, we can use [eqn:] again in the
       same way, allow us to finish the proof. *)
      destruct (beq_nat n 5) eqn:Heqe5.
        SCase "e5 = true".
          apply beq_nat_true in Heqe5.
          rewrite -> Heqe5. reflexivity.
        SCase "e5 = false". inversion eq.
Qed.


(** **** Exercise: 2 stars (destruct_eqn_practice) *)
(* TODO STUDY bool_fn_applied_thrice *)
Theorem bool_fn_applied_thrice :
  forall (f : bool -> bool) (b : bool),
  f (f (f b)) = f b.
Proof.
  intros.
  destruct (f b) eqn:fb.
  Case "f b is true".
    destruct (f true) eqn:ftrue.   (* fb    :  f b      =   true
                                      ftrue :  f true   =   true
                                      ============================
                                            f (f true)  =   true  *)
    SCase "f true is true".
      apply ftrue.
    SCase "f true is false".
      destruct b.
      SSCase "b is true".          (* fb     : f true   =   true
                                      ftrue  : f true   =   false
                                      ============================
                                               f false  =   true  *)
        rewrite <- fb.             (*          f false  = f true  *)
        inversion ftrue.           (*       f (f true)  = f true  *)
        rewrite fb.                (*          f true   =   true  *)
        apply fb.
      SSCase "b is false".         (* fb     : f false  =   true
                                      ftrue  : f true   =   false
                                      ============================
                                               f false  =   true  *)
        apply fb.
  Case "f b is false".
    destruct (f false) eqn:ffalse.
    SCase "f false is true".       (* fb     : f b      =   false
                                      ffalse : f false  =   true
                                      ============================
                                               f true   =   false *)
      destruct b.
      SSCase "b is true".          (* fb     : f true   =   false
                                      ============================
                                               f true   =   false *)
        apply fb.
      SSCase "b is false".         (* fb     : f false  =   false
                                      ffalse : f false  =   true
                                      ============================
                                               f true   =   false *)
        rewrite <- fb.             (*          f true   = f false *)
        inversion ffalse.          (*       f (f false) = f false *)
        rewrite fb.                (*          f false  =   false *)
        apply fb.
    SCase "f false is false".      (* fb     : f b      =   false
                                      ffalse : f false  =   false
                                      ============================
                                               f false  =   false *)
      destruct b.
      SSCase "b is true".          (*          f false  =   false *)
        apply ffalse.
      SSCase "b is false".         (*          f false  =   false *)
        apply ffalse.
Qed.
(** [] *)

(** **** Exercise: 2 stars (override_same) *)
(* For any key, the overriden function returns the same as the non-overriden function. *)
(* https://github.com/flavioc/coq/blob/master/Poly.v *)

Theorem beq_equal : forall (a b : nat),
    beq_nat a   b = true ->
	    a = b.
Proof.
  intros a.
  induction a as [| a'].
  Case "a is 0".
    destruct b as [| b'].
    SCase "b is 0".           (*                          beq_nat    0      0   = true ->   0  =   0  *)
      simpl.                  (*                          true                  = true ->   0  =   0  *)
      intros eq.              (*                                                            0  =   0  *)
      reflexivity.
    SCase "b is S b'".        (*                          beq_nat    0   (S b') = true ->   0  = S b' *)
      simpl.                  (*                          false                 = true ->   0  = S b' *)
      intros contra.          (* contra :                 false                 = true
                                 ===================================================================
                                                                                            0  = S b' *)
      inversion contra.
  Case "a is S a'".
    destruct b as [| b'].
    SCase "b is 0".
      simpl.                  (*                          false                 = true -> S a' =   0  *)
      intros contra.
      inversion contra.
    SCase "b is S b'".
      intros eq.              (* IHa'   : forall b : nat, beq_nat    a'     b   = true ->   a' =   b
                                   eq   :                 beq_nat (S a') (S b') = true
                                 ===================================================================
                                                                                          S a' = S b' *)
      inversion eq.           (*   H0   :                 beq_nat    a'     b'  = true                *)
      apply IHa' in H0.       (*   H0   :                                                   a' =   b' *)
                              (* ===================================================================  *)
      rewrite H0.             (*                                                          S b' = S b' *)
      reflexivity.
Qed.

Theorem override_same : forall (X:Type) x1 k1 k2 (f : nat->X),
            f k1        = x1 ->
  (override f k1 x1) k2 = f k2.
Proof.
  intros X x1 k1 k2 f eq.
  unfold override.                  (*  eq : f k1 = x1
                                         ============================
                                         (if beq_nat k1   k2        then x1 else f k2) = f k2 *)
  destruct (beq_nat k1 k2) eqn:BEQ. (* BEQ : beq_nat k1   k2 = true
                                         ============================
                                                                         x1            = f k2 *)
  Case "true".
    apply beq_equal in BEQ.         (* BEQ :         k1 = k2                                  *)
                                    (*    ============================                        *)
    rewrite <- eq.                  (*                                 f k1            = f k2 *)
    rewrite BEQ.                    (*                                 f k2            = f k2 *)
    reflexivity.
  Case "false".                     (*                                 f k2            = f k2 *)
    reflexivity.
Qed.
(** [] *)

(* ################################################################## *)
(** * Review *)
(* tactic:summary of tactics *)

(** We now have most of what we need to get work done.  More will be
    introduced later.  Main thing missing is _automation_ tactics that
    make Coq do low-level work automatically.

    Summary:

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

      - [destruct... eqn:...]:
        specify the name of an equation to be added to the context,
        recording the result of the case analysis

      - [induction... as...]:
        induction on values of inductively defined types

      - [inversion]:
        reason by injectivity and distinctness of constructors

      - [assert (e) as H]:
        introduce a "local lemma" [e] and call it [H]

      - [generalize dependent x]:
        move the variable [x] (and anything else that depends on it)
        from the context back to an explicit hypothesis in the goal
        formula
*)

(* ###################################################### *)
(** * Additional Exercises *)

(** **** Exercise: 3 stars (beq_nat_sym) *)

Theorem beq_nat_sym : forall (n m : nat),
    beq_nat n m =
    beq_nat m n.
Proof.
  intros.
  generalize dependent m.  (*    this is the critical step *)
  induction n as [| n'].
  intros m.
  Case "n is 0".           (*                        beq_nat    0     m    = beq_nat    m      0 *)
    destruct m as [| m'].
    SCase "m is 0".        (*                        beq_nat    0     0    = beq_nat    0      0  *)
      reflexivity.
    SCase "m is S m'".     (*                        beq_nat    0  (S m')  = beq_nat (S m')    0  *)
      rewrite zero_nbeq_S. (*                        false                 = beq_nat (S m')    0  *)
      rewrite S_nbeq_0.    (*                        false                 = false *)
      reflexivity.
  Case "n is S n'".        (* IHn' : forall m : nat, beq_nat    n'     m   = beq_nat    m      n'  *)
    destruct m as [| m'].
    SCase "m is 0".        (*                        beq_nat (S n')    0   = beq_nat    0   (S n') *)
      rewrite zero_nbeq_S.
      rewrite S_nbeq_0.
      reflexivity.
    SCase "m is S m'".     (*                        beq_nat (S n') (S m') = beq_nat (S m') (S n') *)
      simpl.               (*                        beq_nat    n'     m'  = beq_nat    m'     n'  *)
      rewrite IHn'.        (*                        beq_nat    m'     n'  = beq_nat    m'     n'  *)
      reflexivity.
Qed.
(** [] *)

(** **** Exercise: 3 stars, advanced, optional (beq_nat_sym_informal) *)
(* TODO/informal *)
(** informal proof of formal proof above:

   Theorem: For any [nat]s [n] [m], [beq_nat n m = beq_nat m n].

   Proof:
   (* FILL IN HERE *)
[]
 *)

(** **** Exercise: 3 stars, optional (beq_nat_trans) *)
Theorem beq_nat_trans : forall n m p,
    beq_nat n m = true ->
    beq_nat m p = true ->
    beq_nat n p = true.
Proof.
  intros n m p.              (*       beq_nat n   m = true -> beq_nat m   p = true -> beq_nat n p = true *)
  intro eq1.                 (* eq1 : beq_nat n   m = true
                                ============================
                                                              beq_nat m   p = true -> beq_nat n p = true *)
  apply beq_equal in eq1.    (* eq1 :         n = m *)
  intros eq2.                (* eq2 :                         beq_nat m   p = true
                                ============================
                                                                                      beq_nat n p = true *)
  apply beq_equal in eq2.    (* eq2 :                                 m = p  *)
  rewrite eq2 in eq1.        (* eq1  :        n = p *)
  rewrite eq1.               (*                                                       beq_nat p p = true *)
  symmetry.                  (*                                                       true = beq_nat p p *)
  (* replace next two with: apply beq_nat_refl *)
  rewrite <- beq_nat_refl.   (*                                                       true = true        *)
  reflexivity.
Qed.
(** [] *)

(** **** Exercise: 3 stars, advanced (split_combine) *)
(** Proved above: combine_split: [combine] is inverse of [split].
    Now prove that [split] is inverse of [combine].

    Hints:
    - Keep IH general (only necessary [intros]).
    - What property is needed of [l1] and [l2] for following to be true:
        [split (combine l1 l2) = (l1,l2)]
*)

Lemma hc_length_l_0_means_l_nil : forall X (l : list X),
    length l = 0 -> l = [].
Proof.
  intros.
  induction l.
  reflexivity.
  inversion H.
Qed.

Lemma hc_cons_length_eq_length :  forall X Y (x : X) (y : Y) (l1 : list X) (l2 : list Y),
    length (x :: l1) = length (y :: l2) -> length (l1) = length (l2).
Proof.
  intros X Y x y l1.
  induction l1 as [| v l1'].
  Case "l is []".
    destruct l2 as [| z l2'].
    SCase "l2 is []".
      intro H.
      reflexivity.
    SCase "l2 is z::l2'".
      intro H.
      inversion H.
  Case "l is v::l1'".
    destruct l2 as [| z l2'].
    SCase "l2 is []".
      intro H.
      inversion H.
    SCase "l2 is z::l2'".
      intro H.
      simpl.
      inversion H.
      rewrite H1.
      reflexivity.
Qed.

(* Since both split and combine defined by recursion suggests using induction in proof. *)

Theorem split_combine :  forall X Y  (l1 : list X) (l2 : list Y),
    length l1 = length l2 -> split (combine l1  l2) = (l1, l2).
Proof.
  intros X Y l1.               (* do not intro L2 *)
  induction l1 as [| x l1'].
  Case "l1 is []".
    intros.                    (*     H : length []         = length l2
                                  ==========================================================================================================
                                                                                 split (combine       []         l2) =         ([],      l2)   *)
    symmetry in H.
    simpl in H.                (*     H : length l2         = 0 *)
    apply hc_length_l_0_means_l_nil in H.
                               (*     H : l2                = [] *)
    rewrite H.                 (*                                                split (combine       []         []) =         ([],      [])   *)
    simpl.                     (*                                                                    ([],        []) =         ([],      [])   *)
    reflexivity.
  Case "l1 is x:: l1'".
    intros.
    destruct l2 as [| y l2'].
    SCase "l2 is []".          (*     H : length (x :: l1') = length []
                                  ==========================================================================================================
                                                                                 split (combine (x :: l1')       [])    = (x :: l1',      [])  *)
      inversion H.
    SCase "l2 is y::l2'".      (* IHl1' : forall l2 : list Y,
                                          length       l1'  = length       l2 -> split (combine       l1'        l2)    =      (l1',      l2)
                                      H : length (x :: l1') = length (y :: l2')
                                  ==========================================================================================================
                                                                                 split (combine (x :: l1') (y :: l2'))  = (x :: l1', y :: l2') *)
      apply hc_cons_length_eq_length in H.
                               (*     H : length       l1' = length        l2' *)
      (* David Darais from here out. *)
(*
                               (*  use of specialize here: same as asserting conclusion of IHl1' and applying assumptions *)
      specialize (IHl1' _ H).  (*   IHl1' :                                      split (combine       l1'        l2')  =       (l1',      l2') *)
      simpl.                   (*                                 (let (g, d) := split (combine       l1'        l2')  in (x :: g,   y :: d))
                                                                                                                       =  (x :: l1', y :: l2') *)
      rewrite IHl1'.           (*                                                               (x :: l1',  y :: l2')  =  (x :: l1', y :: l2') *)
      reflexivity.
*)
      (* alternate way: *)
      assert (split (combine l1' l2') = (l1', l2')) as A.
      SSCase "proof of assertion".
        apply IHl1'.           (*         length       l1' = length        l2' *)
        apply H.

                               (*   A :                                          split (combine       l1'        l2')  =       (l1',      l2')
                                  ==========================================================================================================
                                                                                 split (combine (x :: l1') (y :: l2')) =  (x :: l1', y :: l2') *)
      simpl.                   (*                                 (let (g, d) := split (combine       l1'        l2') in  (x :: g,   y :: d))
                                                                                                                       =  (x :: l1', y :: l2') *)
      rewrite A.               (*                                                               (x :: l1',  y :: l2')  =  (x :: l1', y :: l2') *)
      reflexivity.
Qed.
(** [] *)
(* peak at if desparate: http://jeroengoudsmit.com/talks/coq-sessions/split_combine.v *)

(** **** Exercise: 3 stars (override_permute) *)
(* Proved with guidance from David Darais. *)
Theorem override_permute : forall (X:Type) x1 x2 k1 k2 k3 (f : nat->X),
    beq_nat k2 k1 = false ->
    (override (override f k2 x2) k1 x1) k3 = (override (override f k1 x1) k2 x2) k3.
Proof.
  (* Note:
     - Only thing with inductive structure is nat.
     - Override is not implemented with structural recursion.
     - Those suggest proof will not use induction. *)
  intros.
  destruct (beq_nat k1 k3) eqn:D1; (* ';' says next destruct will apply to both subgoals *)
  destruct (beq_nat k2 k3) eqn:D2.
  Case "(beq_nat k1 k3) = true".
    SCase "(beq_nat k2 k3) = true".  (*  H : beq_nat k2 k1 = false
                                        D1 : beq_nat k1 k3 = true
                                        D2 : beq_nat k2 k3 = true
                                        ============================
                                        override (override f k2 x2) k1 x1 k3 =
                                        override (override f k1 x1) k2 x2 k3                                 *)
      (* Could try unfolding, but just shows what is already known: assumptions contradictory (k1 = k3 = k2, but k2 != k1)
         - so prove directly without unfolding. *)
      apply beq_nat_true in D1.      (* D1 : k1 = k3                                                         *)
      rewrite D1 in H.               (*  H : beq_nat k2 k3 = false                                           *)
      rewrite D2 in H.               (*  H : true = false                                                    *)
      inversion H.
    SCase "(beq_nat k2 k3) = false". (* D1 : beq_nat k1 k3 = true
                                        D2 : beq_nat k2 k3 = false
                                        ============================
                                        override (override f k2 x2) k1 x1 k3 =
                                        override (override f k1 x1) k2 x2 k3                                 *)
      unfold override.               (* (if beq_nat k1 k3 then x1 else if beq_nat k2 k3 then x2 else f k3) =
                                        (if beq_nat k2 k3 then x2 else if beq_nat k1 k3 then x1 else f k3)   *)
      rewrite D1.                    (*                        x1                                          =
                                        (if beq_nat k2 k3 then x2 else x1)                                   *)
      rewrite D2.                    (*                        x1                                          =
                                                                       x1                                    *)
      reflexivity.
  Case "(beq_nat k1 k3) = false".
    SCase "(beq_nat k2 k3) = true".  (*  H : beq_nat k2 k1 = false
                                        D1 : beq_nat k1 k3 = false
                                        D2 : beq_nat k2 k3 = true
                                        ============================
                                        override (override f k2 x2) k1 x1 k3 =
                                        override (override f k1 x1) k2 x2 k3                                 *)
      unfold override.               (* (if beq_nat k1 k3 then x1 else if beq_nat k2 k3 then x2 else f k3) =
                                        (if beq_nat k2 k3 then x2 else if beq_nat k1 k3 then x1 else f k3)   *)
      rewrite D1.                    (*                               (if beq_nat k2 k3 then x2 else f k3) =
                                        (if beq_nat k2 k3 then x2 else f k3)                                 *)
      rewrite D2.                    (*                                                      x2            =
                                                               x2                                            *)
      reflexivity.
    SCase "(beq_nat k2 k3) = false". (*  H : beq_nat k2 k1 = false
                                        D1 : beq_nat k1 k3 = false
                                        D2 : beq_nat k2 k3 = false
                                        ============================
                                        override (override f k2 x2) k1 x1 k3 =
                                        override (override f k1 x1) k2 x2 k3                                 *)
      unfold override.               (* (if beq_nat k1 k3 then x1 else if beq_nat k2 k3 then x2 else f k3) =
                                        (if beq_nat k2 k3 then x2 else if beq_nat k1 k3 then x1 else f k3)   *)
      rewrite D1.                    (*                               (if beq_nat k2 k3 then x2 else f k3) =
                                        (if beq_nat k2 k3 then x2 else f k3)                                 *)
      rewrite D2.                    (*                                                              f k3  =
                                                                       f k3                                  *)
      reflexivity.
Qed.
(** [] *)

(** **** Exercise: 3 stars, advanced (filter_exercise) *)
(** Challenging.  Pay attention to the form of IH. *)

Theorem filter_exercise : forall (X : Type) (test : X -> bool) (x : X) (l lf : list X),
     filter test l = x :: lf ->
     test x = true.
Proof.
  intros X test x l lf H.
  induction l as [| v l'].
  Case "l is []".                   (*   H :                       filter test []                            = x :: lf
                                       =================================================================================================
                                                                                                                           test x = true *)
    simpl in H.                     (*    H :                                  []                            = x :: lf *)
    inversion H.
  Case "l is v::l'".
    destruct (test v) eqn:D1.
    SCase "(test v) = true".        (*    H :                      filter test (v :: l')                     = x :: lf
                                       IHl' :                      filter test       l'                      = x :: lf  -> test x = true
                                         D1 :                                                                              test v = true
                                       =================================================================================================
                                                                                                                           test x = true *)
      simpl in H.                   (*    H : (if test v then v :: filter test       l' else filter test l') = x :: lf *)
      rewrite D1 in H.              (*    H :                 v :: filter test       l'                      = x :: lf *)
      inversion H.                  (*   H1 :                 v                                              = x
                                         H2 :                      filter test       l'                      =      lf *)
      rewrite H1 in D1.             (*   D1 :                                                                              test x = true *)
      apply D1.
    SCase "(test v) = false".
      assert (filter test (v :: l') = filter test l') as A.
      SSCase "proof of assertion".
        simpl.                      (*        (if test v then v :: filter test       l' else filter test l') = filter test l' *)
        rewrite D1.                 (*                             filter test       l'                      = filter test l' *)
        reflexivity.

                                    (*    H :                      filter test (v :: l')                     = x :: lf
                                       IHl' :                      filter test       l'                      = x :: lf -> test x = true
                                          A :                      filter test (v :: l')                     = filter test l'
                                       ====================================================================================
                                                                                                                           test x = true *)
      rewrite <- H in IHl'.         (* IHl' :                      filter test l'                            = filter test (v :: l') ->
                                                                                                                           test x = true *)
      rewrite A in IHl'.            (* IHl' :                      filter test l'                            = filter test       l'  ->
                                                                                                                           test x = true *)
      apply IHl'.                   (*                             filter test l'                            = filter test l' *)
      reflexivity.
Qed.
(** [] *)

(** **** Exercise: 4 stars, advanced (forall_exists_challenge) *)
(** Define [forallb] checks whether every element in list satisfies predicate. *)

Fixpoint forallb {X} (f: X->bool) (l:list X) : bool :=
  match l with
  |     [] => true
  | h :: t => if (f h) then (forallb f t) else false
  end.

Example forallb1 :       forallb oddb        [1;3;5;7;9]       = true.
Proof. reflexivity. Qed.
Example forallb2 :       forallb negb        [false;false]     = true.
Proof. reflexivity. Qed.
Example forallb3 :       forallb evenb       [0;2;4;5]         = false.
Proof. reflexivity. Qed.
Example forallb4 :       forallb (beq_nat 5) []                = true.
Proof. reflexivity. Qed.

(** Define [existsb] checks whether there exists an element in list that satisfies predicate. *)

Fixpoint existsb {X} (f: X->bool) (l:list X) : bool :=
  match l with
  |     [] => false
  | h :: t => if (f h) then true else (existsb f t)
  end.

Example existsb1 :       existsb (beq_nat 5) [0;2;3;6]         = false.
Proof. reflexivity. Qed.
Example existsb2 :       existsb (andb true) [true;true;false] = true.
Proof. reflexivity. Qed.
Example existsb3 :       existsb oddb        [1;0;0;0;0;3]     = true.
Proof. reflexivity. Qed.
Example existsb4 :       existsb evenb       []                = false.
Proof. reflexivity. Qed.

(** Define _nonrecursive_ version of [existsb] : [existsb']
    - using [forallb] and [negb]. *)

(* TODO: won't compile: refering to anon fun:
Error: The type of this term is a product while it is expected to be "bool".
Definition existsb' {X} (f: X->bool) (l:list X) : bool :=
    forallb (fun {X} (f : X->bool) (x : X) => negb (f x)) l.
*)

Definition flipb {X} (f : X->bool) (x : X) : bool := negb (f x).
Check      flipb.
Check       (fun {X} (f : X->bool) (x : X)        => negb (f x)).

Definition existsb' {X} (f: X->bool) (l:list X) : bool :=
    negb (forallb (flipb f) l).

Eval compute in  forallb  (beq_nat 5)
                 [0         ;3         ;6].
(*                false/true;false/true;false/true : eol is false -> true *)
Eval compute in  forallb  (beq_nat 5)
                 [0         ;3         ;6         ; 5].
(*                false/true;false/true;false/true; true/false -> true *)
Eval compute in  forallb  (beq_nat 5)
                 [0         ;5         ;6         ; 5].
(*                false/true;true/false -> true *)

Example existsb'1 :       existsb' (beq_nat 5) [0;2;3;6]         = false.
Proof. reflexivity. Qed.
Example existsb'2 :       existsb' (andb true) [true;true;false] = true.
Proof. reflexivity. Qed.
Example existsb'3 :       existsb' oddb        [1;0;0;0;0;3]     = true.
Proof. reflexivity. Qed.
Example existsb'4 :       existsb' evenb       []                = false.
Proof. reflexivity. Qed.

(** Prove [existsb'] and [existsb] have same behavior. *)

Theorem hc_e_eq_ep : forall {X : Type} (f: X->bool) (l:list X) (b : bool),
    (existsb f l) = (existsb' f l).
Proof.
  intros.
  induction l as [| x l'].
  Case "l is []".         (*  existsb f []                                   = existsb'             f                                               []             *)
    simpl.                (*  false                                          = existsb'             f                                               []             *)
    unfold existsb'.      (*  false                                          = negb (forallb (flipb f)                                              [])            *)
    simpl.                (*  false                                          = false *)
    reflexivity.
  Case "l is x::l'".      (*  existsb f (x ::                            l') = existsb'             f  (x ::                                        l')            *)
    simpl.                (*      (if f  x then true else existsb f      l') = existsb'             f  (x ::                                        l')            *)
    unfold existsb'.      (*  (if f x then true else existsb      f      l') = negb (forallb (flipb f) (x ::                                        l'))           *)
    simpl.                (*  (if f x then true else existsb      f      l') = negb (if       flipb f   x  then forallb               (flipb f)     l' else false) *)
    unfold flipb.         (*  (if f x then true else existsb      f      l') = negb (if negb       (f   x) then forallb (fun x0 : X => negb (f x0)) l' else false) *)
    destruct (f x).
    SCase "f x is true".  (*  true                                           = negb (if negb       true    then forallb (fun x0 : X => negb (f x0)) l' else false) *)
      simpl.              (*  true                                           = true *)
      reflexivity.
    SCase "f x is false". (* IHl' :
                              existsb                             f      l'  = existsb'             f                                               l'
                             ====================================================================================================================================
                              existsb                             f      l'  = negb (if negb       false   then forallb (fun x0 : X => negb (f x0)) l' else false) *)
      simpl.              (*  existsb                             f      l'  = negb                            (forallb (fun x0 : X => negb (f x0)) l')            *)
      rewrite IHl'.       (*  existsb'                            f      l'  = negb                            (forallb (fun x0 : X => negb (f x0)) l')            *)
      unfold existsb'.    (*  negb (forallb                (flipb f)     l') = negb                            (forallb (fun x0 : X => negb (f x0)) l')            *)
      unfold flipb.       (*  negb (forallb  (fun x0 : X => negb (f x0)) l') = negb                            (forallb (fun x0 : X => negb (f x0)) l')            *)
      reflexivity.
Qed.

Theorem hc_e_eq_ep' : forall {X : Type} (f: X->bool) (l:list X) (b : bool),
    existsb f l = b -> existsb' f l = b.
Proof.
  intros.
  induction l as [| x l'].
  Case "l is []".            (*   H :
                                 existsb                                               f        []                    = b
                                ============================
                                 existsb'                                              f        []                    = b *)
    unfold existsb in H.     (*   H :
                                 false                                                                                = b *)
    unfold existsb'.         (*  negb                    (forallb               (flipb f)       [])                   = b *)
    simpl.                   (*  false                                                                                = b *)
    apply H.
  Case "l is x::l'".         (*    H :
                                 existsb                                               f  (x :: l')                   = b
                                IHl' :
                                 existsb                                               f        l'                    = b ->
                                 existsb'                                              f        l'                    = b
                                ============================
                                 existsb'                                              f  (x :: l')                   = b *)
    unfold existsb'.         (*  negb                    (forallb               (flipb f) (x :: l'))                  = b *)
    unfold existsb' in IHl'. (* IHl' :
                                 existsb                                               f        l'                    = b ->
                                 negb                    (forallb               (flipb f)       l')                   = b *)
    simpl in H.              (*   H : (if       f x  then true                                     else existsb f l') = b *)
    simpl.                   (*  negb (if flipb f x  then forallb               (flipb f)       l' else false)        = b *)
    unfold flipb.            (*  negb (if negb (f x) then forallb (fun x0 : X => negb (f x0))   l' else false)        = b *)
    destruct (f x).
    SCase "f x is true".     (*   H :
                                 true                                                                                 = b
                                ============================
                                 negb (if negb true  then forallb (fun x0 : X => negb (f x0))   l' else false)        = b *)
      simpl.                 (*  true                                                                                 = b *)
      apply H.
    SCase "f x is false".    (*    H :
                                 existsb                                               f        l'                    = b
                                IHl' :
                                 existsb                                               f        l'                    = b ->
                                 negb                    (forallb (flipb               f)       l')                   = b
                                ============================
                                 negb (if negb false then forallb (fun x0 : X => negb (f x0))   l' else false)        = b *)
      simpl.                 (*  negb                    (forallb (fun x0 : X => negb (f x0))   l')                   = b *)
      unfold flipb in IHl'.  (* IHl' :
                                 existsb                                               f        l'                    = b ->
                                 negb                    (forallb (fun x  : X => negb (f x))    l')                   = b *)
      rewrite IHl'.          (*  b                                                                                    = b *)
      reflexivity.

                             (*  existsb                                               f        l'                    = b *)
      apply H.
Qed.
(** [] *)

(* $Date: 2013-07-17 16:19:11 -0400 (Wed, 17 Jul 2013) $ *)
