(** * Prop: Propositions and Evidence *)

(**
/Applications/CoqIdE_8.4.app/Contents/Resources/bin/coqc x05MoreCoq.v
*)

Require Export x05MoreCoq.

(** So far: seen examples of factual claims (_propositions_)
    and ways of presenting evidence of their truth (_proofs_).

    Mostly have used _equality propositions_ of form
        [e1 = e2],
    with implications
        ([P -> Q]),
    and quantified propositions
        ([forall x, P]).

    This chapter presents propositional (logical) side of Coq.

    Will expand repertoire of primitive propositions to include
    _user-defined_ propositions,
    (besides Coq's "built in" equality propositions).
*)


(* ##################################################### *)
(** * Inductively Defined Propositions *)

(** Define arbitrary property of nats: *)

(** A nat is [beautiful] if it is [0], [3], [5],
    or sum of two [beautiful] numbers.

       - Rule [b_0]  : [0] is [beautiful].
       - Rule [b_3]  : [3] is [beautiful].
       - Rule [b_5]  : [5] is [beautiful].
       - Rule [b_sum]: If [n] and [m] are [beautiful], then so is their sum. *)

(** Lightweight notation for _inference rules_: *)
(**
                              -----------                               (b_0)
                              beautiful 0

                              ------------                              (b_3)
                              beautiful 3

                              ------------                              (b_5)
                              beautiful 5

                       beautiful n     beautiful m
                       ---------------------------                      (b_sum)
                              beautiful (n+m)
*)

(** If _premises_ above line all hold, then _conclusion_ below line follows.

    These rules _define_ the property [beautiful].
    For a number to be [beautiful], it must be based on these rules.

    E.G.,
    Claim [5] is [beautiful].
    - support : [b_5].
    Claim [8] is [beautiful].
    - support : [b_3], [b_5], then sum [8] by l] by [b_sum].
    Expressed graphically: *)
(**
         ----------- (b_3)   ----------- (b_5)
         beautiful 3         beautiful 5
         ------------------------------- (b_sum)
                   beautiful 8
    Other ways of using these rules to argue [8] is [beautiful]:
         ----------- (b_5)   ----------- (b_3)
         beautiful 5         beautiful 3
         ------------------------------- (b_sum)
                   beautiful 8
*)

(** **** Exercise: 1 star (varieties_of_beauty) *)
(** How many different ways are there to show that [8] is [beautiful]? *)
(* Infinite because of b_0. *)
(** [] *)

Inductive beautiful : nat -> Prop :=
  b_0   : beautiful 0
| b_3   : beautiful 3
| b_5   : beautiful 5
| b_sum : forall n m, beautiful n -> beautiful m -> beautiful (n + m).


(** [beautiful] is a a family of propositions
    (often called a _property_ of numbers)
    "indexed by" natural numbers:
    i.e., for each number [n], the claim that "[n] is [beautiful]" is a proposition.

    Remaining lines are rules/axioms for [beautiful] numbers:
    have same status as proven theorems: true axiomatically.

    Use [apply] with rule names to prove [beautiful]:  *)

Theorem three_is_beautiful:
    beautiful 3.
Proof.
   apply b_3.
Qed.

Theorem eight_is_beautiful:
    beautiful 8.
Proof.
   Case "8".
     apply b_sum with (n:=3) (m:=5).
     SCase "3".
       apply b_3.
     SCase "5".
       apply b_5.
Qed.

(** Can prove theorems that have hypotheses about [beautiful]. *)

Theorem beautiful_plus_eight: forall n,
    beautiful n -> beautiful (8 + n).
Proof.
  intros n B.
  apply b_sum with (n:=8) (m:=n).
  apply eight_is_beautiful.
  apply B.
Qed.


(** **** Exercise: 2 stars (b_timesm) *)
(* https://github.com/haklabbeograd/software-foundations-coq-workshop/blob/master/Prop.v *)
(* Notes:
   - doing induction on m not necessary but easier since it is on left
   - key point: we only have ADDING two B numbers to work with. *)
Theorem b_timesm: forall n m,
    beautiful n -> beautiful (m * n).
Proof.
  intros n m H.
  induction m as [| m'].
  Case "m is 0".     (*        beautiful     (0 * n)   *)
    simpl.           (*        beautiful 0             *)
    apply b_0.
  Case "m is S m'".  (*    H : beautiful n
                        IHm' : beautiful     (m' * n)
                        ============================
                               beautiful   (S m' * n)  *)
    simpl.           (*        beautiful (n + m' * n)  *) (* key place - we have an addition to take apart *)
    apply b_sum.     (*        beautiful  n
                 adds subgoal: beautiful     (m' * n)  *)
    apply H.         (*        beautiful     (m' * n)  *)
    apply IHm'.
Qed.
(** [] *)


(* ####################################################### *)
(** ** Induction Over Evidence *)

(** Besides _constructing_ evidence that numbers are beautiful,
    can also _reason about_ such evidence. *)

(** Because [beautiful] is [Inductive] so its constructors
    [b_0], [b_3], [b_5], [b_sum] are the _only_ ways to build evidence
    that numbers are beautiful. *)

(** To give evidence [E] for assertion [beautiful n],
    we know [E] must have one of four shapes:

      - [E] is [b_0] (and [n] is [O]),
      - [E] is [b_3] (and [n] is [3]),
      - [E] is [b_5] (and [n] is [5]), or
      - [E] is [b_sum n1 n2 E1 E2] (and [n] is [n1+n2], where [E1] is
        evidence that [n1] is beautiful and [E2] is evidence that [n2] *)

(** Enables analysis of any hypothesis of form [beautiful n]
    to see how it was constructed.  Particularly using [induction]
    (previously used for reasoning about inductively defined _data_)
    to reason about inductively defined _evidence_.

    E.G.,: *)

Inductive gorgeous : nat -> Prop :=
  g_0     : gorgeous 0
| g_plus3 : forall n, gorgeous n -> gorgeous (3+n)
| g_plus5 : forall n, gorgeous n -> gorgeous (5+n).

(** **** Exercise: 1 star (gorgeous_tree) *)
(** Write out the definition of [gorgeous] numbers using inference rule  notation. *)
(**
                              -----------                               (g_0)
                              gorgeous 0



                       gorgeous n       3
                       ---------------------------                      (g_plus3)
                              gorgeous (3+n)


                       gorgeous n       5
                       ---------------------------                      (g_plus5)
                              gorgeous (5+n)
*)
(** [] *)


(** **** Exercise: 1 star (gorgeous_plus13) *)
Theorem gorgeous_plus13: forall n,
    gorgeous n -> gorgeous (13+n).
Proof.
  intros.
  apply g_plus5.
  apply g_plus5.
  apply g_plus3.
  apply H.
Qed.
(** [] *)

(** Although [gorgeous] and [beautiful] are defined using different rules,
    they are the same property in the sense that they are true of the
    same numbers.

    KEY: Proved by induction on the _evidence_ (the structure of gorgeous): *)

Theorem gorgeous__beautiful : forall n,
    gorgeous n -> beautiful n.
Proof.
  intros n H.               (*   H :        gorgeous       n
                               ============================
                                            beautiful      n              *)
  induction H as [|n'|n'].  (* key: induction on H                        *)
  Case "g_0".               (*              beautiful      0                 note: context empty because induction on H

                                                                             subgoal 2 (ID 84) is:  beautiful (3 + n')
                                                                             subgoal 3 (ID 87) is:  beautiful (5 + n') *)
    apply b_0.
  Case "g_plus3".           (*              beautiful (3 + n')

                                                                             subgoal 2 (ID 87) is:  beautiful (5 + n') *)
    apply b_sum.            (*              beautiful  3


                                                                             subgoal 2 (ID 94) is:  beautiful      n'
                                                                             subgoal 3 (ID 87) is:  beautiful (5 + n') *)
    apply b_3.              (* IHgorgeous : beautiful      n'
                               ============================
                                            beautiful      n'             *)
    apply IHgorgeous.
  Case "g_plus5".           (*              beautiful (5 + n')            *)
    apply b_sum.            (*              beautiful  5

                                                                             subgoal 2 (ID 99) is:  beautiful      n'  *)
    apply b_5.              (* IHgorgeous : beautiful      n'
                               ============================
                                            beautiful      n'             *)
    apply IHgorgeous.
Qed.

(** Can't prove by induction on [n] (instead of induction on evidence): *)

Theorem gorgeous__beautiful_FAILED : forall n,
    gorgeous n -> beautiful n.
Proof.
  intros. induction n as [| n'].
  Case "n = 0". apply b_0.
  Case "n = S n'". (* stuck *)
Abort.

(** because induction on [n] does not give useful induction
    hypothesis.  Knowing how property of interest behaves on
    predecessor of [n] does not help prove it holds for [n].

    We need induction hypotheses that includes other numbers (e.g., [n - 3], [n - 5]).
    This is given by shape of constructors for [gorgeous]. *)


(** **** Exercise: 2 stars (gorgeous_sum) *)
Theorem gorgeous_sum : forall n m,
    gorgeous n -> gorgeous m -> gorgeous (n + m).
Proof.
  intros n m Hn Hm.
  induction Hn as [| n' | n'].
  Case "n is 0".
    simpl.
    apply Hm.
  Case "n is 3 + n'".
    apply g_plus3 with (n:=n'+m).
    apply IHHn.
  Case "n is 5 + n'".
    apply g_plus5 with (n:=n'+m).
    apply IHHn.
Qed.
(** [] *)

(** **** Exercise: 3 stars, advanced (beautiful__gorgeous) *)
Theorem beautiful__gorgeous : forall n,
    beautiful n -> gorgeous n.
Proof.
  intros.
  induction H.
  Case "gorgeous 0".
    apply g_0.
  Case "gorgeous 3".
    assert (3 + 0 = 3) as A.
    SCase "proof of assertion".
      simpl. reflexivity.
                               (*       A : 3 + 0 = 3
                                  =========================
                                  gorgeous  3               *)
    rewrite <- A.              (* gorgeous (3 + 0)          *)
    apply g_plus3.             (* gorgeous      0           *)
    apply g_0.
  Case "gorgeous 5".
    assert (5 + 0 = 5) as A.
    SCase "proof of assertion".
      simpl. reflexivity.
    rewrite <- A.
    apply g_plus5.
    apply g_0.
  Case "gorgeous (n + m)".     (* gorgeous (n + m) *)
    apply gorgeous_sum.        (* IHbeautiful1 : gorgeous n
                                  IHbeautiful2 : gorgeous m
                                  =========================
                                  gorgeous n

                                  subgoal 2 is:  gorgeous m *)
    apply IHbeautiful1.
    apply IHbeautiful2.
Qed.
(** [] *)


(** **** Exercise: 3 stars, optional (g_times2) *)
(** Prove [g_times2] ithout using [gorgeous__beautiful].
    First prove helper lemma: *)

Lemma helper_g_times2 : forall x y z,
    x + (z + y) =
    z +  x + y.
Proof.
  intros.                                (* x + (z + y) = z + x + y *)
  rewrite plus_assoc.                    (* x +  z + y  = z + x + y *)
  rewrite plus_comm with (n:=x) (m:=z).  (* z +  x + y  = z + x + y *)
  reflexivity.
Qed.

Theorem g_times2: forall n, gorgeous n -> gorgeous (2*n).
Proof.
  intros n H.                         (* H :          gorgeous           n
                                         ============================
                                                      gorgeous      (2 * n)                     *)
  simpl.                              (*              gorgeous          (n   +         (n + 0)) *)
  induction H.                        (* note: removes H from context
                                         ============================
                                                         gorgeous       (0   +         (0 + 0))

                                        subgoal 2 is: gorgeous      (3 + n   +     (3 + n + 0))
                                        subgoal 3 is: gorgeous      (5 + n   +     (5 + n + 0)) *)
  Case "0".
    apply g_0.
  Case "2".                           (*              gorgeous      (3 + n   +     (3 + n + 0)) *)
    rewrite plus_comm.                (*              gorgeous      (3 + n   + 0 + (3 + n))     *)
    rewrite <- helper_g_times2.       (*              gorgeous (0 + (3 + n   +     (3 + n)))    *)
    simpl.                            (*              gorgeous (S (S (S (n   +  S (S (S n)))))) *)
    apply g_plus3.                    (*              gorgeous          (n   +  S (S (S n)))    *)
    rewrite plus_comm.                (*              gorgeous  (S (S (S n)) +          n)      *)
    simpl.                            (*              gorgeous (S (S (S (n   +          n))))   *)
    apply g_plus3.                    (* IHgorgeous : gorgeous          (n   +         (n + 0))
                                         ============================
                                                      gorgeous          (n +            n)      *)
    rewrite plus_assoc in IHgorgeous. (* IHgorgeous : gorgeous          (n +            n + 0)  *)
    rewrite plus_comm in IHgorgeous.  (* IHgorgeous : gorgeous (0 +     (n +            n))     *)
    simpl in IHgorgeous.              (* IHgorgeous : gorgeous          (n +            n)      *)
    apply IHgorgeous.
  Case "5".                           (*              gorgeous (5 +      n + (5 + n + 0))       *)
    rewrite plus_comm.                (* exact same proof as "3" case                           *)
    rewrite <- helper_g_times2.
    simpl.
    apply g_plus5.
    rewrite plus_comm.
    simpl.
    apply g_plus5.
    rewrite plus_assoc in IHgorgeous.
    rewrite plus_comm in IHgorgeous.
    simpl in IHgorgeous.
    apply IHgorgeous.
Qed.
(** [] *)


(* ####################################################### *)
(** ** From Boolean Functions to Propositions *)

(** Use [evenb] to define _proposition_ : some number [n] is even: *)

Definition even (n:nat) : Prop :=
  evenb n = true.

(** "[n] is even" means "[evenb] returns [true] when applied to [n]."

    Give name to proposition using [Definition]
    (just like naming other expressions).
    This is not something new, it is still just an equality. *)

(** Alternative: define concept of evenness directly.  Instead of
    using [evenb] function ("a number is even if a certain computation
    yields [true]"), define concept of evenness by giving two
    ways of presenting _evidence_ that a number is even. *)

Inductive ev : nat -> Prop :=
  | ev_0  :               ev O
  | ev_SS : forall n:nat, ev n -> ev (S (S n)).

(** [0] is even, [ev_0] is evidence.
    if [m = S (S n)] for some [n] and have evidence [e] that [n] is even,
    then [m] is also even : [ev_SS n e] is the evidence. *)


(** **** Exercise: 1 star (double_even) *)

Theorem double_even : forall n,
    ev (double n).
Proof.
  intros.
  induction n as [| n'].
  Case "n is 0".     (*        ev       (double    0)    *)
    simpl.           (*        ev                  0     *)
    apply ev_0.
  Case "n is S n'".  (*        ev       (double (S n'))  *)
    simpl.           (*        ev (S (S (double    n'))) *)
    apply ev_SS.     (* IHn' : ev       (double    n')
                        ==============================
                               ev       (double    n')   *)
    apply IHn'.
Qed.
(** [] *)


(** *** Discussion: Computational vs. Inductive Definitions *)

(** Above, proposition "[n] is even" is given two ways:
    - indirectly : via [evenb] function, or
    - directly   : inductively describing what constitutes evidence for evenness.
    Both equally easy to state and work with.
    Choice is a matter of taste/style.

    For many properties, direct inductive definition is preferable,
    because writing a testing function may be awkward or impossible.

    E.G.,: [beautiful] : defines set of numbers.
    But cannot state its definition in a Fixpoint (i.e., recursive function).
    Might find way to test property using a [Fixpoint] (easy in this case).

    But other properties might require arbitrarily deep thinking.
    Also, might not be able to define as a [Fixpoint] if no
    terminating condition.

    Writing inductive definition of what it means to give evidence for
    property [beautiful] is straightforward. *)



(** **** Exercise: 1 star (ev__even) *)
(** Here is a proof that the inductive definition of evenness implies
    the computational one. *)

Theorem ev__even : forall n,
    ev n -> even n.
Proof.
  intros n E.
  induction E as [| n' E'].
  Case "E = ev_0".         (*         even        0           *)
    unfold even.           (*         evenb       0    = true *)
    simpl.                 (*         true             = true *)
    reflexivity.
  Case "E = ev_SS n' E'".  (* IHE' :  even        n'
                              ==============================
                                      even  (S (S n'))        *)
    unfold even.           (*         evenb (S (S n')) = true *)
    (* note: following simpl/unfold note needed nor done in original *)
    simpl.                 (*         evenb       n'   = true *)
    unfold even in IHE'.   (* IHE' :  evenb       n'   = true *)
    apply IHE'.
Qed.

(** Can above be proved by induction on [n]?
    - NO
    Why?
    - Induction on E      gives two successors to work with.
      Induction on n only gives one successor - can't get inside n' of S n'. *)

Theorem hc_ev__even' : forall n,
    ev n -> even n.
Proof.
  intros n eq.
  induction n as [| n'].
  Case "n is 0".     (*                    even    0         *)
    unfold even.     (*                    evenb   0  = true *)
    simpl.           (*                    true       = true *)
    reflexivity.
  Case "n is S n'".  (*   eq : ev (S n')
                        IHn' : ev    n' -> even    n'
                        ====================================
                                          even (S n')        *)
    unfold even.     (*                  evenb (S n') = true *)  (* stuck *)
Abort.
(** [] *)

(** Induction principle for inductively defined propositions does
    not follow the same form as that of inductively defined sets.

    Intuitive view:
    - induction on evidence [ev n] is similar to induction on [n],
      but restricts attention to only those numbers for which evidence [ev n]
      could be generated.

    (Later, deeper look at induction principle of [ev].) *)

(** **** Exercise: 1 star (l_fails) *)
(** The following proof attempt will not succeed.
     Theorem l : forall n,
         ev n.
     Proof.
       intros n. induction n.
         Case "O".     (*       ev    0  *)
           apply ev_0.
         Case "S".     (* IHn : ev    n
                          ===============
                                ev (S n) *)  (* stuck *)
           ...
   Intuitively, we expect the proof to fail because not every
   number is even. However, what exactly causes the proof to fail?

Same as previous case: because ev can only talk about 0 or numbers
with two successors.  *)
(** [] *)

(** **** Exercise: 2 stars (ev_sum) *)
(** Requires induction. *)

Theorem ev_sum : forall n m,
    ev  n      ->
    ev      m  ->
    ev (n + m).
Proof.
  intros n m eq1 eq2.     (*   eq1 : ev        n
                               eq2 : ev               m
                              ============================
                                     ev       (n  +   m)    *)

  induction eq1.          (* note: removes eq1 from context *)
  Case "0".               (*         ev       (0  +   m)    *)
    destruct m as [| m'].
    SCase "m is 0".       (*         ev       (0  +   0)    *)
      apply ev_0.
    SCase "m is S m'".    (*         ev       (0  + S m')   *)
      simpl.              (*   eq2 : ev            (S m')
                             ============================
                                     ev            (S m')   *)
      apply eq2.
  Case "n + m".           (*         ev  (S (S n) +   m)    *)
    destruct m as [| m'].
    SCase "m is 0".       (*         ev  (S (S n) +   0)    *)
      simpl.              (*         ev (S (S (n  +   0)))  *)
      apply ev_SS.        (* IHeq1 : ev       (n  +   0)
                             ============================
                                     ev       (n  +   0)    *)
      apply IHeq1.
    SCase "m is S m'".    (*         ev  (S (S n) + S m')   *)
      simpl.              (*         ev (S (S (n  + S m'))) *)
      apply ev_SS.        (* IHeq1 : ev       (n  + S m')
                             ============================
                                     ev       (n  + S m')   *)
      apply IHeq1.
Qed.
(** [] *)


(* ####################################################### *)
(** ** [Inversion] on Evidence *)
(* TODO right here - think about then continue *)
(** Prove if [n] is even, then [pred (pred n))] is too.
    Do not need an inductive proof.
    Use [inversion].  *)

Theorem ev_minus2: forall n,
    ev             n    ->
    ev (pred (pred n)).
Proof.
  intros n E.                (* E :  ev                   n
                                ============================
                                     ev (pred (pred       n))    *)
  inversion E as [| n' E'].  (* E :  ev                   n
                                H :  0 =                  n
                               ============================
                                     ev (pred (pred       0))

                               subgoal 2 is:
                                     ev (pred (pred (S (S n')))) *)
  Case "E = ev_0".
    simpl.                   (*      ev                   0      *)
    apply ev_0.
  Case "E = ev_SS n' E'".    (* E  : ev                   n
                                E' : ev                   n'
                                H  : S (S n') =           n
                                ============================
                                     ev (pred (pred (S (S n')))) *)
    simpl.                   (*      ev                   n'     *)
    apply E'.
Qed.

(** **** Exercise: 1 star, optional (ev_minus2_n) *)
(** What happens if we try to use [destruct] on [n] instead of [inversion] on [E]? *)

(* FILL IN HERE *)
(** [] *)


(** Another example, in which [inversion] helps narrow down to
the relevant cases. *)

Theorem SSev__even : forall n,
  ev (S (S n)) -> ev n.
Proof.
  intros n E. 
  inversion E as [| n' E']. 
  apply E'. Qed.

(** These uses of [inversion] may seem a bit mysterious at first.
    Until now, we've only used [inversion] on equality
    propositions, to utilize injectivity of constructors or to
    discriminate between different constructors.  But we see here
    that [inversion] can also be applied to analyzing evidence
    for inductively defined propositions.

    (You might also expect that [destruct] would be a more suitable
    tactic to use here. Indeed, it is possible to use [destruct], but 
    it often throws away useful information, and the [eqn:] qualifier
    doesn't help much in this case.)    

    Here's how [inversion] works in general.  Suppose the name
    [I] refers to an assumption [P] in the current context, where
    [P] has been defined by an [Inductive] declaration.  Then,
    for each of the constructors of [P], [inversion I] generates
    a subgoal in which [I] has been replaced by the exact,
    specific conditions under which this constructor could have
    been used to prove [P].  Some of these subgoals will be
    self-contradictory; [inversion] throws these away.  The ones
    that are left represent the cases that must be proved to
    establish the original goal.

    In this particular case, the [inversion] analyzed the construction
    [ev (S (S n))], determined that this could only have been
    constructed using [ev_SS], and generated a new subgoal with the
    arguments of that constructor as new hypotheses.  (It also
    produced an auxiliary equality, which happens to be useless here.)
    We'll begin exploring this more general behavior of inversion in
    what follows. *)


(** **** Exercise: 1 star (inversion_practice) *)
Theorem SSSSev__even : forall n,
  ev (S (S (S (S n)))) -> ev n.
Proof.
  (* FILL IN HERE *) Admitted.

(** The [inversion] tactic can also be used to derive goals by showing
    the absurdity of a hypothesis. *)

Theorem even5_nonsense : 
  ev 5 -> 2 + 2 = 9.
Proof.
  (* FILL IN HERE *) Admitted.
(** [] *)

(** **** Exercise: 3 stars, advanced (ev_ev__ev) *)
(** Finding the appropriate thing to do induction on is a
    bit tricky here: *)

Theorem ev_ev__ev : forall n m,
  ev (n+m) -> ev n -> ev m.
Proof.
  (* FILL IN HERE *) Admitted.
(** [] *)

(** **** Exercise: 3 stars, optional (ev_plus_plus) *)
(** Here's an exercise that just requires applying existing lemmas.  No
    induction or even case analysis is needed, but some of the rewriting
    may be tedious. *)

Theorem ev_plus_plus : forall n m p,
  ev (n+m) -> ev (n+p) -> ev (m+p).
Proof.
  (* FILL IN HERE *) Admitted.
(** [] *)





(* ####################################################### *)
(** * Additional Exercises *)

(** **** Exercise: 4 stars (palindromes) *)
(** A palindrome is a sequence that reads the same backwards as
    forwards.

    - Define an inductive proposition [pal] on [list X] that
      captures what it means to be a palindrome. (Hint: You'll need
      three cases.  Your definition should be based on the structure
      of the list; just having a single constructor
    c : forall l, l = rev l -> pal l
      may seem obvious, but will not work very well.)
 
    - Prove that 
       forall l, pal (l ++ rev l).
    - Prove that 
       forall l, pal l -> l = rev l.
*)


(* FILL IN HERE *)
(** [] *)

(** **** Exercise: 5 stars, optional (palindrome_converse) *)
(** Using your definition of [pal] from the previous exercise, prove
    that
     forall l, l = rev l -> pal l.
*)

(* FILL IN HERE *)
(** [] *)

(** **** Exercise: 4 stars, advanced (subsequence) *)
(** A list is a _subsequence_ of another list if all of the elements
    in the first list occur in the same order in the second list,
    possibly with some extra elements in between. For example,
    [1,2,3]
    is a subsequence of each of the lists
    [1,2,3]
    [1,1,1,2,2,3]
    [1,2,7,3]
    [5,6,1,9,9,2,7,3,8]
    but it is _not_ a subsequence of any of the lists
    [1,2]
    [1,3]
    [5,6,2,1,7,3,8]

    - Define an inductive proposition [subseq] on [list nat] that
      captures what it means to be a subsequence. (Hint: You'll need
      three cases.)

    - Prove that subsequence is reflexive, that is, any list is a
      subsequence of itself.  

    - Prove that for any lists [l1], [l2], and [l3], if [l1] is a
      subsequence of [l2], then [l1] is also a subsequence of [l2 ++
      l3].

    - (Optional, harder) Prove that subsequence is transitive -- that
      is, if [l1] is a subsequence of [l2] and [l2] is a subsequence
      of [l3], then [l1] is a subsequence of [l3].  Hint: choose your
      induction carefully!
*)

(* FILL IN HERE *)
(** [] *)


(** **** Exercise: 2 stars, optional (R_provability) *)
(** Suppose we give Coq the following definition:
    Inductive R : nat -> list nat -> Prop :=
      | c1 : R 0 []
      | c2 : forall n l, R n l -> R (S n) (n :: l)
      | c3 : forall n l, R (S n) l -> R n l.
    Which of the following propositions are provable?

    - [R 2 [1,0]]
    - [R 1 [1,2,1,0]]
    - [R 6 [3,2,1,0]]
*)

(** [] *)


(* $Date: 2013-07-01 18:48:47 -0400 (Mon, 01 Jul 2013) $ *)


