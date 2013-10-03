(** Summary:

    Tactics: TODO

    Theorems: TODO
*)

(** * Lists: Working with Structured Data *)

(** Compile [X.v] into [X.vo] via either of:
    - CoqIDE: Open [X.v].  "Compile" menu, click "Compile Buffer".
    - CLI: [coqc X.v]

     /Applications/CoqIdE_8.3pl5.app/Contents/Resources/bin/coqc
     /Applications/CoqIdE_8.4.app/Contents/Resources/bin/coqc

     Import compiled definitions from previous chapter: *)

Require Export x01Basics.

(* [Module] for pairs of numbers and lists of numbers ---
   So we can use same names later for generic versions. *)

Module NatList.

(* ###################################################### *)
(** * Pairs of Numbers *)

(** In [Inductive] type definition, constructors can take N
    parameters
    - none (as with [true] and [O]),
    - one (as with [S]),
    - or more than one, e.g.,: *)

Inductive natprod : Type :=
  pair : nat -> nat -> natprod.

(** "Only one way to construct a pair of numbers:
     by applying the constructor [pair] to two arguments of type [nat]." *)

Eval simpl in (pair 3 5).

(** Pattern matching on two-argument constructors: *)

Definition fst (p : natprod) : nat :=
  match p with
  | pair x y => x
  end.
Definition snd (p : natprod) : nat :=
  match p with
  | pair x y => y
  end.

Eval simpl in (fst (pair 3 5)).

(** Notation : use in expressions and pattern matches: *)

Notation "( x , y )" := (pair x y).

Eval simpl in (fst (3,5)).

Definition fst' (p : natprod) : nat :=
  match p with
  | (x,y) => x
  end.
Definition snd' (p : natprod) : nat :=
  match p with
  | (x,y) => y
  end.

Definition swap_pair (p : natprod) : natprod :=
  match p with
  | (x,y) => (y,x)
  end.

(** In following proofs, if lemmas stated to expose pair structure,
    they can be proved with just simplification/reflexivity: *)

Theorem surjective_pairing' : forall (n m : nat),
  (n,m) = (fst (n,m), snd (n,m)).
Proof.
  intros n m.  (* (n, m) = (fst (n, m), snd (n, m)) *)
  unfold fst.  (* (n, m) =      (n,     snd (n, m)) *)
  unfold snd.  (* (n, m) =      (n,             m)  *)
  reflexivity.
Qed.

(** Reflexivity not enough if we state as: *)

Theorem surjective_pairing_stuck : forall (p : natprod),
  p = (fst p, snd p).
Proof.
  simpl. (* Doesn't reduce anything! *)
Admitted.

(** Can use [destruct] to expose structure (via pair notation in [as]
    pattern) of [p] so [simpl] can do pattern match in [fst] and
    [snd].

    NOTE: Unlike for [nat]s, [destruct] doesn't generate an
    extra subgoal here because [natprod]s can only be
    constructed in one way.  *)

Theorem surjective_pairing : forall (p : natprod),
  p = (fst p, snd p).
Proof.
  intros p.  destruct p as (n,m).  simpl.  reflexivity.  Qed.

(** **** Exercise: 1 star (snd_fst_is_swap) *)
Theorem snd_fst_is_swap : forall (p : natprod),
  (snd p, fst p) = swap_pair p.
Proof.
  intros p.  destruct p as (n,m).  simpl. reflexivity. Qed.

(** **** Exercise: 1 star, optional (fst_swap_is_snd) *)
Theorem fst_swap_is_snd : forall (p : natprod),
  fst (swap_pair p) = snd p.
Proof.
  intros p.  destruct p as (n,m).  simpl. reflexivity. Qed.

(* ###################################################### *)
(** * Lists of Numbers *)

(** Generalized pairs: _lists_ of numbers:
    - empty or
    - pair of number and another list: *)

Inductive natlist : Type :=
  | nil  : natlist
  | cons : nat -> natlist -> natlist.

Definition mylist := cons 1 (cons 2 (cons 3 nil)).

(** Notation:
    [::] : infix [cons],
    square brackets : "outfix" notation for constructing lists. *)

Notation "x :: l" := (cons x l) (at level 60, right associativity).
Notation "[ ]" := nil.
Notation "[ x , .. , y ]" := (cons x .. (cons y nil) ..).

(** Right-hand side of 3rd notation illustrates syntax for declaring
    n-ary notations and translating them to nested sequences of binary
    constructors.

    [right associativity] of 1st notation says how to parenthesize
    exprs with several uses of [::]: *)

Definition mylist1 := 1 :: (2 :: (3 :: nil)).
Definition mylist2 := 1 :: 2 :: 3 :: nil.
Definition mylist3 := [1,2,3].

(** [at level 60] : how to parenthesize exprs
    that have both [::] and some other infix operator.
    E.G., [+] defined as infix notation for [plus] at level 50,
    so [+] operator binds tighter than [::], so

    1 + 2 :: [3] parsed as  (1 + 2) :: [3]
                rather than  1 + (2 :: [3]) *)

Fixpoint repeat (n count : nat) : natlist :=
  match count with
  |       O  => []
  | S count' => n :: (repeat n count')
  end.

Fixpoint length (l:natlist) : nat :=
  match l with
  |     [] => O
  | _ :: t => S (length t)
  end.

(** [app] : append *)

Fixpoint app (l1 l2 : natlist) : natlist :=
  match l1 with
  |     [] => l2
  | h :: t => h :: (app t l2)
  end.

Notation "x ++ y" := (app x y)
                     (right associativity, at level 60).

Example test_app1:            [1,2,3] ++ [4,5] = [1,2,3,4,5].
Proof. reflexivity.  Qed.
Example test_app2:            nil ++ [4,5] = [4,5].
Proof. reflexivity.  Qed.
Example test_app3:            [1,2,3] ++ nil = [1,2,3].
Proof. reflexivity.  Qed.

(** [hd] ("head")/[tail].  Default value for [hd] of empty list. *)

Definition hd (default:nat) (l:natlist) : nat :=
  match l with
  |     [] => default
  | h :: t => h
  end.

Definition tail (l:natlist) : natlist :=
  match l with
  |     [] => []
  | h :: t => t
  end.

Example test_hd1:             hd 0 [1,2,3] = 1.
Proof. reflexivity.  Qed.
Example test_hd2:             hd 0 [] = 0.
Proof. reflexivity.  Qed.
Example test_tail:            tail [1,2,3] = [2,3].
Proof. reflexivity.  Qed.

(** **** Exercise: 2 stars, recommended (list_funs) *)
(** Define [nonzeros], [oddmembers], [countoddmembers]. *)

Fixpoint nonzeros (l:natlist) : natlist :=
  match l with
  |     [] => []
  | 0 :: t =>       nonzeros t
  | h :: t => h :: (nonzeros t)
  end.

Example test_nonzeros:        nonzeros [0,1,0,2,3,0,0] = [1,2,3].
Proof. reflexivity. Qed.

Fixpoint oddmembers (l:natlist) : natlist :=
  match l with
  |     [] => []
  | h :: t => match oddb h with
              | true  => h :: (oddmembers t)
              | false =>       oddmembers t
              end
  end.

Example test_oddmembers:      oddmembers [0,1,0,2,3,0,0] = [1,3].
Proof. simpl. reflexivity. Qed.

Fixpoint countoddmembers (l:natlist) : nat :=
  match l with
  |     [] => 0
  | h :: t => match oddb h with
              | true  => 1 + (countoddmembers t)
              | false =>      countoddmembers t
              end
  end.

Example test_countoddmembers1:    countoddmembers [1,0,3,1,4,5] = 4.
Proof. simpl. reflexivity. Qed.
Example test_countoddmembers2:    countoddmembers [0,2,4] = 0.
Proof. simpl. reflexivity. Qed.
Example test_countoddmembers3:    countoddmembers nil = 0.
Proof. simpl. reflexivity. Qed.

(** **** Exercise: 3 stars, recommended (alternate) *)
(** [alternate] : "zips up" two lists into one, alternating between
    elements from first and elements from second.
    See tests below for examples.

    Note: one natural/elegant way of writing [alternate] will fail
    [Fixpoint] "obviously terminating" requirement.  Might need a more
    verbose solution that considers elements of both lists at the same
    time.  (One possible solution requires defining a new kind of
    pairs, but this is not the only way.)  *)

Fixpoint alternate (l1 l2 : natlist) : natlist :=
  match l1, l2 with
  |         [],         [] => []
  |         [],         l' => l'
  |         l',         [] => l'
  | l1h :: l1t, l2h :: l2t => l1h :: l2h :: (alternate l1t l2t)
  end.

Example test_alternate1:        alternate [1,2,3] [4,5,6] = [1,4,2,5,3,6].
Proof. simpl. reflexivity. Qed.
Example test_alternate2:        alternate [1] [4,5,6] = [1,4,5,6].
Proof. simpl. reflexivity. Qed.
Example test_alternate3:        alternate [1,2,3] [4] = [1,4,2,3].
Proof. simpl. reflexivity. Qed.
Example test_alternate4:        alternate [] [20,30] = [20,30].
Proof. simpl. reflexivity. Qed.

(* ###################################################### *)
(** ** Bags via Lists *)

(** A [bag] (aka [multiset]) like set, but element can appear multiple
    times instead of just once.  An impl of bags as a list: *)

Definition bag := natlist.

(** **** Exercise: 3 stars, recommended (bag_functions) *)

Fixpoint count (v:nat) (s:bag) : nat :=
  match s with
  |     [] => 0
  | h :: t => match beq_nat h v with
              | true  => 1 + (count v t)
              | false =>      count v t
              end
  end.

Example test_count1:          count 1 [1,2,3,1,4,1] = 3.
Proof. reflexivity. Qed.
Example test_count2:          count 6 [1,2,3,1,4,1] = 0.
Proof. reflexivity. Qed.

(** [sum] similar to set [union]: [sum a b] contains all elements
    of [a] and [b].

    Note: using [Definition] instead of [Fixpoint], and no argument
    names. *)

Definition sum : bag -> bag -> bag := app.

Example test_sum1:            count 1 (sum [1,2,3] [1,4,1]) = 3.
Proof. reflexivity. Qed.

Definition add (v:nat) (s:bag) : bag := v :: s.

Example test_add1:            count 1 (add 1 [1,4,1]) = 3.
Proof. reflexivity. Qed.
Example test_add2:            count 5 (add 1 [1,4,1]) = 0.
Proof. reflexivity. Qed.

Definition member (v:nat) (s:bag) : bool := negb (beq_nat (count v s) 0).

Example test_member1:         member 1 [1,4,1] = true.
Proof. reflexivity. Qed.
Example test_member2:         member 2 [1,4,1] = false.
Proof. reflexivity. Qed.

(** **** Exercise: 3 stars, optional (bag_more_functions) *)

Fixpoint remove_one (v:nat) (s:bag) : bag :=
  match s with
  |     [] => []
  | h :: t => match beq_nat h v with
              | true  => t
              | false => h :: (remove_one v t)
              end
  end.

Example test_remove_one1:     count 5 (remove_one 5 [2,1,5,4,1]) = 0.
Proof. reflexivity. Qed.
Example test_remove_one2:     count 5 (remove_one 5 [2,1,4,1]) = 0.
Proof. reflexivity. Qed.
Example test_remove_one3:     count 4 (remove_one 5 [2,1,4,5,1,4]) = 2.
Proof. reflexivity. Qed.
Example test_remove_one4:     count 5 (remove_one 5 [2,1,5,4,5,1,4]) = 1.
Proof. reflexivity. Qed.

Fixpoint remove_all (v:nat) (s:bag) : bag :=
  match s with
  |     [] => []
  | h :: t => match beq_nat h v with
              | true  =>       remove_all v t
              | false => h :: (remove_all v t)
              end
  end.

Eval simpl in (remove_all 5 [2,1,5,4,5,1,4,5,1,4]).

Example test_remove_all1:     count 5 (remove_all 5 [2,1,5,4,1]) = 0.
Proof. reflexivity. Qed.
Example test_remove_all2:     count 5 (remove_all 5 [2,1,4,1]) = 0.
Proof. reflexivity. Qed.
Example test_remove_all3:     count 4 (remove_all 5 [2,1,4,5,1,4]) = 2.
Proof. reflexivity. Qed.
Example test_remove_all4:     count 5 (remove_all 5 [2,1,5,4,5,1,4,5,1,4]) = 0.
Proof. reflexivity. Qed.

Fixpoint subset (s1:bag) (s2:bag) : bool :=
  match s1 with
  |     [] => true
  | h :: t => match member h s2 with
              | true  => subset t (remove_one h s2)
              | false => false
              end
  end.

Example test_subset1:         subset [1,2] [2,1,4,1] = true.
Proof. reflexivity. Qed.
Example test_subset2:         subset [1,2,2] [2,1,4,1] = false.
Proof. reflexivity. Qed.

(** **** Exercise: 3 stars, recommended (bag_theorem) *)
(** Write/prove theorem about bags involving [count] and [add].
    If you write a true theorem but requires techniques not learned yet,
    ask for help. *)

Eval compute in (member 3 (add 3 [1, 2])).
Eval compute in (ble_nat 1 (count 3 (add 3 [1, 2]))).
Eval compute in (ble_nat 1 (count 3 (add 3 [3, 2]))).
Eval compute in (ble_nat 1 (count 3 (add 3 [3, 3]))).

(** HC: I could not do this.  Then I found
    https://github.com/haklabbeograd/software-foundations-coq-workshop/blob/master/Lists.v
    which had the MAGIC beq_nat_refl step. *)
Theorem bag_theorem : forall (n : nat), forall (b : bag),
  ble_nat 1 (count n (add n b)) = true.
Proof.
  intros n b. induction n as [|n'].
  Case "n is 0". reflexivity.
  Case "n is S n'".          (* IHn' : ble_nat 1 (count    n'  (add    n'  b)) = true *)
                             (*        ble_nat 1 (count (S n') (add (S n') b)) = true *)
    simpl.                   (*    match
                                     match beq_nat n' n' with
                                     | true => S (count (S n') b)
                                     | false => count (S n') b
                                     end
                                   with
                                   | 0 => false
                                   | S _ => true
                                   end = true *)
    rewrite <- beq_nat_refl. (*   true = true
                                The beq_nat n' n' is true so nested match returns S (count (S n') b) .
                                Outer match sees S _ so returns true. *)
    reflexivity.
Qed.

(** HC: here are two theorems from the link that had the magic step. *)

Theorem hc_p_n_count_add : forall (p : nat) (s : bag),
  S (count p s) = count p (add p s).
Proof.
  intros p s.
  simpl.                    (*    S (count p s) = match beq_nat p p with
                                                  | true => S (count p s)
                                                  | false => count p s
                                                  end *)
  rewrite <- beq_nat_refl.  (*    S (count p s) =           S (count p s) *)
  reflexivity.
Qed.

(** HC: I had to edit this one a bit to get it to pass. *)
Theorem homework3: forall (n m:nat) (b:bag),
  (beq_nat m n) = false -> beq_nat (count n b) (count n (add m b)) = true.
Proof.
  intros m n b H. destruct b as [| n' b'].
  Case "b is []".     (* beq_nat (count m []) (count m (add n [])) = true *)
    simpl.            (* match
                           match beq_nat n m with
                           | true => 1
                           | false => 0
                           end
                         with
                         | 0 => true
                         | S _ => false
                         end = true *)
    rewrite H.        (* true = true ; n and m not eq so 0, 0 so true *)
    reflexivity.
  Case "b is n'::b'".        (*    beq_nat (count m (n' :: b')) (count m (add n (n' :: b'))) = true *)
    simpl.                   (*    beq_nat
                                     match beq_nat n' m with
                                     | true => S (count m b')
                                     | false => count m b'
                                     end
                                     match beq_nat n m with          ; following rewrite says this is false leaving the false branch
                                     | true =>
                                         S
                                           match beq_nat n' m with
                                           | true => S (count m b')
                                           | false => count m b'
                                            end
                                     | false =>
                                         match beq_nat n' m with
                                         | true => S (count m b')
                                         | false => count m b
                                     end
                                   end = true  *)
    rewrite H.               (*    beq_nat
                                     match beq_nat n' m with
                                     | true => S (count m b')
                                     | false => count m b'
                                     end
                                     match beq_nat n' m with
                                     | true => S (count m b')
                                     | false => count m b'
                                     end = true *)
    rewrite <- beq_nat_refl.  (*   true  = true ; since both branch of outer beq_nat are same *)
    reflexivity.
Qed.

(* ###################################################### *)
(** * Reasoning About Lists *)

(**  Facts about list functions sometimes proved by [simpl] (like numbers): *)

Theorem nil_app : forall l:natlist,
  [] ++ l = l.
Proof.
   simpl. reflexivity. Qed.

(** Case analysis on possible shapes of unknown list (like numbers): *)

Theorem tl_length_pred : forall l:natlist,
  pred (length l) = length (tail l).
Proof.
  intros l. destruct l as [| n l'].
  Case "l = []".     (* pred (length []) = length (tail []) *)
    simpl.           (*                0 = 0                *)
    reflexivity.
  Case "l = n::l'".  (* pred (length (n :: l')) = length (tail (n :: l')) *)
    (* simpl. *)     (*               length l' = length l'               *)
    unfold length.   (* STEP FOR MORE DETAIL *)
    unfold pred.
    unfold tail.
    reflexivity.
Qed.

(** [nil] case works because [tail nil = nil].
    Note: [destruct] ... [as] introduces two names, [n] and [l'],
    because [cons] takes two arguments. *)

(** Usually theorems about lists require induction for their proofs. *)

(* ###################################################### *)
(** ** Induction on Lists *)

(** [Inductive] declaration defines set of values
    built up from declared constructors:
    - a boolean can be [true] or [false];
    - a number  can be [O]    or [S]    applied to a number;
    - a list    can be [nil]  or [cons] applied to a number and a list.

    Applying declared constructors to each other are the _only_
    possible shapes that elements of set can have.

    Leads to a way of reasoning :

    - a number is [O]   or [S]    applied to                 some _smaller_ number;
    - a list   is [nil] or [cons] applied to some number and some _smaller_ list;

    Given proposition [P] that mentions list [l],
    to argue [P] holds for _all_ lists:

    - show [P] is true of [l] when [l] is [nil].

    - show [P] is true of [l] when [l] is [cons n l']
      for some number [n] and some smaller list [l'],
      assuming that [P] is true for [l'].

    Since larger lists can only be built up from smaller ones,
    eventually reaching [nil], these two things together establish the
    truth of [P] for all lists [l].

    E.G.,: *)

Theorem app_ass : forall l1 l2 l3 : natlist,
  (l1 ++ l2) ++ l3 = l1 ++ (l2 ++ l3).
Proof.
  intros l1 l2 l3. induction l1 as [| n l1'].
  Case "l1 = []".     (*          ([]   ++ l2) ++ l3 =       []   ++ l2 ++ l3 *)
    simpl.            (*                   l2  ++ l3 =               l2 ++ l3 *)
    reflexivity.
  Case "l1 = n::l1'". (* IHl1' :  (l1'  ++ l2) ++ l3 =       l1'  ++ l2 ++ l3 *)
                      (*   ((n ::  l1') ++ l2) ++ l3 = (n :: l1') ++ l2 ++ l3 *)
    simpl.            (*     n :: (l1'  ++ l2) ++ l3 =  n :: l1'  ++ l2 ++ l3 ; by def of app *)
    rewrite -> IHl1'. (*     n ::  l1'  ++ l2  ++ l3 =  n :: l1'  ++ l2 ++ l3 *)
    reflexivity.
Qed.

(** Above proof not illuminating as static document.  Better in
    interactive session to see current goal and context at each point
    (state that is not visible in the written version).

    Natural-language proof should include explicit signposts,
    especially stating the induction hypothesis. *)

(** _Theorem_: For all lists [l1], [l2], and [l3],
   [(l1 ++ l2) ++ l3 = l1 ++ (l2 ++ l3)].

   _Proof_: By induction on [l1].

   - First, suppose [l1 = []].  We must show
       ([] ++ l2) ++ l3 = [] ++ (l2 ++ l3),
     which follows directly from the definition of [++].

   - Next, suppose [l1 = n::l1'], with
       (l1' ++ l2) ++ l3 = l1' ++ (l2 ++ l3)
     (the induction hypothesis). We must show
       ((n :: l1') ++ l2) ++ l3 = (n :: l1') ++ (l2 ++ l3).

     By the definition of [++], this follows from
       n :: ((l1' ++ l2) ++ l3) = n :: (l1' ++ (l2 ++ l3)),
     which is immediate from the induction hypothesis.  [] *)

Theorem app_length : forall l1 l2 : natlist,
  length (l1 ++ l2) = (length l1) + (length l2).
Proof.
  intros l1 l2. induction l1 as [| n l1'].
  Case "l1 = []".     (*         length       ([]        ++ l2)  =    length       []   + length l2 *)
    simpl.            (*         length                     l2   =                  0   + length l2
                                 length                     l2   =                        length l2 *)
    reflexivity.
  Case "l1 = n::l1'". (* IHl1' : length       (l1'       ++ l2)  =    length       l1'  + length l2 *)

                      (*         length ((n :: l1')      ++ l2)  =    length (n :: l1') + length l2  *)
    simpl.            (*      S (length       (l1'       ++ l2)) = S (length       l1'  + length l2) ; by def of length *)
    rewrite -> IHl1'. (*      S (length        l1' + length l2)  = S (length       l1'  + length l2) *)
    reflexivity.
Qed.

(** "cons on the right" [snoc] : *)

Fixpoint snoc (l:natlist) (v:nat) : natlist :=
  match l with
  |     [] => [v]
  | h :: t => h :: (snoc t v)
  end.

Eval simpl in (snoc [1,2,3] 4).

(** Use it to define list-reversing [rev] : *)

Fixpoint rev (l:natlist) : natlist :=
  match l with
  |     [] => []
  | h :: t => snoc (rev t) h
  end.

Example test_rev1:            rev [1,2,3] = [3,2,1].
Proof. reflexivity.  Qed.
Example test_rev2:            rev nil = nil.
Proof. reflexivity.  Qed.

(** Prove reversing does not change length.
    1st attempt gets stuck in the successor case: *)

Theorem rev_length_firsttry : forall l : natlist,
  length (rev l) = length l.
Proof.
  intros l. induction l as [| n l'].
  Case "l = []".
    reflexivity.
  Case "l = n :: l'".  (* IHl' : length       (rev       l')    =    length       l'   *)
                       (*        length       (rev (n :: l'))   =    length (n :: l')  *)
    simpl.             (*        length (snoc (rev       l') n) = S (length       l')  *)
    (* Goal is an equality involving [snoc], but we have no equations
       in context or global environment that deal with [snoc].
       Make a little progress using IH: *)
    rewrite <- IHl'.   (*        length (snoc (rev       l') n) = S (length (rev  l')) *)
    (* But can't go further. *)
Admitted.

(** First prove a lemma about [snoc] to use in [length/rev] proof. *)

Theorem length_snoc : forall n : nat, forall l : natlist,
  length (snoc l n) = S (length l).
Proof.
  intros n l. induction l as [| n' l'].
  Case "l = []".       (*         length (snoc        []  n) =     S (length       [])  *)
    simpl.             (*                                 1  =     1                    *)
    reflexivity.
  Case "l = n'::l'".   (*  IHl' : length (snoc        l'  n)  =    S (length        l')  *)
                       (*         length (snoc (n' :: l') n)  =    S (length (n' :: l')) *)
    simpl.             (*      S (length (snoc        l'  n)) = S (S (length        l')) ; by def of length *)
    rewrite -> IHl'.   (*   S (S (length              l'))    = S (S (length        l')) *)
    reflexivity.
Qed.

(** Now use lemma. *)

Theorem rev_length : forall l : natlist,
  length (rev l) = length l.
Proof.
  intros l. induction l as [| n l'].
  Case "l = []".
    reflexivity.
  Case "l = n::l'".         (* IHl' : length (rev             l')    =    length       l'  *)
                            (*        length       (rev (n :: l'))   =    length (n :: l') *)
    simpl.                  (*        length (snoc (rev       l') n) = S (length       l') ; def of snoc and length *)
    rewrite -> length_snoc. (*     S (length       (rev       l'))   = S (length       l') *)
    rewrite -> IHl'.        (*     S (length                  l')    = S (length       l') *)
    reflexivity.
Qed.

(** Informal proofs of these two theorems:
(* TODO *)
    _Theorem_: For all numbers [n] and lists [l],
       [length (snoc l n) = S (length l)].

    _Proof_: By induction on [l].

    - First, suppose [l = []].  We must show
        length (snoc [] n) = S (length []),
      which follows directly from the definitions of
      [length] and [snoc].

    - Next, suppose [l = n'::l'], with
        length (snoc l' n) = S (length l').
      We must show
        length (snoc (n' :: l') n) = S (length (n' :: l')).
      By the definitions of [length] and [snoc], this
      follows from
        S (length (snoc l' n)) = S (S (length l')),
      which is immediate from the induction hypothesis. []


    _Theorem_: For all lists [l], [length (rev l) = length l].

    _Proof_: By induction on [l].

      - First, suppose [l = []].  We must show
          length (rev []) = length [],
        which follows directly from the definitions of [length]
        and [rev].

      - Next, suppose [l = n::l'], with
          length (rev l') = length l'.
        We must show
          length (rev (n :: l')) = length (n :: l').
        By the definition of [rev], this follows from
          length (snoc (rev l') n) = S (length l')
        which, by the previous lemma, is the same as
          S (length (rev l')) = S (length l').
        This is immediate from the induction hypothesis. []

    These proofs give much detail.  It might be easier to follow with
    a little less detail overall (work out details in your minds) and
    highlight non-obvious steps:

    _Theorem_:
     For all lists [l], [length (rev l) = length l].

    _Proof_: First, observe that
       length (snoc l n) = S (length l)
     for any [l].  This follows by a straightforward induction on [l].
     The main property now follows by another straightforward
     induction on [l], using the observation together with the
     induction hypothesis in the case where [l = n'::l']. [] *)

(* ###################################################### *)
(** ** [SearchAbout] *)

(** Proofs can use other proved theorems using [rewrite].  To refer to
    theorem, we need its name.  Remembering proved names is difficult.

    [SearchAbout foo] will display all theorems involving [foo].

    Uncommenting the following: *)

(* SearchAbout rev. *)

(** ProofGeneral : run via [C-c C-a C-a]. *)

(* ###################################################### *)
(** ** List Exercises, Part 1 *)

(** **** Exercise: 3 stars, recommended (list_exercises) *)

Theorem app_nil_end : forall l : natlist,
  l ++ [] = l.
Proof.
  intros l. induction l as [| n l'].
  Case "l is []".
    reflexivity.
  Case "l is n::l'".  (* IHl' : l'  ++ [] =      l' *)
                      (*  (n :: l') ++ [] = n :: l' *)
    simpl.            (*   n :: l'  ++ [] = n :: l' *)
    rewrite -> IHl'.  (*   n :: l'        = n :: l' *)
    reflexivity.
Qed.

Eval simpl in ([1,2,3] ++ [4]).

(* HC: Thinking about using this for rev_involutive. *)

Theorem hc_snoc : forall l : natlist, forall n : nat,
  snoc l n = l ++ [n].
Proof.
  intros l n.  induction l as [| n' l'].
  Case "l is []".
    simpl. reflexivity.
  Case "l is n'::l'".
    simpl.
    rewrite IHl'.
    reflexivity.
Qed.

(* TODO
Theorem hc_snoc_rev : forall l : natlist, forall n : nat,
  snoc (rev l) n = (rev l) ++ [n].
Proof.
  intros l n.  induction l as [| n' l'].
  Case "l is []".
    simpl. reflexivity.
  Case "l is n'::l'". (* IHl' : snoc (rev        l')      n =       rev        l'     ++ [n] *)
                      (*        snoc (rev (n' :: l'))     n =       rev (n' :: l')    ++ [n] *)
    simpl.            (*  snoc (snoc (rev        l')  n') n = snoc (rev        l') n' ++ [n] *)
    (* STUCK *)
*)

Theorem hc_rev_snoc : forall n : nat, forall l : natlist,
  rev (snoc l n) = n::rev l.
Proof.
  intros n l. induction l as [| n' l'].
  Case "l is []". simpl. reflexivity.
  Case "l is n'::l'". (* IHl' : rev (snoc        l'  n)     = n ::       rev        l'     *)
                      (*        rev (snoc (n' :: l') n)     = n ::       rev (n' :: l')    *)
    simpl.            (*  snoc (rev (snoc        l'  n)) n' = n :: snoc (rev        l') n' *)
    rewrite -> IHl'.  (*  snoc (n :: rev         l')     n' = n :: snoc (rev        l') n' *)
    simpl.            (*        n :: snoc (rev   l')     n' = n :: snoc (rev        l') n' *)
    reflexivity.
Qed.

Theorem rev_involutive : forall l : natlist,
  rev (rev l) = l.
Proof.
  intros l. induction l as [| n l'].
  Case "l is []". simpl. reflexivity.
  Case "l is n::l'".         (* IHl' : rev (rev             l')    =      l' *)
                             (*        rev       (rev (n :: l'))   = n :: l' *)
    simpl.                   (*        rev (snoc (rev       l') n) = n :: l' *)
    rewrite -> hc_rev_snoc.  (*   n :: rev (rev             l')    = n :: l' *)
    rewrite -> IHl'.         (*   n ::                      l'     = n :: l' *)
    reflexivity.
Qed.

(** There is short solution to next exercise.
    If you get stuck, look for a simpler way. *)

Theorem app_ass4 : forall l1 l2 l3 l4 : natlist,
  l1 ++ (l2 ++ (l3 ++ l4)) = ((l1 ++ l2) ++ l3) ++ l4.
Proof.
  intros l1 l2 l3 l4.  induction l1 as [| n l1'].
  Case "l is []".        (*          []  ++ l2  ++ l3  ++ l4 =         (([]  ++  l2) ++ l3) ++ l4 *)
    simpl.               (*                 l2  ++ l3  ++ l4 =                  (l2  ++ l3) ++ l4 *)
    rewrite <- app_ass.  (*                (l2  ++ l3) ++ l4 =                  (l2  ++ l3) ++ l4 *)
    reflexivity.
  Case "l is n::l1'".    (* IHl1' : l1'  ++ l2  ++ l3  ++ l4 =         ((l1'  ++ l2) ++ l3) ++ l4 *)
                         (* (n ::   l1') ++ l2  ++ l3  ++ l4 = (((n ::   l1') ++ l2) ++ l3) ++ l4 *)
    simpl.               (*  n ::   l1'  ++ l2  ++ l3  ++ l4 =    n :: ((l1'  ++ l2) ++ l3) ++ l4 *)
    rewrite -> IHl1'.    (*  n :: ((l1'  ++ l2) ++ l3) ++ l4 =    n :: ((l1'  ++ l2) ++ l3) ++ l4 *)
    reflexivity.
Qed.

Theorem snoc_append : forall (l:natlist) (n:nat),
  snoc l n = l ++ [n].
Proof.
  intros l n. induction l as [| n' l'].
  Case "l is []". simpl. reflexivity.
  Case "l is n'::l'". (*  IHl' : snoc        l'     n  =        l'  ++ [n] *)
                      (*         snoc (n' :: l')    n  = (n' :: l') ++ [n] *)
    simpl.            (*   n' :: snoc        l'     n  =  n' :: l'  ++ [n] *)
    rewrite -> IHl'.  (*   n' ::             l' ++ [n] =  n' :: l'  ++ [n] *)
    reflexivity.
Qed.

Theorem distr_rev : forall l1 l2 : natlist,
  rev (l1 ++ l2) = (rev l2) ++ (rev l1).
Proof.
  intros l1 l2.  induction l1 as [| n l1'].
  Case "l1 is []".          (*         rev       ([]   ++     l2)         = rev l2 ++       rev       []         *)
    simpl.                  (*         rev                    l2          = rev l2 ++                 []         *)
    rewrite -> app_nil_end. (*         rev                    l2          = rev l2                               *)
    reflexivity.
  Case "l is n'::l'".       (* IHl1' : rev       (l1'  ++     l2)         = rev l2 ++       rev       l1'        *)

                            (*         rev ((n :: l1') ++     l2)         = rev l2 ++       rev (n :: l1')       *)
    simpl.                  (*   snoc (rev       (l1'  ++     l2))     n  = rev l2 ++ snoc (rev       l1') n     *)
    rewrite -> snoc_append. (*         rev       (l1'  ++     l2)  ++ [n] = rev l2 ++ snoc (rev       l1') n     *)
    rewrite -> IHl1'.       (*        (rev        l2   ++ rev l1') ++ [n] = rev l2 ++ snoc (rev       l1') n     *)
    rewrite -> snoc_append. (*        (rev        l2   ++ rev l1') ++ [n] = rev l2 ++       rev       l1' ++ [n] *)
    rewrite -> app_ass.     (*         rev        l2   ++ rev l1'  ++ [n] = rev l2 ++       rev       l1' ++ [n] *)
    reflexivity.
Qed.

(** An exercise about your implementation of [nonzeros]: *)
Lemma nonzeros_length : forall l1 l2 : natlist,
  nonzeros (l1 ++ l2) = (nonzeros l1) ++ (nonzeros l2).
Proof.
  intros l1 l2.  induction l1 as [| n l1'].
  Case "l1 is []".          (*         nonzeros          ([]   ++          l2) =         nonzeros          []  ++ nonzeros l2 *)
    simpl.                  (*         nonzeros                            l2  =                                   nonzeros l2 by ; def of nonzeros and app *)
    reflexivity.
  Case "l is n::l'".        (* IHl1' : nonzeros          (l1'  ++          l2) =         nonzeros          l1'  ++ nonzeros l2 *)
    induction n as [|n'].
    SCase "n is 0".         (*         nonzeros    ((0 :: l1') ++          l2) =         nonzeros (0 ::    l1') ++ nonzeros l2 *)
      simpl.                (*         nonzeros          (l1'  ++          l2) =         nonzeros          l1'  ++ nonzeros l2 ; by def of nonzeros*)
      rewrite -> IHl1'.     (*         nonzeros           l1'  ++ nonzeros l2  =         nonzeros          l1'  ++ nonzeros l2 *)
      reflexivity.
    SCase "n is S n'".      (*         nonzeros ((S n' :: l1') ++          l2) =         nonzeros (S n' :: l1') ++ nonzeros l2 *)
      simpl.                (* S n' :: nonzeros          (l1'  ++          l2) = S n' :: nonzeros          l1'  ++ nonzeros l2  by def of nonzero *)
      rewrite -> IHl1'.     (* S n' :: nonzeros           l1'  ++ nonzeros l2  = S n' :: nonzeros          l1'  ++ nonzeros l2 *)
      reflexivity.
Qed.

(* ###################################################### *)
(** ** List Exercises, Part 2 *)

(** **** Exercise: 2 stars, recommended (list_design) *)
(** Design exercise:
     - Write down a non-trivial theorem involving [cons]
       ([::]), [snoc], and [append] ([++]).
     - Prove it. *)

Theorem hc_cons_snoc_append : forall  (n:nat) (m:nat) (l:natlist),
  n :: (snoc l m) = n :: l ++ [m].
Proof.
  intros n m l. induction l as [|n' l'].
  Case "l is []".            (*        n :: snoc []                  m  =  n :: []         ++ [m] *)
    simpl.                   (*       [n,                            m] = [n,                  m] *)
    reflexivity.
  Case "l is n'::l'".        (* IHl' : n ::       snoc        l'     m  =  n ::        l'  ++ [m] *)
                             (*        n ::       snoc (n' :: l')    m  =  n :: (n' :: l') ++ [m] *)
    simpl.                   (*        n :: n' :: snoc        l'     m  =  n ::  n' :: l'  ++ [m] *)
    rewrite -> snoc_append.  (*        n :: n' ::             l' ++ [m] =  n ::  n' :: l'  ++ [m] *)
    reflexivity.
Qed.

(** **** Exercise: 2 stars, optional (bag_proofs) *)
(** Theorems to prove about bag definitions. *)

Theorem count_member_nonzero : forall (s : bag),
  ble_nat 1 (count 1 (1 :: s)) = true.
Proof.
  intros s. destruct s as [| n' s'].
  Case "s is []".     (* ble_nat 1 (count 1 [1])             = true *)
    (* simpl. ; but step thru instead: *)
    unfold count.     (* ble_nat 1 match beq_nat 1 1 with
                                   | true => 1 + 0
                                   | false => 0
                                   end                       = true *)
    unfold beq_nat.   (* ble_nat 1 (1 + 0)                   = true *)
    unfold plus.      (* ble_nat 1  1                        = true *)
    unfold ble_nat.   (* true                                = true *)
    reflexivity.
  Case "s is n'::s'". (* ble_nat 1 (count 1 (1 :: n' :: s')) = true *)
    simpl.            (* QUESTION/TODO: step in detail *)
    reflexivity.
Qed.

(** This lemma might help in the next proof: *)

Theorem ble_n_Sn : forall n,
  ble_nat n (S n) = true.
Proof.
  intros n. induction n as [| n'].
  Case "0".
    simpl.  reflexivity.
  Case "S n'".    (* IHn' : ble_nat    n'     (S n')  = true *)
                  (*        ble_nat (S n') (S (S n')) = true *)
    simpl.        (*        ble_nat    n'     (S n')  = true *)
    rewrite IHn'. (*                             true = true *)
    reflexivity.
Qed.

Theorem remove_decreases_count: forall (s : bag),
  ble_nat (count 0 (remove_one 0 s)) (count 0 s) = true.
Proof.
  intros s. induction s as [| n' s'].
  Case "s is []".    simpl. reflexivity.
  Case "s is n'::s'".      (* IHs' : ble_nat (count 0 (remove_one 0           s'))     (count 0           s')  = true *)
                           (*        ble_nat (count 0 (remove_one 0   (n'  :: s')))    (count 0   (n'  :: s')) = true *)
    destruct n' as [|n''].
    SSCase "n' is 0".      (*        ble_nat (count 0 (remove_one 0   (0   :: s')))    (count 0   (0   :: s')) = true *)
      simpl.               (*        ble_nat (count 0                         s')   (S (count 0           s')) = true *)
      rewrite -> ble_n_Sn. (*                                                                             true = true *)
      reflexivity.
    SSCase "n' is S n''".  (*        ble_nat (count 0 (remove_one 0 (S n'' :: s')))    (count 0 (S n'' :: s')) = true *)
      simpl.               (*        ble_nat (count 0 (remove_one 0           s'))     (count 0           s')  = true *)
      rewrite -> IHs'.     (*                                                                             true = true *)
      reflexivity.
Qed.

(** **** Exercise: 3 stars, optional (bag_count_sum) *)
(** Write down an interesting theorem about bags involving the
    functions [count] and [sum], and prove it.*)
(* TODO *)
(*
Theorem hc_bag_count_sum : forall (n : nat) (s1 s2 : bag),
  count n (sum s1 s2) = count n s1 + count n s2.
Proof.
  intros n s1 s2.  induction s1 as [| n' s1'].
  Case "s1 is []".  (* count n (sum [] s2) = count n [] + count n s2 *)
    simpl.          (* count n s2 = count n s2 *)
    reflexivity.
  Case "l is n::l'". (* IHs1' : count n (sum s1' s2) = count n s1' + count n s2 *)
    induction n as [|n''].
    SCase "n is 0".  (* count 0 (sum (n' :: s1') s2) = count 0 (n' :: s1') + count 0 s2 *)
      simpl.
Admitted.
*)
(** **** Exercise: 4 stars, optional (rev_injective) *)
(** Prove [rev] is injective
    (note: there is hard and easy way to solve this). **)
Lemma hc_rev_i : forall (l1 l2 : natlist),
  l1 = l2 -> rev l1 = rev l2.
Proof.
  intros l1 l2 H. destruct l1 as [|n l1'].
  Case "l1 is []".      rewrite H. reflexivity.
  Case "l1 is n::l1'".  rewrite H. reflexivity.
Qed.

Lemma hc_rev_j : forall (n : nat) (l : natlist),
  l = [] -> snoc l n = n :: l.
Proof.
  intros n l H. destruct n as [|n'].
  Case "n is 0".    rewrite H. simpl. reflexivity.
  Case "n is S n'". rewrite H. simpl. reflexivity.
Qed.
(** Could not do. Found:
    https://github.com/joshcough/software-foundations/blob/master/Lists.v
  *)
Theorem hc_rev_injective : forall (l1 l2 : natlist),
  rev l1 = rev l2 -> l1 = l2.
Proof.
  intros l1 l2 H.               (*    l1 = l2 *)
  assert (rr: rev (rev l1) = rev (rev l2)).
    rewrite H. reflexivity.
                                (* rr : rev (rev l1) = rev (rev l2) *)
  rewrite rev_involutive in rr. (* rr :          l1  = rev (rev l2) *)
  rewrite rev_involutive in rr. (* rr :          l1  =          l2  *)
                                (* goal :        l1  =          l2  *)
  rewrite rr.                   (* goal :        l2  =          l2  *)
  reflexivity.
Qed.

(* ###################################################### *)
(** * Options *)

Inductive natoption : Type :=
  | Some : nat -> natoption
  | None : natoption.

(** Use-case: return "error" from functions.
    E.G., function [nat -> natlist -> nat] that returns [n]th element of list.
    Need to return number, even when list is too short. *)

Fixpoint index_bad (n:nat) (l:natlist) : nat :=
  match l with
  |     []  => 42  (* arbitrary! *)
  | a :: l' => match beq_nat n O with
               | true  => a
               | false => index_bad (pred n) l'
               end
  end.

(** Instead, make type [nat -> natlist -> natoption],
    then return [None] when too short, else [Some a]. *)

Fixpoint index (n:nat) (l:natlist) : natoption :=
  match l with
  |     []  => None
  | a :: l' => match beq_nat n O with
               | true  => Some a
               | false => index (pred n) l'
               end
  end.

Example test_index1 :         index 0 [4,5,6,7]  = Some 4.
Proof. reflexivity.  Qed.
Example test_index2 :         index 3 [4,5,6,7]  = Some 7.
Proof. reflexivity.  Qed.
Example test_index3 :         index 10 [4,5,6,7] = None.
Proof. reflexivity.  Qed.

(** Can use [if]: *)

Fixpoint index' (n:nat) (l:natlist) : natoption :=
  match l with
  |     []  => None
  | a :: l' => if beq_nat n O then Some a else index' (pred n) l'
  end.

(** boolean type not built in.
    But Coq accepts conditional exprs over _any_ inductively defined type with exactly two constructors.
    True if evaluates to first constructor in [Inductive] definition, otherwise false. *)

(** Function to get [nat] from [natoption] or supplied default for [None]: *)

Definition option_elim (d : nat) (o : natoption) : nat :=
  match o with
  | Some n' => n'
  | None    => d
  end.

(** **** Exercise: 2 stars (hd_opt) *)
(** Using the same idea, fix the [hd] function from earlier so we don't
   have to pass a default element for the [nil] case.  *)

Definition hd_opt (l : natlist) : natoption :=
  match l with
  |   [] => None
  | h::t => Some h
  end.

Example test_hd_opt1 :        hd_opt []    = None.
Proof. reflexivity. Qed.
Example test_hd_opt2 :        hd_opt [1]   = Some 1.
Proof. reflexivity. Qed.
Example test_hd_opt3 :        hd_opt [5,6] = Some 5.
Proof. reflexivity. Qed.

(** **** Exercise: 1 star, optional (option_elim_hd) *)
(** Exercise relates [hd_opt] to [hd]. *)

Theorem option_elim_hd : forall (l:natlist) (default:nat),
  hd default l = option_elim default (hd_opt l).
Proof.
  intros l default. induction l as [|n].
  Case "l is []".   simpl. reflexivity.
  Case "l is n::l". simpl. reflexivity.
Qed.

(** **** Exercise: 2 stars, recommended (beq_natlist) *)
(** Write [beq_natlist] to compare two lists of numbers for equality.
    Prove that [beq_natlist l l] yields [true] for every list [l]. *)

Fixpoint beq_natlist (l1 l2 : natlist) : bool :=
  match l1, l2 with
  |      [],      [] => true
  |      [],       _ => false
  |       _,      [] => false
  | n1::l1', n2::l2' => if beq_nat n1 n2 then beq_natlist l1' l2' else false
  end.

Example test_beq_natlist1 :   beq_natlist nil     nil     = true.
Proof. reflexivity. Qed.
Example test_beq_natlist2 :   beq_natlist [1,2,3] [1,2,3] = true.
Proof. reflexivity. Qed.
Example test_beq_natlist3 :   beq_natlist [1,2,3] [1,2,4] = false.
Proof. reflexivity. Qed.

Theorem beq_natlist_refl : forall l:natlist,
  true = beq_natlist l l.
Proof.
  intros l. induction l as [|n'].
  Case "l is []". simpl. reflexivity.
  Case "l is n'::l".          (* IHl : true =                        beq_natlist        l         l             *)
                              (*       true =                        beq_natlist (n' :: l) (n' :: l)            *)
    simpl.                    (*       true = (if beq_nat n' n' then beq_natlist        l         l else false) *)
    rewrite <- beq_nat_refl.  (*       true =                        beq_natlist        l         l             *)
    rewrite <- IHl.           (*       true = true                                                              *)
    reflexivity.
Qed.

(* ###################################################### *)
(** * Extended Exercise: Dictionaries *)

(** Example of defining fundamental data structures:
    [dictionary] : finite map from numbers to numbers.) *)

Module Dictionary.

Inductive dictionary : Type :=
  | empty  : dictionary
  | record : nat -> nat -> dictionary -> dictionary.

(** Two ways to construct a [dictionary]:

    1. [empty] constructor

    2. [record] constructor given a key, a value, and an existing [dictionary]
       to construct [dictionary] with additional key/value mapping. *)

Definition insert (key value : nat) (d : dictionary) : dictionary :=
  (record key value d).

(** [find] searches [dictionary] for given key.
    [None] if key not found.
    [Some val] if key mapped to [val] in dictionary.
    If same key mapped to multiple values, returns first one found. *)

Fixpoint find (key : nat) (d : dictionary) : natoption :=
  match d with
  | empty         => None
  | record k v d' => if (beq_nat key k) then (Some v) else (find key d')
  end.

(** **** Exercise: 1 star (dictionary_invariant1) *)
(** Complete proof: *)

Theorem dictionary_invariant1 : forall (d : dictionary) (k v: nat),
  (find k (insert k v d)) = Some v.
Proof.
  intros d k v. destruct k as [|k'].
  Case "k is 0".                   (* find 0 (insert 0 v d)                             = Some v *)
    (* simpl. *)                   (*                Some v                             = Some v *)
    unfold insert.                 (* find 0 (record 0 v d)                             = Some v *)
    unfold find.                   (* (if beq_nat 0 0 ...                               = Some v *)
    unfold beq_nat.                (*                                            Some v = Some v *)
    reflexivity.
  Case "k is S k'".                (* find (S k') (insert (S k') v d)                   = Some v *)

    (* simpl.                   *) (* (if beq_nat k' k' then Some v else find (S k') d) = Some v *)
    (* rewrite <- beq_nat_refl. *) (*                                            Some v = Some v *)

    unfold insert.                 (* find (S k') (record (S k') v d)                   = Some v *)
    unfold find.                   (* (if beq_nat (S k') (S k') ...                     = Some v *)
    rewrite <- beq_nat_refl.       (*                                            Some v = Some v *)
    reflexivity.
Qed.

(** **** Exercise: 1 star (dictionary_invariant2) *)
(** Complete proof: *)

Theorem dictionary_invariant2 : forall (d : dictionary) (k1 k2 v: nat),
  (beq_nat k1 k2) = false -> (find k1 d) = (find k1 (insert k2 v d)).
Proof.
  intros d k1 k2 v H. induction v as [|v1'].
  Case "v is 0".      (*                                       find k1 d = find k1 (insert k2 0 d)                             *)
    simpl.            (*                                       find k1 d = (if beq_nat k1 k2 then Some 0 else find k1 d)       *)
    rewrite -> H.     (*                                       find k1 d = find k1 d                                           *)
    reflexivity.
  Case "v is S v'".   (* IHv1' :                               find k1 d = find k1 (insert k2    v1'  d)                       *)
                      (*                                       find k1 d = find k1 (insert k2 (S v1') d)                       *)
    rewrite -> IHv1'. (*                       find k1 (insert k2 v1' d) = find k1 (insert k2 (S v1') d)                       *)
    simpl.            (* (if beq_nat k1 k2 then Some v1' else find k1 d) = (if beq_nat k1 k2 then Some (S v1') else find k1 d) *)
    rewrite -> H.     (*                                       find k1 d = find k1 d                                           *)
    reflexivity.
Qed.

End Dictionary.

End NatList.

