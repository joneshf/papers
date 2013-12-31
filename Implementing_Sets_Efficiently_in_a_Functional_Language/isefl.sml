(*
    This is me following along Stephen Adams' paper
    Implementing Sets Efficiently in a Functional Language
*)

type Element = string

datatype Tree = E
  | T of Element * int * Tree * Tree

(* This only works for things with a built-in LT operation. *)
val lt : Element * Element -> bool = op <

(* Number of nodes in a tree. *)
fun size E                   = 0
  | size (T(_, count, _, _)) = count

(* Smart constructor, ensures size is always correct. *)
fun N(v, l, r) = T(v, 1 + size l + size r, l, r)

(* Check if something is a member. *)
fun member (_, E) = false
  | member (x, T(v, _, l, r)) =
    if lt(x, v)
    then member(x, l)
    else if lt(v, x)
    then member(x, r)
    else true

(* Find the smallest node. This is the leftmost node. *)
fun min (T(v, _, E, _)) = v
  | min (T(_, _, l, _)) = min l
  | min E               = raise Match

(* Rotations. *)
fun single_L (a, x, T(b, _, y, z)) = N(b, N(a, x, y), z)
fun single_R (b, T(a, _, x, y), z) = N(a, x, N(b, y, z))
fun double_L (a, x, T(c, _, T(b, _, y1, y2), z)) =
  N(b, N(a, x, y1), N(c, y2, z))
fun double_R (b, T(a, _, x, y1), T(c, _, y2, z)) =
  N(a, x, N(c, N(b, y1, y2), z))

(* Balance maintenance. *)

(* We need a weight to compare each tree to. *)
val weight = 4

(* The first two are T (the constructor), and N, but it's needed above. *)
fun T' (p as (v, l, r)) =
  let
    val ln = size l
    val rn = size r
  in
    if ln + rn < 2
    then (* Either it's got no children, or it only has one descendant.*)
      N p
    else if rn > weight * ln
    then (* Right side is too big. *)
      let
        val T(_, _, rl, rr) = r
        val rln = size rl
        val rrn = size rr
      in
        if rln < rrn (* Double only if the right inner is big. *)
        then single_L p
        else double_L p
      end
    else if ln > weight * rn
    then (* Left side is too big. *)
      let
        val T(_, _, ll, lr) = l
        val lln = size ll
        val lrn = size lr
      in
        if lrn < lln (* Double only if the left inner is big. *)
        then single_R p
        else double_R p
      end
    else N p
  end

(* Insertion *)
fun add (E, x)                     = N(x, E, E)
  | add (tree as T(v, _, l, r), x) =
    if lt(x, v)
    then T'(v, add(l, x), r)
    else if lt(v, x)
    then T'(v, l, add(r, x))
    else N(x, l, r) (* Return a new tree in case new data in `x`. *)

(* Deletion *)
fun delete (E, x)             = E
  | delete (T(v, _, l, r), x) =
    if lt(x, v)
    then T'(v, delete(l, x), r)
    else if lt(v, x)
    then T'(v, l, delete(r, x))
    else delete'(l, r) (* Do some fanciness. *)
and delete' (E, r) = r
  | delete' (l, E) = l
  | delete' (l, r) = T'(min r, l, delmin r)
  (* Optimize since we know r has a minimum. *)
and delmin (T(_, _, E, r)) = r
  | delmin (T(v, _, l, r)) = T'(v, delmin l, r)

(* Processing *)
fun inorder_fold (f, base, tree) =
  let
    fun fold' (base, E) = base
      | fold' (base, T(v, _, l, r)) = fold'(f(v, fold'(base, r)), l)
  in
    fold'(base, tree)
  end

fun treefold (_, base, E) = base
  | treefold (f, base, T(v, _, l, r)) =
    f(v, treefold(f, base, l), treefold(f, base, r))

(* Make a list of all the nodes in the tree. *)
fun members tree = inorder_fold(op ::, [], tree)

(* Sort the tree in O(n*log(n)) time. *)
fun reverse_add (e, t) = add(t, e)
val tree_from_list = foldr reverse_add E
val treesort = members o tree_from_list

(* Set * Set -> Set ops *)
(* Inefficient. *)
fun fold_union t1 t2 = inorder_fold(reverse_add, t1, t2)

(* Our last balance maintainer. *)
fun concat3 (v, E, r) = add(r, v)
  | concat3 (v, l, E) = add(l, v)
  | concat3 (v, l as T(v1, n1, l1, r1), r as T(v2, n2, l2, r2)) =
    if weight * n1 < n2
    then T'(v2, concat3(v, l, l2), r2)
    else if weight * n2 < n1
    then T'(v1, l1, concat3(v, r1, r))
    else N(v, l, r)

fun split_lt (E, _) = E
  | split_lt (T(v, _, l, r), x) =
    if lt(x, v)
    then split_lt(l, x)
    else if lt(v, x)
    then concat3(v, l, split_lt(r, x))
    else l

fun split_gt (E, _) = E
  | split_gt (T(v, _, l, r), x) =
    if lt(v, x)
    then split_gt(r, x)
    else if lt(x, v)
    then concat3(v, split_gt(l, x), r)
    else r

fun union (E, r) = r
  | union (l, E) = l
  | union (t, T(v, _, l, r)) =
    let
      val l' = split_lt(t, v)
      val r' = split_gt(t, v)
    in
      concat3(v, union(l', l), union(r', r))
    end

(*  min and delmin traverse each tree.
    Better to defer this until the last second.
fun concat (t, E) = t
  | concat (t1, t2) = concat3(min t2, t1, delmin t2)
*)

fun concat (E, t) = t
  | concat (t, E) = t
  | concat (t1 as T(v1, n1, l1, r1), t2 as T(v2, n2, l2, r2)) =
    if weight * n1 < n2
    then T'(v2, concat(t1, l2), r2)
    else if weight * n2 < n1
    then T'(v1, l1, concat(r1, t2))
    else T'(min t2, t1, delmin t2)

fun difference (E, _) = E
  | difference (l, E) = l
  | difference (t, T(v, _, l, r)) =
    let
      val l' = split_lt(t, v)
      val r' = split_gt(t, v)
    in
      concat(difference(l', l), difference(r', r))
    end

(* Another slow impl.
fun intersection (a, b) = difference(b, difference(b, a))
*)

fun intersection (E, _) = E
  | intersection (_, E) = E
  | intersection (t, T(v, _, l, r)) =
    let
      val l' = split_lt(t, v)
      val r' = split_gt(t, v)
    in
      if member(v, t)
      then concat3(v, intersection(l', l), intersection(r', r))
      else concat(intersection(l', l), intersection(r', r))
    end

(* Even more inefficient than union. O(n*log(n)) *)
fun union' (E, (t, low, high)) = split_gt(split_lt(t, high), low)
  | union' (T (v, _, l, r), (t, low, high)) =
    concat3(v, union'(l, (t, low, v)), union'(r, (t, v, high)))

fun trim (_, _, E) = E
  | trim (low, high, t as T(v, _, l, r)) =
    if lt(low, v)
    then if lt(v, high)
      then t
      else trim(low, high, l)
    else trim(low, high, r)

fun uni_bd (t, E, _, _) = t
  | uni_bd (E, T(v, _, l, r), low, high) =
    concat3(v, split_gt(l, low), split_lt(r, high))
  | uni_bd (T(v, _, l, r), t, low, high) =
    concat3(v,
            uni_bd(l, trim(low, v, t), low, v),
            uni_bd(r, trim(v, high, t), v, high))

fun trim_low (_, E) = E
  | trim_low (low, t as T(v, _, _, r)) =
    if lt(low, v)
    then t
    else trim_low(low, r)
fun trim_high (_, E) = E
  | trim_high (high, t as T(v, _, l, _)) =
    if lt(v, high)
    then t
    else trim_high(high, l)

fun uni_high (t, E, _) = t
  | uni_high (E, T(v, _, l, r), high) = concat3(v, l, split_lt(r, high))
  | uni_high (T(v, _, l, r), t, high) =
    concat3(v,
      uni_high(l, trim_high(v, t), v),
      uni_bd(r, trim(v, high, t), v, high))

fun uni_low (t, E, _) = t
  | uni_low (E, T(v, _, l, r), low) = concat3(v, split_gt(l, low), r)
  | uni_low (T(v, _, l, r), t, low) =
    concat3(v,
      uni_bd(r, trim(low, v, t), low, v),
      uni_low(l, trim_low(v, t), v))

fun hedge_union (t, E) = t
  | hedge_union (E, t) = t
  | hedge_union (T(v, _, l, r), t) =
    concat3(v, uni_high(l, trim_high(v, t), v), uni_low(r, trim_low(v, t), v))

(* Rank. *)
exception Subscript

fun rank (E, _) = raise Subscript
  | rank (T(v, _, l, r), x) =
    if lt(x, v)
    then rank(l, x)
    else if lt(v, x)
    then rank(r, x) + size l + 1
    else size l

(* Index. *)
fun index (E, _) = raise Subscript
  | index (T(v, _, l, r), i) =
    let
      val nl = size l
    in
      if i < nl
      then index(l, i)
      else if i > nl
      then index(r, i - nl - 1)
      else v
    end
