(* Copyright © 2005 Jakub Wilk <jwilk@jwilk.net>
 * SPDX-License-Identifier: MIT
 *)

type 'a tree = Empty | Node of 'a * 'a tree * 'a tree

type 'a heap = { size : int; depth: int; tree : 'a tree }

let heap_empty = { size = 0; depth = 0; tree = Empty }

let heap_isempty hp =
  hp.size = 0

let rec __heaptree_add tr no dp j =
  match tr with
  | Empty -> Node (j, Empty, Empty)
  | Node (i, l, r) ->
      let i, j = (max i j), (min i j) in
      if (no land (1 lsl dp)) > 0 then
        Node (i, l, __heaptree_add r no (dp-1) j)
      else
        Node (i, __heaptree_add l no (dp-1) j, r)

let rec __heaptree_replace tr z =
  match tr with
  | Node (_, Empty, Empty) -> 
      Node(z, Empty, Empty)
  | Node (_, (Node (j, l, r) as n), Empty) ->
      if z >= j then
        Node (z, n, Empty)
      else
        Node (j, __heaptree_replace (Node (z, l, r)) z, Empty)
  | Node (_, (Node (j1, l1, r1) as n1), (Node (j2, l2, r2) as n2)) ->
      if (z >= j1) && (z >= j2) then
        Node (z, n1, n2)
      else if (j1 >= j2) then
        Node (j1, __heaptree_replace (Node (z, l1, r1)) z, n2)
      else
        Node (j2, n1, __heaptree_replace (Node (z, l2, r2)) z)
  | _ -> raise(Failure "__heaptree_replace")

let rec __heaptree_cut tr no dp =
  match tr with
  | Empty -> raise(Failure "__heaptree_cut")
  | Node (i, l, r) ->
      if dp < 0 then
        i, Empty
      else
      if (no land (1 lsl dp)) > 0 then
        let (q, r) = __heaptree_cut r no (dp-1) in
          q, Node (i, l, r)
      else
        let (q, l) = __heaptree_cut l no (dp-1) in
          q, Node (i, l, r)

let heap_add hp j =
  let size = hp.size + 1 in
  let depth = hp.depth + (if size < (1 lsl hp.depth) then 0 else 1) in
  {
    size = size;
    depth = depth;
    tree = __heaptree_add hp.tree size (depth-2) j
  }

let heap_min =
  function
  | { tree = Node (v, _, _) } -> v
  | { tree = Empty } -> raise (Failure "heap_min")

let heap_extractmin hp =
  if hp.size = 0 then
    raise(Failure "heap_extractmin")
  else if hp.size = 1 then
    heap_empty
  else
  let size = hp.size - 1 in
  let depth = hp.depth - (if size < (1 lsl (hp.depth-1)) then 1 else 0) in
  let (q, tree) = __heaptree_cut hp.tree hp.size (hp.depth-2) in
  let tree = __heaptree_replace tree q in
    {
      size = size;
      depth = depth;
      tree = tree;
    }

let heap_from_list lst =
  List.fold_left heap_add heap_empty lst

let heap_to_list hp =
  let rec __hpl lst hp =
    if heap_isempty hp then
      lst
    else
      __hpl ((heap_min hp)::lst) (heap_extractmin hp)
  in
    __hpl [] hp

let heapsort lst =
  heap_to_list (heap_from_list lst)

(* vim:set ts=2 sw=2 et: *)
