let alpha_num = 3
let alpha_denom = 4
let alpha = float_of_int alpha_num /. float_of_int alpha_denom

type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree
type 'a sgtree = { tree : 'a tree; size : int; max_size : int }

let rec flatten (t : 'a tree) : 'a list =
  let rec flatten_acc t acc =
    match t with
    | Leaf -> acc
    | Node (l, v, r) -> flatten_acc l (v :: flatten_acc r acc)
  in
  flatten_acc t []

let alpha_height (n : int) : int =
  if n <= 1 then 0
  else int_of_float (floor (log (float_of_int n) /. log (1. /. alpha)))

let rebuild_balanced (t : 'a tree) : 'a tree =
  let rec build_balanced lst len =
    if len <= 0 then Leaf
    else
      let mid = len / 2 in
      let left_len = mid in
      let right_len = len - mid - 1 in
      let rec take n lst =
        if n <= 0 then ([], lst)
        else
          match lst with
          | [] -> ([], [])
          | x :: xs ->
            let (l, r) = take (n - 1) xs in
            (x :: l, r)
      in
      let (left, rest) = take left_len lst in
      let mid_val = List.hd rest in
      let right = List.tl rest in
      Node (build_balanced left left_len, mid_val, build_balanced right right_len)
  in
  let flat = flatten t in
  build_balanced flat (List.length flat)

let empty : 'a sgtree = { tree = Leaf; size = 0; max_size = 0 }

let find (x : 'a) (sgt : 'a sgtree) : bool =
  let rec search t =
    match t with
    | Leaf -> false
    | Node (l, v, r) ->
      if v = x then true
      else if x > v then search r
      else search l
  in
  search sgt.tree

let rec subtree_size (t : 'a tree) : int =
  match t with
  | Leaf -> 0
  | Node (l, _, r) -> 1 + subtree_size l + subtree_size r

let insert (x : 'a) (sgt : 'a sgtree) : 'a sgtree =
  let rec add t =
    match t with
    | Leaf -> (Node (Leaf, x, Leaf), Some 1)
    | Node (l, v, r) ->
      if x = v then failwith "blad"
      else if x < v then
        let (l', size_l) = add l in
        match size_l with
        | None -> (Node (l', v, r), None)
        | Some size_l ->
          let size_r = subtree_size r in
          let total = size_l + size_r + 1 in
          let alpha_limit = alpha *. float_of_int total in
          if float_of_int size_l <= alpha_limit && float_of_int size_r <= alpha_limit
          then (Node (l', v, r), Some total)
          else (rebuild_balanced (Node (l', v, r)), None)
      else
        let (r', size_r) = add r in
        match size_r with
        | None -> (Node (l, v, r'), None)
        | Some size_r ->
          let size_l = subtree_size l in
          let total = size_l + size_r + 1 in
          let alpha_limit = alpha *. float_of_int total in
          if float_of_int size_l <= alpha_limit && float_of_int size_r <= alpha_limit
          then (Node (l, v, r'), Some total)
          else (rebuild_balanced (Node (l, v, r')), None)
  in
  let (new_tree, _) = add sgt.tree in
  let size_tree = subtree_size new_tree in
  {tree = new_tree; size = size_tree; max_size = max size_tree sgt.max_size}

let remove (x : 'a) (sgt : 'a sgtree) : 'a sgtree =
  let rec delete t y =
    match t with
    | Leaf -> failwith "blad"
    | Node (l, v, r) ->
      if v < y then Node (l, v, delete r y)
      else if v > y then Node (delete l y, v, r)
      else
        match (l, r) with
        | Leaf, _ -> r
        | _, Leaf -> l
        | _ ->
          let rec find_min t' =
            match t' with
            | Node (Leaf, v, _) -> v
            | Node (l, _, _) -> find_min l
            | Leaf -> failwith "blad"
          in
          let min_val = find_min r in
          Node (l, min_val, delete r min_val)
  in
  let new_tree = delete sgt.tree x in
  let size' = sgt.size - 1 in
  if float_of_int size' < alpha *. float_of_int sgt.max_size then
    { tree = rebuild_balanced new_tree; size = size'; max_size = size' }
  else
    { tree = new_tree; size = size'; max_size = sgt.max_size }