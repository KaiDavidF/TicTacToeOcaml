type gameboard = char list list

(*----------PRINT-FUNCTIONS---------- *)

let to_string_row row =
  "|" ^ String.concat "|" (List.map Char.escaped row) ^ "|"

let to_string_brd brd = String.concat "\n" (List.map to_string_row brd)
let print_board brd = print_string (to_string_brd brd)
(*----------------------------------- *)

let init_board () = List.init 3 (fun _ -> List.init 3 (fun _ -> ' '))

let set j i x board =
  let ch = if x then 'X' else 'O' in
  List.mapi
    (fun i' lst ->
      if i' = i then List.mapi (fun j' ch' -> if j' = j then ch else ch') lst
      else lst)
    board

let get j i board = List.(nth (nth board i) j)

let is_free j i board =
  try get i j board = ' ' with _ -> failwith "is_free-Error"

(* GAME LOGIC *)
let is_o ch = ch = 'O'
let is_x ch = ch = 'X'

let rec transpose = function
  | [] -> []
  | [] :: xs -> transpose xs
  | (x :: xs) :: xss -> List.((x :: map hd xss) :: transpose (xs :: map tl xss))

let has_won_horizontal x =
  if x then List.(exists (for_all is_x)) else List.(exists (for_all is_o))

let has_won_vertical x brd = has_won_horizontal x (transpose brd)

let has_won_diagonal x brd =
  let ch = if x then 'X' else 'O' in
  let rec helper i =
    if i > 2 then true else get i i brd = ch && helper (i + 1)
  in
  helper 0
  ||
  let rec helper' i =
    if i > 2 then true else get (2 - i) i brd = ch && helper' (i + 1)
  in
  helper' 0

let has_won x brd =
  has_won_diagonal x brd || has_won_horizontal x brd || has_won_vertical x brd

(*AI - LOGIC - BASICS [decision tree etc.]*)
(* board, current_player, # of entities on the board *)
type game_state = gameboard * bool * int
type dec_tree = Node of game_state * dec_tree list * int
(* the last int of dec_tree is the weight of the node. *)

let make_move i j (board, x, num) =
  let board = set i j x board in
  (board, not x, num + 1)

(* AI - LOGIC - DECISION TREE *)

(* We build up a simple decision tree, which won't have more than 9! nodes. If
   it finds a game state, where player 'X' wins, it will take the corresponding move. *)
let build_tr (board, x, num) =
  let rec build_trr (board, x, num) =
    let rec helper i j tr_acc =
      if i > 2 then tr_acc
      else if j > 2 then helper (i + 1) 0 tr_acc
      else if is_free i j board then
        let board = set j i x board in
        let state = (board, not x, num + 1) in
        let tr = Node (state, build_trr state, 0) in
        helper i (j + 1) (tr :: tr_acc)
      else helper i (j + 1) tr_acc
    in
    helper 0 0 []
  in
  build_trr (board, x, num)

let rec search (Node (state, lst, _)) =
  match state with
  | board, _, num ->
      print_board board;
      print_string "\n\n";
      List.iter search lst

let rec size lst =
  List.length lst
  + List.fold_left (fun acc (Node (st, lstt, _)) -> size lstt + acc) 0 lst

let rec find_pred p (Node (st, lst, weight)) =
  if lst = [] then weight
  else
    List.fold_left
      (fun curr_weight other ->
        let pot_weight = find_pred p other in
        if p pot_weight curr_weight then pot_weight else curr_weight)
      weight lst

let find_min = find_pred (fun a b -> a < b)
let find_max = find_pred (fun a b -> a > b)

(* the best path is the one where we win in the shortest amount of moves + we lose in the max. amount of moves *)

let rec find_best_node lst =
  List.fold_left
    (fun (Node (_, _, weight) as curr) (Node (_, _, weight') as other) ->
      if weight < weight' then curr else other)
    (List.hd lst) (List.tl lst)

let rec analyze (Node ((b, x, n), lst, weight)) =
  Node
    ( (b, x, n),
      List.map analyze lst,
      if has_won true b then 10 - n
      else if has_won false b then -(10 - n)
      else 0 )

let rec find_min_max (Node ((_, _, _), lst, _)) =
  List.map (fun node -> (find_max node, find_min node)) lst

let check_tuple (old_max, old_min) (new_max, new_min) =
  if -old_min < old_max then true
  else if -new_min >= new_max then false
  else if new_max < old_max then true
  else false
