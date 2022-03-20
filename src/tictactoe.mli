type gameboard = char list list
type game_state = gameboard * bool * int
type dec_tree = Node of game_state * dec_tree list * int

val to_string_row : char list -> string
val to_string_brd : char list list -> string
val print_board : char list list -> unit
val init_board : unit -> char list list
val init_state : unit -> char list list * bool * int
val set : int -> int -> bool -> char list list -> char list list
val get : int -> int -> 'a list list -> 'a
val is_free : int -> int -> char list list -> bool
val is_o : char -> bool
val is_x : char -> bool
val transpose : 'a list list -> 'a list list
val has_won_horizontal : bool -> char list list -> bool
val has_won_vertical : bool -> char list list -> bool
val has_won_diagonal : bool -> char list list -> bool
val has_won : bool -> char list list -> bool

val make_move :
  int -> int -> char list list * bool * int -> char list list * bool * int

val build_tr : char list list * bool * int -> dec_tree list
val init_tree : unit -> dec_tree
val search : dec_tree -> unit
val size : dec_tree list -> int
val find_pred : (int -> int -> bool) -> dec_tree -> int
val find_min : dec_tree -> int
val find_max : dec_tree -> int
val check_tuple : int * int * 'a -> int * int * 'b -> bool
val find_min_max : dec_tree -> (int * int * int) list
val find_best_node : dec_tree -> dec_tree
val find_best_move : dec_tree -> dec_tree * gameboard
val analyze : dec_tree -> dec_tree
val build_tree : unit -> dec_tree
val build_tree_node : dec_tree -> dec_tree
val lst_equals : 'a list -> 'a list -> bool
val board_equals : 'a list list -> 'a list list -> bool
val make_move : int -> int -> dec_tree -> dec_tree
val start_game : unit -> unit
