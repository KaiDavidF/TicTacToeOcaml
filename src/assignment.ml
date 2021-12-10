

let rec inter2 l1 l2 acc =  
  match l1 with 
  | [] -> acc@l2
  | x::xs -> inter2 l2 xs (acc@[x]);;

let interleave3 l1 l2 l3 = 
  let rec inter3 l1 l2 l3 acc = 
  match l1 with 
  | [] -> inter2 l2 l3 acc
  | x::xs -> inter3 l2 l3 xs (acc@[x]) 
  in inter3 l1 l2 l3 [];;