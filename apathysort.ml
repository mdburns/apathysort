(* apathysort : int list -> fucks not given (int) -> int list
 * Input: a list of numbers, il, and a number of fucks given, fg
 * Output: il, possibly sorted. it depends on how the computer's feeling *)
let apathysort (il:int list) (fng:int) : int list =
  Random.self_init(); (* seed the PRNG *)
  (* splits a list over the pivot and returns (less_than_pivot_list,greater_list) *)
  let rec split (pivot:int) (il:int list) (acc:int list) : int list * int list =
    match il with
    | [] -> ([],acc)
    | h::t -> if h>pivot
      then split pivot t (h::acc)
      else match split pivot t acc with
        | (l,a) -> (h::l,a) in
  (* this is the actual sorthing method *)
  let rec maybe_sort (il:int list) : int list =
    if (Random.int (4+fng)) < 4  (* determine whether or not we give a fuck any more *)
      then il
      else match il with
        | [] -> []
        | h::t -> (match split h t [] with
          | (l,r) -> (maybe_sort l) @ (h::(maybe_sort r))) in
  maybe_sort il;; (* here we go *)

let randomlist (i:int) : int list =
  Random.self_init();
  let rec makelist (i:int) :int list =
    if i>0
    then (Random.int 1024)::(makelist (i-1))
    else [] in
  makelist i;;
