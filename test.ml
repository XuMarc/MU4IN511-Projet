open Int64


module BigInt =
struct
  type integer = Int64.t list;; (* liste d'entiers int64 *)

  let get_tete (bigint : integer) : Int64.t = List.hd bigint;;

  let inserer_queue (nelem : Int64.t) (bigint : integer) : integer = bigint @ [nelem];;

  let supprimer_tete (bigint : integer) : integer = List.tl bigint;;
end

let decomposition (x : BigInt.integer) : bool list = 
  let rec inner (x : BigInt.integer) (acc : bool list) : bool list = 
    match x with 
    | [] -> acc
    | h::t ->
      (* si h = 0L alors on ajoute 64 false à acc *)
      if h = 0L then inner t (acc @ (List.init 64 (fun _ -> false))) else 
      (* sinon on ajoute la représentation binaire tranformée de h à acc *)
      let rec convert_to_binary (n : Int64.t) (bits : bool list) : bool list = 
        if n = 0L then bits else 
        let bit = 
          (*on fait le "et" logique pour determiner si le bit de poids faible est 1 ou 0*)
          if logand n 1L = 1L 
            then true 
          else false in
          (* division par 2 <=> decalage vers la droite *)
        convert_to_binary (shift_right_logical n 1) (bit :: bits)
    in 
      inner t (acc @ (List.rev (convert_to_binary h [])))
  in inner x [];;


(* print list and print number of false in the list *)
  let print_list (l : bool list) : unit = 
    let rec inner (l : bool list) (acc : int) : int = 
      match l with 
      | [] -> acc
      | h::t -> inner t (acc + (if h then 0 else 1))
    in List.iter (fun b -> print_string (if b then "1" else "0")) l; print_string "\nNumber of 0 : "; print_int (inner l 0);;
  

  (* Exemple *)
  let result = decomposition([0L]);;
  print_list result;;
  print_string "\n";;
  let result = decomposition([38L]);;
  print_list result;;
  print_string "\n";;
  (* 2¹⁰⁰ *)
  let result = decomposition([0L ; 68719476736L  ]);;
  print_list result;;
  print_string "\n";;
