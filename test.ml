open Int64

(* Question 1.1 *)
module BigInt =
struct
  type integer = Int64.t list;; (* liste d'entiers int64 *)

  let get_tete (bigint : integer) : Int64.t = List.hd bigint;;

  let inserer_queue (nelem : Int64.t) (bigint : integer) : integer = bigint @ [nelem];;

  let supprimer_tete (bigint : integer) : integer = List.tl bigint;;
end

let print_bigint_list (bigint_list : BigInt.integer) : unit =
  let rec inner (bigint_list : BigInt.integer) : unit =
    match bigint_list with
    | [] -> print_string "[]\n"
    | [b] -> print_string (Int64.to_string b ^ "]\n")
    | head :: tail ->
      print_string (Int64.to_string head ^ "; ");
      inner tail
  in
  print_string "[";
  inner bigint_list
;;

  (* Exemple *)
print_string "Question 1.1 test\n";;
let result = BigInt.inserer_queue 1L [2L; 3L; 4L];;
print_bigint_list result;;
let result = BigInt.supprimer_tete [1L; 2L; 3L; 4L];;
print_bigint_list result;;
print_string "\n";;
let result = BigInt.get_tete [1L; 2L; 3L; 4L];;
print_string (Int64.to_string result);;
print_string "\n";;


(* Question 1.2 *)
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
  
    let rec print_bool_list bool_list =
      match bool_list with
      | [] -> print_string "[]\n"
      | [b] -> print_string (string_of_bool b ^ "]\n")
      | head :: tail ->
        print_string (string_of_bool head ^ "; ");
        print_bool_list tail;;
    
  (* Exemple *)
print_string "Question 1.2 test\n";;
let result = decomposition([0L]);;
print_list result;;
print_string "\n";;
let result = decomposition([38L]);;
print_bool_list result;;
print_list result;;
print_string "\n";;
(* 2¹⁰⁰ *)
let result = decomposition([0L ; 68719476736L  ]);;
print_list result;;
print_string "\n";;

(* Question 1.3 *)

(* liste tronquée, n premiers elems *)
let completion (l : bool list) (n : int) : bool list =
  if n <= 0 then [] else
  let rec inner (l : bool list) (n : int) (acc : bool list) : bool list = 
    match l with 
    | [] -> acc@(List.init n (fun _ -> false))
    | h::t -> 
      if n = 0 then acc else
      inner t (n-1) (acc @ [h])
  in inner l n [];;

  

(* Exemple *)
print_string "\nQuestion 1.3 test\n";;
let result = completion [false; true; true; false; false; true] 4;;
print_list result;;
print_string "\n";;
let result = completion [false; true; true; false; false; true] 8;;
print_list result;;
print_string "\n";;


let rec pow (n : Int64.t) (e : int) : Int64.t =
  if e = 0 then 1L
  else if e mod 2 = 0 then pow (Int64.mul n n) (e / 2)
  else Int64.mul n (pow n (e - 1))
;;

(* Question 1.4 *)
let composition (l : bool list) : BigInt.integer =
  (* parcourt la liste des bools *)
  let rec inner (l : bool list) (acc : BigInt.integer) (pos : int): BigInt.integer = 
    match l with 
    | [] -> acc
    | h::t -> 
      (* permet d'ajouter la puissance de 2 adequate a l'accumulateur *)
      let rec add (n : BigInt.integer) (acc : BigInt.integer) (pos : int) : BigInt.integer = 
        match n with 
        | [] -> BigInt.inserer_queue (pow 2L pos) acc
        | h::t -> 
          (* si on depasse la limite, on rappelle la fonction et on essaye de faire l'ajout sur la suite du nombre*)
          if h = 0L then add t (BigInt.inserer_queue h acc) pos 
          (* sinon on fait la somme de l'entier deja existant et la puissance de 2 *)
          else
            BigInt.inserer_queue (Int64.add h (pow 2L pos)) acc
      in 
      if h then inner t (add acc [] pos) (pos+1)
      
      else 
        (* si le nb delement analysé  *)
        if pos = 63 then inner t (BigInt.inserer_queue 0L acc) 0  else
          inner t acc (pos+1) 
    in 
    inner l [] 0 
    ;;


(* Exemple *)
print_string "\nQuestion 1.4 test\n";;
let result = composition [false; true; true; false; false; true];;
print_string "Result : ";;
print_bigint_list result;;
print_string "\n";;
(* 2¹⁰⁰ *)
let test = decomposition([0L ; 68719476736L]);;
let result = composition test;;
print_string "Result : ";;
print_bigint_list result;;
print_string "\n";;
let result = composition 
(* 2²⁰⁰ *)
let test = decomposition([0L;0L;0L;256L]);;
let result = composition test;;
print_string "Result : ";;
print_bigint_list result;;
print_string "\n";;
