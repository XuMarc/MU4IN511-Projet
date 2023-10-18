open Int64


module BigInt =
struct
  type integer = Int64.t list;;

  let get_tete (bigint : integer) : Int64.t = List.hd bigint;;

  let inserer_queue (nelem : Int64.t) (bigint : integer) : integer = bigint @ [nelem];;

  let supprimer_tete (bigint : integer) : integer = List.tl bigint;;
end

(* 
let decomposition (bigint : BigInt.integer) : bool list = 
  let rec conversion_bits nb = 
    match nb with
    | [] -> [] 
    | x::ls -> if x = 0 
      then let centbitstofalse = List.init 100 (fun _ -> false) in centbitstofalse @ (conversion_bits ls)
      else (x mod 2 = 1) :: conversion_bits () 
    in 
    conversion_bits bigint *)
    open Int64

    let decomposition (bigint : BigInt.integer) : bool list =
      let result_bits = ref [] in
      (* fonction qui permet de définir les bits de chaque entier int64 de mon BigInt *)
      let rec set_bits n offset =
        (*Si on a un 0 ça veut dire qu'on a 100 bits à false*)
        if n = 0L then List.init 100 (fun _ -> false)
        (*Sinon on fait la représentation binaire du nombre*)
        else begin
          let int64_to_binary_list (n : Int64) : bool list =
            let rec convert_to_binary n bits =
              (*on fait le "et" logique pour déterminer si c'est un 0 ou 1*)
              let bit = if logand n 1L = 1L then true else false in
                (*On regarde le bit suivant*)
                convert_to_binary (shift_right_logical n 1) (bit :: bits)
            in
            convert_to_binary n []
          ;;

        
        
      in

      let rec aux offset = function
        | [] -> ()
        | h :: t ->
            let current_bits = 64 in
            set_bits h offset;
            aux (offset + current_bits) t
      in
      aux 0 lst;
      !result_bits
    ;;
    
    (* Exemple d'utilisation *)
    let int_list = [5L; 10L; 15L];;
    let result_list = decomposition_list int_list;;
    List.iter (fun b -> print_string (if b then "1" else "0")) result_list;;
    
    open Int64

    (* Fonction pour convertir une liste d'entiers en une liste de bits *)
    let decomposition_list (bigint : BigInt.integer) : bool list =
      (* Initialiser une liste de bits avec la longueur appropriée *)
      let bit_list = List.init (List.length bigint * 64) (fun _ -> false) in
    
      (* Fonction récursive pour définir les bits correspondants dans la liste *)
      let rec set_bits bits n offset =
        if n = 0L then bits
        else
          let index = Int64.to_int (logand n 63L) + offset in
          let new_bits = List.mapi (fun i b -> if i = index then true else b) bits in
          set_bits new_bits (shift_right_logical n 1) offset
      in


(*tout en bas c'est du n'importe quoi nrmlt*)

(*     
      (* Fonction auxiliaire pour parcourir la liste d'entiers *)
      let rec aux bits offset = function
        | [] -> bits
        | h :: t -> aux (set_bits bits h offset) (offset + 64) t
      in
    
      (* Appel à la fonction auxiliaire avec des paramètres initiaux *)
      aux bit_list 0 bigint
    ;;
    
    (* Exemple d'utilisation *)
    let int_list = [5L; 10L; 15L];;
    let result_list = decomposition_list int_list;;
    
    (* Affichage du résultat *)
    List.iter (fun b -> print_string (if b then "1" else "0")) result_list;;
    

(* Exemple d'utilisation *)
let int_list = [5L; 10L; 15L];;
let result_list = decomposition_list int_list;;
List.iter (fun b -> print_string (if b then "1" else "0")) result_list;; *)
