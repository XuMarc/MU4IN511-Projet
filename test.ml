open Int64

(* Question 1.1 *)
module BigInt =
struct
  type integer = Int64.t list (* liste d'entiers int64 *)

  let get_tete (bigint : integer) : Int64.t = List.hd bigint

  let inserer_queue (nelem : Int64.t) (bigint : integer) : integer = bigint @ [nelem]

  let supprimer_tete (bigint : integer) : integer = List.tl bigint
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
          if logand n 1L = 1L then 
            true 
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

(* Question 1.5 *)
(* Je suis pas sur si c'est ce qu'on veut ? surtout la signature mhmm *)
let table (x : int) (n : int) : bool list =
  completion (decomposition [Int64.of_int x]) n
;;

(* avec des Int64.t *)
let rec table2 (x : BigInt.integer) (n : BigInt.integer) : bool list =
  let a = decomposition x in
  (* completion a (Int64.to_int n) *)
  match n with 
  | [] -> []
  | h::t -> completion a (Int64.to_int h) @ table2 x t
;;

(* d'apres les arbres de construction  *)

(* Exemple *)
print_string "\nQuestion 1.5 test\n";;
let result = table 38 10;;
print_list result;;
print_string "\n";;
print_string "\navec les int64\n";;
let result = table2 [38L] [10L];;
print_list result;;
print_string "\n";;

(* Question 1.6 n est un entier naturel parce que je vois pas comment divviser une liste de int64 avec 64*)
let genAlea ( n : int) : BigInt.integer=
  (* pour avoir des valeurs differentes a l'appel de Random.int64 *)
  Random.self_init ();
  (* genere la liste d'int64.t *)
  let rec inner (n : int) (acc : BigInt.integer) : BigInt.integer = 
    if n>63 then inner (n-64) (BigInt.inserer_queue (Random.int64 Int64.max_int) acc) else
      if n<64 then BigInt.inserer_queue (Random.int64 (pow 2L n)) acc else
        acc
  in inner n []
;;

let rec genAleaBigInt ( n : BigInt.integer ) : BigInt.integer =
  match n with 
  [] -> []
  | h::t -> 
    genAlea (Int64.to_int h) @ genAleaBigInt t
;;


(* Exemple *)
print_string "\nQuestion 1.6 test\n";;
print_string "\nGenAlea sur 10\n";;
let result = genAlea 10;;
print_bigint_list result;;
print_string "\n";;
print_string "\nGenAlea sur 100\n";;
let result = genAlea 100;;
print_bigint_list result;;
print_string "\n";;



(* Arbre de decision *)

(* Question 2.7 *)
type decision_tree =
  | Leaf of bool
  | Node of int * decision_tree * decision_tree

(* Exercice 2.8 *)
let cons_arbre (tab : bool list) : decision_tree =
  (* tab : liste, pos : profondeur, taille :  *)
  let rec inner (tab : bool list) (pos : int): decision_tree =
    match tab with
    | [] -> Leaf false
    | [b] -> Leaf b
    | h::s::t ->
      (* on a un noeud avec 2 enfants *)
      if List.length tab = 2 then 
        let g = Leaf(h) 
        and d = Leaf(s) in
        Node (pos, g, d)
      else
        (* g contient la premiere moitié de la liste et d lautre *)
        let g = inner (List.init (List.length tab/2) (fun i -> List.nth tab i)) (pos+1) 
        and d = inner (List.init (List.length tab /2) (fun i -> List.nth tab (i + List.length tab/2))) (pos+1)  in
        Node (pos, g, d)
  in
  inner tab 1 
;;

let print_tree (dt : decision_tree ) : unit =
  let rec inner (dt : decision_tree) : unit =
    match dt with
    | Leaf b -> print_string (string_of_bool b)
    | Node (pos, left, right) ->
      print_string ("(" ^ string_of_int pos ^ ", ");
      inner left;
      print_string ", ";
      inner right;
      print_string ")"
  in
  inner dt
;;



(* Exemple *)
print_string "\nQuestion 2.8 test\n";;
let mtable = completion (decomposition [25899L]) 16;;
print_string "Table : ";;
print_list mtable;;
print_string "\n";;
let result = cons_arbre mtable;;
print_string "Result : ";;
print_tree result;;
print_string "\n";;
let mtable = completion (decomposition [38L]) 8;;
print_string "Table : ";;
print_list mtable;;
print_string "\n";;
let result = cons_arbre mtable;;
print_string "Result : ";;
print_tree result;;
print_string "\n";;

(* Question 2.9 *)

(* liste les feuilles d'un sous noeud de l'arbre *)
let liste_feuilles (noeud : decision_tree) : bool list =
  let rec inner (noeud : decision_tree) (acc : bool list) : bool list =
    match noeud with
    | Leaf b -> b::acc
    | Node (pos, left, right) ->
      inner left acc @ inner right acc
  in
  inner noeud []
;;

(* Exemple *)
print_string "\nQuestion 2.9 test\n";;
let mtable = completion (decomposition [25899L]) 16;;
print_string "Table : ";;
print_list mtable;;
print_string "\n";;
let result = cons_arbre mtable;;
print_string "Result : ";;
print_tree result;;
print_string "\n";;
let result = liste_feuilles (Node (3, Node (4, Leaf true, Leaf true), Node (4, Leaf false, Leaf true)));;
print_string "Result : ";;
print_list result;;
print_string "\n";;

(* Exercice 3 : Compression de l'arbre de décision et ZDD *)

type listeDejaVus = (BigInt.integer * decision_tree ref) list

(* Question 3.1 *)
(* 
let compressionParListe ( gt : decision_tree) (ldv : listeDejaVus) : decision_tree * listeDejaVus =
  match gt with 
  | Leaf b -> (Leaf b, ldv)
  | x-> let l = liste_feuilles x in 
  (* deuxieme moitié de l *)
  let lg = List.init (List.length l /2) (fun i -> List.nth l (i + List.length l/2)) in
  (* premiere moitié de l *)
  let ld = List.init (List.length l /2) (fun i -> List.nth l i) in
  (* on rajoute le noeud dans les neouds visité *)
  let ldv = (lg, x)::ldv in
  (* si la deuxieme moitié ne contient que des false on remplace le pointeur vers N vers un pointeur vers l'enfant gauche de N*)
  if List.for_all (fun x -> x=false) ld then 

;; *)
(*
let rec compressionParListe (arbre : decision_tree) (listeDejaVus : listeDejaVus) : decision_tree =
  match arbre with
  | Leaf b -> Leaf b
  | Node (pos, left, right) ->
    let liste_feuilles_N = liste_feuilles arbre in
    let moitie_gauche, moitie_droite = List.split_at (List.length liste_feuilles_N / 2) liste_feuilles_N in
    if List.for_all (fun b -> not b) moitie_droite then
      compressionParListe left listeDejaVus
    else
      let n = composition moitie_feuilles_N in
      match List.assoc_opt n listeDejaVus with
      | Some compressed_tree_ref -> compressionParListe !compressed_tree_ref listeDejaVus
      | None ->
        let new_compressed_tree_ref = ref arbre in
        let new_listeDejaVus = (n, new_compressed_tree_ref) :: listeDejaVus in
        compressionParListe (Node (pos, compressionParListe left new_listeDejaVus, compressionParListe right new_listeDejaVus)) new_listeDejaVus
;;




(* Exemple *)
print_string "\nQuestion 3.1 test\n";;
let mtable = completion (decomposition [25899L]) 16;;
print_string "Table : ";;
print_list mtable;;
print_string "\n";;
let result = cons_arbre mtable;;
print_string "Result : ";;
print_tree result;;
print_string "\n";;
let (result, ldv) = compressionParListe result [];;
print_string "Result : ";;
print_tree result;;
print_string "\n";;
print_string "Liste de noeuds visités : ";;
(* print_bigint_list (List.map (fun (x, y) -> x) ldv);; *)
print_string "\n";;

*)

(* Question 3.2 *)



(* Question 3.12 *)


open Printf

(*

let rec write_dot_tree_edges tree chan parent_id =
  match tree with
  | Leaf b ->
    let node_id = string_of_int (Random.int 100000) in
    fprintf chan "%s [label=\"%b\", shape=box];\n" node_id b;
    begin
      match parent_id with
      | Some p_id -> fprintf chan "%s -> %s;\n" p_id node_id
      | None -> ()
    end
  | Node (feature, left, right) ->
    let node_id = string_of_int (Random.int 100000) in
    fprintf chan "%s [label=\"%d\"];\n" node_id feature;
    begin
      match parent_id with
      | Some p_id -> fprintf chan "%s -> %s;\n" p_id node_id
      | None -> ()
    end;
    write_dot_tree_edges left chan (Some node_id);
    write_dot_tree_edges right chan (Some node_id)
*)

let rec write_dot_tree_edges (tree : decision_tree) (chan : out_channel) (parent_id : string option) (is_right_child : bool) =
  match tree with
  | Leaf b ->
    let node_id = string_of_int (Random.int 100000) in
    let shape = "box" in
    let color = if b then "green" else "red" in
    fprintf chan "%s [label=\"%b\", shape=%s, color=%s];\n" node_id b shape color;
    begin
      match parent_id with
      | Some p_id ->
        let style = if is_right_child then "solid" else "dashed" in
        fprintf chan "%s -> %s [style=%s];\n" p_id node_id style
      | None -> ()
    end
  | Node (feature, left, right) ->
    let node_id = string_of_int (Random.int 100000) in
    fprintf chan "%s [label=\"%d\", shape=ellipse];\n" node_id feature;
    begin
      match parent_id with
      | Some p_id ->
        let style = if is_right_child then "solid" else "dashed" in
        fprintf chan "%s -> %s [style=%s];\n" p_id node_id style
      | None -> ()
    end;
    write_dot_tree_edges left chan (Some node_id) false;
    write_dot_tree_edges right chan (Some node_id) true

let create_dot_tree (tree : decision_tree) (filename : string) =
  let chan = open_out filename in
  fprintf chan "digraph Tree {\n";
  write_dot_tree_edges tree chan None false;
  fprintf chan "}\n";
  close_out chan
;;



let mtable = completion (decomposition [25899L]) 16;;
print_string "Table : ";;
print_list mtable;;
print_string "\n";;
let result = cons_arbre mtable
in create_dot_tree result "./tree.dot"

let rec traverse_postfix tree =
  match tree with
  | Leaf b -> [b]
  | Node (_, left, right) ->
      let left_result = traverse_postfix left in
      let right_result = traverse_postfix right in
      left_result @ right_result

let example_tree =
  Node (1, Leaf true, Node (2, Leaf false, Leaf true))

let result = traverse_postfix example_tree

type listeDejaVus = (BigInt.integer * decision_tree ref) list

type decision_tree =
  | Leaf of bool
  | Node of int * decision_tree * decision_tree

type arbreDejaVus = 
  | Leaf of bool 
  | Node of 