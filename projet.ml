open Int64

(* Question 1.1 *)
module BigInt =
struct
  type integer = Int64.t list (* liste d'entiers int64 *)
  let get_tete (bigint : integer) : Int64.t = if bigint = [] then 0L else List.hd bigint
  let inserer_queue (nelem : Int64.t) (bigint : integer) : integer = bigint @ [nelem]
  let supprimer_tete (bigint : integer) : integer = List.tl bigint
end

let print_bigint_list (bigint_list : BigInt.integer) : unit =
  let rec aux (bigint_list : BigInt.integer) : unit =
    match bigint_list with
    | [] -> print_string "[]\n"
    | [b] -> print_string (Int64.to_string b ^ "]\n")
    | head :: tail ->
      print_string (Int64.to_string head ^ "; ");
      aux tail
  in
  print_string "[";
  aux bigint_list
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
  let rec aux (x : BigInt.integer) (acc : bool list) : bool list = 
    match x with 
    | [] -> acc
    | h::t ->
      (* si h = 0L alors on ajoute 64 false à acc *)
      if h = 0L then aux t (acc @ (List.init 64 (fun _ -> false))) else 
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
          aux t (acc @ (List.rev (convert_to_binary h [])))
  in aux x [];;


(* print list and print number of false in the list *)
  let print_list (l : bool list) : unit = 
    let rec aux (l : bool list) (acc : int) : int = 
      match l with 
      | [] -> acc
      | h::t -> aux t (acc + (if h then 0 else 1))
    in List.iter (fun b -> print_string (if b then "1" else "0")) l; print_string "\nNumber of 0 : "; print_int (aux l 0);;
  
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
  let rec aux (l : bool list) (n : int) (acc : bool list) : bool list = 
    match l with 
    | [] -> acc@(List.init n (fun _ -> false))
    | h::t -> 
      if n = 0 then acc else
      aux t (n-1) (acc @ [h])
  in aux l n [];;

  let completion (l : bool list) (n : int) : bool list =
    if n <= 0 then [] else
    let rec aux (l : bool list) (n : int) (acc : bool list) : bool list = 
      match l with 
      | [] -> acc@(List.init n (fun _ -> false))
      | h::t -> 
        if n = 0 then acc else
        aux t (n-1) (acc @ [h])
    in aux l n [];;
  

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

(* Question 1.4 *)let composition (l : bool list) : BigInt.integer =
  match l with     
  | [] -> []
  | _ ->
  (* parcourt la liste des bools avec pos la puissance de 2 à rajouter td 2^pos *)
      let rec aux (l : bool list) (acc : BigInt.integer) (pos : int): BigInt.integer = 
        if pos = 64 then aux l (BigInt.inserer_queue 0L acc) 0 else
          match l with 
          | [] -> acc
          | h::t -> 
              let rec add (n : Int64.t) (acc2 : BigInt.integer) : BigInt.integer = 
                match acc2 with 
                | [] -> [n]
                | h::t -> if h = 0L then h::(add n t) else (Int64.add n h)::t
              in
              if h then 
            (* on ajoute 2^pos à acc *)
                aux t (add (pow 2L pos) acc) (pos+1)
              else 
            (* on ajoute 0 à acc *)
                aux t acc (pos+1) 
      in      
      aux l [] 0 
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
let test = decomposition([0L;0L;0L;256L]);;
let result = composition test;;
print_string "Result : ";;
print_bigint_list result;;
print_string "\n";;

(* Question 1.5 *)
let table (x : int) (n : int) : bool list =
  completion (decomposition [Int64.of_int x]) n
;;

(* d'apres les arbres de construction  *)

(* Exemple *)
print_string "\nQuestion 1.5 test\n";;
let result = table 38 10;;
print_list result;;
print_string "\n";;


(* générant un grand entier aléatoire de n bits au maximum *)
let genAlea (n : int) : BigInt.integer =
  (* pour avoir des valeurs differentes a l'appel de Random.int64 *)
  Random.self_init (); 
  (* genere la liste d'int64.t *)
  let rec aux (n : int) (acc : BigInt.integer) : BigInt.integer = 
    match n with
    | n when n <= 0 -> acc 
    | n when n < 64 -> BigInt.inserer_queue (Random.int64 (pow 2L n)) acc 
    | n -> aux (n-64) (BigInt.inserer_queue (Random.int64 Int64.max_int) acc) 
  in
  aux n [] 
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
  let rec aux (tab : bool list) (pos : int): decision_tree =
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
        let g = aux (List.init (List.length tab/2) (fun i -> List.nth tab i)) (pos+1) 
        and d = aux (List.init (List.length tab /2) (fun i -> List.nth tab (i + List.length tab/2))) (pos+1)  in
        Node (pos, g, d)
  in
  aux tab 1 
;;

let print_tree (dt : decision_tree ) : unit =
  let rec aux (dt : decision_tree) : unit =
    match dt with
    | Leaf b -> print_string (string_of_bool b)
    | Node (pos, left, right) ->
      print_string ("(" ^ string_of_int pos ^ ", ");
      aux left;
      print_string ", ";
      aux right;
      print_string ")"
  in
  aux dt
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
  let rec aux (noeud : decision_tree) (acc : bool list) : bool list =
    match noeud with
    | Leaf b -> b::acc
    | Node (pos, left, right) ->
      aux left acc @ aux right acc
  in
  aux noeud []
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
type listeDejaVus = (BigInt.integer * decision_tree ) list


let compressionParListe (dt : decision_tree) (ldv : listeDejaVus) : decision_tree * listeDejaVus =
  let rec aux (dt : decision_tree) (ldv : listeDejaVus): decision_tree * listeDejaVus =
    (* calcul du nombre associé *)
    let n = composition (liste_feuilles dt) in
    match dt with 
    | Leaf b -> 
      (* on regarde si la feuille existe déja *)
      (match List.assoc_opt n ldv with 
        |Some d -> d, ldv
        |None -> dt, (n, dt)::ldv)
    | Node (pos, left, right) -> 
      let leftT, leftLdv = aux left ldv in
      let rightT, rightLdv = aux right leftLdv in
      (* on regarde si le noeud existe déja *)
      (match List.assoc_opt n rightLdv with 
        |Some d -> d,  rightLdv
        | None -> 
          (match rightT with 
            | Leaf false -> 
              Node(pos-1, leftT, leftT), (n, Node(pos-1, leftT, leftT))::rightLdv
            | _ -> 
              Node(pos, leftT, rightT), (n, Node(pos, leftT, rightT))::rightLdv
          )
      )

  in aux dt ldv
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
let (t, ldv) = compressionParListe result [];;
print_string "Result : ";;
print_tree t;;
print_string "\n";;
print_string "Liste de noeuds visités : ";;
(* print_bigint_list (List.map (fun (x, y) -> x) ldv);; *)
print_string "\n";;

open Printf

let rec stringB (n : BigInt.integer) : string =
  match n with
  | [] -> ""
  | h::t -> Int64.to_string h ^ stringB t
;;

let rec notCompressed (tree : decision_tree) (chan : out_channel) (parent_id : string option) (is_right_child : bool) =
  let n = composition (liste_feuilles tree) in
  match tree with
  | Leaf b ->
    let node_id = if stringB n = "" then "0" else stringB n in
    let shape = "box" in
    let color = if b then "green" else "red" in
    fprintf chan "%s [label=\"%b\", shape=%s, color=%s];\n" node_id b shape color;
    begin
      match parent_id with
      | Some p_id ->
        let style = if is_right_child then "solid" else "dashed" in
        let line = p_id ^ " -> " ^ node_id ^ " [style=" ^ style ^ "];\n" in
        fprintf chan "%s" line ;
      | None -> ()
    end
  | Node (pos, left, right) ->
    let node_id = (stringB n)^"."^(string_of_int pos) in
    fprintf chan "%s [label=\"%d\", shape=ellipse];\n" node_id pos;
    begin
      match parent_id with
      | Some p_id ->
        let style = if is_right_child then "solid" else "dashed" in
        let line = p_id ^ " -> " ^ node_id ^ " [style=" ^ style ^ "];\n" in
        fprintf chan "%s" line ;
      | None -> ()
    end;
    notCompressed left chan (Some node_id) false;
    notCompressed right chan (Some node_id) true
;;



let rec compressedTreeDot (tree : decision_tree) (chan : out_channel) (parent_id : string option) (is_right_child : bool) (c : string list) =
  (* Calcul du nombre de feuilles dans l'arbre *)
  let n = composition (liste_feuilles tree) in
  (* Si l'arbre a été compressé *)
  match tree with 
  | Leaf b ->
    let node_id = stringB n in
    let shape = "box" in
    let color = if b then "green" else "red" in
    fprintf chan "%s [label=\"%b\", shape=%s, color=%s];\n" node_id b shape color;
    begin
      match parent_id with
      | Some p_id ->
        let style = if is_right_child then "solid" else "dashed" in
        let line = p_id ^ " -> " ^ node_id ^ " [style=" ^ style ^ "];\n" in
        (* On vérifie que le lien n'a pas déjà été écrit *)
        if (List.mem line c) then () else 
          fprintf chan "%s" line ;
      | None -> ()
    end
  | Node (pos, left, right) -> 
    let node_id = (stringB n)^"."^(string_of_int pos) in
    fprintf chan "%s [label=\"%d\", shape=ellipse];\n" node_id pos;
    
    begin 
      match parent_id with 
      | Some p_id ->
        let style = if is_right_child then "solid" else "dashed" in
        let line = p_id ^ " -> " ^ node_id ^ " [style=" ^ style ^ "];\n" in
        (* On vérifie que le lien n'a pas déjà été écrit *)
        if List.mem node_id c then () else 
          fprintf chan "%s" line ;
          compressedTreeDot left chan (Some node_id) false (node_id::c);
          compressedTreeDot right chan (Some node_id) true (node_id::c)
      | None -> if pos= 1 then 
        compressedTreeDot left chan (Some node_id) false c;
        compressedTreeDot right chan (Some node_id) true c
    end;
    (* On continue la récursion sur les fils gauche et droit *)


;;



let dot (tree : decision_tree) (ldv : listeDejaVus) (filename: string) : unit= 

  let chan = open_out filename in
  (* Écriture de l'en-tête du fichier .dot *)
  fprintf chan "digraph Tree {\n";
  match ldv with
  | [] -> notCompressed tree chan None false
  | _ -> compressedTreeDot tree chan None false [];
  fprintf chan "}\n"; 
  close_out chan
;;

  
  (* Exemple *)
print_string "\nQuestion 3.2 test\n";;
let mtable = completion (decomposition [25899L]) 16;;
print_string "Table : ";;
print_list mtable;;
print_string "\n";;
let result = cons_arbre mtable;;
print_string "Result : ";;
print_tree result;;
print_string "\n";;
dot result [] "./dotFiles/tree.dot" 
(* create jpg *)
let _ = Sys.command "dot -Tjpg ./dotFiles/tree.dot -o ./jpgFiles/tree.jpg"
let (tree, ldv) = compressionParListe result [];;
dot tree ldv "./dotFiles/tree_compressed.dot" 
let _ = Sys.command "dot -Tjpg ./dotFiles/tree_compressed.dot -o ./jpgFiles/tree_compressed.jpg";;


print_string "\nQuestion 3.2 test\n";;
let mtable = completion (decomposition [0L; 1L]) 128 ;;
print_string "Table : ";;
print_list mtable;;
print_string "\n";;
let result = cons_arbre mtable;;
print_string "Result : ";;
print_tree result;;
print_string "\n";;
dot result [] "./dotFiles/tree2.dot" 
(* create jpg *)
let _ = Sys.command "dot -Tjpg ./dotFiles/tree2.dot -o ./jpgFiles/tree2.jpg"
let (tree, ldv) = compressionParListe result [];;
dot tree ldv "./dotFiles/tree_compressed2.dot" 
let _ = Sys.command "dot -Tjpg ./dotFiles/tree_compressed2.dot -o ./jpgFiles/tree_compressed2.jpg";;

let mtable = completion (decomposition [21L] ) 8 ;;
print_string "Table : ";;
print_list mtable;;
print_string "\n";;
let result = cons_arbre mtable;;
print_string "Result : ";;
print_tree result;;
print_string "\n";;
dot result [] "./dotFiles/tree3.dot" 
(* create jpg *)
let _ = Sys.command "dot -Tjpg ./dotFiles/tree3.dot -o ./jpgFiles/tree3.jpg"
let (tree, ldv) = compressionParListe result [];;
dot tree ldv "./dotFiles/tree_compressed3.dot" 
let _ = Sys.command "dot -Tjpg ./dotFiles/tree_compressed3.dot -o ./jpgFiles/tree_compressed3.jpg";;

(* 
let count_nodes (dt : decision_tree) : int =
  let rec aux (dt : decision_tree) (acc : int) : int =
    match dt with
    | Leaf _ -> acc + 1
    | Node (_, left, right) -> aux left (aux right (acc + 1))
  in
  aux dt 0
;;

let compression_ratio (table_size : int) : float =
  let truth_table = genAlea table_size in
  let decision_tree = cons_arbre (table 38 table_size) in
  let (compressed_tree, _) = compressionParListe decision_tree [] in
  let uncompressed_size = count_nodes decision_tree in
  let compressed_size = count_nodes compressed_tree in
  float_of_int compressed_size /. float_of_int uncompressed_size
;;

let n = 100
let truth_tables = List.init n (fun _ -> genAlea (Random.int 100))

let compression_ratios = List.map (fun table -> compression_ratio (List.length table)) truth_tables

module Gp = Gnuplot

let () =
  let gp = Gp.create () in
  Gp.plot_many gp ~range:(Gp.Range.XY (-10., 10., -1.5, 1.5))
   [ Gp.Series.lines_func  "sin(x)" ~title:"Plot a line" ~color:`Blue
   ; Gp.Series.points_func "cos(x)" ~title:"Plot points" ~color:`Green ];
  Gp.close gp *)