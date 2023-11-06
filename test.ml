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

(* Question 1.4 *)
let composition (l : bool list) : BigInt.integer =
  match l with     
  | [] -> []
  | _ ->
  (* parcourt la liste des bools avec pos la puissance de 2 à rajouter td 2^pos *)
  let rec aux (l : bool list) (acc : BigInt.integer) (pos : int): BigInt.integer = 
    match l with 
    | [] -> acc
    | h::t -> 
      (* appel de add si on rencontre un "true" *)
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
      if h then aux t (add acc [] pos) (pos+1)
      else 
        (* si on atteint la fin d'un bloc de 64 bits. *)        
        if pos = 63 then aux t (BigInt.inserer_queue 0L acc) 0  else
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
let result = composition 
(* 2²⁰⁰ *)
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
    
    (* Calcul du nombre de feuilles dans l'arbre *)
    let n = composition (liste_feuilles dt) in
    (* Si l'arbre a déjà été visité, on renvoie le noeud correspondant *)
    match List.assoc_opt n ldv with
    | Some d -> (d, ldv)
    (* Sinon, on continue la compression *)
    | None ->
      match dt with
      (* Si le noeud est une feuille, on renvoie le noeud tel quel *)
      | Leaf b -> (dt, (n, dt)::ldv)
      | Node (pos, left, right) ->
        (* On compresse les fils et on donne au fils droit la ldv resultante de l'appel a la compression du fils gauche *)
        let (dl, ll) = aux left ldv in
        let (dr, lr) = aux right ll in
        let new_node =
          match dr with
          (* REGLE-Z : Si le fils droit est une feuille false*)
          | Leaf false -> 
          (match List.assoc_opt (composition (liste_feuilles dl)) lr with
            | Some d -> d
            | None -> Node(pos-1, dl, dl))
          | _ -> Node (pos, dl, dr)
        in
        (new_node, (n, new_node)::lr)
  in
  aux dt ldv
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

let dot (tree : decision_tree) (ldv : listeDejaVus) (filename: string)= 
  let rec notCompressed (tree : decision_tree) (chan : out_channel) (parent_id : string option) (is_right_child : bool)=
    (* Si l'arbre n'a pas été compressé *)
    if ldv = [] then
      match tree with
      | Leaf b ->
        let node_id = string_of_int (Random.int 100000) in
        let shape = "box" in
        let color = if b then "green" else "red" in
        fprintf chan "%s [label=\"%b\", shape=%s, color=%s];\n" node_id b shape color;
        (* Si le noeud a un parent, on écrit le lien entre le parent et le noeud *)
        begin
          match parent_id with
          | Some p_id ->
            let style = if is_right_child then "solid" else "dashed" in
            fprintf chan "%s -> %s [style=%s];\n" p_id node_id style
          | None -> ()
        end
      | Node (pos, left, right) ->
        let node_id = string_of_int (Random.int 100000) in
        fprintf chan "%s [label=\"%d\", shape=ellipse];\n" node_id pos;
        begin
          match parent_id with
          | Some p_id ->
            let style = if is_right_child then "solid" else "dashed" in
            fprintf chan "%s -> %s [style=%s];\n" p_id node_id style
          | None -> ()
        end;
        (* On continue la récursion sur les fils gauche et droit *)
        notCompressed left chan (Some node_id) false;
        notCompressed right chan (Some node_id) true
      in  
  let rec write_dot_tree_edges (tree : decision_tree) (chan : out_channel) (parent_id : string option) (is_right_child : bool) (c : string list ref)=
    (* Calcul du nombre de feuilles dans l'arbre *)
    let n = (composition (liste_feuilles tree)) in
    (* Si l'arbre a été compressé *)
    match tree with 
    | Leaf b ->
      let node_id = (Int64.to_string (BigInt.get_tete n)) in
      let shape = "box" in
      let color = if b then "green" else "red" in
      fprintf chan "%s [label=\"%b\", shape=%s, color=%s];\n" node_id b shape color;
      begin
        match parent_id with
        | Some p_id ->
          let style = if is_right_child then "solid" else "dashed" in
          let line = p_id ^ " -> " ^ node_id ^ " [style=" ^ style ^ "];\n" in
          (if not (List.mem line !c) then begin
            c:= line::!c;
            fprintf chan "%s" line;
          end
            
          )
        | None -> ()
      end;
    | Node (feature, left, right) ->
      let node_id = (Int64.to_string (BigInt.get_tete n))^"."^(string_of_int feature) in
      fprintf chan "%s [label=\"%d\", shape=ellipse];\n" node_id feature;
      begin
        match parent_id with
        | Some p_id ->
          let style = if is_right_child then "solid" else "dashed" in
          let line = p_id ^ " -> " ^ node_id ^ " [style=" ^ style ^ "];\n" in
          (if not (List.mem line !c) then begin
            c:= line::!c;
            fprintf chan "%s" line
          end
          )
        | None -> ()
      end;
      (* On continue la récursion sur les fils gauche et droit *)
      write_dot_tree_edges left chan (Some node_id) false c;
      write_dot_tree_edges right chan (Some node_id) true c

in 

  let chan = open_out filename in
  (* Écriture de l'en-tête du fichier .dot *)
  fprintf chan "digraph Tree {\n";
  match ldv with
  | [] -> notCompressed tree chan None false
  | _ -> write_dot_tree_edges tree chan None false (ref []);
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
let mtable = (decomposition [28L] );;
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

let rec powInt (n : int) (e : int) : int =
  if e = 0 then 1
  else if e mod 2 = 0 then powInt (n * n) (e / 2)
  else n * (powInt n (e - 1))
(* Step 1: Generate a random truth table of size n *)
let gen_table (n : int) : bool list list =
  let rec aux (n : int) (acc : bool list list) : bool list list =
    match n with
    | n when n <= 0 -> acc
    | n -> aux (n-1) ((List.init (powInt 2 n) (fun i -> completion (decomposition [Int64.of_int i]) n))) @ acc
  in
  aux n []
(* ;;

(* Step 2: Construct the decision tree from the truth table *)
let construct_tree (table : bool list) : decision_tree =
  cons_arbre table
;;

(* Step 3: Compress the decision tree and return its size *)
let compress_tree_size (tree : decision_tree) : int =
  let (compressed_tree, _) = compressionParListe tree [] in
  let rec count_nodes (tree : decision_tree) : int =
    match tree with
    | Leaf _ -> 1
    | Node (_, left, right) -> 1 + count_nodes left + count_nodes right
  in
  count_nodes compressed_tree
;;
(* Step 4: Repeat steps 1-3 a large number of times *)
let simulate_compression (n : int) (num_trials : int) : int list =
  let tables = gen_table n in
  let rec aux (tables : bool list list) (num_trials : int) (acc : int list) : int list =
    match num_trials with
    | num_trials when num_trials <= 0 -> acc
    | num_trials ->
      let table = List.hd tables in
      let tree = construct_tree table in
      let size = compress_tree_size tree in
      aux (List.tl tables) (num_trials-1) (size::acc)
  in
  aux tables num_trials []
;;



(* Step 5: Collect the sizes of the compressed trees and calculate their frequency distribution *)
let frequency_distribution (sizes : int list) : (int * float) list =
  let total = List.length sizes in
  let freqs = List.fold_left (fun acc size -> 
    let count = try List.assoc size acc with Not_found -> 0 in
    (size, count + 1)
    :: List.remove_assoc size acc) [] sizes in
  List.map (fun (size, count) -> (size, float_of_int count /. float_of_int total)) freqs
  ;;
(* Step 6: Plot the frequency distribution to visualize the probability distribution of compressed tree sizes *)
(* This step is not implemented in OCaml, as it requires external libraries for plotting. *)

(* Example usage *)
let sizes = simulate_compression 10 10000 in
let freqs = frequency_distribution sizes in
print_string "Compressed tree size\tProbability\n";
List.iter (fun (size, prob) -> Printf.printf "%d\t\t\t%f\n" size prob) freqs;; *)
