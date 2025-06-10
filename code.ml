
open Graphics;;

type variable = int;;
type formula = 
  | Top
  | Bot
  | Var of variable
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula
  | Nand of formula * formula
  | Xor of formula * formula
  | Nor of formula * formula
  | Xnor of formula * formula;;
(*(* Taille et positions *)
let gate_width = 50;;
let gate_height = 30;;
let margin = 20;;
let input_spacing = 40;;

(* Dessin d'un composant logique *)
let draw_gate x y label =
  moveto (x - gate_width / 2) (y - gate_height / 2);
  lineto (x + gate_width / 2) (y - gate_height / 2);
  lineto (x + gate_width / 2) (y + gate_height / 2);
  lineto (x - gate_width / 2) (y + gate_height / 2);
  lineto (x - gate_width / 2) (y - gate_height / 2);
  moveto (x - 10) (y - 5);
  draw_string label;;

(* Dessin des connexions entre les composants *)
let draw_connection (x1, y1) (x2, y2) =
  moveto x1 y1;
  lineto x2 y2;;

(* Positionnement r?cursif des composants *)
let rec draw_formula x y spacing formula =
  match formula with
  | Top ->
      draw_gate x y "1"; (x, y)
  | Bot ->
      draw_gate x y "0"; (x, y)
  | Var v ->
      let label = "x" ^ string_of_int v in
      draw_gate x y label; (x, y)
  | Neg f ->
      let child_x, child_y = draw_formula (x - spacing) y spacing f in
      draw_gate x y "NOT";
      draw_connection (child_x, child_y) (x - gate_width / 2, y);
      (x, y)
  | And (f1, f2) ->
      let left_x, left_y = draw_formula (x - spacing) (y + input_spacing) spacing f1 in
      let right_x, right_y = draw_formula (x - spacing) (y - input_spacing) spacing f2 in
      draw_gate x y "AND";
      draw_connection (left_x, left_y) (x - gate_width / 2, y + 10);
      draw_connection (right_x, right_y) (x - gate_width / 2, y - 10);
      (x, y)
  | Or (f1, f2) ->
      let left_x, left_y = draw_formula (x - spacing) (y + input_spacing) spacing f1 in
      let right_x, right_y = draw_formula (x - spacing) (y - input_spacing) spacing f2 in
      draw_gate x y "OR";
      draw_connection (left_x, left_y) (x - gate_width / 2, y + 10);
      draw_connection (right_x, right_y) (x - gate_width / 2, y - 10);
      (x, y)
  | Nand (f1, f2) ->
      let left_x, left_y = draw_formula (x - spacing) (y + input_spacing) spacing f1 in
      let right_x, right_y = draw_formula (x - spacing) (y - input_spacing) spacing f2 in
      draw_gate x y "NAND";
      draw_connection (left_x, left_y) (x - gate_width / 2, y + 10);
      draw_connection (right_x, right_y) (x - gate_width / 2, y - 10);
      (x, y)
  | Nor (f1, f2) ->
      let left_x, left_y = draw_formula (x - spacing) (y + input_spacing) spacing f1 in
      let right_x, right_y = draw_formula (x - spacing) (y - input_spacing) spacing f2 in
      draw_gate x y "NOR";
      draw_connection (left_x, left_y) (x - gate_width / 2, y + 10);
      draw_connection (right_x, right_y) (x - gate_width / 2, y - 10);
      (x, y)
  | Xor (f1, f2) ->
      let left_x, left_y = draw_formula (x - spacing) (y + input_spacing) spacing f1 in
      let right_x, right_y = draw_formula (x - spacing) (y - input_spacing) spacing f2 in
      draw_gate x y "XOR";
      draw_connection (left_x, left_y) (x - gate_width / 2, y + 10);
      draw_connection (right_x, right_y) (x - gate_width / 2, y - 10);
      (x, y)
  | Xnor (f1, f2) ->
      let left_x, left_y = draw_formula (x - spacing) (y + input_spacing) spacing f1 in
      let right_x, right_y = draw_formula (x - spacing) (y - input_spacing) spacing f2 in
      draw_gate x y "XNOR";
      draw_connection (left_x, left_y) (x - gate_width / 2, y + 10);
      draw_connection (right_x, right_y) (x - gate_width / 2, y - 10);
      (x, y)
  | Imply (f1, f2) ->
      let left_x, left_y = draw_formula (x - spacing) (y + input_spacing) spacing f1 in
      let right_x, right_y = draw_formula (x - spacing) (y - input_spacing) spacing f2 in
      draw_gate x y "IMPL";
      draw_connection (left_x, left_y) (x - gate_width / 2, y + 10);
      draw_connection (right_x, right_y) (x - gate_width / 2, y - 10);
      (x, y)
  | Equiv (f1, f2) ->
      let left_x, left_y = draw_formula (x - spacing) (y + input_spacing) spacing f1 in
      let right_x, right_y = draw_formula (x - spacing) (y - input_spacing) spacing f2 in
      draw_gate x y "EQV";
      draw_connection (left_x, left_y) (x - gate_width / 2, y + 10);
      draw_connection (right_x, right_y) (x - gate_width / 2, y - 10);
      (x, y);;

(* Exemple d'utilisation *)*)

(* 1. Calcule la largeur d'une formule *)
let rec formula_width f =
  match f with
  | Var _ | Top | Bot -> 1
  | Neg f1 -> formula_width f1
  | And (f1, f2) | Or (f1, f2) | Imply (f1, f2) | Equiv (f1, f2)
  | Nand (f1, f2) | Xor (f1, f2) | Nor (f1, f2) | Xnor (f1, f2) ->
      formula_width f1 + formula_width f2

(* 2. Rendu du nom de chaque op�rateur *)
let string_of_formula = function
  | Top -> "?"
  | Bot -> "?"
  | Var n -> "x" ^ string_of_int n
  | Neg _ -> "�"
  | And _ -> "AND"
  | Or _ -> "OR"
  | Imply _ -> "?"
  | Equiv _ -> "?"
  | Nand _ -> "NAND"
  | Xor _ -> "XOR"
  | Nor _ -> "NOR"
  | Xnor _ -> "XNOR"

(* 3. Affichage r�cursif avec placement horizontal �quilibr� *)
let rec draw_formula f x y x_scale y_spacing =
  let node_width = 40 and node_height = 20 in
  let box_x = x - node_width / 2 in
  let box_y = y - node_height / 2 in

  draw_rect box_x box_y node_width node_height;
  moveto (x - 10) (y - 5);
  draw_string (string_of_formula f);

  match f with
  | Var _ | Top | Bot -> ()
  | Neg f1 ->
      draw_line x (y - node_height / 2) x (y - y_spacing + node_height / 2);
      draw_formula f1 x (y - y_spacing) x_scale y_spacing
  | And (f1, f2) | Or (f1, f2) | Imply (f1, f2) | Equiv (f1, f2)
  | Nand (f1, f2) | Xor (f1, f2) | Nor (f1, f2) | Xnor (f1, f2) ->
      let w1 = formula_width f1 in
      let w2 = formula_width f2 in
      let total_w = w1 + w2 in
      let left_x = x - (w2 * x_scale / 2) in
      let right_x = x + (w1 * x_scale / 2) in
      draw_line x (y - node_height / 2) left_x (y - y_spacing + node_height / 2);
      draw_line x (y - node_height / 2) right_x (y - y_spacing + node_height / 2);
      draw_formula f1 left_x (y - y_spacing) x_scale y_spacing;
      draw_formula f2 right_x (y - y_spacing) x_scale y_spacing
  
let simplify_nand phi = phi;;
let simplify_usu phi = phi;;
let simplify_nor phi = phi;;

let rec convert_usu phi = match phi with
  |Imply (f1,f2) -> Or (Neg (convert_usu f1),convert_usu f2)
  |Equiv (f1,f2) -> And (convert_usu (Imply (f1,f2)), convert_usu (Imply (f2,f1)))
  |Nor (f1,f2) -> Neg (Or (convert_usu f1, convert_usu f2))
  |Nand (f1,f2) -> Neg (And (convert_usu f1, convert_usu f2))
  |Xor (f1,f2) -> let a = convert_usu f1 in let b = convert_usu f2 in
      And (Or (a,b),Neg (And(a,b)))
  |Xnor (f1,f2) -> Neg ( convert_usu (Xor(f1,f2)))
  |psi->psi;;

let convert_nor phi = 
  let rec aux = function
    |Neg f -> let a = aux f in Nor (a,a)
    |And (f1,f2) -> let a = aux f1 in let b = aux f2 in Nor (Nor (a,a), Nor (b,b))
    |Or (f1,f2) -> let a = aux f1 in let b = aux f2 in Nor (Nor (a,b),Nor (a,b)) 
    |psi -> psi
  in
  let psi = convert_usu phi in
  simplify_nor (aux psi);;

let convert_nand phi =
  let rec aux = function
    |Neg f -> let a = aux f in Nand (a,a)
    |And (f1,f2) -> let a = aux f1 in let b = aux f2 in Nand (Nand(a,b),Nand(a,b))
    |Or (f1,f2) -> let a = aux f1 in let b = aux f2 in Nand (Nand(a,a),Nand(b,b))
    |psi -> psi
  in
  let psi = convert_usu phi in
  simplify_nand (aux psi);;

(* D?finir une fonction semblable ? simplify qui : 
r??crit les -> et <-> uniquement avec des !, /\ et \/
fait descendre les n?gations jusqu'aux feuilles
fait descendre les disjonction sous les conjonctions
*)

let percolate_down phi = 
  let rec aux phi =
    match phi with
    |Imply (f1,f2) -> Or (aux (Neg f1),aux f2)
    |Equiv (f1,f2) -> And (aux (Or (Neg f1, f2)) ,aux (Or ( Neg f2, f1)) )
    |Neg ( Or (f1,f2))-> And (aux (Neg f1) , aux (Neg f2))
    |Neg (And (f1,f2))-> Or (aux (Neg f1), aux (Neg f2))
    |Or ( And (f1,f2), f3) -> And (aux ( Or (f1,f3)),aux ( Or (f2,f3)))
    |Or (f1, And (f2,f3)) -> And (aux (Or(f1,f2)), aux (Or(f1,f3)))
    |Neg (Imply (f1,f2)) -> And (aux f1,aux (Neg f2))
    |Neg (Equiv  (f1,f2))-> Or ( aux (And(f1,Neg f2)) ,aux (And (f2, Neg f1)))
    |And (f1,f2) -> And (aux f1, aux f2)
    |Or (f1,f2) -> Or (aux f1, aux f2)
    |Neg (Neg f1) -> aux f1
    |Neg f1 -> Neg (aux f1)
    |Top -> Top
    |Bot -> Bot
    |Var x-> Var x
    |psi->psi
  in
  let phi1 = ref phi in
  let phi2 = ref (aux phi) in
  while !phi1 <> !phi2 do
    phi1 := !phi2;
    phi2 := aux (!phi2);
  done;
  !phi2
;; 

(*let () =
  open_graph " 800x600";
  set_window_title "Circuit Logique";
  let formula = And (Var 1, Or (Var 2, Neg (Var 3))) in
  ignore (draw_formula 400 300 100 formula);
  let formula1 = convert_nand formula in
  ignore (draw_formula 800 (-200) 100 formula1); 
  let formula2 = convert_nor formula in
  ignore (draw_formula 1200 100 100 formula2);
  ignore(read_key ()); 
  close_graph ()
  ;;*)
(* G?n?ration d'une formule al?atoire *)
let rec random_formula variables depth =
  if depth = 0 then
    Var (Random.int variables + 1)
  else
    let op = Random.int 7 in
    match op with
    | 0 -> And (random_formula variables (depth - 1), random_formula variables (depth - 1))
    | 1 -> Or (random_formula variables (depth - 1), random_formula variables (depth - 1))
    | 2 -> Nand (random_formula variables (depth - 1), random_formula variables (depth - 1))
    | 3 -> Nor (random_formula variables (depth - 1), random_formula variables (depth - 1))
    | 4 -> Xor (random_formula variables (depth - 1), random_formula variables (depth - 1))
    | 5 -> Neg (random_formula variables (depth - 1))
    | _ -> Var (Random.int variables + 1);;

(* ?valuation d'une formule logique *)
let rec eval_formula formula values =
  match formula with
  | Top -> true
  | Bot -> false
  | Var v -> List.assoc v values
  | Neg f -> not (eval_formula f values)
  | And (f1, f2) -> (eval_formula f1 values) && (eval_formula f2 values)
  | Or (f1, f2) -> (eval_formula f1 values) || (eval_formula f2 values)
  | Nand (f1, f2) -> not ((eval_formula f1 values) && (eval_formula f2 values))
  | Nor (f1, f2) -> not ((eval_formula f1 values) || (eval_formula f2 values))
  | Xor (f1, f2) -> (eval_formula f1 values) <> (eval_formula f2 values)
  | Xnor (f1, f2) -> (eval_formula f1 values) = (eval_formula f2 values)
  | Imply (f1, f2) -> not (eval_formula f1 values) || (eval_formula f2 values)
  | Equiv (f1, f2) -> (eval_formula f1 values) = (eval_formula f2 values);;


(* Dessine une formule centr�e horizontalement autour de x *)
let draw_formula_centered f x_center y_start x_scale y_spacing =
  draw_formula f x_center y_start x_scale y_spacing
;;

(* Affiche plusieurs formules c�te � c�te dans la fen�tre *)
let draw_population formulas window_width window_height =
  let n = List.length formulas in
  let spacing = 100 in
  let total_scale = window_width - (n + 1) * spacing in

  (* Largeurs relatives de chaque formule *)
  let widths = List.map formula_width formulas in
  let total_width_units = List.fold_left ( + ) 0 widths in

  (* Calcule position horizontale centr�e pour chaque formule *)
  let x_positions =
    let rec aux acc_x acc = function
      | [] -> List.rev acc
      | w :: rest ->
          let x_center = acc_x + (w * total_scale / total_width_units) / 2 in
          let next_x = acc_x + (w * total_scale / total_width_units) + spacing in
          aux next_x (x_center :: acc) rest
    in
    aux spacing [] widths
  in

  (* Dessine toutes les formules avec leurs positions *)
  List.iter2 (fun f x ->
    draw_formula_centered f x (window_height - 100) 80 80
  ) formulas x_positions


(* G?n?ration d'une population al?atoire de circuits v?rifiant une formule *)
let rec generate_population target_formula variables size depth =
  let rec aux acc count =
    if count = 0 then acc
    else
      let candidate = random_formula variables depth in
      let values = [(1, true); (2, false); (3, true)] in (* Remplacez par votre propre jeu de valeurs *)
      if eval_formula target_formula values = eval_formula candidate values then
        aux (candidate :: acc) (count - 1)
      else
        aux acc count
  in
  aux [] size;;
  
let () =
  Random.self_init ();
  open_graph " 8000x6000";
  set_window_title "Circuit Logique";

  let target_formula = And (Var 1, Or (Var 2, Neg (Var 3))) in
  let population = generate_population target_formula 3 10 3 in
  draw_population population 2000 1000;
  ignore (read_key ());
  close_graph ();;
