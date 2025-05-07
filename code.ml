
open Graphics;;

(* Définission des types *)
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

(* Fonctions d'affichage *)
(* Calcule la largeur d'une formule *)
let rec formula_width f =
  match f with
  | Var _ | Top | Bot -> 1
  | Neg f1 -> formula_width f1
  | And (f1, f2) | Or (f1, f2) | Imply (f1, f2) | Equiv (f1, f2)
  | Nand (f1, f2) | Xor (f1, f2) | Nor (f1, f2) | Xnor (f1, f2) ->
      formula_width f1 + formula_width f2
;;

let rec formula_depth f =
  match f with
  | Top | Bot | Var _ -> 1
  | Neg f1 -> 1 + formula_depth f1
  | And (f1, f2) | Or (f1, f2) | Imply (f1, f2) | Equiv (f1, f2)
  | Nand (f1, f2) | Xor (f1, f2) | Nor (f1, f2) | Xnor (f1, f2) ->
      1 + max (formula_depth f1) (formula_depth f2)

(* Rendu du nom de chaque opérateur *)
let string_of_formula = function
  | Top -> "⊤"
  | Bot -> "⊥"
  | Var n -> "x" ^ string_of_int n
  | Neg _ -> "¬"
  | And _ -> "AND"
  | Or _ -> "OR"
  | Imply _ -> "→"
  | Equiv _ -> "≡"
  | Nand _ -> "NAND"
  | Xor _ -> "XOR"
  | Nor _ -> "NOR"
  | Xnor _ -> "XNOR"
;;

(* Fonctions de tracé *)
let draw_gate x y label =
  let w, h = 40, 20 in
  draw_rect (x - w / 2) (y - h / 2) w h;
  moveto (x - 10) (y - 5);
  draw_string label
;;

let draw_connection x1 y1 x2 y2 =
  moveto x1 y1;
  lineto x2 y2
;;

(* 3. Affichage récursif avec placement horizontal équilibré *)
let rec draw_formula f x y x_scale y_spacing =
  let node_width = 40 and node_height = 20 in
  let box_x = x - node_width / 2 in
  let box_y = y - node_height / 2 in

  draw_gate x y (string_of_formula f);

  match f with
  | Var _ | Top | Bot -> ()
  | Neg f1 ->
      draw_connection x (y - node_height / 2) x (y - y_spacing + node_height / 2);
      draw_formula f1 x (y - y_spacing) x_scale y_spacing
  | And (f1, f2) | Or (f1, f2) | Imply (f1, f2) | Equiv (f1, f2)
  | Nand (f1, f2) | Xor (f1, f2) | Nor (f1, f2) | Xnor (f1, f2) ->
      let w1 = formula_width f1 in
      let w2 = formula_width f2 in
      let total_w = w1 + w2 in
      let left_x = x - (w2 * x_scale / 2) in
      let right_x = x + (w1 * x_scale / 2) in
      draw_connection x (y - node_height / 2) left_x (y - y_spacing + node_height / 2);
      draw_connection x (y - node_height / 2) right_x (y - y_spacing + node_height / 2);
      draw_formula f1 left_x (y - y_spacing) x_scale y_spacing;
      draw_formula f2 right_x (y - y_spacing) x_scale y_spacing
;;

(* Dessine une formule centrée horizontalement autour de x *)
let draw_formula_centered f x_center y_start x_scale y_spacing =
  draw_formula f x_center y_start x_scale y_spacing
;;

(* Affiche plusieurs formules côte à côte dans la fenêtre *)
let draw_population formulas window_width window_height =
  let n = List.length formulas in
  let spacing = 100 in
  let total_scale = window_width - (n + 1) * spacing in

  (* Largeurs relatives de chaque formule *)
  let widths = List.map formula_width formulas in
  let total_width_units = List.fold_left ( + ) 0 widths in

  (* Calcule position horizontale centrée pour chaque formule *)
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
;;

let draw_population_grid_scaled_per_formula formulas win_w win_h =
  let n = List.length formulas in
  let cols = int_of_float (ceil (sqrt (float_of_int n))) in
  let rows = (n + cols - 1) / cols in

  let margin_top = 40 in
  let margin_bottom = 40 in
  let vertical_space = win_h - margin_top - margin_bottom in

  let cell_w = win_w / cols in
  let cell_h = vertical_space / rows in

  let margin_x = 20 in
  let margin_y = 20 in

  List.iteri (fun idx formula ->
    let col = idx mod cols in
    let row = idx / cols in

    (* Coordonnées de la cellule *)
    let cell_x = col * cell_w in
    let cell_y = win_h - margin_top - (row + 1) * cell_h in
    let center_x = cell_x + cell_w / 2 in
    let center_y = cell_y + cell_h / 2 in

    (* Encadrer la cellule *)
    set_color (rgb 180 180 180);
    draw_rect cell_x cell_y cell_w cell_h;

    (* Taille de la formule *)
    let fw = formula_width formula in
    let fd = formula_depth formula in

    let scale_x = max 10 ((cell_w - 2 * margin_x) / fw) in
    let scale_y = max 10 ((cell_h - 2 * margin_y) / fd) in

    set_color black;
    let height_px = fd * scale_y in
    let formula_top_y = center_y + height_px / 2 in
    draw_formula formula center_x formula_top_y scale_x scale_y
  ) formulas
;;
      
(* Fonctions de conversions de formules logiques, notemment pour les mutations *)

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
  |Neg f -> Neg (convert_usu f)
  |And (f1,f2) -> And (convert_usu f1, convert_usu f2)
  |Or (f1,f2) -> Or(convert_usu f1,convert_usu f2)
  |f->f

;;

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

(* Mise en CNF pour utiliser un Sat solver afin d'évaluer l'équivalence de formules logiques *)


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
    |Neg Top -> Bot
    |Neg Bot -> Top
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
exception ClauseTop;;
exception CNFBot;;
exception NotCNF of formula;;

(*conversion d'une cnf dans le format adapté*)
let convert_cnf phi =
  let rec convert_cnf_aux phi = match phi with
  |Bot->[[]]
  |Top->[]
  |Var x -> [[x]]
  |Neg(Var x)->[[-x]]
  |And(f1,f2)->(convert_cnf_aux f1)@(convert_cnf_aux f2)
  |Or (f1,f2) -> begin let rec aux f = match f with
                  |Bot->[]
                  |Top->raise ClauseTop
                  |Var x -> [x]
                  |Neg(Var x)->[-x]
                  |Or (f3,f4) -> (aux f3)@(aux f4)
                  |_->failwith"Not CNF"
                in try [(aux f1)@(aux f2)]
              with |ClauseTop -> []
            end
  |_ -> failwith"Not CNF"
  in List.map (fun x -> List.sort (fun a b -> compare (abs a) (abs b)) x) (convert_cnf_aux phi)
;;

let rec simplify_clause c = match c with
|[]->[]
|t::q -> begin match q with
  |[]->[t]
  |t1::q1 when t1 = t -> simplify_clause (t::q1)
  |t1::q1 when t1= -t -> raise ClauseTop
  |t1::q1 -> t::(simplify_clause q)
end
;;
let simplify_cnf cnf = 
  let ncnf = List.sort compare (List.map (fun x -> try simplify_clause x with |ClauseTop -> [0]) cnf) in
  let rec aux cnf_a =
    match cnf_a with
    |[]->[]
    |[]::q->raise CNFBot
    |[0]::q->aux q
    |t::q -> begin 
      match q with
      |[] -> [t]
      |t1::q1 when t = t1 -> aux (t::q1)
      |t1::q1 -> t::(aux (t1::q1))
    end
  in aux ncnf
;;
  
(*appel du SatSolver Cryptominisat*)
let sat_solve f n=
  try
    let cnf = simplify_cnf (convert_cnf (percolate_down (convert_usu f))) in
    let fic = open_out "sat_f.cnf" in
    let l = List.length cnf in
    output_string fic "p cnf ";
    output_string fic (string_of_int n);
    output_string fic " ";
    output_string fic (string_of_int l);
    output_string fic "\n";
    List.iter (fun x->List.iter (fun y -> output_string fic (string_of_int y); output_string fic " ") x; output_string fic "0\n") cnf;
    flush fic;
    let output = Unix.open_process_in "cryptominisat --verb 0 sat_f.cnf" in
    let s = In_channel.input_all output in
    Unix.close_process_in;
    print_string s;
    Sys.remove "sat_f.cnf";
    Scanf.sscanf s "s %s\n" (fun s -> if (String.equal s "SATISFIABLE") then true else false)
  with |CNFBot->false
;;


(* Génération d'une formule aléatoire *)
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

(* évaluation d'une formule logique *)
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


(* Génération d'une population aléatoire de circuits vérifiant une heuristique *)

let rec generate_population target_formula variables size depth =
  let rec aux acc count =
    if count = 0 then acc
    else
      let candidate = random_formula variables depth in
      let values = [(1, true); (2, false); (3, true)] in 
      if eval_formula target_formula values = eval_formula candidate values then
        aux (candidate :: acc) (count - 1)
      else
        aux acc count
  in
  aux [] size
;;

let heuri k phi =
  true
;;

let mutations phi = phi;;

(*Algorithme génétique *)

let algo_gen phi n nvar depth nbgen= 
  (*Initialisation de la pop*)
  let pop = ref (generate_population phi nvar n depth) in
  for i=1 to nbgen do
    let pop_mut = List.map mutations !pop in
    pop := List.filter (heuri i) pop_mut;
  done;
  !pop;;

;;

(*let () =
  Random.self_init ();
  open_graph " 8000x6000";
  set_window_title "Circuit Logique";

  let target_formula = random_formula 3 5 in
  let population = generate_population target_formula 3 40 3 in
  draw_population_grid_auto_scale (target_formula::population) 2000 1000;
  ignore (read_key ());
  close_graph ();;*)

let () =
  Random.self_init ();
  open_graph " 8000x6000";
  set_window_title "Circuit Logique";
  let target_formula = random_formula 3 5 in
  Printf.fprintf stdout "\n %b\n" (sat_solve target_formula 3);
  draw_population_grid_scaled_per_formula [target_formula] 1500 900;
  ignore (read_key ());
  close_graph ();;

