**🔬 Optimiseur de circuits logiques**

Un projet OCaml qui utilise un **algorithme génétique** pour optimiser des **formules logiques booléennes**, en les représentant sous forme de **circuits logiques visuels**.  
Les circuits sont affichés grâce au module `Graphics`.

---

**📐 Objectif**

Minimiser une formule logique en réduisant le nombre de portes logiques tout en conservant **la même table de vérité**.  
Les formules sont exprimées dans un type de données déclaratif, avec des connecteurs logiques classiques et avancés (`AND`, `OR`, `XOR`, `NAND`, etc.).

---

**💡 Représentation des formules**

ocaml

`type variable = int`

`type formula =
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
  | Xnor of formula * formula`


---

**🔁 Algorithme génétique**

Le système repose sur une boucle évolutive :

Population initiale : Formules générées aléatoirement respectant au minimum l heuristique

Mutation : Changement local (porte, branche, variable…).

Nouvelle génération : Sélection des individus conservés en fonction d une heuristique plus fine.

**🖼 Affichage graphique**

Utilise le module OCaml Graphics pour dessiner les circuits :

Les formules sont affichées sous forme de boîtes reliées par des lignes.

Un placement en grille ajuste automatiquement la taille des formules pour éviter les chevauchements.

**🛠 Dépendances**

OCaml (>= 4.14 recommandé)
Module Graphics et Unix
Compatible avec Linux/WSL + X Server (ex. : XLaunch)

**🚀 Exécution**

bash
Copy
Edit
ocamlc -package graphics -package unix -linkpkg your_project.ml -o circuit_opt
./circuit_opt
Si vous êtes sous WSL, pensez à lancer un serveur X (ex. : VcXsrv) avec Disable access control activé.

