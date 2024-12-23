# Projet PF5

## Installation d'`opam`

Pour commencer, installez le gestionnaire de paquets [`opam`](https://opam.ocaml.org/) en suivant les instructions données [ici](https://opam.ocaml.org/doc/Install.html).

## Installation des paquets

Placez-vous dans le répertoire cloné.
De là, exécutez les commandes suivantes, qui créent un switch `opam` local en y installant les paquets nécessaires :

```
opam update
opam switch create . 4.14.1 -y --deps-only
```

## Compilation

Pour compiler le projet, exécutez la commande `make`.

## Toplevel

Afin de vous-même tester et déboguer, vous pouvez utiliser le toplevel `utop` qui a été installé.
Pour le lancer, exécutez la commande `make top`.

## Tests

Pour lancer tous les tests disponibles, exécutez `make test`.
Pour tester seulement les fonctions de l'exercice *i*, exécutez `make test-i`.

## Lancer l'interpreteur

Le fichier qui contient la fonction "main" du project est `bin/interp.ml` .
La commande pour compiler le projet est `dune build`,
et celle pour compiler et lancer le main est `dune exec interp` suivie des options et des arguments éventuels.
Exemple : `dune exec interp -- -abs 10 10 20 20 -cr -bc 255 255 255 -fc 0 0 0 -rc 255 0 0 -pc 0 0 255 -size 1080 720 -print 1`

## Options :

-abs X_MIN Y_MIN X_MAX Y_MAX :
    Définit la zone d'affichage des rectangles et l'approximation initiale qui doit contenir le point (0, 0).

-cr :
    Affiche des points dans la simulation.

-bc r v b :
    Définit la couleur de l'arrière-plan de la fenêtre. Les valeurs r, v, et b représentent les composantes rouge, verte et bleue (de 0 à 255).

-fc r v b :
    Définit la couleur de l'avant-plan (l'élément principal du programme, comme les rectangles ou les points). Les valeurs r, v, et b représentent les composantes rouge, verte et bleue (de 0 à 255).

-rc r v b :
    Définit la couleur du rectangle.

-pc r v b :
    Définit la couleur du point.

-size W H :
    Définit la taille de la fenêtre en pixels. W est la largeur et H est la hauteur.

-start X Y :
    Définit le point de départ du programme. Par défaut, il est initialisé à (0,0).

-print (WIP):
    Affiche les lignes du code exécutées dans la sortie standard.