module Main where

import Ya
import Ya.ASCII
import Ya.World

import Ya.Operators.Composer
import Ya.Operators.Renderer

type Arity = Nonempty List Unit

arrangements = arguments
 `yok_` Plane `ha` positions
 `yok_` Plane `ha` variances

arguments = is @(Nonempty List Arity) ["#", "##"]

positions arity = arity
 `kyo` Range @(Scrolling List Unit)

variances = Both @(P) `ho` to @(Nonempty List)
 `ha__` (`lu` by Contra) `lo` (`lu` by Co)

operator template = template
 `yokl` Forth `ha` New `ha` layer
 `he'he'hv___` wrap [Unit] `lu` wrap [Unit] `lu` intro @Tree [by E] `lu` []
 `yi__` render `ha` that @(Namespace `P` Tree Name `P` List Layer)

main = enter @World
 `yuk___` World `hv____` arrangements
  `yo` intro @(Nonempty List)
  `yokl` Forth `ha` Await `ha` operator
 `yuk___` World `hv____` arrangements
  `lu` Cross `hv` (arrangements `yo` intro @(Nonempty List))
  `yp'yo'hd` push @(Nonempty List) `ho'ho` that
  `yokl` Forth `ha` Await `ha` operator