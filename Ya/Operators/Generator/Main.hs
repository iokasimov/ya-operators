module Main where

import Ya
import Ya.ASCII
import Ya.World

import Ya.Operators.Composer
import Ya.Operators.Renderer

arguments = is @(Nonempty List Arity) ["#", "##"]

arrangements = arguments
 `yok_` Plane `ha` positions
 `yok_` Plane `ha` variances

positions arity = arity
 `kyo` Range @(Scrolling List Unit)

variances = Both @(P) `ho` to @(Nonempty List)
 `ha__` (`lu` by Contra) `lo` (`lu` by Co)

combinations template = arrangements
 `lu` Cross `hv` template
 `yp'yo'hd` push @(Nonempty List)
 `ho'ho` that `ho'ho` intro @(Nonempty List)

operator template = template
 `yokl` Forth `ha` New `ha` layer
 `he'he'hv___` wrap [Unit] `lu` wrap [Unit] `lu` intro @Tree [by E] `lu` []
 `yi__` render `ha` that @(Namespace `P` Tree Name `P` List Layer)

main = enter @World
 `yuk___` World `hv____` arrangements
  `yo` intro @(Nonempty List) `ho` intro @(Nonempty List)
  `yokl'yokl` Forth `ha` Forth `ha` Await `ha` operator

 `yuk___` World `hv____` arrangements
  `yo` intro @(Nonempty List) `ho` intro @(Nonempty List)
  `yok_` Plane `ha` combinations
  `yokl'yokl` Forth `ha` Forth `ha` Await `ha` operator
 `yuk___` World `hv____` arrangements
  `yo` intro @(Nonempty List) `ho` intro @(Nonempty List)
  `yok_` Plane `ha` combinations
  `yok_` Plane `ha` combinations
  `yokl'yokl` Forth `ha` Forth `ha` Await `ha` operator
 `yuk___` World `hv____` arrangements
  `yo` intro @(Nonempty List) `ho` intro @(Nonempty List)
  `yok_` Plane `ha` combinations
  `yok_` Plane `ha` combinations
  `yok_` Plane `ha` combinations
  `yokl'yokl` Forth `ha` Forth `ha` Await `ha` operator