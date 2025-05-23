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
 `kyo` Range @(Position Unit)

variances = to @(Nonempty List)
 `ha__` Both @(P) @(Position Unit `P` Variance)
 `ha__` (`lu` Contra Unit)
   `lo` (`lu` Co Unit)
 `ha__` is @(Position Unit)

deviation = to @(Nonempty List)
 `ha__` Both @(P) @(Position Unit `P` Morphism)
 `ha__` (`yio` is @Variance `ho` (`lu` by Flat))
   `lo` (`yio` is @Variance `ho` (`lu` by Rise))
 `ha__` is @(Position Unit `P` Variance)

flattened x = x `yio` is @Variance `ho` (`lu` by Flat)

combinations affix template =
 affix `lu` Cross `hv` template
 `yp'yo'hd` push @(Nonempty List)
 `ho'ho` that @(Nonempty List _)
 `ho'ho` intro @(Nonempty List)

operator template = template
 `yokl` Forth `ha` New `ha` layer
 `he'he'hv___` wrap [Unit] `lu` wrap [Unit] `lu` intro @Tree [by E] `lu` []
 `yi__` render `ha` that @(Namespace `P` Tree Name `P` List Layer)

elementary = arrangements
 `yo` flattened

determinant = arrangements
 `yok` Plane `ha` deviation

genesis templates = templates
 `yo` intro @(Nonempty List)
 `ho` intro @(Nonempty List)

main = intro @World `hv` Unit
 `yuk___` World `hv____` determinant `yi` genesis
  `yokl'yokl` Forth `ha` Forth `ha` Await `ha` operator
 `yuk___` World `hv____` elementary `yi` genesis
  `yok_` Plane `ha` combinations determinant
  `yokl'yokl` Forth `ha` Forth `ha` Await `ha` operator
 `yuk___` World `hv____` elementary `yi` genesis
  `yok_` Plane `ha` combinations elementary
  `yok_` Plane `ha` combinations determinant
  `yokl'yokl` Forth `ha` Forth `ha` Await `ha` operator
 `yuk___` World `hv____` elementary `yi` genesis
  `yok_` Plane `ha` combinations elementary
  `yok_` Plane `ha` combinations elementary
  `yok_` Plane `ha` combinations determinant
  `yokl'yokl` Forth `ha` Forth `ha` Await `ha` operator
 `yuk___` World `hv____` elementary `yi` genesis
  `yok_` Plane `ha` combinations elementary
  `yok_` Plane `ha` combinations elementary
  `yok_` Plane `ha` combinations elementary
  `yok_` Plane `ha` combinations determinant
  `yokl'yokl` Forth `ha` Forth `ha` Await `ha` operator