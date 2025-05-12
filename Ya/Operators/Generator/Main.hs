module Main where

import Ya
import Ya.ASCII
import Ya.World

import Ya.Operators.Composer
import Ya.Operators.Renderer

type Arity = Nonempty List Unit

arguments = is @(Nonempty List Arity) ["#", "##"]

parameters = arguments
 `yok` Plane `ha` positions
 `yok` Plane `ha` variances

positions arity = arity `kyo` Range @(Scrolling List Unit)

variances = Both @(P) `ho` to @(Nonempty List)
 `ha__` (`lu` by Contra) `lo` (`lu` by Co)

operator template = template
 `yokl` Forth `ha` New `ha` layer
 `he'he'hv___` wrap [Unit] `lu` wrap [Unit] `lu` intro @Tree [by E] `lu` []
 `yi__` render `ha` that @(Namespace `P` Tree Name `P` List Layer)

templates = Nonempty @List
 `ha_` Next `ho` Item (parameters `yo` intro @(Nonempty List))
 `ha_` Last `ho` Item (parameters `lu` Cross `hv` parameters `yp'yo` Both `ho` to @(Nonempty List))

main = by templates `yokl'yokl` Forth `ha` Forth `ha` World `ha` operator