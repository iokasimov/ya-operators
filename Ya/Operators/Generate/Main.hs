module Main where

import Ya
import Ya.ASCII
import Ya.World
import Ya.Console

import Ya.Operators.Template
import Ya.Operators.Renderer

type Arity = Nonempty List Unit

arguments = is @(Nonempty List Arity) ["#", "##"] -- , "###"]

positions x = to @(Scrolling List) `hv` x
 `kyo` Range `ha` is @(Scrolling List _)
 `yi_` to @(Nonempty List)

variances :: Scrolling List `T'I` Unit `AR___` Nonempty List `T'I_` Scrolling List `T'I` Variated Unit
variances = Both @(P) `ho` to @(Nonempty List) `ha__` variate Contra `lo` variate Co

variate :: (Unit `AR` Variance) `AR__` Scrolling List `T'I` Unit `AR_` Scrolling List `T'I` Variated Unit
variate f = (focus `ho` this `ho_'yo` Tempt `ha` f) `lo` (other `ho` this `ho'yo` Repel)

parameters = arguments
 `yok` Plane `ha` positions
 `yok` Plane `ha` variances

templates = Nonempty @List
 `ha_` Next `ho` Item (parameters `yo` intro @(Nonempty List))
 `ha_` Last `ho` Item (parameters `lu` Cross `hv` parameters `yp'yo` Both `ho` to @(Nonempty List))

main = by templates
 `yokl'yokl` Forth `ha` Forth `ha` World
 `ha__'yuk` World `ha` output `ha` Caret `ha` Newline `hv` Unit
  `ha__` is -- @(Nonempty List `T'I_` Scrolling List `T'I` Variated Unit)
  `ho_'yokl` Forth `ha` New `ha` layer
  `ho_'he'he'hv` (wrap [Unit] `lu` wrap [Unit] `lu` intro @Tree [by E])
  `ho_` render `ha` that