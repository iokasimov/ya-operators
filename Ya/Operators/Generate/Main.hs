module Main where

import Ya
import Ya.ASCII
import Ya.World
import Ya.Console

import Ya.Operators.Argument
import Ya.Operators.Namespace
import Ya.Operators.Variance
import Ya.Operators.Tokenizer
import Ya.Operators.Template
import Ya.Operators.Renderer

arguments = is @(Nonempty List Arity) ["#", "##", "###"]

positions x = to @(Scrolling List) `hv` x
 `kyo` Range `ha` is @(Scrolling List _)

-- variances = Both @(P) `ha__` variate Contra `lo` variate Co

-- variate f = is @(Scrolling List `T'I` Variated Unit)
--  `ha___` focus `ho` this `ho_'yo` Record (f Unit)
--    `lo_` other `ho` this `ho_'yo` Ignore

parameters = arguments
 `yok` Plane `ha` to @(Nonempty List) `ha` positions
 -- `yok` Plane `ha` to @(Nonempty List) `ha` variances

templates = Nonempty @List
 `ha_` Next `ho` Item (parameters `yo` intro @(Nonempty List))
 `ha_` Last `ho` Item (parameters `lu` Cross `hv` parameters `yp'yo` Both `ho` to @(Nonempty List))

initial = wrap [Unit] `lu` wrap [Unit] `lu` intro @Tree `ha` intro @(Nonempty List) `hv` by E `lu` empty @List

main = by templates
 `yokl'yokl` Forth `ha` Forth `ha` World
 `ha__'yuk` World `ha` output `ha` Caret `ha` Newline `hv` Unit
  `ha__` is @(Nonempty List `T'I_` Scrolling List `T'I` Unit)
   `ho_'yokl` Forth `ha` New `ha` layer
   `ho_'he'he'hv` initial
   `ho_` render `ha` that @(Namespace `P` Functorial `P` Tokens)