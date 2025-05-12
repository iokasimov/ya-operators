module Main where

import Ya
import Ya.ASCII
import Ya.World

import Ya.Operators.Namespace
import Ya.Operators.Variance
import Ya.Operators.Renderer

-- TODO: import Ya.Operators.Generate

type Arity = Nonempty List Unit

arguments = is @(Nonempty List Arity) ["#", "##"]

positions x = x `kyo` Range `ha` is @(Scrolling List Unit)

variances = Both @(P) `ho` to @(Nonempty List)
 `ha__` (`lu` by Contra) `lo` (`lu` by Co)

parameters = arguments
 `yok` Plane `ha` positions
 `yok` Plane `ha` variances

templates = Nonempty @List
 `ha_` Next `ho` Item (parameters `yo` intro @(Nonempty List))
 `ha_` Last `ho` Item (parameters `lu` Cross `hv` parameters `yp'yo` Both `ho` to @(Nonempty List))

initial = wrap [Unit] `lu` wrap [Unit] `lu` intro @Tree `ha` intro @(Nonempty List) `hv` by E `lu` empty @List

type Layer = Scrolling List Name `P` Variance

variable symbol attr = quant symbol `ha` this `ha` attr `ho'he'he` is

layer x = intro @(State `T'I` Namespace `P` Tree Name `P` List Layer) (x `yi` is @(Scrolling List Unit `P` Variance) `yi` this)
 `yok_____` New `ha` State `ha___` Event `ha_` variable T focus `ho__'ha` Scope `hv` at @(Counter Parametric) `ho_'he` Scope `hv` it
 `lo____'yp` New `ha` State `ha___` Event `ha_` variable I other `ho__'ha` Scope `hv` at @(Counter Positioned) `ho_'he` Scope `hv` it
 `yok_____` New `ha` State `ha___` Event `ha_` inject `ha` this `ho_'ha` Scope `hv` at @(Tree Name)
 `lo____'yp` New `ha` State `ha___` Event `ha_` push @List `ha'yoi` wrap @(AR) `ho_'ha` Scope `hv` at @(List Layer)
 `ha______` (`lu` that x)

inject x tree = Only tree `lu` that @(Shafted List `T'I` Name) `ho'yo` intro @Tree `hv` x
 `yi` wrap @(AR) @(Scrolling List `T'I` Tree Name) `ho` to @(Nonempty List) `ho` to @List `ho'yo` unwrap @(AR)
 `yi` (\st -> tree `lu` Root (unwrap `ha` this @(Only Name) `hv` x) st)

main = by templates `yokl'yokl` Forth `ha` Forth `ha` World
 `ha__` is @(Nonempty List `T'I` Scrolling List Unit `P` Variance)
  `ho_'yokl` Forth `ha` New `ha` layer `ho_'he'he'hv` initial `ho_` render `ha` that