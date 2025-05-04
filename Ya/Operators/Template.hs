module Ya.Operators.Template where

import Ya
import Ya.ASCII
import Ya.World
import Ya.Console

type Name = Nonempty List Latin

quant symbol variables = variables
 `yukl` Forth `ha` New `ha` State `ha` Event
 `hv__` unwrap `ho'yo` symbol
   `lo` rewrap (push Unit `ho` that)

type Variance = Unit `S` Unit

pattern Contra e = This e
pattern Co e = That e

type Variated e = Maybe (e `S` e)

data Variable = Parametric | Positioned

type Counter label = label `L` Nonempty List `T'I` Unit

type Namespace = Counter Positioned `P` Counter Parametric

layer x = enter @(State `T'I` Namespace `P` Tree Name)
 `yuk____` New `ha` State `hv___` Event `hv_` get `ha__` Scope `hv` at @(Tree Name)
 `lu___'yp` New `ha` State `hv___` Event `hv_'he'he` quant I `ha` that `hv'he` x `ha__` Scope `hv` at @(Counter Positioned)
 `yok____` New `ha` State `ha___` Event `ha_` put `ha` inject `ho__'ha` Scope `hv` at @(Tree Name) `ho_` Scope `hv` sub @Tree
 `yuk____` New `ha` State `hv___` Event `hv_'he'he` quant T `ha` this `hv'he` x `ha__` Scope `hv` at @(Counter Parametric)
 `yok____` New `ha` State `ha___` Event `ha_` put @Name `ha'he` is `ho__'ha` Scope `hv` at @(Tree Name) `ho_` Scope `hv` top @Tree
 -- `yuk____` New `ha` State `hv___` Event `hv_` push `hv` v `ha__` Scope `hv` at @(List Variance)

inject = to @List `ha` to @(Nonempty List) `ha` is @(Scrolling List `T'I` Tree Name)
 `ha__` Only `ha` this @(Tree Name) `lo` that @(Shafted List Name) `ho'yo` intro @Tree
