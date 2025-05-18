module Ya.Operators.Composer where

import Ya
import Ya.ASCII

type Arity = Nonempty List Unit

type Variance = Unit `S` Unit

pattern Contra e = This e
pattern Co e = That e

type Position = Scrolling List

compare current result = Some `hu` by Contra `la` Some `hu` by Co `li` current `hd'q` result

type Name = Nonempty List Latin

quant symbol variables = variables
 `yukl` Forth `ha` New `ha` State `ha` Event `hv` fill symbol

fill symbol ns = ns `yo` symbol `lu_` push Unit `ho` that `hv` ns

data Variable = Parametric | Positioned

type Counter label = label `L` Nonempty List `T'I` Unit

type Namespace = Counter Positioned `P` Counter Parametric

type Layer = Position Name `P` Variance

layer x = enter @(State `T'I` Namespace `P` Tree Name `P` List Layer)
 `yuk_____` New `ha` State `ha___` Event `ha_` variable T focus `ho__'ha` Scope `hv` at @(Counter Parametric) `ho_'he` Scope `hv` it
 `lo____'yp` New `ha` State `ha___` Event `ha_` variable I other `ho__'ha` Scope `hv` at @(Counter Positioned) `ho_'he` Scope `hv` it
 `hv______` this @(Position Unit) `hv` x
 `yok_____` New `ha` State `ha___` Event `ha_` wrap @(AR) `ho'hj` (wrap @(AR) `ho` Aloft @Position `ho` to @Tree) `ho'ho` get `ho_'ha` Scope `hv` at @(Tree Name)
 `lo____'yp` New `ha` State `ha___` Event `ha_` wrap @(AR) `ho` (`lu` that @Variance `hv` x) `ho` push @List `ho_'ha` Scope `hv` at @(List Layer)

variable symbol attr = quant symbol `ha` this `ha` attr `ho'he'he` is

-- `lo'lu`
-- `lu'lo`
