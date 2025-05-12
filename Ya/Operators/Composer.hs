module Ya.Operators.Composer where

import Ya
import Ya.ASCII

type Variance = Unit `S` Unit

pattern Contra e = This e
pattern Co e = That e

compare current result = Some `hu` by Contra `la` Some `hu` by Co `li` current `hd'q` result

type Name = Nonempty List Latin

quant symbol variables = variables
 `yukl` Forth `ha` New `ha` State `ha` Event `hv` fill symbol

fill symbol ns = ns `yo` symbol `lu_` push Unit `ho` that `hv` ns

data Variable = Parametric | Positioned

type Counter label = label `L` Nonempty List `T'I` Unit

type Namespace = Counter Positioned `P` Counter Parametric

type Layer = Scrolling List Name `P` Variance

layer x = intro @(State `T'I` Namespace `P` Tree Name `P` List Layer) `ha` this `hv` x
 `yok_____` New `ha` State `ha___` Event `ha_` variable T focus `ho__'ha` Scope `hv` at @(Counter Parametric) `ho_'he` Scope `hv` it
 `lo____'yp` New `ha` State `ha___` Event `ha_` variable I other `ho__'ha` Scope `hv` at @(Counter Positioned) `ho_'he` Scope `hv` it
 `yok_____` New `ha` State `ha___` Event `ha_` inject `ha` this `ho_'ha` Scope `hv` at @(Tree Name)
 `lo____'yp` New `ha` State `ha___` Event `ha_` push @List `ha'yoi` wrap @(AR) `ho_'ha` Scope `hv` at @(List Layer)
 `ha______` (`lu` that `ha` is @(Scrolling List Unit `P` Variance) `hv` x)

variable symbol attr = quant symbol `ha` this `ha` attr `ho'he'he` is

inject x tree = Only tree `lu` that @(Shafted List `T'I` Name) `ho'yo` intro @Tree `hv` x
 `yi` wrap @(AR) @(Scrolling List `T'I` Tree Name) `ho` to @(Nonempty List) `ho` to @List `ho'yo` unwrap @(AR)
 `yi` (\st -> tree `lu` Root (unwrap `ha` this @(Only Name) `hv` x) st)
