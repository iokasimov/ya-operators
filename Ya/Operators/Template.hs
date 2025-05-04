module Ya.Operators.Template where

import Ya
import Ya.ASCII

import Ya.Operators.Variance
import Ya.Operators.Namespace

quant symbol variables = variables `yokl` Forth `ha` New `ha` State `ha` Event `ha` fill symbol

fill symbol x ns = x `yu` (unwrap ns `yo` symbol) `lu_` rewrap (push Unit `ho` that) ns

type Functorial = Tree `T'I` Variated Name

layer x = enter @(State `T'I` Namespace `P` Functorial)
 `yuk____` New `ha` State `hv___` Event `hv_` get `ha__` Scope `hv` at @Functorial
 `lu___'yp` New `ha` State `hv___` Event `hv_'he'he` quant I `ha` that `hv'he` x `ha__` Scope `hv` at @(Counter Positioned)
 `yok____` New `ha` State `ha___` Event `ha_` put `ha` inject `ho__'ha` Scope `hv` at @Functorial `ho_` Scope `hv` sub @Tree
 `yuk____` New `ha` State `hv___` Event `hv_'he'he` quant T `ha` this `hv'he` x `ha__` Scope `hv` at @(Counter Parametric)
 `yok____` New `ha` State `ha___` Event `ha_` put `ha'he` is `ho__'ha` Scope `hv` at @Functorial `ho_` Scope `hv` top @Tree

inject :: Functorial `P` (Shafted List `T'I` Variated Name) `AR___` List `T'I` Functorial
inject = to @List `ha` to @(Nonempty List) `ha` is @(Scrolling List `T'I` Functorial)
 `ha__` Only `ha` this @Functorial `lo` that @(Shafted List `T'I` Variated Name) `ho'yo` intro @Tree
