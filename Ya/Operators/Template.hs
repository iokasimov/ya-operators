module Ya.Operators.Template where

import Ya
import Ya.ASCII

import Ya.Operators.Variance

type Name = Nonempty List Latin

quant symbol variables = variables `yokl` Forth `ha` New `ha` State `ha` Event `ha` fill symbol

fill symbol x ns = variated (Some `hu` (unwrap ns `yo` symbol)) x `lu` rewrap (push Unit `ho` that) ns

data Variable = Parametric | Positioned

type Counter label = label `L` Nonempty List `T'I` Unit

type Namespace = Counter Positioned `P` Counter Parametric

type Functorial = Tree `T'I` Variant Name

layer x = enter @(State `T'I` Namespace `P` Functorial)
 `yuk____` New `ha` State `hv___` Event `hv_` get `ha__` Scope `hv` at @Functorial
 `lu___'yp` New `ha` State `hv___` Event `hv_'he'he` quant I `ha` that `hv'he` x `ha__` Scope `hv` at @(Counter Positioned)
 `yok____` New `ha` State `ha___` Event `ha_` put `ha` inject `ho__'ha` Scope `hv` at @Functorial `ho_` Scope `hv` sub @Tree
 `yuk____` New `ha` State `hv___` Event `hv_'he'he` quant T `ha` this `hv'he` x `ha__` Scope `hv` at @(Counter Parametric)
 `yok____` New `ha` State `ha___` Event `ha_` put `ha'he` is `ho__'ha` Scope `hv` at @Functorial `ho_` Scope `hv` top @Tree

inject :: Functorial `P` (Shafted List `T'I` Variant Name) `AR___` List `T'I` Functorial
inject = to @List `ha` to @(Nonempty List) `ha` is @(Scrolling List `T'I` Functorial)
 `ha__` Only `ha` this @Functorial `lo` that @(Shafted List `T'I` Variant Name) `ho'yo` intro @Tree
