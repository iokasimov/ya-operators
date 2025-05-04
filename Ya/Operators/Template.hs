module Ya.Operators.Template where

import Ya
import Ya.ASCII
import Ya.World
import Ya.Console

import Ya.Operators.Variance

type Name = Nonempty List Latin

-- quant :: Unit `AR` Latin `AR____` t (Variant Unit) `AR__` State Namespace `T'I` t (Variant Name)
quant symbol variables = variables
 `yokl` Forth `ha` New `ha` State `ha` Event
 `ha__` f symbol

f symbol vx ns = unwrap ns `yu` variated symbol vx
 `lu` rewrap (push Unit `ho` that) ns

data Variable = Parametric | Positioned

type Counter label = label `L` Nonempty List `T'I` Unit

type Namespace = Counter Positioned `P` Counter Parametric

type Functorial = Tree `T'I` Variant Name

-- layer :: Scrolling List `T'I` Variant Name `AR___` State `T'I` Namespace `P` Functorial `T'I` Functorial `P` _
layer x = enter @(State `T'I` Namespace `P` Functorial)
 `yuk____` New `ha` State `hv___` Event `hv_` get `ha__` Scope `hv` at @Functorial
 `lu___'yp` New `ha` State `hv___` Event `hv_'he'he` quant I `ha` that `hv'he` x `ha__` Scope `hv` at @(Counter Positioned)
 -- `yok____` New `ha` State `ha___` Event `ha_` put `ha` inject `ho__'ha` Scope `hv` at @Functorial `ho_` Scope `hv` sub @Tree

 -- `yuk____` New `ha` State `hv___` Event `hv_'he'he` quant T `ha` this `hv'he` x `ha__` Scope `hv` at @(Counter Parametric)
 -- `yok____` New `ha` State `ha___` Event `ha_` put @Name `ha'he` is `ho__'ha` Scope `hv` at @Functorial `ho_` Scope `hv` top @Tree

-- inject :: Functorial `P` (Shafted List `T'I` Variant Name) `AR___` Tree `T'I` Variant Name
-- inject = to @List `ha` to @(Nonempty List) `ha` is @(Scrolling List `T'I` Functorial)
--  `ha__` Only `ha` this @Functorial
   -- `lo` that @(Shafted List `T'I` Variant Name) `ho'yo` intro @Tree
