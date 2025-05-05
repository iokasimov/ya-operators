module Ya.Operators.Namespace where

import Ya
import Ya.ASCII

type Name = Nonempty List Latin

quant symbol variables = variables `yokl` Forth `ha` New `ha` State `ha` Event `ha` fill symbol where

 fill symbol _ ns = ns `yo` symbol `lu_` push Unit `ho` that `hv` ns

data Variable = Parametric | Positioned

type Counter label = label `L` Nonempty List `T'I` Unit

type Namespace = Counter Positioned `P` Counter Parametric