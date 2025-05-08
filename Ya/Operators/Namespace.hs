module Ya.Operators.Namespace where

import Ya
import Ya.ASCII

import Ya.Operators.Variance

type Name = Nonempty List Latin

quant symbol variables = variables
 `yukl` Forth `ha` New `ha` State `ha` Event `hv` fill symbol where

 fill symbol ns = ns `yo` symbol `lu_` push Unit `ho` that `hv` ns

data Variable = Parametric | Positioned

type Counter label = label `L` Nonempty List `T'I` Unit

type Namespace = Counter Positioned `P` Counter Parametric