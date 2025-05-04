module Ya.Operators.Namespace where

import Ya
import Ya.ASCII

type Name = Nonempty List Latin

data Variable = Parametric | Positioned

type Counter label = label `L` Nonempty List `T'I` Unit

type Namespace = Counter Positioned `P` Counter Parametric