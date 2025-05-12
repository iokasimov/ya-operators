module Ya.Operators.Variance where

import Ya

type Variance = Unit `S` Unit

pattern Contra e = This e
pattern Co e = That e

compare current result = Some `hu` by Contra `la` Some `hu` by Co `li` current `hd'q` result
