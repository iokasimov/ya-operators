module Ya.Operators.Variance where

import Ya

type Variant e = e `S` e `S` e

pattern Record e = This e
pattern Ignore e = That e

pattern Contra e = This e
pattern Co e = That e

variated f = Record `ha` Contra `ha` f `la` Record `ha` Co `ha` f `la` Ignore `ha` f