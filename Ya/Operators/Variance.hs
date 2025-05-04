module Ya.Operators.Variance where

import Ya

type Variance = Unit `S` Unit

pattern Contra e = This e
pattern Co e = That e

type Variated = Equipped `T'I` Maybe Variance

pattern Record v e = Equip (These e (Some v)) :: Variated e
pattern Ignore e = Equip (These e (None Unit)) :: Variated e
