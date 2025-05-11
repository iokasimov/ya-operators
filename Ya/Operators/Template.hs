module Ya.Operators.Template where

import Ya
import Ya.ASCII

import Ya.Operators.Variance
import Ya.Operators.Namespace

type Layer = Scrolling List Name `P` Variance

variable symbol attr = quant symbol `ha` this `ha` attr `ho'he'he` is

-- quantification :: Scrolling List Unit `P` Variance `AR___` State `T'I` Namespace `P` Tree Name `P` List Token `P` List Layer `T'I` Only Name `P` Shafted List Name
quantification = State `ha___` Event `ha_` variable T focus `ho__'ha` Scope `hv` at @(Counter Parametric) `ho_'he` Scope `hv` it
 `lo____'yp` New `ha` State `ha___` Event `ha_` variable I other `ho__'ha` Scope `hv` at @(Counter Positioned) `ho_'he` Scope `hv` it

-- substitution :: Only Name `P` Shafted List Name `P` Variance `AR_____` State `T'I` Namespace `P` Tree Name `P` List Token `P` List Layer `T'I` (Tree Name `P` Layer)
substitution = State `ha__` Event `ha_` inject `ha` this `ho_'ha` Scope `hv` at @(Tree Name)
 `lo__'yp` New `ha` State `ha__` Event `ha_` push @List `ha'yoi` wrap @(AR) `ho_'ha` Scope `hv` at @(List Layer)

-- inject :: Only Name `P` Shafted List Name `AR___` Tree Name `AR_` Tree Name `P` Tree Name
inject x tree = Only tree `lu` that @(Shafted List `T'I` Name) `ho'yo` intro @Tree `hv` x
 `yi` wrap @(AR) @(Scrolling List `T'I` Tree Name) `ho` to @(Nonempty List) `ho` to @List `ho'yo` unwrap @(AR)
 `yi` (\st -> tree `lu` Root (unwrap `ha` this @(Only Name) `hv` x) st)

layer x = enter @(State `T'I` Namespace `P` Tree Name `P` List Layer)
 `yuk___` New `ha` quantification `ha` this `hv` (x :: Scrolling List Unit `P` Variance)
 `yok___` New `ha` substitution `ha` (`lu` that x)
