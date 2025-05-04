module Ya.Operators.Renderer where

import Ya
import Ya.ASCII
import Ya.World
import Ya.Console

import Ya.Operators.Variance
import Ya.Operators.Namespace
import Ya.Operators.Template

target = Some `hu_` output `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Space `hv` Unit
 `lo__'yp` Some `hu_` Await `ha` output `ha` Glyph `ha` Symbol `ha` Bracket `ha` Opened `hv` Round
 `lo__'yp` Await `ha` target_element `ha` this @(Variated Name) `ha` top @Tree
 `lo__'yp` Await `ha` target_subtree `ha` this @(List Functorial) `ha` sub @Tree
 `lo__'yp` Some `hu_` Await `ha` output `ha` Glyph `ha` Symbol `ha` Bracket `ha` Closed `hv` Round

target_element = is @(Equipped `T'I` Maybe Variance `T'I` Name)
 `ho______'he` this @Name `ho` render_variable
  `lo___'yp` Await `ha__` that @(Maybe Variance) `ho_'yokl` Run `ha` World `ha` output `ha` Glyph `ha` Symbol `ha` Punctuate `ha` (Hyphen `la` Plus)

render_variable x = x `yokl` Forth `ha` World `ha` output `ha` Glyph `ha` Letter `ha` Lower

target_subtree subtree = Empty @List `hu` enter @World
 `la` Same `hu` (subtree `yokl` Forth `ha` World `ha` target) `ho'yu` Unit
 `li` unwrap @AR @(List _) subtree

-- functorial_constraint l v from into = enter @World
 -- `yuk___` World (variant v `yokl` Forth `ha` World `ha` output)

-- calculate_variance :: List `T'I` Variant Name `AR__` Variant Name
-- calculate_variance variances = variances
--  `yokl` Forth `ha` New `ha` State `ha` Event
--  `ha__` compare `ho'ho` auto
--  `he'he'hv___` Perhaps `hv` Co [by A]
--  `yi__` that @(Variant Name)

-- compare :: Variant Name `AR__` Variant Name `AR_` Unit `S` Unit
-- compare x y = Some `hu_` by Contra `la_` Some `hu_` by Co `li_` x `hd'q` y

-- title :: Tree Name `AR___`Scrolling Tree Name
-- title = to @(Scrolling Tree)

-- type Title = List ASCII

-- titling = enter @(State `T'I` Scrolling Tree Name `P` List ASCII)
--  `yuk___` New `ha` State `hv__` Event `ha` shift `ha` Level `ha` Down `hv` Unit `ha_`Scope `hv` at @(Scrolling Tree Name)

-- TODO: how can we can quickly understand, if we return previous state or only a new one? Using some labels/constructors?

-- categories = is @(Nonempty List ASCII)
 -- `ha__` Some `hu` "from into"
   -- `la` Some `hu` "into into"

-- variant = is @(Nonempty List ASCII)
 -- `ha__` Some `hu` "Covariant"
   -- `la` Some `hu` "Contravariant"

-- is_first = is @(Nonempty List ASCII)
 -- `ha__` Some `hu` "Yoneda Functor from into"
   -- `la` Some `hu` "Endo Semi Functor from"

-- is @(Namespace `P` Tree Name `P` List Variance)
render (These namespace functorial) = enter @World
 `yuk____` Await `hv` target functorial
