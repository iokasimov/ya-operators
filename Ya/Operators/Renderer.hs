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
 `lo__'yp` Await `ha` render_variable `ha` this @(Name) `ha` top @Tree
 `lo__'yp` Await `ha` target_subtree `ha` this @(List Functorial) `ha` sub @Tree
 `lo__'yp` Some `hu_` Await `ha` output `ha` Glyph `ha` Symbol `ha` Bracket `ha` Closed `hv` Round

-- target_element = is @(Equipped `T'I` Maybe Variance `T'I` Name)
--  `ho______'he` this @Name `ho` render_variable
--   `lo___'yp` Await `ha__` that @(Maybe Variance) `ho_'yokl` Run `ha` World `ha` output `ha` Glyph `ha` Symbol `ha` Punctuate `ha` (Hyphen `la` Plus)

render_variable x = x `yokl` Forth `ha` World `ha` output `ha` Glyph `ha` Letter `ha` Lower

target_subtree subtree = Empty @List `hu` enter @World
 `la` Same `hu` (subtree `yokl` Forth `ha` World `ha` target) `ho'yu` Unit
 `li` unwrap @AR @(List _) subtree

-- calculate :: List `T'I` Maybe Variance `AR__` Variance
calculate variances = variances
 `yokl` Forth `ha` New `ha` State `ha` Event
 `ha__` compare `ho'ho` auto
 `he'he'hv___` by Co
 `yi__` that @Variance

-- is @(Equipped _ _) `ho'he'he` this `ho'ho` 

compare :: Maybe Variance `AR__` Variance `AR_` Unit `S` Unit
compare current result = Some `hu` result
 `la` (Some `hu` by Contra `la` Some `hu` by Co) `ha` (`hd'q` result)
 `li` current
 
-- title :: Tree Name `AR___` Scrolling Tree Name
-- title = to @(Scrolling Tree)

-- type Title = List ASCII

titling = enter @(State `T'I` Scrolling Tree Name `P` List Name)
 `yuk___` State `ho` Old
  `hv___` Event `hv` get @Name
   `ha__` Scope `hv` at @(Scrolling Tree Name)
  `ho_'he` Scope `hv` at @(Scrolling List `T'TT'I` Tree `T'I_` Name)
  `ho_'he` Scope `hv` focus
  `ho_'he` Scope `hv` it
    `ho_` Scope `hv` top
 `yok___` State `ho` New
  `ha___` Event `ha` push
  `ho_'ha` Scope `hv` at @(List Name)
 `yuk___` State `ho` New
  `hv___` Event `ha` shift `ha` Level `ha` Down `hv` Unit
   `ha__` Scope `hv` at @(Scrolling Tree Name)

-- TODO: how can we can quickly understand, if we return previous state or only a new one? Using some labels/constructors?

render_token x = enter @(State `T'I` List ASCII)
 `yuk__` New (x `yokl` Prior `ha` New `ha` State `ha` Event `ha` push @List `ha` Glyph `ha` Letter `ha` Lower)
 `yuk__` New `ha` State `ha` Event `ha` push `ha` Glyph `ha` Letter `ha` Lower `ha` Y `hv` Unit
 `yuk__` New `ha` State `ha` Event `ha` push `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Singlequote `hv` Unit

render_tokens x = x
 `yokl` Prior `ha` New `ha` render_token
 `yuk_` New `ha` State `ha` Event `hv` pop @List
 `he'he'hv___` empty @List
 `yi__` that @(List _)
 `yokl` Forth `ha` World `ha` output

-- is @(Namespace `P` Tree Name `P` List Variance)
render (These (These _namespace functorial) _tokens) = enter @World
 `yuk____` Await `hv_____` render_tokens _tokens
 `yuk____` Await `hv_____` output `ha` Glyph `ha` Symbol `ha` Punctuate `hv` by Colon
 `yuk____` Await `hv_____` target functorial
 -- `yuk____` Await `hv` functorial_constraint (by Co)
