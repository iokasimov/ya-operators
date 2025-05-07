module Ya.Operators.Renderer where

import Ya
import Ya.ASCII
import Ya.World
import Ya.Console

import Ya.Operators.Variance
import Ya.Operators.Namespace
import Ya.Operators.Constraint
import Ya.Operators.Template

target = Some `hu_` output `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Space `hv` Unit
 `lo__'yp` Some `hu_` Await `ha` output `ha` Glyph `ha` Symbol `ha` Bracket `ha` Opened `hv` Round
 `lo__'yp` Await `ha` render_variable `ha` this @(Name) `ha` top @Tree
 `lo__'yp` Await `ha` target_subtree `ha` this @(List Functorial) `ha` sub @Tree
 `lo__'yp` Some `hu_` Await `ha` output `ha` Glyph `ha` Symbol `ha` Bracket `ha` Closed `hv` Round

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

compare :: Maybe Variance `AR__` Variance `AR_` Unit `S` Unit
compare current result = Some `hu` result
 `la` (Some `hu` by Contra `la` Some `hu` by Co) `ha` (`hd'q` result)
 `li` current

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

render_wrapper x = Some `hu_` State `ha` Event `ha` push @List `ha` Glyph `ha` Symbol `ha` Punctuate `hv` by Space
 `lo_'yp` New `ha` render_position `ha` this @(Singular `T'I` Name) `ha` focus
 `lo_'yp` Empty @List `hu_` New `ha` intro `ha` Glyph `ha` Letter `ha` Upper `hv` by E
    `la` New `ha` render_position `ha` is @(Nonempty List _)
   `ha_` to @List `ha` this @(Shafted List `T'I` Name) `ha` other
 `lo_'yp` Some `hu_` New `ha` State `ha` Event `ha` push @List `ha` Glyph `ha` Letter `ha` Upper `hv` by T
 `li_` quant I x `he'he'hv` intro @(Nonempty List) Unit `yi` this @(Scrolling List `T'I` _)
 `he'he'hv__` empty @List
 `yi_` that @(List ASCII)
 `yokl` Forth `ha` World `ha` output

render_position name = name
 `yokl'yokl` Forth `ha` Forth `ha` New `ha` State `ha` Event `ha` push `ha` Glyph `ha` Letter `ha` Upper
 `yuk_____` New  `ha` State `ha` Event `ha` push `ha` Glyph `ha` Symbol `ha` Punctuate `hv` by Singlequote

render_variance = is @(Nonempty List ASCII)
 `ho_'yokl` Forth `ha` World `ha` output
 `ha__` Contra `hu` "Contravariant" `la` Co `hu` "Covariant"

render_constraint = that @Variance `ho` render_variance
 `lo____'yp` Some `hu____` Await (" Yoneda Functor from into (" `yi` is @(List ASCII) `yokl` Forth `ha` World `ha` output)
 `lo____'yp` Await `ha_` this @(Scrolling List Unit) `ho` render_wrapper
 `lo____'yp` Some `hu____` Await (") => " `yi` is @(List ASCII) `yokl` Forth `ha` World `ha` output)

-- is @(Namespace `P` Tree Name `P` List Variance `P` List Layer)
render (These (These (These _namespace functorial) tokens) layers) = enter @World
 `yuk____` World `hv_____` render_tokens tokens
 `yuk____` World `hv_____` output `ha` Glyph `ha` Symbol `ha` Punctuate `hv` by Colon
 `yuk____` World `hv_____` target functorial
 `yuk____` World `hv_____` output `ha` Glyph `ha` Symbol `ha` Punctuate `hv` by Space
 `yuk____` World `hv_____` layers `yokl` Forth `ha` World `ha` render_constraint
