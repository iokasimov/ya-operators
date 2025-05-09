module Ya.Operators.Renderer where

import Ya
import Ya.ASCII
import Ya.World
import Ya.Console

import Ya.Operators.Variance
import Ya.Operators.Namespace
import Ya.Operators.Template

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
 `li_` quant I x `he'he'hv` intro @(Nonempty List) Unit `yi` this @(Scrolling List `T'I` _) `he'he'hv__` empty @List
 `yi_` that @(List ASCII) `ho_'yokl` Forth `ha` World `ha` output

render_position name = name
 `yokl'yokl` Forth `ha` Forth `ha` New `ha` State `ha` Event `ha` push `ha` Glyph `ha` Letter `ha` Upper
 `yuk_____` New  `ha` State `ha` Event `ha` push `ha` Glyph `ha` Symbol `ha` Punctuate `hv` by Singlequote

render_variance = is @(Nonempty List ASCII)
 `ho_'yokl` Forth `ha` World `ha` output
 `ha__` Contra `hu` "Contravariant" `la` Co `hu` "Covariant"

render_variables = intro @World
 `lo____'yp` World `ha____` focus `ho` this `ho'he` is `ho__'yokl` Forth `ha` World `ha` output `ha` Glyph `ha` Letter `ha` Lower
 `lo____'yp` World `ha____` other `ho` this `ho___'yokl` Forth `ha` World `ha` render_separate_variable

render_separate_variable = Some `hu_` output `ha` Glyph `ha` Symbol `ha` Punctuate `hv` by Space
 `lo____'yp` World `ha__'yokl` Forth `ha` World `ha` output `ha` Glyph `ha` Letter `ha` Lower

render_constraint = Some `hu_` output `ha` Glyph `ha` Symbol `ha` Punctuate `hv` by Space
 `lo____'yp` World `ha___` that @Variance `ho` render_variance
 `lo____'yp` Some `hu____` World (" Yoneda Functor into into (" `yi` is @(List ASCII) `yokl` Forth `ha` World `ha` output)
 `lo____'yp` World `ha___` this @(Scrolling List Name) `ho` render_wrapper
 `lo____'yp` World `ha___` this @(Scrolling List Name) `ho` render_variables
 `lo____'yp` Some `hu____` World (") => \n" `yi` is @(List ASCII) `yokl` Forth `ha` World `ha` output)

render_declaration = Some `hu_` output `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Space `hv` Unit
 `lo__'yp` Some `hu_` World `ha` output `ha` Glyph `ha` Symbol `ha` Bracket `ha` Opened `hv` Round
 `lo__'yp` World `ha` render_variable `ha` this @Name `ha` top @Tree
 `lo__'yp` World `ha` render_declaration_subtree `ha` this @(List `T'I` Tree Name) `ha` sub @Tree
 `lo__'yp` Some `hu_` World `ha` output `ha` Glyph `ha` Symbol `ha` Bracket `ha` Closed `hv` Round

render_declaration_subtree subtree = Empty @List `hu` enter @World
 `la` Same `hu` (subtree `yokl` Forth `ha` World `ha` render_declaration) `ho'yu` Unit
 `li` unwrap @(AR) @(List _) subtree

render_variable x = x `yokl` Forth `ha` World `ha` output `ha` Glyph `ha` Letter `ha` Lower

render_universal_variables symbol x = x
 `yi` is @(Nonempty List Unit)
 `kyo` Range @(Nonempty List Latin) `ha'yo` (symbol :: Unit `AR` Latin) `ha` is @(Nonempty List Unit)
 `yokl` Prior `ha` World `ha` render_separate_variable

-- is @(Namespace `P` Tree Name `P` List Variance `P` List Layer)
render (These (These (These namespace functorial) tokens) layers) = enter @World
 `yuk____` World `hv_____` output `ha` Caret `ha` Newline `hv` Unit
 `yuk____` World `hv_____` render_tokens tokens
 `yuk____` World `hv_____` is @(List ASCII) `hv` " :: forall" `yokl` Forth `ha` World `ha` output
 `yuk____` World `hv_____` namespace `yi` at @(Counter Parametric) `ho` this `ho'he` pop `ho` that `ho` render_universal_variables T
 `yuk____` World `hv_____` namespace `yi` at @(Counter Positioned) `ho` this `ho'he` pop `ho` that `ho` render_universal_variables I
 `yuk____` World `hv_____` " a o ." `yi` is @(List ASCII) `yokl` Forth `ha` World `ha` output
 `yuk____` World `hv_____` output `ha` Caret `hv` by Newline
 `yuk____` World `hv_____` layers `yokl` Forth `ha` World `ha__` render_constraint
 `yuk____` World `hv_____` render_declaration functorial
 `yuk____` World `hv_____` output `ha` Caret `hv` by Newline