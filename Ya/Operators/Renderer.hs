module Ya.Operators.Renderer where

import Ya
import Ya.ASCII
import Ya.World
import Ya.Console

import Ya.Operators.Composer

calculate_variance layers = layers
 `yokl` Forth `ha` New `ha` State `ha` Event
 `ha__` that @Variance `ho` compare `ho'ho` auto
 `he'he'hv___` by Co
 `yi__` that @Variance

-- TODO: how can we can quickly understand, if we return previous state or only a new one? Using some labels/constructors?

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

render_wrapper_variables = intro @World
 `lo____'yp` World `ha____` focus `ho` this `ho'he` is `ho__'yokl` Forth `ha` World `ha` output `ha` Glyph `ha` Letter `ha` Lower
 `lo____'yp` World `ha____` other `ho` this `ho___'yokl` Forth `ha` World `ha` render_separate_variable

render_separate_variable = Some `hu_` output `ha` Glyph `ha` Symbol `ha` Punctuate `hv` by Space
 `lo____'yp` World `ha__'yokl` Forth `ha` World `ha` output `ha` Glyph `ha` Letter `ha` Lower

pattern Yonedaic e = This e
pattern Ordinary e = That e

-- render_yoneda_functor_constraint = Some `hu_` output `ha` Glyph `ha` Symbol `ha` Punctuate `hv` by Space
 -- `lo____'yp` World `ha___` that @Variance `ho` render_variance
 -- `lo____'yp` Some `hu____` World `ha` print `hv` " Yoneda Functor from into ("
 -- `lo____'yp` World `ha___` this @(Scrolling List Name) `ho` render_wrapper
 -- `lo____'yp` World `ha___` this @(Scrolling List Name) `ho` render_wrapper_variables
 -- `lo____'yp` Some `hu____` World `ha` print `hv`  ") => \n"
 -- `lo____'yp` World `ha___` this @(Scrolling List Name) `ho` render_wrapper_constraint

render_wrapper_constraint cat x = Some `hu` render_wrapper_constraint' cat x `ho'yu` Unit
  `la_` Some `hu` enter @World
 `ha__` other `ho` this @(Shafted List Name) `ho` (`hd'q` empty) `hv__` x

render_wrapper_constraint' cat = Some `hu` print " (forall e . Wrapper "
 `lo____'yp` Some `hu_` World `hv` print cat
 `lo____'yp` Some `hu_` World `hv` print " ("
 `lo____'yp` World `ha____` render_wrapper
 `lo____'yp` World `ha____` render_wrapper_variables
 `lo____'yp` Some `hu____` World `ha` print `hv` " e)) => \n"

render_target variance = Some `hu_` output `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Space `hv` Unit
 `lo__'yp` Some `hu_` World `ha` output `ha` Glyph `ha` Symbol `ha` Bracket `ha` Opened `hv` Round
 `lo__'yp` World `ha` render_target_variable variance `ha` this @Name `ha` top @Tree
 `lo__'yp` World `ha` render_target_subtree variance `ha` this @(List `T'I` Tree Name) `ha` sub @Tree
 `lo__'yp` Some `hu_` World `ha` output `ha` Glyph `ha` Symbol `ha` Bracket `ha` Closed `hv` Round

render_target_subtree variance subtree = Empty @List `hu` enter @World
 `la` Same `hu` (subtree `yokl` Forth `ha` World `ha` render_target variance) `ho'yu` Unit
 `li` unwrap @(AR) @(List _) subtree

render_variable x = x `yokl` Forth `ha` World `ha` output `ha` Glyph `ha` Letter `ha` Lower

-- TODO: This conditional is not elegant and error prone, we do it because we loose `Scrolling List` structure due to convertation to `Tree`
render_target_variable variance x = that @Name
 `la` Some `hu` prepare_variance_target variance
 `li` [by E] `hd'q` x
 `yokl` Forth `ha` World `ha` output `ha` Glyph `ha` Letter `ha` Lower

prepare_variance_target = Some `hu` [by A] `la` Some `hu` [by O]

render_universal_variables symbol x = x
 `yi` is @(Nonempty List Unit)
 `kyo` Range @(Nonempty List Latin) `ha'yo` (symbol :: Unit `AR` Latin) `ha` is @(Nonempty List Unit)
 `yokl` Prior `ha` World `ha` render_separate_variable

render_remaining_functors tokens = tokens
 `yokl` Prior `ha` New
 `ha__'yuk` New `hv` (" `compose` " `yi` is @(List ASCII) `yokl` Prior `ha` New `ha` State `ha` Event `ha` push)
 `ha__` render_remaining_functor_token
 `yuk_` New `hv` ("fai (identity" `yi` is @(List ASCII) `yokl` Prior `ha` New `ha` State `ha` Event `ha` push)

render_remaining_functor_token x = enter @(State `T'I` List ASCII)
 `yuk____` New `hv____` " @from" `yi` is @(List ASCII) `yokl` Prior `ha` New `ha` State `ha` Event `ha` push
 `yuk____` New `hv____` x `yokl` Prior `ha` New `ha` State `ha` Event `ha` push `ha` Glyph `ha` Letter `ha` Lower
 `yuk____` New `hv____` State `ha` Event `ha` push `ha` Glyph `ha` Letter `ha` Lower `ha` F `hv` Unit

render_definition layers = let These popped remains = layers `yo` tokenize `yi` pop in
 popped `yokl'yokl` Check `ha` Prior `ha` New `ha` State `ha` Event `ha` push `ha` Glyph `ha` Letter `ha` Lower
 `yuk_____` New `hv____` State `ha` Event `ha` push `ha` Glyph `ha` Letter `ha` Lower `hv` by Y
 `yuk_____` New `hv____` is @(List ASCII) `hv` ") `compose` " `yokl` Prior `ha` New `ha` State `ha` Event `ha` push
 `yuk_____` New `hv____` render_remaining_functors remains
 `he'he'hv_______` " @from @into" `yi_______` that @(List ASCII)

tokenize (These x v) = is
 `li` focus `ho` this `ho'yu` variant v
 `lo` other `ho` this `ho'yu` by I
 `li` is @(Scrolling List Name) x
 `yi` to @(Nonempty List) @(Scrolling List) @Latin

variant = Contra `hu` by A `la` Co `hu` by O `ha__` is @Variance

tokens layers = layers
 `yokl` Prior `ha` New
  `ha__` tokenize `ha` is @(Scrolling List _ `P` Variance)
  `ho_'yokl` Prior `ha` New `ha` State `ha` Event `ha` push @List `ha` Glyph `ha` Letter `ha` Lower
  `ho_'yuk` New `ha` State `ha` Event `ha` push `ha` Glyph `ha` Letter `ha` Lower `hv` by Y
  `ho_'yuk` New `ha` State `ha` Event `ha` push `ha` Glyph `ha` Symbol `ha` Punctuate `hv` by Singlequote
 `yuk_` New `ha` State `ha` Event `hv` pop @List
 `he'he'hv___` empty @List `yi__` that @(List ASCII)

print x = x `yi` is @(List ASCII) `yokl` Forth `ha` World `ha` output

render_functor_constraint prefix = Some `hu_` output `ha` Glyph `ha` Symbol `ha` Punctuate `hv` by Space
 `lo____'yp` World `ha___` that @Variance `ho` render_variance
 `lo____'yp` Some `hu____` World `ha` print `hv___` Yonedaic `hu` " Yoneda" `la` Ordinary `hu` " Endo Semi" `li` prefix
 `lo____'yp` Some `hu____` World `ha` print `hv` " Functor from"
 `lo____'yp` Some `hu____` World `ha` print `hv___` Yonedaic `hu` " into (" `la` Ordinary `hu` " (" `li` prefix
 `lo____'yp` World `ha___` this @(Scrolling List Name) `ho` render_wrapper
 `lo____'yp` World `ha___` this @(Scrolling List Name) `ho` render_wrapper_variables
 `lo____'yp` Some `hu____` World `ha` print `hv`  ") => \n"
 `lo____'yp` World `ha___` this @(Scrolling List Name) `ho` render_wrapper_constraint (Yonedaic `hu` "into" `la` Ordinary `hu` "from" `li`  prefix)

reverse_layers layers = layers
 `yokl` Forth `ha` New `ha` State `ha` Event `ha` push @List
 `he'he'hv___` empty @List
 `yi__` that @(List _)

-- TODO: this code is terrible! I should rewrite it ASAP.

render (These (These namespace functorial) layers@(calculate_variance -> variance)) = enter @World
 `yuk_____` World `hv_____` output `ha` Caret `ha` Newline `hv` Unit
 `yuk_____` World `hv_____` print `ha` tokens `hv` layers
 `yuk_____` World `hv_____` is @(List ASCII) `hv` " :: forall from into" `yokl` Forth `ha` World `ha` output
 `yuk_____` World `hv_____` namespace `yi` at `ho` this @(Counter Parametric) `ho'he` pop `ho` that `ho` render_universal_variables T
 `yuk_____` World `hv_____` namespace `yi` at `ho` this @(Counter Positioned) `ho'he` pop `ho` that `ho` render_universal_variables I
 `yuk_____` World `hv_____` print " a o ."
 `yuk_____` World `hv_____` output `ha` Caret `hv` by Newline
 -- TODO: I don't like the fact that `layers` is a `List` - it should be a `Nonempty List` (or we should iterate over `Scrolling Tree` instead)
 `yuk_____` World `hv______` Some `hu__` print "Impossible happened!" `yu` Unit
  `la____` Only `ha` this `ha` top @(Nonempty List) `ho__'yokl` Forth `ha` Await `ha_` render_functor_constraint `hv` by Yonedaic
  `lo__'yp` this `ha` sub @(Nonempty List) `ho__'yokl'yokl` Check `ha` Forth `ha` Await `ha_` render_functor_constraint `hv` by Ordinary `ho__` Await
  `ho___'yu` Unit
  `li____` layers
 `yuk_____` World `hv_____` print " Contravariant Endo Semi Functor (->) (T'II'I into"
 `yuk_____` World `hv_____` render_target variance functorial
 `yuk_____` World `hv_____` print ") => \n"
 `yuk_____` World `hv_____` print " (forall e ee . Wrapper into (T'I'II from e ee)) => \n"
 `yuk_____` World `hv_____` render_target (not variance) functorial
 `yuk_____` World `hv_____` print " -> into (from a o)"
 `yuk_____` World `hv_____` render_target variance functorial
 `yuk_____` World `hv_____` output `ha` Caret `hv` by Newline
 `yuk_____` World `hv_____` print `ha` tokens `hv` layers
 `yuk_____` World `hv_____` print " = "
 `yuk_____` World `hv_____` render_definition `hv` layers `yokl` Forth `ha` World `ha` output
 `yuk_____` World `hv_____` output `ha` Caret `hv` by Newline
