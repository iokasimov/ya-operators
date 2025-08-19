module Ya.Operators.Renderer where

import Ya
import Ya.ASCII
import Ya.World
import Ya.Console

import Ya.Operators.Composer

calculate_variance layers = layers
 `yokl` Forth `ha` New `ha` State `ha` Event
 `ha__` that @Morphism `ho` this @Variance `ho` compare `ho'ho` auto
 `he'he'hv___` by Co
 `yi__` that @Variance

calculate_deviation layers = layers
 `yokl` Prior `ha` Check
 `ha__` Continue `la` Interrupt
 `ha__` that @Morphism `ho` that @Deviation

render_wrapper x = Some `hu_` State `ha` Event `ha` push @List `ha` Glyph `ha` Symbol `ha` Punctuate `hv` by Space
 `lo_'yp` New `ha` render_position `ha` this @(Singular `T'I` Name) `ha` focus
 `lo_'yp` Empty @List `hu_` New `ha` intro @_ @AR `ha` Glyph `ha` Letter `ha` Upper `hv` by E
    `la` New `ha` render_position `ha` is @(Nonempty List _)
   `ha_` to @List `ha` this @(Shafted List `T'I` Name) `ha` other
 `lo_'yp` Some `hu_` New `ha` State `ha` Event `ha` push @List `ha` Glyph `ha` Letter `ha` Upper `hv` by T
 `li_` quant I x `he'he'hv` intro @(Nonempty List) Unit `yi` this @(Position _) `he'he'hv__` empty @List
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

render_wrapper_constraint cat x = Some `hu` render_wrapper_constraint' cat x `ho'yu` Unit
 `la_` Some `hu` intro @World Unit
 `ha__` other `ho` this @(Shafted List Name) `ho` (`lu'q` empty) `hv__` x

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

render_target_subtree variance subtree = Empty @List `hu` intro @World Unit
 `la` Same `hu` (subtree `yokl` Forth `ha` World `ha` render_target variance) `ho'yu` Unit
 `li` unwrap @(AR) @(List _) subtree

render_variable x = x `yokl` Forth `ha` World `ha` output `ha` Glyph `ha` Letter `ha` Lower

-- TODO: This conditional is not elegant and error prone, we do it because we loose `Position` structure due to convertation to `Tree`
render_target_variable variance x = that @Name
 `la_` Some `hu` prepare_variance_target variance
 `li_` [by E] `lu'q` x
 `yokl` Forth `ha` World `ha` output `ha` Glyph `ha` Letter `ha` Lower

prepare_variance_target = Some `hu` [by A] `la` Some `hu` [by O]

render_universal_variables symbol x = x
 `yi` is @(Nonempty List Unit)
 `kyo` Range @(Nonempty List) `ha'yo` (symbol :: Unit `AR` Latin) `ha` is @(Nonempty List Unit)
 `yokl` Prior `ha` World `ha` render_separate_variable

render_remaining_functors tokens = tokens
 `yokl` Prior `ha` New
 `ha__'yuk` New `hv` (" `compose` " `yi` is @(List ASCII) `yokl` Prior `ha` New `ha` State `ha` Event `ha` push)
 `ha__` render_remaining_functor_token
 `yuk_` New `hv` ("fai (identity @(AR)" `yi` is @(List ASCII) `yokl` Prior `ha` New `ha` State `ha` Event `ha` push)

render_remaining_functor_token x = intro @(State `T'I` List ASCII) `hv` Unit
 `yuk____` New `hv____` " @from" `yi` is @(List ASCII) `yokl` Prior `ha` New `ha` State `ha` Event `ha` push
 `yuk____` New `hv____` x `yokl` Prior `ha` New `ha` State `ha` Event `ha` push `ha` Glyph `ha` Letter `ha` Lower
 `yuk____` New `hv____` State `ha` Event `ha` push `ha` Glyph `ha` Letter `ha` Lower `ha` F `hv` Unit

-- TODO: rewrite using `s` operator!
render_definition layers = let These popped remains = layers `yo` tokenize `yi` pop in
 popped `yokl'yokl` Check `ha` Prior `ha` New `ha` State `ha` Event `ha` push `ha` Glyph `ha` Letter `ha` Lower
 `yuk_____` New `hv____` State `ha` Event `ha` push `ha` Glyph `ha` Letter `ha` Lower `hv` by Y
 `yuk_____` New `hv____` is @(List ASCII) `hv` ") `compose` " `yokl` Prior `ha` New `ha` State `ha` Event `ha` push
 `yuk_____` New `hv____` render_remaining_functors remains
 `he'he'hv_______` " @from @into" `yi_______` that @(List ASCII)

tokenize (These p (These v k)) = is
 `li` focus `ho` this `ho'yu` variant v
 `lo` other `ho` this `ho'yu` by I
 `li` is @(Position Name) p
 `yi` to @(Nonempty List) @Position @Latin
 `yi` maybe_source_kleisli k

maybe_source_kleisli kleisli x =
 None `hu` x `la` Some `hu` source_kleisli x `li` kleisli

source_kleisli x = x `lu's` intro @(Nonempty List) `hv` K Unit

variant = Contra `hu` by A `la` Co `hu` by O `ha__` is @Variance

tokens layers = layers
 `yokl` Prior `ha` New
  `ha__` tokenize `ha` is @(Position _ `P` Morphism)
  `ho_'yokl` Prior `ha` New `ha` State `ha` Event `ha` push @List `ha` Glyph `ha` Letter `ha` Lower
  `ho_'yuk` New `ha` State `ha` Event `ha` push `ha` Glyph `ha` Letter `ha` Lower `hv` by Y
  `ho_'yuk` New `ha` State `ha` Event `ha` push `ha` Glyph `ha` Symbol `ha` Punctuate `hv` by Singlequote
 `yuk_` New `ha` State `ha` Event `hv` pop @List
 `he'he'hv___` empty @List `yi__` that @(List ASCII)

print x = x `yi` is @(List ASCII) `yokl` Forth `ha` World `ha` output

render_source_kleisli_functor_constraint =
 Some `hu_` output `ha` Glyph `ha` Symbol `ha` Punctuate `hv` by Space
 `lo____'yp` World `ha___` that @Variance `ho` render_variance
 `lo____'yp` Some `hu____` World `ha` print `hv` " Endo Transformation Functor from (("
 `lo____'yp` World `ha___` this @(Position Name) `ho` render_wrapper
 `lo____'yp` World `ha___` this @(Position Name) `ho` render_wrapper_variables
 `lo____'yp` Some `hu____` World `ha` print `hv`  ") `T'TT'I` l `L` t') ("
 `lo____'yp` World `ha___` this @(Position Name) `ho` render_wrapper
 `lo____'yp` World `ha___` this @(Position Name) `ho` render_wrapper_variables
 `lo____'yp` Some `hu____` World `ha` print `hv`  ") =>\n"
 `lo____'yp` Some `hu____` World `ha` print `hv`  " (forall e . Wrapper from ("
 `lo____'yp` World `ha___` this @(Position Name) `ho` render_wrapper
 `lo____'yp` World `ha___` this @(Position Name) `ho` render_wrapper_variables
 `lo____'yp` Some `hu____` World `ha` print `hv`  " `T'TT'I` l `L` t' `T'I_` e)) =>\n"

-- TODO: I would not need this function if I would have `yiokl'yiokl` operator:
render_source_kleisli_functor_constraint_optionally (These p (These v k)) =
 k `yukl` Check `ha` World `ha` render_source_kleisli_functor_constraint `hv__` p `lu` v

render_functor_constraint prefix =
 Some `hu_` output `ha` Glyph `ha` Symbol `ha` Punctuate `hv` by Space
 `lo____'yp` World `ha___` that @Morphism `ho` this @Variance `ho` render_variance
 `lo____'yp` Some `hu____` World `ha` print `hv___` Yonedaic `hu` " Yoneda" `la` Ordinary `hu` " Endo Semi" `li` prefix
 `lo____'yp` Some `hu____` World `ha` print `hv` " Functor from"
 `lo____'yp` Some `hu____` World `ha` print `hv___` Yonedaic `hu` " into (" `la` Ordinary `hu` " (" `li` prefix
 `lo____'yp` World `ha___` this @(Position Name) `ho` render_wrapper
 `lo____'yp` World `ha___` this @(Position Name) `ho` render_wrapper_variables
 `lo____'yp` Some `hu____` World `ha` print `hv`  ") => \n"
 `lo____'yp` World `ha___` render_source_kleisli_functor_constraint_optionally
 `lo____'yp` World `ha___` this @(Position Name) `ho` render_wrapper_constraint (Yonedaic `hu` "into" `la` Ordinary `hu` "from" `li`  prefix)

-- TODO: this code is terrible! I should rewrite it ASAP.

render_morphism layers = is
 `li` Some `hu` " -> into (from (a) (l `L` t' `T'I` o))"
 `la` Some `hu` " -> into (from (a) (o))"
 `li` calculate_deviation layers
 `yi` print

render (These (These namespace functorial) layers@(calculate_variance -> variance)) = intro @World `hv` Unit
 `yuk_____` World `hv_____` output `ha` Caret `ha` Newline `hv` Unit
 `yuk_____` World `hv_____` print `ha` tokens `hv` layers
 `yuk_____` World `hv_____` is @(List ASCII) `hv` " :: forall from into" `yokl` Forth `ha` World `ha` output
 `yuk_____` World `hv_____` namespace `yi` at `ho` this @(Counter Parametric) `ho'he` pop `ho` that `ho` render_universal_variables T
 `yuk_____` World `hv_____` namespace `yi` at `ho` this @(Counter Positioned) `ho'he` pop `ho` that `ho` render_universal_variables I
 `yuk_____` World `hv_____` print " t' l a o ."
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
 `yuk_____` World `hv_____` print " (forall e ee . Wrapper into (" 
 `yuk_____` World `hv_____` Some `hu__` print "Impossible happened!"
    `la___` print `ha__` Contra `hu` "T'II'I" `la` Co `hu` "T'I'II" 
     `ha__` this @Variance `ha` that @Morphism `ha` this `ha` top @(Nonempty List)
    `li___` layers
 `yuk_____` World `hv_____` print " from e ee)) => \n"
 `yuk_____` World `hv_____` render_target (not variance) functorial
 `yuk_____` World `hv_____` render_morphism layers
 `yuk_____` World `hv_____` render_target variance functorial
 `yuk_____` World `hv_____` output `ha` Caret `hv` by Newline
 `yuk_____` World `hv_____` print `ha` tokens `hv` layers
 `yuk_____` World `hv_____` print " = "
 `yuk_____` World `hv_____` render_definition `hv` layers `yokl` Forth `ha` World `ha` output
 `yuk_____` World `hv_____` output `ha` Caret `hv` by Newline
