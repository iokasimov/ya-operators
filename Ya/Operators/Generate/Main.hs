module Main where

import Ya
import Ya.ASCII
import Ya.World
import Ya.Console

type Name = Nonempty List Latin

name symbol variables = variables
 `yukl` Forth `ha` New `ha` State `ha` Event
 `hv__` unwrap `ho'yo` symbol
   `lo` rewrap (push Unit `ho` that)

data Variable = Parametric | Positioned

type Counter label = label `L` Nonempty List `T'I` Unit

type Namespace = Counter Positioned `P` Counter Parametric

templates = Nonempty @List
 `ha__` Next `ho` Item (parameters `yo` intro @(Nonempty List))
 `ha__` Last `ho` Item (parameters `lu` Cross `hv` parameters `yp'yo` Both `ho` to @(Nonempty List))

render = Some `hu_` output `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Space `hv` Unit
 `lo__'yp` Some `hu_` Await `ha` output `ha` Glyph `ha` Symbol `ha` Bracket `ha` Opened `hv` Round
 `lo__'yp` Await `ha_'yokl` Forth `ha` World `ha` output `ha` Glyph `ha` Letter `ha` Lower `ha_` this `ha` top @Tree
 `lo__'yp` Await `ha` render_subtree  `ha` this `ha` sub
 `lo__'yp` Some `hu_` Await `ha` output `ha` Glyph `ha` Symbol `ha` Bracket `ha` Closed `hv` Round

render_subtree subtree = Empty @List `hu` enter @World
 `la` Same `hu` (subtree `yokl` Forth `ha` World `ha` render) `ho'yu` Unit
 `li` unwrap @AR @(List _) subtree

type Arity = Nonempty List Unit

arguments = is @(Nonempty List Arity) ["#", "##"] -- , "###"]

positions x = x
  `yi` to @(Scrolling List)
 `kyo` Range `ha` is @(Scrolling List _)
 `yi_` to @(Nonempty List)

type Variance = Unit `S` Unit

pattern Co e = This e
pattern Contra e = That e

variances = Both @(P) `ho` to @(Nonempty List)
 `ha__` (`lu` by Co) `lo` (`lu` by Contra)

parameters = arguments
 `yok` Plane `ha` positions
 `yok` Plane `ha` variances

layer (These x v) = enter @(State `T'I` Namespace `P` Tree Name `P` List Variance)
 `yuk____` New `ha` State `hv___` Event `hv_` get `ha__` Scope `hv` at @(Tree Name)
 `lu___'yp` New `ha` State `hv___` Event `hv_'he'he` name I `ha` that `hv'he` x `ha__` Scope `hv` at @(Counter Positioned)
 `yok____` New `ha` State `ha___` Event `ha_` put `ha` inject `ho__'ha` Scope `hv` at @(Tree Name) `ho_` Scope `hv` sub @Tree
 `yuk____` New `ha` State `hv___` Event `hv_'he'he` name T `ha` this `hv'he` x `ha__` Scope `hv` at @(Counter Parametric)
 `yok____` New `ha` State `ha___` Event `ha_` put @Name `ha'he` is `ho__'ha` Scope `hv` at @(Tree Name) `ho_` Scope `hv` top @Tree
 `yuk____` New `ha` State `hv___` Event `ha_` push @List `hv_` v `ha__` Scope `hv` at @(List Variance)

inject = to @List `ha` to @(Nonempty List) `ha` is @(Scrolling List `T'I` Tree Name)
 `ha__` Only `ha` this @(Tree Name) `lo` that @(Shafted List Name) `ho'yo` intro @Tree

main = by templates
 `yokl'yokl` Forth `ha` Forth `ha` World
 `ha__'yuk` World `ha` output `ha` Caret `ha` Newline `hv` Unit
 `ha__` is @(Nonempty List `T'I` Scrolling List Unit `P` Variance)
  `ho_'yokl` Forth `ha` New `ha` layer
  `ho_'he'he'hv` (wrap [Unit] `lu` wrap [Unit] `lu` intro @Tree [by E] `lu` empty @List)
  `ho_` render `ha` that @(Tree Name) `ha` this `ha` that