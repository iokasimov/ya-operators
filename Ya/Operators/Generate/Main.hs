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

layer x = enter @(State `T'I` Namespace `P` Tree Name)
 `yuk____` New `ha` State `hv___` Event `hv_` get `ha__` Scope `hv` at @(Tree Name)
 `lu___'yp` New `ha` State `hv___` Event `hv_'he'he` name I `ha` that `hv'he` x `ha__` Scope `hv` at @(Counter Positioned)
 `yok____` New `ha` State `ha___` Event `ha_` put `ha` inject `ho__'ha` Scope `hv` at @(Tree Name) `ho_` Scope `hv` sub @Tree
 `yuk____` New `ha` State `hv___` Event `hv_'he'he` name T `ha` this `hv'he` x `ha__` Scope `hv` at @(Counter Parametric)
 `yok____` New `ha` State `ha___` Event `ha_` put @Name `ha'he` is `ho__'ha` Scope `hv` at @(Tree Name) `ho_` Scope `hv` top @Tree

inject = to @List `ha` to @(Nonempty List) `ha` is @(Scrolling List `T'I` Tree Name)
 `ha__` Only `ha` this @(Tree Name) `lo` that @(Shafted List Name) `ho'yo` intro @Tree

templates = Nonempty @List
 `ha__` Next `ho` Item (parameters `yo` intro @(Nonempty List))
 `ha__` Last `ho` Item (parameters `lu` Cross `hv` parameters `yp'yo` Both `ho` to @(Nonempty List))

initial = wrap [Unit] `lu` wrap [Unit] `lu` intro @Tree [by O]

render = Some `hu_` output `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Space `hv` Unit
 `lo__'yp` Some `hu_` Await `ha` output `ha` Glyph `ha` Symbol `ha` Bracket `ha` Opened `hv` Round
 `lo__'yp` Await `ha_'yokl` Forth `ha` World `ha` output `ha` Glyph `ha` Letter `ha` Lower `ha_` this `ha` top @Tree
 `lo__'yp` Await `ha` render_subtree  `ha` this `ha` sub
 `lo__'yp` Some `hu_` Await `ha` output `ha` Glyph `ha` Symbol `ha` Bracket `ha` Closed `hv` Round

render_subtree subtree = Empty @List `hu` enter @World
 `la` Same `hu` (subtree `yokl` Forth `ha` World `ha` render) `ho'yu` Unit
 `li` unwrap @AR @(List _) subtree

type Arity = Nonempty List Unit

arguments = is @(Nonempty List Arity) ["#", "##", "###"]

positions x = x `yi` to @(Scrolling List) `kyo` Range `ha` is @(Scrolling List _)

parameters :: Nonempty List (Scrolling List Unit)
parameters = arguments `yok` Plane `ha` to @(Nonempty List) `ha` positions

main = by templates
 `yokl'yokl` Forth `ha` Forth `ha` World
 `ha__'yuk` World `ha` output `ha` Caret `ha` Newline `hv` Unit
 `ha__` is @(Nonempty List `T'I` Scrolling List Unit)
  `ho_'yokl` Forth `ha` New `ha` layer `ho_'he'he'hv` initial
  `ho_` render `ha` that @(Tree Name) `ha` that
