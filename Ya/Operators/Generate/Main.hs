module Main where

import Ya
import Ya.ASCII
import Ya.World
import Ya.Console

type Arity = Nonempty List Unit

-- I think I should reimplement Scrolling Tree, so we have a Scrolling List as descendants

print_subtree subtree = Empty @List `hu` enter @World
 `la` Same `hu` (subtree `yokl` Forth `ha` World `ha` print) `ho'yu` Unit
 `li` unwrap subtree

print = Some `hu_` output `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Space `hv` Unit
 `lo__'yp` Some `hu_` Await `ha` output `ha` Glyph `ha` Symbol `ha` Bracket `ha` Opened `hv` Round
 `lo__'yp` Await `ha_'yokl` Forth `ha` World `ha` output `ha` Glyph `ha` Letter `ha` Lower `ha_` this `ha` top
 `lo__'yp` Await `ha` print_subtree  `ha` this `ha` sub
 `lo__'yp` Some `hu_` Await `ha` output `ha` Glyph `ha` Symbol `ha` Bracket `ha` Closed `hv` Round

type Name = Nonempty List Latin

name symbol variables = variables
 `yukl` Forth `ha` New `ha` State `ha` Event
 `hv__` unwrap `ho'yo` symbol
   `lo` rewrap (push Unit `ho` that)

data Variable = Parametric | Positioned

type Counter label = label `L` Nonempty List `WR` Unit

type Namespace = Counter Parametric `P` Counter Positioned

layer x = enter @(State `WR_` Namespace `P` Tree Name)
 `yuk____` New `ha` State `hv___` Event `hv_` get `ha__` Scope `hv` at @(Tree Name)
 `lu___'yp` New `ha` State `hv___` Event `hv_'he'he` name I `ha` that `hv'he` x `ha__` Scope `hv` at @(Counter Positioned)
 `yok____` New `ha` State `ha___` Event `ha_` put `ha` inject `ho__'ha` Scope `hv` at @(Tree Name) `ho_` Scope `hv` sub @Tree
 `yuk____` New `ha` State `hv___` Event `hv_'he'he` name T `ha` this `hv'he` x `ha__` Scope `hv` at @(Counter Parametric)
 `yok____` New `ha` State `ha___` Event `ha_` put @Name `ha'he` is `ho__'ha` Scope `hv` at @(Tree Name) `ho_` Scope `hv` top @Tree

inject = to @List `ha` to @(Nonempty List) `ha` is @(Scrolling List `WR` Tree Name)
 `ha__` Only `ha` this @(Tree Name) `lo` that @(Shafted List Name) `ho'yo` intro @Tree

initial = wrap [Unit] `lu` wrap [Unit] `lu` intro @Tree [by E]

arguments = is @(Nonempty List Arity) ["#", "##", "###"]

positions x = is @Arity x `yi` to @(Scrolling List)
 `kyo` unwrap @AR @(Unit `L` Scrolling List `WR` _)
 `yi_` to @(Nonempty List) `ho` to @List

-- TODO: _ `yok` Plane `ha` variance
operators = arguments `yi` to @List `yok` Plane `ha` positions

combined = operators `lu` Cross operators `yp'yo` by `ha` atop

atop (These x y) = Nonempty @List `ha` Item x `ha` Next `ha` Item y `ha` Last

main = combined
 `yokl` Forth `ha` World
 `ha__'yuk` World `ha` output `ha` Caret `ha` Newline `hv` Unit
 `ha__` is @(Nonempty List `T'I` Scrolling List Unit)
  `ho_'yokl` Forth `ha` New `ha` layer
  `ho_'he'he'hv` initial
  `ho_` print `ha` that @(Tree Name) `ha` that
