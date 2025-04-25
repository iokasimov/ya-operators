module Main where

import Ya
import Ya.ASCII
import Ya.World
import Ya.Console

-- variable = None `hu` by I `la` Some `hu` by T
 -- `ha__` sub `ho` this @(List `WR` Tree Latin)

-- rename = (top `ho` that `lo` variable) `ho'hd` is

data Variable = Parametric | Positioned

type Namespace = (Parametric `L` List) Unit `P` (Positioned `L` List) Unit

type Arity = Nonempty List Unit

type Name = Nonempty List Latin

arguments = is @(Nonempty List _) ["#", "##", "###"]
-- operators = is @(Nonempty List _) [".", "..", "..."] --, "....", "....."]

quantified letter current = current
 `yokl` Forth `ha` Run `ha` Only `ha` letter
 `lu__` push `hv` Unit `ho` that `hv` current

-- I think I should reimplement Scrolling Tree, so we have a Scrolling List as descendants

print_subtree subtree = Empty @List `hu` enter @World
 `la` Same `hu` (subtree `yokl` Forth `ha` World `ha` print) `ho'yu` Unit
 `li` unwrap subtree

print = Some `hu_` output `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Space `hv` Unit
 `lo__'yp` Some `hu_` Await `ha` output `ha` Glyph `ha` Symbol `ha` Bracket `ha` Opened `hv` Round
 `lo__'yp` Await `ha` output `ha` Glyph `ha` Letter `ha` Lower `ha` this `ha` top
 `lo__'yp` Await `ha` print_subtree  `ha` this `ha` sub
 `lo__'yp` Some `hu_` Await `ha` output `ha` Glyph `ha` Symbol `ha` Bracket `ha` Closed `hv` Round

-- TODO: Use a Scrolling Tree instead of a Scrolling List
-- TODO: Put a proper quantified name in each variable slot
positions :: Nonempty List Unit `AR___` List `T'I` Scrolling List Unit
positions x = is @Arity x `yi` to @(Scrolling List)
 `kyo` unwrap @AR @(Unit `L` Scrolling List `WR` _)
 `yi_` to @(Nonempty List) `ho` to @List
 -- `kyo` unwrap @AR @(Unit `L` No List `WR` _)

operators :: List `T'I` Scrolling List Unit
operators = to @List `hv` arguments `yok` Cascade `ha` positions

atop (These x y) = Nonempty @List `ha` Item x `ha` Next `ha` Item y `ha` Last

-- Since traversables uses applicative instance inside - can I use Cross label here?
combine :: List `T'I` Scrolling List Unit `AR___` List `T'I` Nonempty List (Scrolling List Unit)
combine operators = operators `lu` Cross operators `yp'yo` by `ha` atop

inject :: Scrolling List `T'I` Tree Unit `AR___` Tree Latin `AR__` List `T'I` Tree Latin
inject x = to @List `ha` to @(Nonempty List) `ha_` focus `ho` that `hv` (x `yo'yo` I) `ha_` Only @(Tree Latin)

-- TODO: stateful variable length
layer x = enter @(State `T'I` Tree Latin)
 `yuk_` Old `ha` State `hv__` Event `hv_` get @(Tree Latin)
 `yok_` New `ha` State `ha__` Event `ha_` put `ha` inject x `ho_'ha` Scope `hv` sub @Tree
 `yuk_` New `ha` State `hv__` Event `hv_` put `hv` by T `ha_` Scope `hv` top @Tree

main = combine operators
 `yokl` Forth `ha` World
 `ha__'yuk` World `ha` output `ha` Caret `ha` Newline `hv` Unit
 `ha__` is @(Nonempty List `T'I` Scrolling List Unit)
  `ho_'yokl` Forth `ha` New `ha` layer `ha'yo` intro @Tree
  `ho_'he'he'hv` intro @Tree `hv` by A
  `ho_` print `ha` that @(Tree Latin)
