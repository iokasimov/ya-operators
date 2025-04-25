module Main where

import Ya
import Ya.ASCII
import Ya.World
import Ya.Console

-- variable = None `hu` by I `la` Some `hu` by T
 -- `ha__` sub `ho` this @(List `WR` Tree Latin)

-- rename = (top `ho` that `lo` variable) `ho'hd` is

type Arity = Nonempty List Unit

arguments = is @(Nonempty List _) ["#", "##", "###"]
-- operators = is @(Nonempty List _) [".", "..", "..."] --, "....", "....."]

-- I think I should reimplement Scrolling Tree, so we have a Scrolling List as descendants

print_subtree subtree = Empty @List `hu` enter @World
 `la` Same `hu` (subtree `yokl` Forth `ha` World `ha` print) `ho'yu` Unit
 `li` unwrap subtree

print = Some `hu_` output `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Space `hv` Unit
 `lo__'yp` Some `hu_` Await `ha` output `ha` Glyph `ha` Symbol `ha` Bracket `ha` Opened `hv` Round
 `lo__'yp` Await `ha_'yokl` Forth `ha` World `ha` output `ha` Glyph `ha` Letter `ha` Lower `ha_` this `ha` top
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
operators = arguments `yi` to @List `yok` Cascade `ha` positions

atop (These x y) = Nonempty @List `ha` Item x `ha` Next `ha` Item y `ha` Last

-- Since traversables uses applicative instance inside - can I use Cross label here?
combine :: List `T'I` Scrolling List Unit `AR___` List `T'I` Nonempty List (Scrolling List Unit)
combine operators = operators `lu` Cross operators `yp'yo` by `ha` atop

inject :: Scrolling List Unit `AR___` Tree Name `AR__` List `T'I` Tree Name
inject x = to @List `ha` to @(Nonempty List)
 `ha_` focus `ho` that `hv` (x `yo` I `ho` intro @(Nonempty List) `ho` intro @Tree)
 `ha_` Only @(Tree Name)

data Variable = Parametric | Positioned

type Namespace = (Parametric `L` List) Unit `P` (Positioned `L` List) Unit

type Name = Nonempty List Latin

quantified letter current = current
 `yokl` Forth `ha` Run `ha` Only `ha` letter
 `lu__` push `hv` Unit `ho` that `hv` current

layer x = enter @(State `WR_` Namespace `P` Tree Name)
 `yuk__` Old `ha` State `hv___` Event `hv_` get @(Tree Name)`ha__` Scope `hv` at @(Tree Name)
 `yok__` New `ha` State `ha___` Event `ha_` put `ha` inject x `ho__'ha` Scope `hv` at @(Tree Name) `ho_` Scope `hv` sub @Tree
 `yuk__` New `ha` State `hv___` Event `hv_` put `hv` [by T] `ha__` Scope `hv` at @(Tree Name) `ho_` Scope `hv` top @Tree

initial = wrap [Unit] `lu` wrap [Unit] `lu` intro @Tree `hv` [by A]

main = combine operators
 `yokl` Forth `ha` World
 `ha__'yuk` World `ha` output `ha` Caret `ha` Newline `hv` Unit
 `ha__` is @(Nonempty List `T'I` Scrolling List Unit)
  `ho_'yokl` Forth `ha` New `ha` layer
  `ho_'he'he'hv`  initial
  `ho_` print `ha` that @(Tree Name) `ha` that
