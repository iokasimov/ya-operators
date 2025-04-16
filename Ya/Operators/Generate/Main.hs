module Main where

import Ya
import Ya.ASCII
import Ya.World
import Ya.Console

data Variable = Parametric | Positioned

parentheses x = output `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Space `hv` Unit
 `lu'yp` output `ha` Glyph `ha` Symbol `ha` Bracket `ha` Opened `hv` Round
 `lu'yp` x `lu'yp` output `ha` Glyph `ha` Symbol `ha` Bracket `ha` Closed `hv` Round

print_subtree subtree = Empty @List `hu` enter @World
 `la` Same `hu` parentheses (subtree `yokl` Forth `ha` World `ha` print_tree) `ho'yu` Unit
 `li` unwrap subtree

print_tree = top `ho` this `ho` output
 `lo__'yp` sub `ho` this `ho` print_subtree

print = this `he'ho'he` (shaft `hv` by Passed) `ho` this `ho_'yokl` Forth `ha` World `ha` print_tree
 `lo__'yp` Some `hu_` output `ha` Glyph `ha` Symbol `ha` Bracket `ha` Opened `hv` Round
 `lo__'yp` this `he'ho'he` focus `ho` this `ho_'yokl` Forth `ha` World `ha` print_tree
 `lo__'yp` Some `hu_` output `ha` Glyph `ha` Symbol `ha` Bracket `ha` Closed `hv` Round
 `lo__'yp` this `he'ho'he` (shaft `hv` by Future) `ho` this `ho_'yokl` Forth `ha` World `ha` print_tree
 `lo__'yp` Some `hu_` output `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Comma `hv` Unit
 `lo__'yp` Some `hu_` output `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Space `hv` Unit

type Arity = Nonempty List Unit

arguments = is @(Nonempty List _) ["#", "##", "###"]

quantifier letter current = current
 `yokl` Forth `ha` Run `ha` Only `ha` letter
 `lu__` push `hv` Unit `ho` that `hv` current

variable = Glyph `ha` Letter `ha` Lower
 `ha__` None `hu` by I `la` Some `hu` by T
 `ha__` sub `ho` this @(List `WR` Tree ASCII)

rename = (top `ho` that `lo` variable) `ho'hd` is

-- 1. Use a Scrolling Tree instead of a Scrolling List
-- 2. Put a proper quantifier name in each variable slot
positions x = is @Arity x
 `yo` Glyph `ha` Letter `ha` Lower `ha` I
 `yi` to @(Scrolling List)
 `kyo` unwrap @AR @(Unit `L` Scrolling List `WR` _)

main = arguments `yo` positions `ho` to @(Nonempty List)
 `yokl'yokl` Forth `ha` Forth `ha` World `ha` print `ha` to @(Scrolling Tree)