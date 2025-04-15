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

arguments = Nonempty @List
 `ha__` Next `ho_` Item `hv` by (Nonempty @List `ha_` Item `hv` Unit `ha` Last)
 `ha__` Next `ho_` Item `hv` by (Nonempty @List `ha_` Item `hv` Unit `ha` Next `ha_` Item `hv` Unit `ha` Last)
 `ha__` Last `ho_` Item `hv` by (Nonempty @List `ha_` Item `hv` Unit `ha` Next `ha_` Item `hv` Unit `ha` Next `ha_` Item `hv` Unit `ha` Last)

quantifier letter current = current
 `yokl` Forth `ha` Run `ha` Only `ha` letter
 `lu__` push `hv` Unit `ho` that `hv` current

-- 1. Use a Scrolling Tree instead of a Scrolling List
-- 2. Put a proper quantifier name in each variable slot
positions x = to @(Scrolling List)
 `hv` (x `yo` Glyph `ha` Letter `ha` Lower `ha` I)
 `kyo` unwrap @AR @(Unit `L` Scrolling List `WR` _)
 `yi_` to @(Nonempty List)

main = by arguments `yo` positions
 `yokl'yokl`  Forth `ha` Forth `ha` World `ha` print `ha` to @(Scrolling Tree)