module Main where

import Ya
import Ya.ASCII
import Ya.World
import Ya.Console

data Variable = Parametric | Positioned

parentheses x = output `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Space `hv` Unit
 `lu'yp` output `ha` Glyph `ha` Symbol `ha` Bracket `ha` Opened `hv` Round
 `lu'yp` x `lu'yp` output `ha` Glyph `ha` Symbol `ha` Bracket `ha` Closed `hv` Round

type Arity = Nonempty List Unit

arguments = is @(Nonempty List _) ["#", "##", "###"]
-- operators = is @(Nonempty List _) [".", "..", "...", "....", "....."]

quantified letter current = current
 `yokl` Forth `ha` Run `ha` Only `ha` letter
 `lu__` push `hv` Unit `ho` that `hv` current

variable = Glyph `ha` Letter `ha` Lower
 `ha__` None `hu` by I `la` Some `hu` by T
 `ha__` sub `ho` this @(List `WR` Tree ASCII)

rename = (top `ho` that `lo` variable) `ho'hd` is

-- TODO: Use a Scrolling Tree instead of a Scrolling List
-- TODO: Put a proper quantified name in each variable slot
positions x = is @Arity x
 `yo` Glyph `ha` Letter `ha` Lower `ha` I
 `yi` to @(Scrolling List)
 `kyo` unwrap @AR @(Unit `L` Scrolling List `WR` _)

-- I think I should reimplement Scrolling Tree, so we have a Scrolling List as descendants

rename' :: Scrolling List ASCII `AR` Scrolling List ASCII
rename' x = focus `ho` that `hv` x `hv` by (Only `ha` Glyph `ha` Letter `ha` Lower `ha` A)

singleton :: Scrolling List ASCII `AR__` Tree ASCII
singleton x = Construct (Node (Glyph `ha` Letter `ha` Lower `ha` T `hv` Unit)
 (x `yi` rename' `yo` intro @Tree `ho` unwrap @AR `yi` to @(Nonempty List) `ho` to @List))

-- TODO: I hope with this code I would be able to generate operators like these
-- `yokl'yokl'yokl` Forth `ha` Forth `ha` Forth `ha` World `ho` output

-- print = (shaft `hv` by Passed) `ho` this `ho_'yokl` Forth `ha` World `ha` output
--  `lo__'yp` Some `hu_` output `ha` Glyph `ha` Symbol `ha` Bracket `ha` Opened `hv` Round
--  `lo__'yp` focus `ho` this `ho_'yokl` Forth `ha` World `ha` output
--  `lo__'yp` Some `hu_` output `ha` Glyph `ha` Symbol `ha` Bracket `ha` Closed `hv` Round
--  `lo__'yp` (shaft `hv` by Future) `ho` this `ho_'yokl` Forth `ha` World `ha` output
--  `lo__'yp` Some `hu_` output `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Comma `hv` Unit
--  `lo__'yp` Some `hu_` output `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Space `hv` Unit

-- TODO: Define traversable instance for Scrolling Tree
-- main = (arguments `yo` positions)
--  -- `yokl'yokl` Forth `ha` Forth `ha` World `ha__'yokl` Forth `ha` World `ha` output
--  `yokl'yokl` Forth `ha` Forth `ha` World `ha` print

main = arguments `yo` positions
 `yokl'yokl` Forth `ha` Forth `ha` World
  `ha__'yuk` World `ha` output `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Space `hv` Unit
 `ha__'yokl` Forth `ha` World `ha` output
 `ha__` singleton
 -- `ha__` rename'
