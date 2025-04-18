module Main where

import Ya
import Ya.ASCII
import Ya.World
import Ya.Console

-- TODO: I hope with this code I would be able to generate operators like these
-- `yokl'yokl'yokl` Forth `ha` Forth `ha` Forth `ha` World `ho` output

-- variable = None `hu` by I `la` Some `hu` by T
 -- `ha__` sub `ho` this @(List `WR` Tree Latin)

-- rename = (top `ho` that `lo` variable) `ho'hd` is

data Variable = Parametric | Positioned

type Namespace = (Parametric `L` List) Unit `P` (Positioned `L` List) Unit

type Arity = Nonempty List Unit

arguments = is @(Nonempty List _) ["#", "##", "###"]
operators = is @(Nonempty List _) [".", "..", "...", "....", "....."]

quantified letter current = current
 `yokl` Forth `ha` Run `ha` Only `ha` letter
 `lu__` push `hv` Unit `ho` that `hv` current

-- TODO: Use a Scrolling Tree instead of a Scrolling List
-- TODO: Put a proper quantified name in each variable slot
positions x = is @Arity x `yo` I `yi` to @(Scrolling List)
 `kyo` unwrap @AR @(Unit `L` Scrolling List `WR` _)

-- I think I should reimplement Scrolling Tree, so we have a Scrolling List as descendants

-- name :: Scrolling List Unit `AR__` State `T'I` Namespace `T'I` Scrolling List Latin
-- name x = State `hv__` Event `hv` switch `hv` by I   `ha_` Scope `hv` at @(Posit

rename :: Scrolling List Latin `AR` Scrolling List Latin
rename x = focus `ho` that `hv` x `hv` by (Only `ha` A)

target :: Scrolling List Latin `AR__` Tree Latin
target x = Construct (Node (by T) (x `yi` rename `yo` intro @Tree `ho` unwrap @AR `yi` to @(Nonempty List) `ho` to @List))

print_subtree subtree = Empty @List `hu` enter @World
 `la` Same `hu` (subtree `yokl` Forth `ha` World `ha` print_tree) `ho'yu` Unit
 `li` unwrap subtree

print_tree = Some `hu_` output `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Space `hv` Unit
 `lo__'yp` Some `hu_` output `ha` Glyph `ha` Symbol `ha` Bracket `ha` Opened `hv` Round
 `lo__'yp` output `ha` Glyph `ha` Letter `ha` Lower `ha` this `ha` top
 `lo__'yp` sub `ho` this `ho` print_subtree
 `lo__'yp` Some `hu_` output `ha` Glyph `ha` Symbol `ha` Bracket `ha` Closed `hv` Round

main = arguments `yo` positions
 `yokl'yokl` Forth `ha` Forth `ha` World `ha` print_tree `ha` target
