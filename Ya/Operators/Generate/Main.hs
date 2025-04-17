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
-- operators = is @(Nonempty List _) [".", "..", "...", "....", "....."]

quantified letter current = current
 `yokl` Forth `ha` Run `ha` Only `ha` letter
 `lu__` push `hv` Unit `ho` that `hv` current

variable = Glyph `ha` Letter `ha` Lower
 `ha__` None `hu` by I `la` Some `hu` by T
 `ha__` sub `ho` this @(List `WR` Tree ASCII)

rename = (top `ho` that `lo` variable) `ho'hd` is

-- impact' :: Scrolling List Unit `AR_`  State (Scrolling Tree Unit) ((Only `P'T'I'TT'I` Shafted List `T'TT'I` Tree) Unit)
-- impact' x = enter @(State `WR` Scrolling Tree Unit)
 -- `yuk__` State `ho` New `hv___` Event `ha` push `ha` stacked `hv` x
  -- `ha__` Scope `hv` it @(Scrolling Tree Unit)
 -- `ho_'he` Scope `hv` at @(Stacked Only Tree List Unit)
 -- `ho_'he'he` Scope `hv` it @(List ((Only `P'T'I'TT'I` Shafted List `T'TT'I` Tree) Unit))

-- stacked :: Scrolling List Unit `AR_______` (Only `P'T'I'TT'I` Shafted List `T'TT'I` Tree) Unit
-- stacked (U_T_I_TT_I (These _ xs)) = U_T_I_TT_I (Only Unit `lu` (xs `yo` intro @Tree @Unit `yi` wrap @AR @((Shafted List `T'TT'I` Tree) Unit)))

-- impact :: Scrolling List Unit `AR_`  Scrolling Tree Unit
-- impact x = let init = is @(Nonempty List Unit) [Unit] `yi` to @(Scrolling List) `ho` to @(Scrolling Tree) in
 -- (impact' x `yuk` New `ha` shift `hv` by (Level `ha`Lift)) `he'he'hv` init `yi` that

-- TODO: Use a Scrolling Tree instead of a Scrolling List
-- TODO: Put a proper quantified name in each variable slot
positions x = is @Arity x
 `yo` Glyph `ha` Letter `ha` Lower `ha` I
 `yi` to @(Scrolling List)
 `kyo` unwrap @AR @(Unit `L` Scrolling List `WR` _)

-- I think I should reimplement Scrolling Tree, so we have a Scrolling List as descendants

-- TODO: Okay, I have tried to use Scrolling Tree, it's very uncomfortable. I'll try to use regular Tree instead

-- TODO: Define traversable instance for Scrolling Tree
main = (arguments `yo` positions)
 `yokl'yokl` Forth `ha` Forth `ha` World `ha` print `ha` to @(Scrolling Tree)