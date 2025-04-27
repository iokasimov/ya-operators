module Main where

import Ya
import Ya.ASCII
import Ya.World
import Ya.Console

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
-- TODO: Put a proper quantify name in each variable slot
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
combine ops = ops `lu` Cross ops `yp'yo` by `ha` atop

type Functorial = Tree Name

-- data Variable = Parametric | Positioned

type Parametric = Void
type Positioned = Void `P` Void

-- type Namespace = (Parametric `L` Nonempty List) Unit `P` (Positioned `L` Nonempty List) Unit

type Name = Nonempty List Latin

type Namespace = Nonempty List Unit

name :: (Unit `AR` Latin) `AR___` Nonempty List Unit `AR_` (Name `P` Namespace)
name symbol = (is `ho'yo` symbol) `lo` (push Unit `ho` that)

layer :: Scrolling List Unit `AR__` State `WR_` Namespace `P` Functorial `WR_` Name
layer x = enter @(State `WR_` Namespace `P` Functorial)
 `yuk___` New `ha` State `hv___` Event `hv_` get @Functorial`ha__` Scope `hv` at @Functorial
 `lu__'yp` New (that `hv'he` x `yukl` Forth `ha` New `ha` State `hv__` Event `hv` name I `ha_` Scope `hv` at @Namespace @(Namespace `P` Functorial))
 `yok___` New `ha` State `ha___` Event `ha_` put `ha` inject `ho__'ha` Scope `hv` at @Functorial `ho_` Scope `hv` sub @Tree
 `yuk___` New `ha` State `hv___` Event `hv_` put `hv` [by T] `ha__` Scope `hv` at @Functorial `ho_` Scope `hv` top @Tree

inject :: (Functorial `P` Shafted List Name) `AR__` List `T'I` Functorial
inject (These focused shafted) = Only focused `lu` (shafted `yo` intro @Tree)
 `yi` wrap @AR @(Scrolling List Functorial) `ho` to @(Nonempty List) `ho` to @List

-- initial = wrap [Unit] `lu` wrap [Unit] `lu` intro @Tree [by A]
initial = [Unit] `lu` intro @Tree [by A]

main = combine operators
 `yokl` Forth `ha` World
 `ha__'yuk` World `ha` output `ha` Caret `ha` Newline `hv` Unit
 `ha__` is @(Nonempty List `T'I` Scrolling List Unit)
  `ho_'yokl` Forth `ha` New `ha` layer
  `ho_'he'he'hv` initial
  `ho_` print `ha` that @Functorial `ha` that
