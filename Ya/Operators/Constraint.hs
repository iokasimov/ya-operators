module Ya.Operators.Constraint where

import Ya
import Ya.ASCII
import Ya.World
import Ya.Console

import Ya.Operators.Variance
import Ya.Operators.Namespace

functorial_constraint _ = enter @World
 `yuk___` World `ha` output `ha` Glyph `ha` Symbol `ha` Punctuate `hv` by Space
 -- `yuk___` World (variant v `yokl` Forth `ha` World `ha` output)
 -- `yuk___` World `ha` output `ha` Glyph `ha` Symbol `ha` Punctuate `hv` by Space
 -- `yuk___` World (categories v `yokl` Forth `ha` World `ha` output)

-- categories = is @(Nonempty List ASCII)
--  `ha__` Some `hu` "from from"
--    `la` Some `hu` "from into"

-- variant = is @(Nonempty List ASCII)
--  `ha__` Some `hu` "Contravariant"
--    `la` Some `hu` "Covariant"

-- is_first = is @(Nonempty List ASCII)
--  `ha__` Some `hu` "Yoneda Functor from into"
--    `la` Some `hu` "Endo Semi Functor from"

-- TODO: this is my next challenge!

-- U_I_II = x[x]
-- U_II_I = [x]x

-- U_I_II_III = xx[x]
-- W_III_I_II = x[x]x
-- W_III_II_I = [x]xx

type Constraint = List ASCII

f x = Some `hu_` State `ha` Event `ha` push `ha` Glyph `ha` Letter `ha` Upper `hv` by U
 `lo_'yp` New `ha` positioned `ha` this @(Shafted List `T'I` Name) `ha` other
 `lo_'yp` New `ha` positioned `ha` this @(Singular `T'I` Name) `ha` focus
 `li_` quant I x `he'he'hv` intro @(Nonempty List) Unit `yi` this @(Scrolling List `T'I` _)

positioned name = name
 `yokl'yokl` Forth `ha` Forth `ha` New `ha` State `ha` Event `ha` push `ha` Glyph `ha` Letter `ha` Upper
 `yuk_____` New  `ha` State `ha` Event `ha` push `ha` Glyph `ha` Symbol `ha` Punctuate `hv` by Underscore
