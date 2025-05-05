module Ya.Operators.Constraint where

import Ya
import Ya.ASCII
import Ya.World
import Ya.Console

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

-- render_wrapper = 
--   `lo__'yp` Some `hu_` Await `ha` output `ha` Glyph `ha` Symbol `ha` Bracket `ha` Opened `hv` Round
