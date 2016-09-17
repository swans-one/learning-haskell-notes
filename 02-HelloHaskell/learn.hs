module Learn where

x = 10 * 5 + y
myResult = x * 5
y = 10

-- Indentation matters:
--
-- Miss-indenting the `z =` declaration or the `in` block would be an
-- error.
foo x =
    let y = x * 2
        z = x ^ 2
    in 2 * y * x
