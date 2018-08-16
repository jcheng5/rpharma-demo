# Quoting and unquoting

x <- 10


## Example 1: value
x

## Example 2: quoted string
"x"

## Example 3: quoted string, with unquoted value
library(glue)
glue("x == {x}")

## Example 4: quoted string, with substituted value
library(stringr)
glue("x == {x}") %>% str_replace_all("x", "y")

















## Example 1a: value
x

## Example 2a: quoted code
library(rlang)
expr(x)

## Example 3a: quoted code, with unquoted value
expr(x == !!x)

## Example 4a: quoted code, with substituted value
expr(x == !!x) %>% substituteDirect(list(x = expr(y)))
















