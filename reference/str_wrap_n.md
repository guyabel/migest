# Wrap character string to fit a target number of lines

Inserts line breaks for spaces, where the position of the line breaks
are chosen to provide the most balanced length of each line.

## Usage

``` r
str_wrap_n(string = NULL, n = 2)
```

## Arguments

- string:

  Character string to be broken up

- n:

  Number of lines to break the string over

## Value

The original `string` with line breaks inserted at optimal positions.

## Details

Function is intended for a small number of line breaks. The `n` argument
is not allowed to be greater than 8 as all combinations of possible line
breaks are explored.

When there a number of possible solutions that provide equally balanced
number of characters in each line, the function returns the character
string where the number of spaces are distributed most evenly.

## Examples

``` r
str_wrap_n(string = "a bb ccc dddd eeee ffffff", n = 2)
#> [1] "a bb ccc dddd\neeee ffffff"
str_wrap_n(string = "a bb ccc dddd eeee ffffff", n = 4)
#> [1] "a bb\nccc dddd\neeee\nffffff"
str_wrap_n(string = "a bb ccc dddd eeee ffffff", n = 8)
#> Asking for more lines than words
#> [1] "a\nbb\nccc\ndddd\neeee\nffffff"
str_wrap_n(string = c("a bb", "a bb ccc"), n = 2)
#> [1] "a\nbb"     "a bb\nccc"
```
