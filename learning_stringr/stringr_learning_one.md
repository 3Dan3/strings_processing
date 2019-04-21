stringr 1
================
DanielH
April 8, 2019

-   [concatenating with `str_c()`](#concatenating-with-str_c)
-   [number of characters with `str_length()`](#number-of-characters-with-str_length)
-   [substring with `str sub()`](#substring-with-str-sub)
-   [duplication with `str_dup()`](#duplication-with-str_dup)
-   [padding with `str pad()`](#padding-with-str-pad)
-   [wrapping with `str_wrap()`](#wrapping-with-str_wrap)
-   [trimming with `str trim()`](#trimming-with-str-trim)
-   [word extraction with `word()`](#word-extraction-with-word)

from the book: <https://www.gastonsanchez.com/r4strings/>

``` r
library(tidyverse)
```

The package stringr adds more functionality to the base functions for handling strings in R.

#### concatenating with `str_c()`

``` r
# default usage
str_c("May", "The", "Force", "Be", "With", "You")
```

    ## [1] "MayTheForceBeWithYou"

``` r
# # removing zero length objects
str_c("May", "The", "Force", NULL, "Be", "With", "You", character(0))
```

    ## [1] "MayTheForceBeWithYou"

NOTE: zero length arguments like NULL and character(0) are silently removed by `str_c()`.

#### number of characters with `str_length()`

``` r
# some vector
text <- c("one", "two", "three", NA, "five")

text %>%
  str_count()
```

    ## [1]  3  3  5 NA  4

NOTE: `str_length()` preserves missing values just as `NA`s, plus it converts factors to character

``` r
# some factor
some_factor <- factor(c(1, 1, 1, 2, 2, 2), labels = c("good", "bad"))
some_factor
```

    ## [1] good good good bad  bad  bad 
    ## Levels: good bad

``` r
# count factor levels, implicit conversion to character
some_factor %>%
  str_length() 
```

    ## [1] 4 4 4 3 3 3

### substring with `str sub()`

``` r
# some text
lorem <- "Lorem Ipsum"

# apply str sub
lorem %>%
  str_sub(start = 1, end = 5) 
```

    ## [1] "Lorem"

``` r
# another example
"adios" %>%
  str_sub(1:3)  # this means subset from the first/second/third element on
```

    ## [1] "adios" "dios"  "ios"

``` r
# some strings
resto <- c("brasserie", "bistrot", "creperie", "bouchon")

# str_sub  with negative positions
resto %>%
  str_sub(start = -4, end = -1)
```

    ## [1] "erie" "trot" "erie" "chon"

``` r
resto %>%
  str_sub(start = -5, end = -2)
```

    ## [1] "seri" "stro" "peri" "ucho"

NOTE: We can also give `str sub()` a set of positions which will be recycled over the string. But even better, we can give `str sub()` a negative sequence,

``` r
# extracting sequentially
lorem %>%
  str_sub(seq_len(nchar(.)))
```

    ##  [1] "Lorem Ipsum" "orem Ipsum"  "rem Ipsum"   "em Ipsum"    "m Ipsum"    
    ##  [6] " Ipsum"      "Ipsum"       "psum"        "sum"         "um"         
    ## [11] "m"

``` r
# reverse substrings with negative positions
lorem %>%
  str_sub(-seq_len(nchar(.)))
```

    ##  [1] "m"           "um"          "sum"         "psum"        "Ipsum"      
    ##  [6] " Ipsum"      "m Ipsum"     "em Ipsum"    "rem Ipsum"   "orem Ipsum" 
    ## [11] "Lorem Ipsum"

NOTE: We can use `str sub()` not only for extracting subtrings but also for replacing substrings:

``` r
# replacing 
lorem <- "Lorem Ipsum"
str_sub(lorem, 1, 5) <- "Nullam"
lorem
```

    ## [1] "Nullam Ipsum"

``` r
# equivalently
lorem %>%
  str_sub(1,5) -> "Nullam"
lorem
```

    ## [1] "Nullam Ipsum"

``` r
# replacing with negative positions
lorem <- "Lorem Ipsum"
str_sub(lorem, -1) <- "Nullam"
lorem
```

    ## [1] "Lorem IpsuNullam"

``` r
# multiple replacements
lorem <- "Lorem Ipsum"
str_sub(lorem, c(1, 7), c(5, 8)) <- c("Nullam", "Enim") # means replace position 1:5 and 7:8
lorem
```

    ## [1] "Nullam Ipsum"  "Lorem Enimsum"

### duplication with `str_dup()`

A common operation when handling characters is duplication. `str_dup()` duplicates and concatenates strings within a character vector.

``` r
# default usage
"hola" %>%
  str_dup(3)
```

    ## [1] "holaholahola"

``` r
# use with different times
"adios" %>%
  str_dup(1:3)
```

    ## [1] "adios"           "adiosadios"      "adiosadiosadios"

``` r
# use with a string vector
words <- c("lorem", "ipsum", "dolor", "sit", "amet")

words %>%
  str_dup(2)
```

    ## [1] "loremlorem" "ipsumipsum" "dolordolor" "sitsit"     "ametamet"

### padding with `str pad()`

The idea of `str pad()` is to take a string and pad it with leading or trailing characters to a specified total width.

`str_pad(string, width, side = "left", pad = " ")` default inputs

The default padding character is a space (`pad = " "`), and consequently the returned string will appear to be either left-aligned (`side = "left"`), right-aligned (`side = "right"`), or both (`side = "both"`)

``` r
# default usage
"hola" %>%
  str_pad(width = 7)
```

    ## [1] "   hola"

``` r
# pad both sides
"adios" %>%
  str_pad(width = 7, side = "both")
```

    ## [1] " adios "

``` r
# left padding with #
"hashtag" %>%
  str_pad(width = 8, pad = "#")
```

    ## [1] "#hashtag"

``` r
# pad both sides with -
"hashtag" %>%
  str_pad(width = 9, side = "both", pad = "-")
```

    ## [1] "-hashtag-"

### wrapping with `str_wrap()`

The idea of wrapping a (long) string is to first split it into paragraphs according to the given width, and then add the specied indentation in each line (first line with `indent`, following lines with `exdent`).

Its default usage has the following form:

`str_wrap(string, width = 80, indent = 0, exdent = 0)`

For instance, consider the following quote (from Douglas Adams) converted into a paragraph:

``` r
# quote 
some_quote <-  
  c("I may not have gone",
    "where I intended to go,",
    "but I think I have ended up",
    "where I needed to be")

# some_quote in a single paragraph
some_quote <-
  some_quote %>%
  paste0(collapse = " ")
```

Now, say we want to display the text of some quote within some pre-specified column width (e.g. width of 30). We can achieve this by applying `str_wrap()` and setting the argument `width = 30`

``` r
# display paragraph with width=30
cat(str_wrap(some_quote, width = 30))
```

    ## I may not have gone where I
    ## intended to go, but I think I
    ## have ended up where I needed
    ## to be

Or equivalently

``` r
some_quote %>%
  str_wrap(width = 30) %>%
  cat()
```

    ## I may not have gone where I
    ## intended to go, but I think I
    ## have ended up where I needed
    ## to be

Besides displaying a (long) paragraph into several lines, we may also wish to add some indentation. Here's how we can indent the rst line, as well as the following lines:

``` r
# display paragraph with first line indentation of 2
cat(str_wrap(some_quote, width = 30, indent = 2), "nn")
```

    ##   I may not have gone where I
    ## intended to go, but I think I
    ## have ended up where I needed
    ## to be nn

``` r
# equivalently
some_quote %>%
  str_wrap(width = 30, indent = 2) %>%
  cat("nn")
```

    ##   I may not have gone where I
    ## intended to go, but I think I
    ## have ended up where I needed
    ## to be nn

``` r
# display paragraph with following lines indentation of 3
cat(str_wrap(some_quote, width = 30, exdent = 3), "nn")
```

    ## I may not have gone where I
    ##    intended to go, but I think I
    ##    have ended up where I needed
    ##    to be nn

``` r
# equivalently
some_quote %>%
  str_wrap(width = 30, exdent = 3) %>%
  cat("nn")
```

    ## I may not have gone where I
    ##    intended to go, but I think I
    ##    have ended up where I needed
    ##    to be nn

### trimming with `str trim()`

One of the typical tasks of string processing is that of parsing a text into individual words. Usually, we end up with words that have blank spaces, called whitespaces, on either end of the word. In this situation, we can use the str trim() function to remove any number of whitespaces at the ends of a string. Its usage requires only two arguments:

`str_trim(string, side = "both")`

Consider the following vector of strings, some of which have whitespaces either on the left, on the right, or on both sides.

Here's what `str trim()` would do to them under different settings of side

``` r
# text with whitespaces
bad_text <- c("This", " example ", "has several ", " whitespaces ")

# remove whitespaces on the left side
bad_text %>%
  str_trim(side = "left")
```

    ## [1] "This"         "example "     "has several " "whitespaces "

``` r
# remove whitespaces on the right side
bad_text %>%
  str_trim(side = "right")
```

    ## [1] "This"         " example"     "has several"  " whitespaces"

``` r
# remove whitespaces on both sides
bad_text %>%
  str_trim(side = "both")
```

    ## [1] "This"        "example"     "has several" "whitespaces"

``` r
# default setting
bad_text %>%
  str_trim()
```

    ## [1] "This"        "example"     "has several" "whitespaces"

### word extraction with `word()`

We now describe the `word()` function that is designed to extract words from a sentence.

general form: `word(string, start = 1L, end = start, sep = fixed(" "))`

The way in which we use `word()` is by passing it a string, together with a start position of the first word to extract, and an end position of the last word to extract.

By default, the separator sep used between words is a single space.

Let's see some examples:

``` r
# some sentence
change <- c("Be the change", "you want to be")

# extract first word
change %>%
  word(1)
```

    ## [1] "Be"  "you"

``` r
# extract second word
change %>%
  word(2)
```

    ## [1] "the"  "want"

``` r
# extract last word
change %>%
  word(-1)
```

    ## [1] "change" "be"

``` r
# extract all but the first words
change %>%
  word(2, -1)
```

    ## [1] "the change" "want to be"
