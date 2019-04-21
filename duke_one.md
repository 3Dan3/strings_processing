duke
================
DanielH
April 10, 2019

-   [data exploration](#data-exploration)
-   [Using regex to clean data](#using-regex-to-clean-data)
-   [a recap of the problem, and a fix](#a-recap-of-the-problem-and-a-fix)
-   [from this point on it doesn't work](#from-this-point-on-it-doesnt-work)
-   [putting all together](#putting-all-together)

data exploration
----------------

``` r
beauty %>%
  str()
```

    ## Classes 'spec_tbl_df', 'tbl_df', 'tbl' and 'data.frame': 697 obs. of  2 variables:
    ##  $ person: chr  "Narrator" "Belle" "Townsfolk 1" "Townsfolk 2" ...
    ##  $ line  : chr  "Once upon a time, in a faraway land, a young prince lived in a;shining castle.  Although he had everything his "| __truncated__ "Little town, it's a quiet village;Every day, like the one before;Little town, full of little people;Waking up to say...;" "Bonjour!" "Bonjour!" ...

We have two character vars, `person` and `line`

``` r
beauty %>%
  count(person, sort = T)
```

    ## # A tibble: 58 x 2
    ##    person         n
    ##    <chr>      <int>
    ##  1 Belle        137
    ##  2 Beast         79
    ##  3 Lumiere       67
    ##  4 Gaston        66
    ##  5 Maurice       66
    ##  6 Cogsworth     60
    ##  7 Mrs. Potts    43
    ##  8 Lefou         35
    ##  9 Chip          24
    ## 10 All           16
    ## # ... with 48 more rows

### how many times do the main characters speak?

First of all we should count hte times the character's name is mentioned in the text

There are many ways to answer that questions. We've already seen one (right above) using `count()`. However here we want to use string manipualtion functions form the stringr package.

Let'say we want to count the times Gaston|Belle speaks

``` r
# count Gaston
beauty %>%
  pull(person) %>% 
  str_detect("Gaston") %>%
  sum()
```

    ## [1] 66

``` r
# count Belle
beauty %>%
  pull(person) %>%
  str_detect("Belle") %>%
  sum()
```

    ## [1] 137

That doesn’t tell us anything about how much time each of them speaks, since we are just counting numbers of uninterrupted lines.

Let’s first remove the semicolon separators and count the total number of characters(nchars) each person has

``` r
# remove semicolon
beauty %>%
  mutate(line = str_replace_all(line, ";", " ")) %>%
  group_by(person) %>%
  summarize(tot_char = sum(nchar(line))) %>%
  filter(str_detect(person, "Gaston"))
```

    ## # A tibble: 1 x 2
    ##   person tot_char
    ##   <chr>     <int>
    ## 1 Gaston     4903

We now want to extract some type of names using regex

``` r
# filter for names containing numbers
beauty %>%
  filter(str_detect(person, "[0-9]")) %>%
  pull(person) %>%
  unique()
```

    ##  [1] "Townsfolk 1" "Townsfolk 2" "Townsfolk 3" "Townsfolk 4" "Townsfolk 5"
    ##  [6] "Woman 1"     "Woman 2"     "Woman 3"     "Woman 4"     "Man 1"      
    ## [11] "Woman 5"     "Man 2"       "Man 3"       "Man 4"       "Man 5"      
    ## [16] "Man 6"       "Group 1"     "Group 2"     "Bimbette 1"  "Bimbette 2" 
    ## [21] "Bimbette 3"  "Crony 1"     "Crony 2"     "Crony 3"

``` r
# matches names containing punctuations
beauty %>%
  filter(str_detect(person, "[[:punct:]]+")) %>%
  pull(person) %>%
  unique()
```

    ## [1] "Mrs. Potts"

We Can see that we could do some replacements by grouping some person names in order to improve the readability

``` r
beauty %>%
  pull(person) %>%
  #str_subset("Bimbet|Group|Man|All|Both|Cron") %>%
  unique() %>%
  str_sort()
```

    ##  [1] "All"           "Arque"         "Baker"         "Barber"       
    ##  [5] "Beast"         "Belle"         "Bimbette 1"    "Bimbette 2"   
    ##  [9] "Bimbette 3"    "Bimbettes"     "Bookseller"    "Both"         
    ## [13] "Bystanders"    "Chip"          "Chorus"        "Cogsworth"    
    ## [17] "Crony 1"       "Crony 2"       "Crony 3"       "Driver"       
    ## [21] "Featherduster" "Gaston"        "Group 1"       "Group 2"      
    ## [25] "Lefou"         "Lumiere"       "Man"           "Man 1"        
    ## [29] "Man 2"         "Man 3"         "Man 4"         "Man 5"        
    ## [33] "Man 6"         "Maurice"       "Men"           "Merchant"     
    ## [37] "Mob"           "Mrs. Potts"    "Mugs"          "Narrator"     
    ## [41] "Objects"       "Old Cronies"   "Pierre"        "Prince"       
    ## [45] "Stove"         "Townsfolk"     "Townsfolk 1"   "Townsfolk 2"  
    ## [49] "Townsfolk 3"   "Townsfolk 4"   "Townsfolk 5"   "Wardrobe"     
    ## [53] "Woman 1"       "Woman 2"       "Woman 3"       "Woman 4"      
    ## [57] "Woman 5"       "Wrestler"

We use regex to define the pattern of the perosn names we want to replace with a grouping name

``` r
beauty_processed <-
  beauty %>%
  mutate(person = str_replace_all(person, 
                                  c("Townsfolk[\\s0-9]*" = "Townsfolk",
                                    "Bimbettes?[\\s0-9]*|Woman[\\s0-9]*" = "Woman",
                                    "Crony[\\s0-9]*|Old Cronies" = "Crony",
                                    "Group[\\s0-9]*|Bystanders|All|Both|Mugs|Mob|Chorus" = "Group",
                                    "M[ae]n[\\s0-9]*" = "Man")))


beauty_processed  %>% 
  pull(person) %>%
  unique() %>%
  str_sort()
```

    ##  [1] "Arque"         "Baker"         "Barber"        "Beast"        
    ##  [5] "Belle"         "Bookseller"    "Chip"          "Cogsworth"    
    ##  [9] "Crony"         "Driver"        "Featherduster" "Gaston"       
    ## [13] "Group"         "Lefou"         "Lumiere"       "Man"          
    ## [17] "Maurice"       "Merchant"      "Mrs. Potts"    "Narrator"     
    ## [21] "Objects"       "Pierre"        "Prince"        "Stove"        
    ## [25] "Townsfolk"     "Wardrobe"      "Woman"         "Wrestler"

### how rich is each character's vocabulary?

##### Counting unique words

We want one unique word count for each character, not each line. There are several ways to do it. An example:

``` r
beauty_processed %>%
  filter(person %in% c("Townsfolk", "Crony")) %>% 
  group_by(person) %>% 
  summarize(line = str_c(line, collapse = "; "),
            num_lines = n(),
            num_words = str_count(line, "[\\w']+"),
            words_unique = str_extract_all(line, "[\\w']+") %>% map_int(~length(unique(.x)))) %>%
  ungroup() %>%
  select(-line)
```

    ## # A tibble: 2 x 4
    ##   person    num_lines num_words words_unique
    ##   <chr>         <int>     <int>        <int>
    ## 1 Crony             8        55           43
    ## 2 Townsfolk         7        29           23

**NOTE**

Here we've decided to handle contractions as one word. This is a good example of when a seemingly small change in our regular expression makes a meaningful difference. `it'`s as a single word

We used this:

``` r
"Little town, it's a quiet village;Every day, like the one before;Little town, full of little people;Waking up to say...;" %>%
  str_extract_all("[\\w']+")
```

    ## [[1]]
    ##  [1] "Little"  "town"    "it's"    "a"       "quiet"   "village" "Every"  
    ##  [8] "day"     "like"    "the"     "one"     "before"  "Little"  "town"   
    ## [15] "full"    "of"      "little"  "people"  "Waking"  "up"      "to"     
    ## [22] "say"

not this, where `it's` gets split into two parts

``` r
"Little town, it's a quiet village;Every day, like the one before;Little town, full of little people;Waking up to say...;" %>%
  str_extract_all("\\w+")
```

    ## [[1]]
    ##  [1] "Little"  "town"    "it"      "s"       "a"       "quiet"   "village"
    ##  [8] "Every"   "day"     "like"    "the"     "one"     "before"  "Little" 
    ## [15] "town"    "full"    "of"      "little"  "people"  "Waking"  "up"     
    ## [22] "to"      "say"

Using regex to clean data
-------------------------

The data frame above was already set up for us. But the file started as a plain text document on a website.

Let’s rewind and show how string manipulation lets us take a slightly messy dataset and turn it into the data frame we used above.

``` r
beauty <- read_lines('http://www.fpx.de/fp/Disney/Scripts/BeautyAndTheBeast.txt')

# check
beauty %>%
  head(10) %>%
  str_subset("[:upper:]{2,}") %>%
  str_to_lower()
```

    ## [1] "narrator:     once upon a time, in a faraway land, a young prince lived in a"

``` r
beauty %>%
  str_extract_all("[:upper:]{2,}") %>% flatten_chr() %>% unique() %>% str_to_title()
```

    ##  [1] "Narrator"       "Beast"          "Belle"          "Townsfolk"     
    ##  [5] "Baker"          "Woman"          "Barber"         "Driver"        
    ##  [9] "Merchant"       "Man"            "Bookseller"     "Men"           
    ## [13] "Women"          "All"            "Geese"          "Lefou"         
    ## [17] "Gaston"         "Bimbettes"      "Group"          "Bimbette"      
    ## [21] "Maurice"        "Machine"        "Phillipe"       "Wolves"        
    ## [25] "Lumiere"        "Cogsworth"      "Ow"             "Ouch"          
    ## [29] "Footstool"      "Coatrack"       "Mrs"            "Potts"         
    ## [33] "Chip"           "Pov"            "Minister"       "Others"        
    ## [37] "Pierre"         "Featherduster"  "Pallenquin"     "Old"           
    ## [41] "Cronies"        "Wrestler"       "Crony"          "Guess"         
    ## [45] "Both"           "Wardrobe"       "What"           "Objects"       
    ## [49] "Starve"         "Magic"          "Mirror"         "Stove"         
    ## [53] "Music"          "Chair"          "China"          "Flatware"      
    ## [57] "Mugs"           "Candlesticks"   "Silverware"     "Plates"        
    ## [61] "Featherdusters" "Suits"          "Of"             "Armor"         
    ## [65] "Get"            "Out"            "Wolf"           "Monsieur"      
    ## [69] "Arque"          "Bystanders"     "Crowd"          "Orderlies"     
    ## [73] "Mob"            "Prince"         "Chorus"

**Goal**: A data frame with one row per line of dialogue, with a column for the dialogue text and a column for the speaker name.

Plan ideas:

We can’t make a one-column data frame from our vector then split it into two columns using the tidyr package, since we do not have each cell representing a new line of dialogue.

The basic plan will be to collapse the entire script into a single string, extract each new line of dialogue as its own cell in a vector, using the distinct structure of the dialogue identifiers, clean up little issues along the way

``` r
# read in the raw text file , each line is an entry in a vector, skip first 6 rows
beauty <- 
  read_lines('http://www.fpx.de/fp/Disney/Scripts/BeautyAndTheBeast.txt', skip = 6)

# collapse each line into a single string separate lines by a ;
beauty <- 
  beauty %>%
  str_trim(side = "both") %>%
  paste(collapse = ";")
```

------------------------------------------------------------------------

``` r
test <-
  "I don't usually leave the asylum in the middle of the night, but they said you'd make it worth my while.
GASTON: It's like this.  I've got my heart set on marrying Belle, but she needs a little persuasion."
```

> How do we extract everything before the pattern `GASTON`? Using what’s called a look-ahead.

**Look-ahead**

positive look-ahead: `pattern1(?=pattern2)` will match pattern1 only when it is followed by pattern2

negative look-ahead: `pattern1(?!pattern2)` will match pattern1 when it is NOT followed by pattern2

positive and negative look-behinds are similar, coded with `(?<=pattern2)pattern1` and `(?<!pattern2)pattern1` respectively

##### simple example

Let start with something simpler than our case. The expression below matches candle only when it is followed by the phrase stick.

``` r
# candel when followed by stick
c("candlestick", "candlemaker", "smart candle") %>%
  str_detect("candle(?=stick)")
```

    ## [1]  TRUE FALSE FALSE

``` r
# candle followed by some punctuation
c("candle!!?", "candlemaker", "smart candle%") %>%
  str_detect("candle(?=[[:punct:]]+)")
```

    ## [1]  TRUE FALSE  TRUE

Look-behinds are similar, except that you can’t use the asterisk or plus quantifiers (which match as large a string as you want) for the pattern in parentheses.

``` r
c("candle!!?", "candlemaker", "smart candle%") %>%
  str_extract("(?<=[a-z]{1,10})[[:punct:]]+")
```

    ## [1] "!!?" NA    "%"

------------------------------------------------------------------------

Back to the question:

**How do we extract everything before GASTON: in the test phrase above? Use the catch-all.**

``` r
test %>%
  str_extract(".+(?=GASTON:)") %>%
  str_trim()
```

    ## [1] NA

### problems with a more realistic example

Let’s use an example closer to what we will see in our data. We will need all dialogue between character names. Now there is a problem:

We have to change the look-ahead statement to get any character name, of variable lengths, some with punctuation and spaces. The pattern `[A-Z\\s[:punct:]]+[:]` will match that

``` r
test <- "BEAST: What are you doing here? MAURICE: Run, Belle! BEAST: The master of this castle. BELLE: I've come for my father.  Please let him out!  Can't you see he's sick? BEAST: Then he shouldn't have trespassed here."
```

``` r
test %>%
  str_extract(".+(?=[A-Z\\s[:punct:]]+:)")
```

    ## [1] "BEAST: What are you doing here? MAURICE: Run, Belle! BEAST: The master of this castle. BELLE: I've come for my father.  Please let him out!  Can't you see he's sick? BEAS"

It returned almost everything and missed the final bit of dialogue. Here’s why:

-   The function checks each character to see if it matches . (everything does) and is followed by the look-ahead expression.
-   If both of those are true, it returns the match to the pattern we gave it before the look-ahead statement—in this case everything there is.
-   When it got to the letter S in the last case of BEAST:, the look-ahead statement matched T: and returned everything else before it.

Here are some other failed attempts at getting what we want, to show other things you can do:

matching only lower cases, spaces and punctuation in the first pattern fails to pick up the capital letters starting sentences and proper names in the dialogue.

``` r
test %>%
  str_extract_all("[a-z[:punct:]\\s]+(?=[A-Z\\s[:punct:]]+:)")
```

    ## [[1]]
    ## [1] "hat are you doing here? "   "elle! "                    
    ## [3] "he master of this castle. " "an't you see he's sick? "

Matching specific punctuation allowing upper cases in patterns NOT followed by the character naming pattern returns nicer chunks but leaves the character identifiers in the preceding string. We want to be able to extract the separately.

``` r
test %>%
  str_extract_all("[A-z;.,'!?\\s]+(?![A-Z\\s[:punct:]]+:)") %>%
  modify_depth(1, str_trim)
```

    ## [[1]]
    ## [1] "BEAST"                                                                        
    ## [2] "What are you doing here? MAURICE"                                             
    ## [3] "Run, Belle! BEAST"                                                            
    ## [4] "The master of this castle. BELLE"                                             
    ## [5] "I've come for my father.  Please let him out!  Can't you see he's sick? BEAST"
    ## [6] "Then he shouldn't have trespassed here."

Notice we put specific punctuation in excluding the colon. But colons in our data do show up outside of person identifiers, so we can’t do that either.

a recap of the problem, and a fix
---------------------------------

Our basic problem is that the person identifiers are not good string separation criteria because they have too much in common with the rest of the text. That makes it difficult to use the look-ahead or other means for splitting the dataset in the way we want.

But the person identifiers are still distinct enough that we can match them—which means **we can replace them with identifiers that are different enough from the dialogue to be good split criteria**.

Extracting the person identifiers, *adding some bogus lines to show this works for character names with punctuation and numbers*:

``` r
test <- "BEAST: What are you doing here? MAURICE: Run, Belle! BEAST: The master of this castle. BELLE: I've come for my father.  Please let him out!  Can't you see he's sick? BEAST: Then he shouldn't have trespassed here. TOWNSFOLK 2: He's a monster! MRS. POTTS: Now pipe down!"

str_extract_all(test, "[A-Z]+[\\s0-9[:punct:]]*:|MRS. POTTS:") %>%
  flatten_chr()
```

    ## [1] "BEAST:"       "MAURICE:"     "BEAST:"       "BELLE:"      
    ## [5] "BEAST:"       "TOWNSFOLK 2:" "MRS. POTTS:"

As we can see, to keep the MRS. in MRS. POTTS we have to handle it separately, with a regular expression or statement.

We now replace person identifiers:

``` r
test %>%
  str_replace_all("[A-Z]+[\\s0-9[:punct:]]*:|MRS. POTTS:", "&&&&&&&&")
```

    ## [1] "&&&&&&&& What are you doing here? &&&&&&&& Run, Belle! &&&&&&&& The master of this castle. &&&&&&&& I've come for my father.  Please let him out!  Can't you see he's sick? &&&&&&&& Then he shouldn't have trespassed here. &&&&&&&& He's a monster! &&&&&&&& Now pipe down!"

### new plan

Our new plan is:

-   **replace each character identifier with a unique id** that allows us to separate the text more easily using look-ahead expressions

-   **extract the identifiers and the dialogue in between them**, giving two equal-length vectors matching speakers to what they say

-   **create a data frame** with one column for speaker name and one for dialogue

-   **match the ids back to the character names**

#### replacing multiple items using lists

A useful feature of `str_replace_all` is that you can pass it a list of things to match and to replace.

``` r
test %>%
  str_replace_all(c("BEAST:" = "001>", "MAURICE:" = "002>", "BELLE:" = "003>"))
```

    ## [1] "001> What are you doing here? 002> Run, Belle! 001> The master of this castle. 003> I've come for my father.  Please let him out!  Can't you see he's sick? 001> Then he shouldn't have trespassed here. TOWNSFOLK 2: He's a monster! MRS. POTTS: Now pipe down!"

Alternatively, we could create a list object whose names are the patterns we want to replace and whose entries are the things we want to replace them with. Then we pass that to the function.

``` r
# extract characters
codes <- 
  test %>%
  str_extract_all("[A-Z]+[\\s0-9[:punct:]]*:|MRS. POTTS:") %>%
  flatten_chr() %>%
  unique()

# list of code names to use for replacement
codes_list <-
  seq(from = 100, to = 100 + length(codes) - 1) %>%
  paste0(">") %>%
  as.list() %>%
  set_names(codes)

# replacement
test %>%
  map_chr(~str_replace_all(.x, codes_list %>% as_vector())) -> test2
```

And that gives us what we want

``` r
# Dialogue
str_extract_all(test2, "[A-z[:punct:][:space:]]+(?![0-9]{3}>)") 
```

    ## [[1]]
    ## [1] " What are you doing here?"                                               
    ## [2] " Run, Belle!"                                                            
    ## [3] " The master of this castle."                                             
    ## [4] " I've come for my father.  Please let him out!  Can't you see he's sick?"
    ## [5] " Then he shouldn't have trespassed here."                                
    ## [6] " He's a monster!"                                                        
    ## [7] " Now pipe down!"

from this point on it doesn't work
----------------------------------

putting all together
--------------------

``` r
# extract characters
codes <- 
  beauty %>%
  str_extract_all("[A-Z]+[\\s0-9[:punct:]]*:|MRS. POTTS:") %>%
  flatten_chr() 

# list of code names to use for replacement


codes_list <-
  seq(from = 100, to = 100 + length(codes) - 1) %>%
  paste0(">") %>%
  as.list() %>%
  set_names(codes)

# replacement
tibble(person = codes %>% str_replace_all(":", ""),
       line = str_extract_all(beauty, "[A-z[:punct:][:space:]]+(?![0-9]{3}>)", simplify = T))



str_extract_all(beauty, "[A-z[:punct:][:space:]]+(?![0-9]{3}>)", simplify = T) %>% as_tibble()
```

So

``` r
test <- "BEAST: What are you doing here? MAURICE: Run, Belle! BEAST: The master of this castle. BELLE: I've come for my father.  Please let him out!  Can't you see he's sick? BEAST: Then he shouldn't have trespassed here. TOWNSFOLK 2: He's a monster! MRS. POTTS: Now pipe down!"

str_replace_all(test, c("BEAST:" = "001>", "MAURICE:" = "002>", "BELLE:" = "003>"))

# little annoying or statement for MRS. POTTS. Can't rearrage the previous statement without getting more
codes <- unique(str_extract_all(beauty, "[A-Z]+[\\s0-9[:punct:]]*:|MRS. POTTS:|OLD CRONIES:")[[1]])
codes_list <- as.list(paste0(seq(from = 100, to = 100 + length(codes) - 1), ">"))
names(codes_list) <- codes

beauty <- str_replace_all(beauty, codes_list) %>%
  # final line ends with this
  str_replace("</pre>", "") %>%
  # first removing the scene descriptions between two brackets
  str_replace_all("(\\(){1}[A-Za-z0-9\\s,.;:'!?]*(\\)){1}", "") %>%
  # now getting the dangling brackets with open ends at the end of the line or dangling brackets
  str_replace_all("(\\(){1}[A-Za-z0-9\\s,.;:'!?]*(\\))*", "")

beauty <- data.frame(person = str_extract_all(beauty, "[0-9]{3}>")[[1]],
                     line = str_extract_all(beauty, "[A-z[:punct:][:space:]]+(?![0-9]{3}>)")[[1]])

# Now switch the codes back to names and clean up a little
names(codes) <- unlist(codes_list)
codes <- as.list(codes)
beauty$person <- str_replace_all(beauty$person, codes) %>% str_replace_all(":", "")
beauty$line <- str_trim(beauty$line, side = "both") %>% str_replace_all(";+", ";")

tail(beauty)
```

Next, look at the lab: <https://idc9.github.io/stor390/labs/6/strings_lab.html>

------------------------------------------------------------------------

#### extra cazzeggio

``` r
test <- "BEAST: What are you doing here? MAURICE: Run, Belle! BEAST: The master of this castle. BELLE: I've come for my father.  Please let him out!  Can't you see he's sick? BEAST: Then he shouldn't have trespassed here. TOWNSFOLK 2: He's a monster! MRS. POTTS: Now pipe down!"


test %>%
  str_extract_all("[A-Z]+[\\s0-9[:punct:]]*:|MRS. POTTS:")


# define wrangling function
cleaner <- function(x) {
  x %>% 
  map(~str_replace_all(.x, ":", " ")) %>%
  map(str_trim) %>%
  flatten_chr() %>%
  unique() %>%
  str_to_title() %>%
  str_sort()
 
}


# call function
beauty %>%
  str_extract_all("[A-Z]+[\\s0-9[:punct:]]*:|MRS. POTTS:") %>%
  cleaner()
```

``` r
beauty %>% str_extract_all("COGSWORTH|BELLE") %>% modify_depth(1, ~str_detect(.x,"BEL") %>% sum)
beauty %>% str_extract_all("COGSWORTH|BELLE") %>% modify_depth(1, ~str_detect(.x,"COGS") %>% sum)



beauty %>% str_extract_all("COGSWORTH|BELLE") %>% modify_depth(1, length)


tibble(cogswort = beauty %>% 
         str_extract_all("COGSWORTH|BELLE") %>%
         modify_depth(1, ~str_detect(.x,"COGS") %>% sum) %>%
         as_vector,
       belle = str_extract_all("COGSWORTH|BELLE") %>%
         modify_depth(1, ~str_detect(.x,"BEL") %>% sum) %>%
         as_vector)



tibble(belle = beauty %>% 
         str_extract_all("COGSWORTH|BELLE") %>%
         modify_depth(1, ~str_detect(.x,"BEL") %>% sum) %>%
         as_vector,
       cogswort = beauty %>% 
         str_extract_all("COGSWORTH|BELLE") %>%
         modify_depth(1, ~str_detect(.x,"COGS") %>% sum) %>%
         as_vector,
       total = belle + cogswort)  %>%
  mutate(" " = "occurrencies") %>%
  select(4, everything())
```

#### look at the lab: <https://idc9.github.io/stor390/labs/6/strings_lab.html>
