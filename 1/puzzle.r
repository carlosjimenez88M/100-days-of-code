#========================================#
#               Puzzle                   #
#             2022-06-29                 #
#========================================#


## Reference --------------
# URL : https://fivethirtyeight.com/features/can-you-solve-the-vexing-vexillology/
# Note that letters can be repeated. For example, the words GAME and AMALGAM are both acceptable words. 
#Four-letter words are worth 1 point each, while five-letter words are worth 5 points, six-letter words are worth 6 points, seven-letter words are worth 7 points, etc. 
#Words that use all of the seven letters in the honeycomb are known as “pangrams” and earn 7 bonus points (in addition to the points for the length of the word). So in the above example, MEGAPLEX is worth 15 points.

## Libraries  ------

library(tidyverse)
library(tidytext)
library(stringr)
library(reshape2)

##  Data -----
words<-tibble(word = read_lines("https://norvig.com/ngrams/enable1.txt"))


## honeycomb -----

honeycomb<- c("a","p","l","e","m","x")

## Filter by letter G -----
 
focus_words<-words %>%
  filter(str_length(word) >=4,
         str_detect(word,"g")) %>%
  mutate(letters = str_split(word,""),
         not_focus_letters = map(letters,
                                 setdiff,
                                 c(honeycomb,"g")),
         unique_letters =  map_int(letters,n_distinct))
  
  
# Four-letter words are worth 1 point each, while five-letter words are worth
# 5 points, six-letter words are worth 6 points, seven-letter words are worth 7 points, etc.

## Not use S in permutation ------

win_words<-focus_words %>%
  mutate(points =  ifelse (str_length(word)==4,1,str_length(word)) 
         + 15 * (unique_letters==7)) %>%
  filter(unique_letters<=7) %>%
  arrange(desc(points))

## Filter Letter rank --------


letters_rank <-win_words %>%
    select(word,points) %>%
    unnest_tokens(letter,
                  word,
                  token = 'characters',
                  drop = FALSE) %>%
    distinct(word,
             letter,
             .keep_all = TRUE) %>%
    group_by(word,letter) %>%
    summarize(n_words = n(),
              n_points = sum(points)) %>%
    arrange(desc(n_points))


pattern_matrix<-letters_rank %>%
  acast(word ~ letter, fun.aggregate = length)




focus_letters<-as.integer(colnames(pattern_matrix) %in% c(honeycomb,"g"))

forbin <- pattern_matrix%*%(1-focus_letters)

pattern_matrix[forbin ==0 & pattern_matrix[,"g"]==1, ]



### Organize steps 



get_words <- function(focus_letter,other_letters) {
  words %>%
    filter(str_length(word) >=4,
           str_detect(word,focus_letter)) %>%
    mutate(letters = str_split(word,""),
           not_focus_letters = map(letters,
                                   setdiff,
                                   c(other_letters,focus_letter)),
           unique_letters =  map_int(letters,n_distinct)) %>%
    mutate(points =  ifelse (str_length(word)==4,1,str_length(word)) 
           + 15 * (unique_letters==7)) %>%
    filter(lengths(not_focus_letters) == 0) %>%
    filter(unique_letters<=7)
    
 
}



get_score <- function(honeycomb_letters) {
  points_per_word <- win_words$points
  names(points_per_word) <- win_words$word
  points_per_word <- points_per_word[rownames(pattern_matrix)]
  "g" <- honeycomb_letters[1]
  focus_letters<-colnames(pattern_matrix) %in% c(honeycomb,"g")
  forbidden <- pattern_matrix %*% (1L - focus_letters)
  word_permitted <- forbidden == 0L & pattern_matrix[, 'g'] == 1L
  sum(points_per_word[word_permitted])
}


get_score(c("e", "i", "a", "r", "n", "t", "l"))
get_words("e", c("i", "a", "r", "n", "t", "l"))



center_letter <- "e"
find_best_combination <- function(center_letter, possible_letters) {
  points_per_word <- win_words$points
  names(points_per_word) <- win_words$word
  points_per_word <- points_per_word[rownames(pattern_matrix)]
  good_letter_combinations <- combn(possible_letters, 6)
  forbidden_matrix <- 1L - apply(good_letter_combinations,
                                 2,
                                 function(.) colnames(pattern_matrix) %in% c(center_letter, .))
  
  filtered_word_matrix <- pattern_matrix[pattern_matrix[, center_letter] == 1, ]
  word_allowed_matrix <- filtered_word_matrix %*% forbidden_matrix == 0
  scores <- t(word_allowed_matrix) %*% points_per_word[rownames(word_allowed_matrix)]
  
  list(center_letter = center_letter,
       other_letters = good_letter_combinations[, which.max(scores)],
       score = max(scores))
}


best_combination <- function(difference,num){
  dinamic_pool<-letters_rank$letter %>%
    head(num)
  return(find_best_combination("e",setdiff(dinamic_pool,difference)))
}

for(i in c('a','e','i','o','u')){
  print(best_combination(i,7))
}



