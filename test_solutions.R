library(microbenchmark)
library(tidyverse)


##### prepare sample dataset #####

#number of variables/colums to search
n_variables <- 1000

# number of rows (persons)
n_rows <- 100000

vars <- str_c('var', 1:n_variables)

df <- tibble(
  id = rep(c(1:n_rows), each = n_variables),
  variables = rep(vars, n_rows),
  strings = sample(c('88111', '88222', '88333'), size = n_variables * n_rows, replace = T, prob = c(60/100, 39/100, 1/100))
)

df <- pivot_wider(df, id_cols = id, names_from = variables, values_from = strings)


##### 4 solutions (see StackOverflow-posts) #####

# posters solution: finds all matches, not poperly separated
original_solution <- function() {
  df %>% 
    mutate(across(starts_with('var'),
                  ~ str_match(.x, '(^883.+)')[,1]) %>% 
             mutate(across(everything(), stringi::stri_na2empty)) %>% 
             unite('RESULT', sep = "")) -> dd5
}

# finds all matches and separate with commas
tmfmnk_solution <- function() {
  df %>%
    mutate(result = pmap_chr(across(everything()), ~ toString(c(...)[str_detect(c(...), "^883")]))) 
}

# finds the first match
ThomasIsCoding_solution <- function() {
  inds <- grepl("^883", as.matrix(df))
  df$EXPECTED_RESULT <- replace(
    rep("", nrow(df)),
    rowSums(`dim<-`(inds, dim(df))) > 0,
    as.matrix(df)[inds] 
  )
}

# finds first match, non-matches are NAs
akrun_solution <- function() {
  df %>%
    rowwise %>%
    mutate(result = str_subset(c_across(starts_with('var')), "^883")[1]) %>%
    ungroup
}

##### run benchmark #####

res <- microbenchmark(
  original_solution, 
  tmfmnk_solution,
  ThomasIsCoding_solution,
  akrun_solution,
  times = 1000L)
print(res)
