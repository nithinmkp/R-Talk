library(tidyverse)
library(glue)

## Purrr-Examples
df_function <- function(n) {
  df <- data.frame(A = sample(n), runif(n), runif(n), rbinom(n, 2, .2)) %>% 
    tibble(.name_repair = "unique")
  names(df)[-1] <- sample(LETTERS[-1], 3)
  return(df)
}

set.seed(123)
df_list1 <- 1:5 %>% map(., ~ df_function(5))

# 1) renaming dataframes
df_list1 <- set_names(
  x = df_list1,
  nm = glue("df_{1:5}")
)


# 2) renaming all columns

col_names <- c("m", "n", "o", "l")
df_list1 <- map(
  .x = df_list1,
  .f = ~ set_names(
    x = .x,
    nm = col_names
  )
)

# 3) Rename specific columns
col_indices <- c(2, 4)
col_names <- c("i", "j")
map(
  .x = df_list1,
  .f = function(df) {
    existing_names <- names(x = df)
    existing_names[col_indices] <- col_names
    set_names(
      x = df,
      nm = existing_names
    )
  }
)

rename_fn <- function(df, col_ind = NULL, new_names, colname = NULL) {
  if (!is.null(colname) & !is.null(col_ind)) {
    stop("Either column name or column index only")
  }
  else if (!is.null(colname)) {
    col_ind <- which(names(df) == colname)
  }
  
  
  names(df)[col_ind] <- new_names
  return(df)
}

map(df_list1, ~ rename_fn(.x, col_ind = col_indices, new_names = col_names))
map(df_list1, ~ .x %>% rename_fn(col_ind = col_indices, new_names = col_names))
df_list1 %>% map(.,~rename_with(.,~col_names,c(1,2)))


# 4) merging dataframes
df_list1 %>% reduce(left_join, by = "A")

# 5) Renaming dataframes by name of a column of a dataframe
df_list1 <- df_list1 %>% set_names(map_chr(., ~ names(.x)[3]))

# Renaming columns of a dataframe by a name of dataframe
df_list1<-df_list1 %>% map2(.,names(df_list1),
                            ~rename_fn(.x,col_ind = 2,new_names = .y))


# 6) Conversion of character column to numeric
foo <- list(
  testA = as.data.frame(matrix(rnorm(25), ncol = 5)),
  testB = as.data.frame(matrix(rnorm(25), ncol = 5)),
  testC = as.data.frame(matrix(rnorm(25), ncol = 5))
)

char_fn <- function(df, col_ind) {
  df <- df %>% mutate(across(.cols = col_ind, .fns = as.character))
}

k <- map(foo, ~ .x %>% char_fn(col_ind = c(3, 5)))
map(k, ~ map_lgl(.x, is.numeric))
k <- map(k, ~ .x %>% mutate(across(where(is.character), as.numeric)))

k[]<-map_if(foo,is.character,as.numeric)
## Changing Column names in a vectorised manner

#a)
names_change<-c("Ram","krish","han","yud","arj")

df_list1<-map2(df_list1,names_change,~rename_fn(.x,3,.y))

#b)
nm1<-map(1:5,~paste0(letters[sample(1:26,3)],collapse = ""))
nm2<-map(1:5,~paste0(letters[sample(1:26,3)],collapse = ""))
nm_list<-map2(nm1,nm2,rbind)
df_list1<-map2(df_list1,nm_list,~rename_fn(.x,c(2,4),.y))


## Mutate columns
map(df_list1,~.x %>% mutate(across(2:3,~.x*2,.names = "{.col}_{.fn}")))
