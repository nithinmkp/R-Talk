# Regresions

library(gapminder)
library(tidyverse)
library(broom)
library(kableExtra)
library(flextable)
library(dotwhisker)

data <- gapminder::gapminder

#Regression-Normal way- Full sample
lm(pop~gdpPercap,data=data)
summary(lm(pop~gdpPercap,data=data))

#Regression country

lm(pop~gdpPercap,data = data,subset = data$country=="India")

# If you want for all countries
reg_countries<-list()
for(i in data$country){
  reg_countries[i]<-list(summary(lm(pop~gdpPercap,data = data,subset = data$country==i)))
}

# Map Technique
nest_data <- data %>%
  group_by(country) %>%
  nest()

nest_mod <- nest_data %>%
  mutate(model = map(.x = data, ~ lm(lifeExp ~ pop, data = .x)))
nest_coef1 <- nest_mod %>%
  mutate(coef = map(model, ~ tidy(.x))) %>%
  unnest(coef) %>%
  select(term, estimate, p.value)



countrywise <- split(nest_coef1, nest_coef1$country)


tab1 <- map(countrywise, ~.x %>% ungroup() %>% select(-country))
tab1 <- map(tab1, ~ .x %>% mutate(across(where(is.double), formatC,
  digits = 3,
  format = "f",
  width = 3
)))

# Word output
names_country <- names(countrywise)

library(rtf)
tabs <- RTF(file = "tables.rtf")
table_fn <- function(dest, x, y) {
  addParagraph(dest, x, "\n")
  addTable(dest, y,
    header.col.justify = c("L", "C", "C"),
    col.justify = c("L", "C", "C")
  )
  addParagraph(dest, "\n")
}

walk2(names_country, tab1, ~ table_fn(tabs, .x, .y))
done(tabs)

#mtcars data
mtcars

t1<-mtcars %>% group_by(cyl) %>% group_modify(.f= ~tidy(lm(mpg~disp,data = .x)))
cyls<-unique(t1$cyl)

term_un<-unique(t1$term)
t2<-t1 %>% pivot_wider(names_from = c(cyl),names_repair = "minimal",
                     values_from = c("estimate","p.value"),-c(std.error,statistic),
                     names_glue = "{cyl}_{.value}") %>% 
  select(starts_with(paste(cyls))) %>% bind_cols(term=term_un) %>% 
  relocate(term)

# Time Series
library(tseries)
install.packages(c("tidyverse","tseries"))
data("USeconomic")
adf.test(USeconomic[, 1]) # One way

#For-loops
test_adf <- list()
for (i in 1:ncol(USeconomic)) {
  test_adf[i] <- list(adf.test(USeconomic[, i])) # Second
}

#Efficient-Apply
lapply(USeconomic, function(x) {
  list(
    adf.test(x) %>% tidy(),
    pp.test(x) %>% tidy()
  ) %>% bind_rows()
})

source("unitroots.R")
tab_unitroot_main <- unitroot_fn(data.frame(USeconomic))
varnames <- rep(c("Statistic", "P-Value", "Lag Length"), 2)
rename_fn <- function(df, col_ind, new_names) {
  names(df)[col_ind] <- new_names
  return(df)
}

#Publication Tables
tab_adj<-rename_fn(tab_unitroot_main,-1,varnames)


kbl(tab_adj, booktabs = T) %>%
  kable_classic(latex_options = "scale_down") %>%
  add_header_above(c(" " = 1, "ADF Test" = 3, "PP Test" = 3)) %>%
  save_kable("table.jpg")

tab_flex <- tab_unitroot_main %>% flextable()
tab_flex_2 <- tab_flex %>%
  add_header_row(
    colwidths = c(1, 3, 3),
    values = c(" ", "ADF Test", "PP Test")
  ) %>%
  align(i = 1, part = "header", align = "center") %>%
  theme_booktabs() %>%
  merge_h(part = "header", i = 1) %>%
  vline(j = c(1, 4, 7), border = fp_border_default()) %>%
  merge_v(part = "header", j = 1)
save_as_docx(tab_flex_2, path = "tabe3.docx")


# Complete TS
data <- data.frame(USeconomic)


vars <- names(data)

x <- ts_fn(data, vars)

y <- map(x, unitroot_fn) %>% map_df(bind_rows)



