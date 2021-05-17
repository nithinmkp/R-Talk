
# Model Summary Package ---------------------------------------------------

library(modelsummary)
library(gt)
library(kableExtra)


#Fitting the model- simple
mtcars$cyl<-factor(mtcars$cyl)
mod1<-lm(mpg~disp+cyl,data = mtcars)
mod2<-lm(drat~hp+wt,data = mtcars)
models<-list(mod1,mod2)

modelsummary(models)
gof_map2 <- tribble(
        ~raw,      ~clean,          ~fmt,  ~omit,
        "nobs",      "Observations",     0,  FALSE,
        "r.squared", "$R^2$",               3,  FALSE,
        "adj.r.squared","Adj.$R^2$",3,FALSE,
        "AIC","AIC",3,F
)
gof_map2_html <- tribble(
        ~raw,      ~clean,          ~fmt,  ~omit,
        "nobs",      "Observations",     0,  FALSE,
        "r.squared", "R<sup>2</sup>",               3,  FALSE,
        "adj.r.squared","Adj.R<sup>2</sup>",3,FALSE,
        "AIC","AIC",3,F
)


modelsummary(models = models,
             stars = T,
             statistic = "statistic",
             coef_rename =c("disp"="Displacement",
                            "cyl6"= "6-Cylinder",
                            "cyl8"="8-Cylinder",
                            "hp"="Horse Power",
                            "wt"="Kerb Weight"),
             gof_map = gof_map2_html,
             escape=F)


# More Models
url <- 'https://vincentarelbundock.github.io/Rdatasets/csv/HistData/Guerry.csv'
dat <- read.csv(url)

models <- list(
        "OLS 1"     = lm(Donations ~ Literacy + Clergy, data = dat),
        "Poisson 1" = glm(Donations ~ Literacy + Commerce, family = poisson, data = dat),
        "OLS 2"     = lm(Crime_pers ~ Literacy + Clergy, data = dat),
        "Poisson 2" = glm(Crime_pers ~ Literacy + Commerce, family = poisson, data = dat),
        "OLS 3"     = lm(Crime_prop ~ Literacy + Clergy, data = dat)
)

cm <- c('Literacy'    = 'Literacy (%)',
        'Commerce'    = 'Patents per capita',
        '(Intercept)' = 'Constant')
modelsummary(models,stars = T,
             gof_map = gof_map2_html,
             statistic = "statistic",
             coef_map = cm,
             escape=F) # change to "std.error" for standard eroor

#Custom with "gt"
cap <- 'A modelsummary table customized with gt'

tab<-modelsummary(models,stars = T,
                  gof_map = gof_map2_html,
                  statistic = "statistic",
                  coef_map = cm,
                  output = "gt",
                  escape=F,
                  title = cap) %>% fmt_markdown(columns = 1)

tab_gt<-tab %>% 
        
        # column labels
        tab_spanner(label = 'Donations', columns = 2:3) %>%
        tab_spanner(label = 'Crimes (persons)', columns = 4:5) %>%
        tab_spanner(label = 'Crimes (property)', columns = 6) 

tab_gt %>% gtsave("tab2.rtf")


# Custom with "kableExtra"
cap_latex<- 'A modelsummary table customized with latex'
tab1<-modelsummary(models,stars = T,
                   gof_map = gof_map2,
                   statistic = "statistic",
                   coef_map = cm,
                   output = "latex",
                   escape=F,
                   title = cap_latex)
tab1_latex<-tab1%>% 
        
        # column labels
        add_header_above(c(" " = 1, "Donations" = 2, 
                           "Crimes (person)" = 2, "Crimes (property)" = 1)) 


kableExtra::save_kable(tab1_latex, file = "table3.tex")
