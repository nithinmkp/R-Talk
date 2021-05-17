library(gapminder)
library(tidyverse)
library(dotwhisker)


data(gapminder)
dir.create("ggplots")

plot_fn <- function(country, coef) {
        coef %>% ggplot(aes(x = term, y = estimate)) +
                geom_col() +
                labs(title = country)+
                theme_bw()
}

gapminder %>%
        group_by(country, continent) %>%
        nest() %>%
        mutate(model = map(.x = data, ~ lm(lifeExp ~ log(pop), data = .x))) %>%
        mutate(coef = map(model, ~ tidy(.x))) %>%
        ungroup() %>%
        select(country, coef) %>%
        mutate(coef = map(.x = coef, function(x) {
                x$term[which(x$term == "(Intercept)")] <- "Constant"
                return(x)
        })) %>%
        mutate(plot = map2(country, coef, plot_fn)) %>%
        head() %>%
        mutate(filename = paste0("ggplots/", country, ".png")) %>%
        select(-coef, -country) %>%
        pwalk(ggsave)




# Another example

nest_list <- nest_mod %>%
        mutate(coef = map(model, ~ tidy(.x))) %>%
        unnest(coef)
mods <- split(nest_list, nest_list$country) %>%
        map(~ .x %>% select(-c(1:3))) %>%
        head()


map2(mods, names(mods), ~ dwplot(.x, show_intercept = T) +
             theme_bw() + theme(legend.position = "none") +
             ggtitle(.y))

nest_mod %>% head() %>% ungroup() %>% mutate(
        plot=map2(model,country,~dwplot(.x,show_intercept = T)+theme_bw()+
                          theme(legend.position = "none")+
                          ggtitle(.y))
) %>% 
        mutate(filename = paste0("ggplots/", country,"_dw",".png")) %>% 
        select(plot,filename) %>% 
        pwalk(ggsave)

map(nest_mod$model,confint) %>% set_names(nest_mod$country) %>% head() %>% 
        map_if(is.numeric,round,3)


# Correlation Plots

library(ggpubr)
library(corrr)
library(ggcorrplot)
library(ggcorrplot2)

data(diamonds)

groups<-split(diamonds,diamonds$color) 

names2<-names(groups)

plot3<-map2(groups,names2, ~.x %>% select(price,carat) %>% 
                    cor() %>% 
                    ggcorrplot::ggcorrplot(type = "lower",
                                           method = "square",
                                           ggtheme = ggthemes::theme_clean(),
                                           lab=T,
                                           show.diag = T,
                                           title = paste(.y),
                                           legend.title = "Correlation Range",
                                           colors = c("goldenrod4","white","darkseagreen"),
                                           outline.color = "darkorange2")+
                    theme(plot.title = element_text(hjust = 0.5))
)

ggarrange(plotlist = plot3,widths = 2.5,heights = 1)
