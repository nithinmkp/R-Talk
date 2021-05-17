
# Unit root tests-Table ---------------------------------------------------

unit_tests<-function(x){
        
        list("ADF"=adf.test(x) %>% tidy() ,
             "PP"=pp.test(x) %>% tidy()
        )
}

unitroot_fn<-function(x){
        r1<-map(x,~unit_tests(.x)) %>% map(bind_rows)
        methods<-map_dfr(r1,"method")
        methods<-methods[,1,drop=T]
        r2<-r1  %>% 
                map_df(bind_rows,.id = "Variable")%>% 
                select(-alternative) %>% 
                pivot_wider(names_from = "method",
                            values_from = c("statistic","p.value","parameter"),
                            names_glue = "{method}_{.value}") %>% 
                select(Variable,starts_with(methods))
        return(r2)
        
}

# Complete TS table -------------------------------------------------------

ts_fn <- function(df, cols, order = 1) {
        for (col in cols) {
                df <- mutate(df, "ln_{col}" := log(get(col)))
        }
        varnames <- df %>%
                select(cols, starts_with("ln")) %>%
                names()
        
        diff_fn <- function(x) {
                assign(paste0("diff_", x), diff(x))
        }
        
        diff_lst <- df %>%
                select(varnames) %>%
                map(diff_fn) %>%
                map(bind_cols) %>%
                map_dfc(bind_cols) %>%
                setNames(paste0("diff_", varnames))
        
        df <- as_tibble(df)
        ln_df <- df %>% select(starts_with("ln"))
        df <- df %>% select(-starts_with("ln"))
        
        
        lst <- list(
                levels = df,
                log_levels = ln_df,
                diff = diff_lst
        )
        return(lst)
}