# Render one file
library(glue)
library(rlist)
dir.create("Reports")
rmarkdown::render(input = "Reporting.Rmd",
                  params = list(species="Gentoo"),
                  output_file = glue("Reports/Adelie-Report.pdf"))


render_fn<-function(sp){
        rmarkdown::render(input = "Reporting.Rmd",
                          params = list(species=sp),
                          output_file = glue("Reports/report-{sp}.pdf"))
}
render_fn("Chinstrap")
species<-unique(penguins$species)
paths<-paste0("Reports/",species,".pdf")
params<-map(species,~list(species=.x))
walk2(paths,params,~rmarkdown::render("Reporting.Rmd",output_file = .x,
                                      params = .y))


# Multiple parameter
category<-data %>% select(where(is.factor)) %>% select(-1) %>% names()
paths2<-cross2(species,category) %>% list.stack() %>% unite(sep = "-",col = "path",remove = T) %>% 
        map(~paste0("Reports/",.x,".pdf")) %>% flatten()

params2<-cross2(species,category) %>% map(setNames,c("species","category"))
walk2(paths2,params2,~rmarkdown::render("Reporting.Rmd",output_file = .x,
                                      params = .y))
