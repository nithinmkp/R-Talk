# Standard Error Augumenting ----------------------------------------------
library(sandwich)
library(lmtest)
library(tidyverse)
library(modelsummary)

data("PetersenCL")

mod1<-lm(y~x,PetersenCL)
summary(mod1)

#Robust Errors-Method 1
sqrt(diag(vcovHC(mod1)))

#Robust Errors-Method 2
coeftest(mod1,vcov. = vcovHC)

#Multiple Robust Errors

VC_function<-function(x){
        list(
                "Standard"=vcov(x),
                "Sandwich (Basic)"=sandwich(x),
                "Newway-West"=NeweyWest(x),
                "Clustered"=vcovCL(x,cluster = ~firm),
                "Two-way Clustered"=vcovCL(x,cluster = ~firm + year)
                )
}
vc_list<-VC_function(mod1)

lm_mods<-lapply(vc_list,function(x) coeftest(mod1,vcov. = x))
lm_mods2<-map(vc_list,~coeftest(mod1,vcov. = .x))

modelsummary(lm_mods2)
