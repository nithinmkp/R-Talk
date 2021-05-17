# Read Data-WACR
library(readxl)
library(lubridate)
library(dint)

df1<-read_excel("wacr_data.xlsx")

#Get monthly data- tidy form

df_m<-wacr_data_fn(df1)

#df_m$`Year-Month`<-as_date_ym(df_m$`Year-Month`)
#df_m$`Year-Month`<-format_ym(df_m$`Year-Month`,format = "%b-%Y")

writexl::write_xlsx(df_m,"data.xlsx")

#Get quarterly data- mean method
df_q<-wacr_qtr_function(df_m,fn="mean")


# Get Yearly data

df_y<-wacr_data_fn(df1,freq = "year")

## CMIE Data

#create input and output folders
dir.create("data_input")
dir.create("data_output")
