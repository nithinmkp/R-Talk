#packages
packages<-c("tidyverse","rlist","haven","readxl","here","lubridate","janitor","rio",
            "writexl")
#sapply(packages,install.packages,character.only=T)
sapply(packages,library,character.only=T)

#output and directory
InputFolder<-paste(here(),"/data_input",sep = "")
OutputFolder<-paste(here(),"/data_output",sep = "")

#read files
files<-list.files(paste0(InputFolder,"/"),pattern = "*.xlsx",full.names = T)
data_set<-import_list(files)


panel_fn<-function(df,n_row=3,n_col=1){
        df <- df[-c(1:n_row),]
        #Get the years from the numeric values in row 1
        df_years <- year(as.Date(as.numeric(df[1,seq(n_col+1,ncol(df))]), origin = "1900-01-01"))
        #Reduce years by 2008 and put it instead of the current values of row 1
        yr_base<-df_years[1]-1
        df_years <- df_years - yr_base
        df_years<-as.character(df_years)
        df[1, seq(n_col+1,ncol(df))] <- as.list(df_years)
        names(df)[1:n_col] <- as.character(df[2,seq(1,n_col)])
        names(df)[seq(n_col+1,ncol(df))] <- paste(df[2,seq(n_col+1,ncol(df))], df[1,seq(n_col+1,ncol(df))], sep = "_")
        #Remove the no longer useful rows 1 and 2
        df <- df[-c(1:2),]
        #Transform from wide to long
        df_long <- pivot_longer(df,cols = -c(1:n_col),names_to = c(".value","Year"),names_sep = "_")
        #Rename time column to Year and add 2008 back to the year values, remove the id column
        df_long$Year <- as.numeric(df_long$Year) + yr_base
        df_long[,-seq(1,n_col)]<-lapply(df_long[,-seq(1,n_col)],as.numeric)
        return(df_long)
        
}
data_list<-map(data_set,~panel_fn(.x,n_row = 3,n_col = 1))

names_data<-names(data_list)
walk2(data_list,names_data,~write_xlsx(x=.x,path = paste(OutputFolder,"/",.y,".xlsx",
                                                         sep ="" )))
#Write to excel workbook by sheets
#writexl::write_xlsx(data_list,"Final_Data.xlsx")

#Merge the data
merged_data<-data_list %>% reduce(left_join)
