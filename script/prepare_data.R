# Helper function to get data from the access db and create csv files that will used by the 
# Main dashboard

library(RODBC) #library to get data from SQL server/MsAccess DB

df_file_path =  "C://SearchYouth//MSAccessDatabase//SY_Endpoint.mdb"


writeCsvFile <- function(df, fname) {
  
  file_name<-paste("./data/",fname, ".csv", sep="")
  write.csv(df, file_name, row.names = FALSE)
}

dfs_all <- readData(df_file_path)

## Write to the csv files
writeCsvFile(dfs_all$endpoint,"endpoint")
writeCsvFile(dfs_all$pss,"pss")
