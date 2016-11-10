require(RODBC)
conn = odbcConnectExcel2007("D:/My Stuff/CRM/Audit data/Audit.xlsx") # open a connection to the Excel file
sqlTables(conn)$TABLE_NAME # show all sheets
audit_data= sqlFetch(conn, "Sheet1") # read a sheet
audit_data = sqlQuery(conn, "select * from [Sheet1$]") # read a sheet (alternative SQL sintax)
close(conn)

CRM_Data<-read.csv("D:/My Stuff/CRM/CRM_Data_Updated.csv")
#CRM_Data<-CRM_Data[,-c(1,196,197,198,199,200,201)]
  
CRM_Data[CRM_Data=="NULL"] <- NA
CRM_Data[CRM_Data==""] <- NA
  
NAs_removed<-CRM_Data
Reduced_columns<-CRM_Data
  
    for(i in 1:length(CRM_Data)) 
       { 
           if(sum(is.na( CRM_Data[, i]))/nrow(CRM_Data) == 1) 
             {  
                 for(j in 1:length(NAs_removed)) 
                   {
                       if( length( grep(names(CRM_Data[i]), names(NAs_removed)[j]) ) ==1)
                         { 
                             NAs_removed <- NAs_removed[ , -j] 
                           }   
                     } 
           }
          if(sum(is.na( CRM_Data[, i]))/nrow(CRM_Data) >= 0.9) 
          {  
            for(j in 1:length(Reduced_columns)) 
            {
              if( length( grep(names(CRM_Data[i]), names(Reduced_columns)[j]) ) ==1)
              { 
                Reduced_columns <- Reduced_columns[ , -j] 
              }   
            } 
          }
        }
---------------------------------------------------------
  #  function to remove Duplicate Rows 
  
  Remove_Duplicate_Rows<-function(Data)
  {
    library(sqldf)
    unique_rows<-sqldf('SELECT DISTINCT * FROM Data')
    return(unique_rows)  
  }

reduced_duplicate_rows_CRM<-Remove_Duplicate_Rows(Reduced_columns)
----------------------------------------------------------------

audit<-audit_data[,c(1,2,3,4,6,7,12,13,14,17,18)]
audit$CreatedOn<-as.POSIXct(strptime(audit$CreatedOn,format="%Y-%m-%d"))
  
audit_History<-merge(audit,reduced_duplicate_rows_CRM,by="OpportunityId")

  dim(audit_History)
#  [1] 60535    96
 
write.csv(audit_History,file = "D:/My Stuff/CRM/audit_History.csv")
write.csv(audit,file = "D:/My Stuff/CRM/audit.csv")
  
audit_History<-read.csv("D:/My Stuff/CRM/audit_History.csv")
  
  # 1 - Create, 2 - Update,3 - Delete, 4 - Activate,5 - Deactivate, 13 - Assign,
  # 14 - Share,15 - Retrieve,21 - Reopen,24 - Qualify,  41 - Set State, 44 - Win,45 - Lose, 
  
  Remove_sd_is_zero_cols<-function(Data)
  {
    Data<-Reduced_columns[complete.cases(Data),]
    for(i in 1:ncol(Data)) 
    { 
      if(sd(as.numeric(as.factor(Data[, i])))==0) 
      {  
        Data<- Data[ , -i] 
      }
    }
    return(Data)
  }
---------------------------------------------------------------------------
#  finding Time Difference

date_strings<-as.character(audit_History$CreatedOn.x)
datetimes = strptime(date_strings, format = "%Y-%m-%d")
for(i in seq(length(datetimes)-1))
{
  diff_in_days = difftime(datetimes[i+1], datetimes[i], units = "days") # days
  print(diff_in_days)
}

