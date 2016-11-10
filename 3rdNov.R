opportunity_contact<-read.csv("D:/My Stuff/CRM/Audit data/contact_data.csv")

 require(RODBC)
 conn = odbcConnectExcel2007("D:/My Stuff/CRM/Audit data/Contact_Data.xlsx") # open a connection to the Excel file
 sqlTables(conn)$TABLE_NAME # show all sheets
 contact_data= sqlFetch(conn, "Sheet1") # read a sheet
 df = sqlQuery(conn, "select * from [Sheet1$]") # read a sheet (alternative SQL sintax)
 close(conn) # close the connection to the file

require(RODBC)
conn = odbcConnectExcel2007("D:/My Stuff/CRM/Audit data/AccountData.xlsx") # open a connection to the Excel file
sqlTables(conn)$TABLE_NAME # show all sheets
account_data= sqlFetch(conn, "Sheet1$") # read a sheet
account_data = sqlQuery(conn, "select * from [Sheet1$]") # read a sheet (alternative SQL sintax)
close(conn) 

require(RODBC)
conn = odbcConnectExcel2007("D:/My Stuff/CRM/Audit data/Audit.xlsx") # open a connection to the Excel file
sqlTables(conn)$TABLE_NAME # show all sheets
audit_data= sqlFetch(conn, "Sheet1") # read a sheet
audit_data = sqlQuery(conn, "select * from [Sheet1$]") # read a sheet (alternative SQL sintax)
close(conn) 

account_data<-account_data[,-248]
opportunity_contact<-opportunity_contact[,-1]

CRM_Data<-read.csv("D:/My Stuff/CRM/CRM_Data.csv")
CRM_Data<-CRM_Data[,-c(1,196,197,198,199,200,201)]


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
  if( sum( is.na( CRM_Data[, i] ) ) /nrow(CRM_Data) >= 0.6 ) 
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
names(Reduced_columns)

Reduced_columns<-Reduced_columns[,-c(1,2,3,5,6,10,16)]
Reduced_columns1<-Reduced_columns[complete.cases(Reduced_columns),]

for(i in 1:ncol(Reduced_columns1)) 
{ 
  if(sd(as.numeric(as.factor(Reduced_columns1[, i])))==0) 
  {  
         Reduced_columns1 <- Reduced_columns1[ , -i] 
  }
}

Reduced_columns1[,c(4,5)]

z<-as.numeric(as.factor(Reduced_columns1$OwnerId))
y<-as.numeric(as.factor(Reduced_columns1$OwnerIdName))

a<-merge(Reduced_columns1,audit_data,by="OpportunityId")
merge(Reduced_columns1,account_data,by="OpportunityId")
b<-merge(CRM_Data,account_data,by="AccountId")


sapply(a,function(x)any(is.na(x)))

---------------------------------------------------------------------------------
# Function For Removing Columns having all NULL values
Remove_NAs<-function(Data)
{
  Data[Data=="NULL"] <- NA
  Data[Data==""] <- NA
  NAs_removed<-Data
  for(i in 1:length(Data)) 
  { 
    if( sum( is.na( Data[, i] ) ) /nrow(Data) == 1 ) 
    {  
      for(j in 1:length(NAs_removed)) 
      {
        if( length( grep(names(Data[i]), names(NAs_removed)[j]) ) ==1)
        { 
          NAs_removed <- NAs_removed[ , -j] 
        }   
      } 
    }
  }
  return (NAs_removed)
}

--------------------------------------------------------------------------------
# Function for Removing columns having 60% of NAs
  
  Remove_more_than95<-function(Data)
  {
    Data[Data=="NULL"] <- NA
    Data[Data==""] <- NA
    Reduced_columns<-Data
    for(i in 1:length(Data)) 
    { 
      if( sum( is.na( Data[, i] ) ) /nrow(Data) >= 0.95 ) 
      {  
        for(j in 1:length(Reduced_columns)) 
        {
          if( length( grep(names(Data[i]), names(Reduced_columns)[j]) ) ==1)
          { 
            Reduced_columns <- Reduced_columns[ , -j] 
          }   
        } 
      }
    }
    return (Reduced_columns)
  }
---------------------------------------------------------------------------------
# function for removing columns having standard deviation =0
  
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

sapply(account_data_reduced,function(x)any(is.na(x)))
  
#---------------------------------------------------------------------------------
#function for finding NAs distributio in data
  
  
w <- t( account_data_reduced ) 
# remove duplicate rows 
x <- unique(w) 
# transpose again 
y <- t( x ) 
# convert back to data frame 
z <- data.frame( y ) 

  z_names<-as.data.frame(names(z))
  account_red_names<-as.data.frame(names(account_data_reduced))

require(sqldf)
a1NotIna2 <- sqldf('SELECT * FROM account_red_names EXCEPT SELECT * FROM z_names')
a1Ina2 <- sqldf('SELECT * FROM account_red_names INTERSECT SELECT * FROM z_names')

#---------------------------------------------------------------------------------
#function for removing duplicate columns
correlation<-function(a,b)
{
  a<-as.data.frame(a)
  b<-as.data.frame(b)
  c<-cbind(a,b)
  d<-c[complete.cases(c),]
  corltn<-cor(as.numeric(as.factor(d$a)),as.numeric(as.factor(d$b)))
  return(corltn)
}


audit_data[audit_data=="NULL"] <- NA
audit_data[audit_data==""] <- NA

names(audit_data)

contact_data<-read.csv("D:/My Stuff/CRM/ContactData.csv")
removed_contacts<-Remove_NAs(contact_data)
a1<-as.data.frame(names(contact_data))
a2<-as.data.frame(names(contactmorethan95))

require(sqldf)
a1NotIna2 <- sqldf('SELECT * FROM a1 EXCEPT SELECT * FROM a2')
a1Ina2 <- sqldf('SELECT * FROM a1 INTERSECT SELECT * FROM a2')
a1NotIna2

audit<-audit_data[,c(1,2,3,4,6,7,12,13,14,17,18)]
audit$CreatedOn<-as.POSIXct(strptime(audit$CreatedOn,format="%Y-%m-%d"))

audit_History<-merge(audit,NAs_removed,by="OpportunityId")

write.csv(audit_History,file = "D:/My Stuff/CRM/audit_History.csv")








