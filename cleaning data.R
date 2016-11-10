#                                          CRM Data
# ---------------------------------------------------------------------------------------------

sheet1<-read.csv("D:/My Stuff/CRM/Data/Sheet1.csv")
sheet2<-read.csv("D:/My Stuff/CRM/Data/Sheet2.csv")
sheet3<-read.csv("D:/My Stuff/CRM/Data/Sheet3.csv")

a<-rbind(sheet1,sheet2)
crm_data<-rbind(a,sheet3)
# write.csv(crm_data,"D:/My Stuff/CRM/CRM_Data.csv")

dim(crm_data)
# 6803  201

crm_data[crm_data=="NULL"] <- NA
crm_data[crm_data==""] <- NA


#install.packages("RODBC", repos = "http://cran.rstudio.com", type = "source")
#install.packages("reshape", repos = "http://cran.rstudio.com", type = "source")

library(RODBC)
conn=odbcConnectExcel2007("D:/My Stuff/CRM/Data/Sheet1.xlsx")
sqlTables(conn)$TABLE_NAME # show all sheets
df = sqlFetch(conn, "Sheet1") 
sheet1 = sqlQuery(conn, "select * from [Sheet1$]")
close(conn) 

conn=odbcConnectExcel2007("D:/My Stuff/CRM/Data/Sheet2.xlsx")
df = sqlFetch(conn, "Sheet1") 
sheet2 = sqlQuery(conn, "select * from [Sheet1$]") 
close(conn)

conn=odbcConnectExcel2007("D:/My Stuff/CRM/Data/Sheet3.xlsx")
df = sqlFetch(conn, "Sheet1") 
sheet3 = sqlQuery(conn, "select * from [Sheet1$]") 
close(conn) 

a<-as.data.frame(sheet1$ModifiedOn)
b<-as.data.frame(sheet2$ModifiedOn)
c<-as.data.frame(sheet3$ModifiedOn)
names(a)[1]<-"ModifiedOn"
names(b)[1]<-"ModifiedOn"
names(c)[1]<-"ModifiedOn"
a<-rbind(a,b)
a<-rbind(a,c)

x<-as.data.frame(sheet1$CreatedOn)
y<-as.data.frame(sheet2$CreatedOn)
z<-as.data.frame(sheet3$CreatedOn)
names(x)[1]<-"CreatedOn"
names(y)[1]<-"CreatedOn"
names(z)[1]<-"CreatedOn"
x<-rbind(x,y)
x<-rbind(x,z)

crm_data<-cbind(crm_data,a)
crm_data<-cbind(crm_data,x)


NAs_removed<-crm_data
Reduced_columns<-crm_data

for(i in 1:length(crm_data)) 
{ 
  if( sum( is.na( crm_data[, i] ) ) /nrow(crm_data) == 1 ) 
  {  
    for(j in 1:length(NAs_removed)) 
    {
        if( length( grep(names(crm_data[i]), names(NAs_removed)[j]) ) ==1)
        { 
          NAs_removed <- NAs_removed[ , -j] 
        }   
    } 
  }
  if( sum( is.na( crm_data[, i] ) ) /nrow(crm_data) >= 0.6 ) 
  {  
    for(j in 1:length(Reduced_columns)) 
    {
      if( length( grep(names(crm_data[i]), names(Reduced_columns)[j]) ) ==1)
      { 
        Reduced_columns <- Reduced_columns[ , -j] 
      }   
    } 
  }
}

NotNA<-NAs_removed[which(is.na(NAs_removed$CreatedOn.1)==FALSE),]
#NotNA$ModifiedOn<-NAs_removed[which(is.na(NAs_removed$ModifiedOn.1)==FALSE),]

NotNA<-cbind(NotNA, read.table(text = as.character(NotNA$ModifiedOn.1), sep = " "))
names(NotNA)[155]<-"ModifiedOn_Date"
names(NotNA)[156]<-"ModifiedOn_Time"

NotNA<-cbind(NotNA, read.table(text = as.character(NotNA$CreatedOn.1), sep = " "))
dim(NotNA)
names(NotNA)[157]<-"CreatedOn_Date"
names(NotNA)[158]<-"CreatedOn_Time"

#dataForPrediction<-NAs_removed[,c("OpportunityRatingCode","Name","StepName","CloseProbability","CreatedOn","ModifiedOn","PricingErrorCode","VersionNumber","StateCode","StatusCode","IsRevenueSystemCalculated","Cyb_salesStage","CustomerIdName","cyb_CompanyInformation")]
dataForPrediction<-NAs_removed[,c(45,47,50,54,154,153,52,64:67,69,124,130)]
write.csv(NAs_removed,"D:/My Stuff/CRM/NAs_Removed.csv")

#Removing column containing all NAs and Null values
dim(NAs_removed)
# 6803  152

# When NAs in column are equal to or greater than 60 % 
dim(Reduced_columns)
# 6803  70

a1<-data.frame(names(NAs_removed))
a2<-data.frame(names(Reduced_columns))

#install.packages("sqldf", repos = "http://cran.rstudio.com", type = "source")

require(sqldf)
a1NotIna2 <- sqldf('SELECT * FROM a1 EXCEPT SELECT * FROM a2')
a1Ina2 <- sqldf('SELECT * FROM a1 INTERSECT SELECT * FROM a2')

a3<-data.frame(names(crm_data))
a3NotIna2 <- sqldf('SELECT * FROM a3 EXCEPT SELECT * FROM a1')

as.data.frame(names(crm_data))

complete_cases<-Reduced_columns[complete.cases(Reduced_columns),]
write.csv(complete_cases,"D:/My Stuff/CRM/Complete cases.csv")

for(i in seq(1:length(complete_cases)))
{
  complete_cases[,i]<-as.numeric(complete_cases[,i])
}

# Correlation Plot

cor(complete_cases)

#install.packages("corrplot", repos = "http://cran.rstudio.com", type = "source")
library(corrplot)
M <- cor(complete_cases)
corrplot(M, method="number",order="hclust", addrect=5)
summary(complete_cases)


for(i in seq(1:length(complete_cases)))
{
  print(names(complete_cases[i]))
  print(sd(complete_cases[,i]))
}

names(complete_cases)

complete_cases1<-complete_cases[,c(1:6,8:10,13:18,20:21,23:27,29:37,42,68:70)]
M <- cor(complete_cases1)
corrplot(M,order="hclust")
summary(complete_cases1)


write.csv(complete_cases,"D:/My Stuff/abc.csv")


a1<-data.frame(names(complete_cases))
a2<-data.frame(names(complete_cases1))

#install.packages("sqldf", repos = "http://cran.rstudio.com", type = "source")

require(sqldf)
a1NotIna2 <- sqldf('SELECT * FROM a1 EXCEPT SELECT * FROM a2')
a1Ina2 <- sqldf('SELECT * FROM a1 INTERSECT SELECT * FROM a2')

for(i in seq (1:35))
{
 hist(complete_cases1[,i], main= paste0("Histogram of Column ",names(complete_cases1)[i]), 
     xlab= names(complete_cases1[i]), 
     border="black", 
     col="lightblue",
     las=1,
     labels = TRUE
     )
}


histogram<-function(dataset)
{
  for(i in seq (1: ncol(dataset)))
  {
    hist(dataset[,i], main= paste0("Histogram of Column ",names(dataset)[i]), 
         xlab= names(dataset[i]), 
         border="black", 
         col="lightblue",
         las=1)
  }
}

# customerid 
# estimatedvalue
# estimatedclosedate
# statuscode
# name
# salesstagecode
# cyb_companyinformation
# cyb_primaryonsitebdid
# cyb_secondaryonsitebdid
# transactioncurrencyid
# pricelevelid
# cyb_customertype
# cyb_datetoreconnect
# opportunityratingcode
# cyb_reconnectdateprovidedfrom
# cyb_reasontype
# cyb_reasondetails
# cyb_followupreminderdate
# cyb_followupremindercount
# cyb_lastdateforratingchange
# cyb_lastdateforresponse
# isrevenuesystemcalculated
# closeprobability
# cyb_fixedprice
# cyb_airportcode
# cyb_initialresourcecount
# cyb_potentialresourcecount
# cyb_projectstartdate
# cyb_contractdate
# cyb_description
# ownerid
# originatingleadid
# campaignid
# totallineitemamount
# totalamountlessfreight
# discountpercentage
# freightamount
# discountamount
# totaltax
# totalamount
# statecode
# cyb_opportunitysource
# cyb_opportunitysubsource
# cyb_conferencename
# cyb_lastdateforratingchange
# cyb_intiatedby

g<-crm_data[,c("CustomerId","EstimatedValue","EstimatedCloseDate","StateCode","Name",
            "SalesStageCode","cyb_CompanyInformation","cyb_primaryonsitebdid",
         "cyb_secondaryonsitebdid","TransactionCurrencyId","PriceLevelId","cyb_customertype",
         "cyb_datetoreconnect","OpportunityRatingCode","cyb_reconnectdateprovidedfrom",
         "cyb_reasontype","cyb_reasondetails","cyb_followupreminderdate","cyb_followupremindercount",
         "cyb_lastdateforratingchange","cyb_lastdateforresponse","IsRevenueSystemCalculated","CloseProbability"
         ,"cyb_FixedPrice","Cyb_AirportCode","Cyb_InitialResourceCount","Cyb_PotentialResourceCount",
         "TotalTax","TotalAmount","StateCode","cyb_OpportunitySource","cyb_OpportunitySubSource",
         "cyb_Conferencename","cyb_lastdateforratingchange","cyb_InitiatedBy",
        "cyb_ProjectStartDate","cyb_ContractDate","OwnerId","OriginatingLeadId","CampaignId",
        "TotalAmountLessFreight","TotalLineItemAmount","DiscountPercentage")]
