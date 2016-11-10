dates<-read.csv("C:/Users/madhumitaj/Desktop/datefull.csv")

#Arranging Data by Date
dates<-dates[order(as.Date(dates$CreatedOn_Audit, format="%m/%d/%Y")),]

# Removing rows having NUll
dates<-dates[which(dates$Opportunity.Id!=""),]
a<-unique(dates$Opportunity.Id)

for(j in seq(length(unique(dates$Opportunity.Id))))
{
  print(paste0("--------",a[j],"-----------"))
  dates1<-dates[which(dates$Opportunity.Id=="000715A4-A9C0-E411-9E6C-001E676A2ACA"),]
  date_strings<-as.character(dates1$CreatedOn_Audit)
  datetimes = strptime(date_strings, format = "%m/%d/%Y")

  for(i in seq(length(datetimes)))
  {
    diff_in_days = difftime(datetimes[i+1], datetimes[i], units = "days") # days
    print(diff_in_days)
  }
}

---------------------------------------------------------------------------
dates<-read.csv("C:/Users/madhumitaj/Desktop/datefull.csv")
dates<-dates[order(as.Date(dates$CreatedOn_Audit, format="%m/%d/%Y")),]
dates<-dates[which(dates$Opportunity.Id!=""),]
dates<-dates[which(dates$CreatedOn_Audit!=""),]

a<-unique(dates$Opportunity.Id)

df=NULL
k=0
for(j in seq(length(unique(dates$Opportunity.Id))))
{
  #print(paste0("******",a[j],"*****"))
  dates1<-dates[which(dates$Opportunity.Id==a[j]),]
  date_strings<-as.character(dates1$CreatedOn_Audit)
  actions<-as.character(dates1$Action)
  datetimes = strptime(date_strings, format = "%m/%d/%Y")
  for(i in seq(length(datetimes)))
  {
    diff_in_days = difftime(datetimes[i+1], datetimes[i], units = "days") # days
    df$Duration_of_Action[k]<-diff_in_days
    df$Opportunity.Id[k]<-as.character(a[j])
    df$CreatedOn_Audit[k]<-as.character(datetimes[i])
    df$Action[k]<-as.character(actions[i])
   # print(diff_in_days)
    k=k+1
  }
}  
df<-as.data.frame(df)

dates$CreatedOn_Audit<-as.Date(dates$CreatedOn_Audit,format = "%m/%d/%Y")
df$CreatedOn_Audit<-as.Date(df$CreatedOn_Audit)
df[which(is.na(df$Duration_of_Action)==TRUE),1]<--1

z<-aggregate(df$Duration_of_Action ~ (df$CreatedOn_Audit+df$Opportunity.Id), df, function(x) max(x))
names(z)[1]<-"CreatedOn_Audit"
names(z)[2]<-"Opportunity.Id"
names(z)[3]<-"Duration_of_Action"


z<-merge(z,df,by=c("CreatedOn_Audit","Opportunity.Id"))

MainFile<-merge(dates,z,by=c("Opportunity.Id","CreatedOn_Audit","Action"))

write.csv(MainFile,file = "D:/My Stuff/CRM/MainFile.csv")







