tableau_opportunity<-read.csv("D:/My Stuff/CRM/tableau_opportunity.csv")
length(unique(tableau_opportunity$Opportunity.Id))
# 3798
------------------------------------------------------
  
Wining_Opp<-tableau_opportunity[which(tableau_opportunity$Action..group.=="Win"),]
length(unique(Wining_Opp$Opportunity.Id))
# 352
unique_winning_opp<-unique(Wining_Opp$Opportunity.Id)
unique_winning_opp<-as.data.frame(unique_winning_opp)
names(unique_winning_opp)[1]<-"Opportunity.Id"

z<-merge(unique_winning_opp,tableau_opportunity,by="Opportunity.Id")


a1<-data.frame(unique_winning_opp$Opportunity.Id)
a2<-data.frame(unique(tableau_opportunity$Opportunity.Id))

#install.packages("sqldf", repos = "http://cran.rstudio.com", type = "source")

require(sqldf)
a1NotIna2 <- sqldf('SELECT * FROM a2 EXCEPT SELECT * FROM a1')
a1Ina2 <- sqldf('SELECT * FROM a1 INTERSECT SELECT * FROM a2')







-------------------------------------------------------

Creating_Opp<-tableau_opportunity[which(tableau_opportunity$Action..group.=="Create"),]
length(unique(Creating_Opp$Opportunity.Id))
#2086

unique_Creating_opp<-unique(Creating_Opp$Opportunity.Id)
unique_Creating_opp<-as.data.frame(unique_Creating_opp)
names(unique_Creating_opp)[1]<-"Opportunity.Id"
y<-merge(unique_Creating_opp,tableau_opportunity,by="Opportunity.Id")



a1<-data.frame(unique_Creating_opp$Opportunity.Id)
a2<-data.frame(unique(tableau_opportunity$Opportunity.Id))

#install.packages("sqldf", repos = "http://cran.rstudio.com", type = "source")

require(sqldf)
a1NotIna2 <- sqldf('SELECT * FROM a2 EXCEPT SELECT * FROM a1')
a1Ina2 <- sqldf('SELECT * FROM a1 INTERSECT SELECT * FROM a2')

-------------------------------------------------------------
  
Losing_Opp<-tableau_opportunity[which(tableau_opportunity$Action..group.=="Lose"),]
length(unique(Losing_Opp$Opportunity.Id))
#1079

unique_Losing_opp<-unique(Losing_Opp$Opportunity.Id)
unique_Losing_opp<-as.data.frame(unique_Losing_opp)
names(unique_Losing_opp)[1]<-"Opportunity.Id"
y<-merge(unique_Losing_opp,tableau_opportunity,by="Opportunity.Id")



a1<-data.frame(unique_Losing_opp$Opportunity.Id)
a2<-data.frame(unique(tableau_opportunity$Opportunity.Id))

#install.packages("sqldf", repos = "http://cran.rstudio.com", type = "source")

require(sqldf)
a1NotIna2 <- sqldf('SELECT * FROM a2 EXCEPT SELECT * FROM a1')
a1Ina2 <- sqldf('SELECT * FROM a1 INTERSECT SELECT * FROM a2')
  
names(a1Ina2)<-"Opportunity.Id"
  
w<-merge(a1Ina2,tableau_opportunity,by="Opportunity.Id")  
  
  
  write.csv(w,"Losing_Opportunity.csv")
  
  
