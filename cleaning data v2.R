# crm<-read.csv("D:/My Stuff/CRM/NOTNA.csv")
# Reduced_columns<-crm
# 
# for(i in 1:length(crm)) 
# { 
#   if( sum( is.na( crm[, i] ) ) /nrow(crm) >= 0.6 ) 
#   {  
#     for(j in 1:length(Reduced_columns)) 
#     {
#       if( length( grep(names(crm[i]), names(Reduced_columns)[j]) ) ==1)
#       { 
#         Reduced_columns <- Reduced_columns[ , -j] 
#       }   
#     } 
#   }
# }
# dim(Reduced_columns)
# dim(crm)
# 
# write.csv(Reduced_columns,"D:/My Stuff/CRM/Reduced_Columns1.csv")

Reduced_columns<-read.csv("D:/My Stuff/CRM/CRM_Reduced_Columns.csv")
names(Reduced_columns)

install.packages("digest", repos = "http://cran.rstudio.com", type = "source")
library(digest)

Reduced_columns[!duplicated(lapply(Reduced_columns, digest))]










