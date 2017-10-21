setwd("~/Desktop/")
dataset <- read.csv('onlinesales.csv')

#Check Col names
dim(dataset)
head(dataset)

#Monetary Function
monetary <- aggregate(sales~customer_id,FUN=mean,data=dataset)

#Frequency Function
frequency <- aggregate(sales~customer_id,FUN=length,data=dataset)

#Recency Function
#Transform dates
dataset$date<-as.Date(dataset$date,format("%m/%d/%y"))
lastpurchase <- aggregate(date~customer_id,FUN=max,data=dataset)

#Merge RFM summary
fmsummary<- merge(monetary,frequency, by="customer_id")
fmsummary<- merge(fmsummary,lastpurchase,by="customer_id")

head(fmsummary)

#Change column names
colnames(fmsummary)<- c("customer_id", "monetary", "frequency", "lastpurchased")
head(fmsummary)

#Get Recency
fmsummary$recency=as.numeric(max(fmsummary$lastpurchased) - fmsummary$lastpurchased)

dim(fmsummary)
head(fmsummary)

#Create 1-5 Scoring system for RFM
fmsummary$rankr<-cut(fmsummary$recency, 5,labels=F) 
fmsummary$rankf<-cut(fmsummary$frequency, 5,labels=F) 
fmsummary$rankm<-cut(fmsummary$monetary, 5,labels=F) 

#Create overall score
fmsummary$totalrfm<-paste(fmsummary$rankr, fmsummary$rankf, fmsummary$rankm)

head(fmsummary)

# Write CSV i
write.csv(fmsummary, file = "rfm.csv")
