#library("readxl")
#eRetail <- read_excel("Online Retail_Clean.xlsx")

eRetail <- read.csv('R On.csv')

#create Amount variable
eRetail$Amount <- eRetail$Quantity * eRetail$UnitPrice

#create SKU variable
eRetail$SKU <- substr(eRetail$StockCode,1,3)

#separate date & time
eRetail$InvoiceDate<-strptime(eRetail$InvoiceDate,"%m/%d/%Y %H:%M")
eRetail$InvoiceTime = format(eRetail$InvoiceDate,"%H")
eRetail$InvoiceDate<-as.Date(eRetail$InvoiceDate,"%m/%d/%Y")

#look at internal structure#
str(eRetail)

#View a summary
summary(eRetail)

#View the top#
head(eRetail)

## boxplot of Amount
boxplot(eRetail$Amount)$stats[c(1, 5), ]

#cutoff outliner
Retail<-subset(eRetail,eRetail$Amount>= 0 & eRetail$Amount<= 6000 )
boxplot(Retail$Amount)$stats[c(1, 5), ]

#What are top 5 selling products accross all times?
install.packages('plyr')
library(plyr)
Retail1 <- ddply(Retail, .(StockCode,Description), summarize, sumAmount= sum(Amount), sumQuantity= sum(Quantity), nCustomer= length(unique(CustomerID)), nPurchase= length(unique(InvoiceNo)) )
head(Retail1[order(-Retail1$sumQuantity),] )

head(Retail1[order(-Retail1$nCustomer),] )

head(Retail1[order(-Retail1$sumAmount),] )

head(Retail1[order(-Retail1$nPurchase),] )

#Do these sales of top selling products change with time (months)?
library(lubridate)
Retail2 <- subset(Retail, Description%in%c("MEDIUM CERAMIC TOP STORAGE JAR","JUMBO BAG RED RETROSPOT","REGENCY CAKESTAND 3 TIER","WHITE HANGING HEART T-LIGHT HOLDER","PARTY BUNTING","WORLD WAR 2 GLIDERS ASSTD DESIGNS"), select = c(Description,InvoiceDate,InvoiceTime,Quantity,CustomerID,Amount,InvoiceNo))
Retail2$Invoice_month<-month(Retail2$InvoiceDate)
Retail2$Decription<-as.character(Retail2$Description)

library(ggplot2)
ggplot(Retail2, aes(x=Invoice_month, y= Quantity))+ facet_wrap(~Description, ncol=2) + 
  geom_bar(stat="identity") + 
  labs(title = "Sales by month", x = "Month", y = "Sales Volume")

ggplot(Retail2, aes(x=Invoice_month, y= length(unique(CustomerID)) )) + facet_wrap(~Description, ncol=2) + 
  geom_bar(stat="identity") + 
  labs(title = "Sales by month", x = "Month", y = "Number of Customer") 

ggplot(Retail2, aes(x=Invoice_month, y= Amount )) + facet_wrap(~Description, ncol=2) + 
  geom_bar(stat="identity") + 
  labs(title = "Sales by month", x = "Month", y = "Sales Revenue") 

ggplot(Retail2, aes(x=Invoice_month, y= length(unique(InvoiceNo)) )) + facet_wrap(~Description, ncol=2) + 
  geom_bar(stat="identity") + 
  labs(title = "Sales by month", x = "Month", y = "Number of Purchases") 

#What are the busiest hours of a day?
Retail3<-ddply(Retail, .(InvoiceTime), summarize, sumAmount=sum(Amount), sumQuantity=sum(Quantity), nCustomer=length(unique(CustomerID)))
names(Retail3) [1] <-"InvoiceHour"

ggplot(Retail3, aes(x=InvoiceHour, y= sumQuantity)) + 
  geom_bar(stat="identity") + 
  labs(title = "Sales by hours", x = "Hours", y = "Sales Volume")

ggplot(Retail3, aes(x=InvoiceHour, y= nCustomer)) + 
  geom_bar(stat="identity") + 
  labs(title = "Sales by hours", x = "Hours", y = "Number of customer")

#RFM Analysis
getRFMdf<-function (RFM_raw){
  RFM_raw <- RFM_raw[!duplicated(RFM_raw$CustomerID),]
  RFM_raw <- cbind(RFM_raw, First_date = with(df,
                                              as.Date(as.integer(by(InvoiceDate, CustomerID, min)), "1970/01/01")))
  RFM_raw <- cbind(RFM_raw, Last_date = with(df,
                                             as.Date(as.integer(by(InvoiceDate, CustomerID, max)), "1970/01/01")))
  #Recency
  AsOfDate <- max(RFM_raw$Last_date)
  RFM_raw <- cbind(RFM_raw, Recency = with(df,
                                           as.numeric(difftime(AsOfDate,RFM_raw$Last_date,units="days")))/30)
  #First_purchase
  RFM_raw <- cbind(RFM_raw, First_purchase = with(df,
                                                  as.numeric(difftime(AsOfDate,RFM_raw$First_date,units="days")))/30)
  #Frequency
  RFM_raw <- cbind(RFM_raw, Frequency = with(df,
                                             as.numeric(by(InvoiceNo, CustomerID, function(x) length(unique(x))))))
  #Monetary & related
  RFM_raw <- cbind(RFM_raw, Monetary = with(df,
                                            as.numeric(by(Amount, CustomerID, sum))))
  RFM_raw <- cbind(RFM_raw, AvgM = with(df,
                                        as.numeric(by(Amount, CustomerID, mean))))
  RFM_raw <- cbind(RFM_raw, maxM = with(df,
                                        as.numeric(by(Amount, CustomerID, max))))
  #Breadth
  RFM_raw <- cbind(RFM_raw, Breadth = with(df,
                                           as.numeric(by(SKU, CustomerID, function(x) length(unique(x))))))
  #Tenure
  RFM_raw <- cbind(RFM_raw, Tenure = with(df, as.numeric(difftime(RFM_raw$Last_date,RFM_raw$First_date,units="days")))/30)
  #sum Quantity
  RFM_raw <- cbind(RFM_raw, sumQuant = with(df,
                                            as.numeric(by(Quantity, CustomerID, mean))))
}

#define getRFMnor
getRFMnor<-function (RFMn){
  RFMn<- as.data.frame(scale(df2[14:22], center= TRUE))
  RFMn<- cbind(df2[,c(1:13)],RFMn)
  RFMn<- rename(RFMn, c("Recency" = "R", "Frequency" = "Fq", "Monetary" = "M", "Breadth" = "B" , "Tenure" = "Ten", "sumQuant" = "Q" ) )
}

#define getRFMscore function
#score 1 to 5
score15<-function(x){
  ceiling((rank(x))/(length(x))*5)
}
getRFMscore<-function (RFMs){
  RFMs <- as.data.frame(lapply(df3[,c(15:22)], score15))
  RFMs <- cbind(df3[,c(1:13)], R= ceiling((rank(-df3$R))/(length(df3$R))*5), RFMs)
  RFMs <- cbind(RFMs,RFMScore = 100*RFMs$R + 10*RFMs$Fq+RFMs$M)
}

#RFM score on the whole data (12/2010 to 12/2011)
df<- eRetail
rawRFM<-as.data.frame(getRFMdf(df))

#some rawRFM score EDA
#take a look at disturbution
par(mfrow = c(1,3))
boxplot(rawRFM$Recency)$stats[c(1, 5), ]
boxplot(rawRFM$Frequency)$stats[c(1, 5), ]
boxplot(rawRFM$Monetary)$stats[c(1, 5), ]

#Exclude outliners
RFM<-subset(rawRFM,rawRFM$Recency<= 12 & rawRFM$Frequency<= 25 & rawRFM$Monetary>= 0 & rawRFM$Monetary<= 10000)
summary(rawRFM$Monetary)

par(mfrow = c(1,3))
hist(RFM$Recency)
hist(RFM$Frequency)
hist(RFM$Monetary)

#Now, the Left-skewness is better.
#take a look at disturbution
par(mfrow = c(1,3))
boxplot(RFM$Recency)$stats[c(1, 5), ]
boxplot(RFM$Frequency)$stats[c(1, 5), ]
boxplot(RFM$Monetary)$stats[c(1, 5), ]

#data normalization
df2<- RFM
nRFM<-as.data.frame(getRFMnor(df2))
#score
df3 <- nRFM
RFMs<-as.data.frame(getRFMscore(df3))
par(mfrow = c(1,3))
hist(RFMs$R)
hist(RFMs$Fq)
hist(RFMs$M)

#K-means cluster analysis
RFM_cluster <- data.frame(nRFM$R,nRFM$Fq,nRFM$M)
km <- kmeans(RFM_cluster,centers=5)
RFM_cluster$cluster <- as.factor(km$cluster)
RFM_cluster <- cbind(RFM_cluster,RFMs)
ggplot(RFM_cluster,aes(x=nRFM.R, y=nRFM.M, color= cluster,size= nRFM.Fq))+geom_point()+ scale_size_area(max_size=10)+labs(x="Recency", y="Monetary")

ggplot(RFM_cluster,aes(x=R, y= M, color= cluster,size= Fq))+geom_point()+ scale_size_area(max_size=20)+labs(x="Recency", y="Monetary")

#Seperate cluster
RFM_cluster1<-RFM_cluster[which(RFM_cluster$cluster==1),]
ggplot(RFM_cluster1,aes(x=R, y=M, color= Fq, size = 10))+geom_point()+ labs(x="Recency", y="Monetary")
apply(RFM_cluster1[,c(18,20:27)],2,mean)
ggplot(RFM_cluster1,aes(x=RFMScore))+geom_histogram(bins=50)+ labs(x="RFMScore", y="Count")

RFM_cluster2<-RFM_cluster[which(RFM_cluster$cluster==2),]
ggplot(RFM_cluster2,aes(x=R, y=M, color= Fq, size = 10))+geom_point()+ labs(x="Recency", y="Monetary")
apply(RFM_cluster2[,c(18,20:27)],2,mean)
ggplot(RFM_cluster2,aes(x=RFMScore))+geom_histogram(bins=50)+ labs(x="RFMScore", y="Count")

RFM_cluster3<-RFM_cluster[which(RFM_cluster$cluster==3),]
ggplot(RFM_cluster3,aes(x=R, y=M, color= Fq, size = 10))+geom_point()+ labs(x="Recency", y="Monetary")
apply(RFM_cluster3[,c(18,20:27)],2,mean)
ggplot(RFM_cluster3,aes(x=RFMScore))+geom_histogram(bins=50)+ labs(x="RFMScore", y="Count")


