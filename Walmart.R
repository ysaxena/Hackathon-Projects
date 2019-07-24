setwd('F:/Analytics/R/Project/Part 2/Wallmart case')
getwd()

Wallmart<-read.csv('Wallmart.csv')
View(Wallmart)
str(Wallmart)

#Data Cleaning

# Missing Value Removal

sapply(Wallmart, function(x) sum(is.na(x)))

# No Missing values

cor(Wallmart)


box

boxplot(Wallmart$Customer_Satisfaction)
boxplot(Wallmart$Product_Quality)
boxplot(Wallmart$E_Commerce)# 8 Outliers
plot(Wallmart$E_Commerce, Wallmart$Customer_Satisfaction)
abline(lm(Wallmart$Customer_Satisfaction ~ Wallmart$E_Commerce), col='red')
Outvalue<-boxplot.stats(Wallmart$E_Commerce)$out
Outvalue

boxplot(Wallmart$Technical_Support)# Detected
boxplot(Wallmart$Complaint_Resolution)#1 Detected
Outvalue<-boxplot(Wallmart$Complaint_Resolution)$out
Wallmart<-Wallmart[-which(Wallmart$Complaint_Resolution %in% Outvalue), ]


boxplot(Wallmart$Advertising)
boxplot(Wallmart$Product_Line)
boxplot(Wallmart$Salesforce_Image)# Detected
boxplot(Wallmart$Competitive_Pricing)
boxplot(Wallmart$Warranty_Claims)# detected
Wallmart<-Wallmart[-which(Wallmart$Packaging %in% boxplot(Wallmart$Packaging)$out), ]

Outvalue<-boxplot(Wallmart$Order_Billing)$out
Wallmart<-Wallmart[-which(Wallmart$Order_Billing %in% Outvalue),]
boxplot(Wallmart$Delivery_Speed)

cor(Wallmart, Wallmart$Customer_Satisfaction)

model1<-lm(Wallmart$Customer_Satisfaction~Wallmart$Product_Line + Wallmart$Delivery_Speed + Wallmart$Complaint_Resolution + Wallmart$Product_Quality + Wallmart$Order_Billing + Wallmart$Salesforce_Image)
model1
summary(model1)


library(car)
car::vif(model1)
