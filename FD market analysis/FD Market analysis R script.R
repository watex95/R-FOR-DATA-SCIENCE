setwd("C:/Users/Administrator/Desktop/Executive cyber")
getwd()
#Data processing
FD=read.csv("SAMPLE_FD_DATA.csv",header = T)
str(FD)

#descriptive statistics
attach(FD)

#Descriptive statistics
#Qualitative analysis
table(GENDER)
table(LOYALTY_SEGMENT)

#Quantitative analysis
#Correlation analysis
cor(FD$X12_Mo_Sales,FD$INCOME)
cor(FD$X12_Mo_Sales,FD$AGE)
cor(FD$X12_Mo_Sales,FD$X12.Mo..DELIVERY_FEE_PAID)

      
#SEGMENTATION OF CUSTOMERS
table(FD$LOYALTY_SEGMENT,FD$GEOGRAPHY)



#MODEL

model=lm(FD$X12_Mo_Sales~FD$X12.Mo..DELIVERYPASS_USED+FD$X12.Mo..ORDERS_W_PROMO+FD$GENDER+FD$AGE+FD$INCOME+FD$GENDER+
    FD$X12.Mo..DELIVERY_FEE_PAID+FD$X12.Mo..DISCOUNT_AMOUNT+
      FD$X12.Mo..Orders)
summary(model)



