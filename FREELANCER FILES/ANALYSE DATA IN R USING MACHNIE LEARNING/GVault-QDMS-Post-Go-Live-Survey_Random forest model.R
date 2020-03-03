
install.packages("randomForest") 
install.packages("e1071")

library(randomForest)  
library(e1071)  
library(caret)  
library(ggplot2)  
set.seed(123) 
library(RCurl)
# Original dataset
#Raw dataset
df=read.csv("Gvault_survey_encoded.csv",header = T)

# Drop irrelevant columns
df = subset(df, select = -c(Respondent.ID,Collector.ID,Start.Date,
          End.Date,explain_training_effectiveness,explain_support,
          explain_complete_without_help,explain_easy_access_documents,
          explain_satisfaction,explain_Gvault_efficiency,explain_Gvault_improved,
          explain_office_location,explain_job_level) )

# Explore the data before fitting a model to get an idea of what to expect.
# I am plotting a variable on two axes and using colors to see the relationship
# among the levels of satisfaction.

library(ggplot2)
# Lets explore the relationship between satisfaction and frequency of use

ggplot(df,aes(x=df1$freq_use,y=df$satisfaction,color=df$satisfaction))+ geom_jitter(alpha=0.3)+scale_color_manual(breaks = c('Satisfied',
          'Very Unsatisfied','Very Satisfied','Slightly Unsatisfied'),
             values=c('darkgreen','red','blue','yellow'))

# A comparison of Frequency of use (freq_use) & effectiveness of training to 
# satisfaction shows us: Users who used Gvault daily were more likely to be 
# satisfied or very satisfied

# I'm looking for spots where there exists an overwhelming majority of one color.

p1=ggplot(df,aes(x=df1$freq_use,y=df$training_effectiveness,
                 color=df$satisfaction))+theme(axis.text.x = element_text(angle = 90)) 
p1 + geom_jitter(alpha=0.3) +  
  scale_color_manual(breaks = c('Satisfied','Very Unsatisfied','Very Satisfied','Slightly Unsatisfied'),
    values=c('darkgreen','red','blue','yellow'))

# A comparison of role and the rate of completing work without help in terms
# likelihood of satisfaction show that:
# Reveiwer/Approver role users completed work in Gvault without help all the time
# and were more likely to be satisfied
p1=ggplot(df,aes(y=df$role,x=df$complete_without_help,
                  color=df$satisfaction))
p1 + geom_jitter(alpha=0.3) +  
  scale_color_manual(breaks = c('Satisfied','Very Unsatisfied','Very Satisfied','Slightly Unsatisfied'),
                     values=c('darkgreen','red','blue','yellow'))

#Facet plots 
library(ggplot2)
df1$office_location
df1$satisfaction

counts=table(df1$satisfaction,df1$office_location)
as.data.frame(counts)
counts
t_counts=t(counts)
t_counts
facet_data=as.data.frame.matrix(t_counts)

write.csv(facet_data,"D:/mydata/facet_data.csv")
facet_data1=read.csv("D:/mydata/facet_data.csv")

attach(facet_data1)
office_location
Satisfied

dev.new()
library(ggplot2)
ggplot(facet_data1,aes(x=office_location,y=Satisfied))+
  geom_bar(stat='identity',fill="forest green")+
  ylab("Satisfied")

require(tidyr)
require(dplyr)
require(lazyeval)

facetLong=gather(facet_data1,key="measure",value="value",c("Very.Satisfied",
                "Satisfied","Slightly.Unsatisfied","Very.Unsatisfied"))

myplot=ggplot(facetLong,aes(x=office_location,y=value))+geom_bar(stat='identity',fill="forest green")+facet_wrap(~measure,ncol = 1)

print(myplot)

require(reshape)

dat <- melt(facet_data1,id.vars = "office_location")

ggplot(dat,aes(x=office_location, y = value)) + 
  facet_wrap(~variable) +
  geom_bar(aes(fill = dat$office_location), stat = "identity")


#Create data for training
sample.ind = sample(2,nrow(df1),replace = T,prob = c(0.9,0.1))
data.dev = df[sample.ind==1,]  
data.val = df[sample.ind==2,]


# I wanted to know the split of edible to poisonous mushrooms in the data set and compare it to the training and test data.
# Original Data
table(df$satisfaction)/nrow(df)

# Training Data
table(data.dev$satisfaction)/nrow(data.dev)

# Testing Data
table(data.val$satisfaction)/nrow(data.val) 

# Model Training

# I finally fit the random forest model to the training data. Plotting the model shows us that after about 20 trees, not much changes in terms of error. It fluctuates a bit but not to a large degree.
#Fit Random Forest Model
library(randomForest)
rf = randomForest(satisfaction ~ .,ntree = 100,data = data.dev)
plot(rf)


# Feature engineering

# Variable Importance
varImpPlot(rf, sort = T, n.var=10,main="Top 10 - Variable Importance")


#Variable Importance
# Odor is by far the most important variable in terms of 
# "Mean Decreasing Gini" - a similar term for information gain in this
# example. The rest of the results are listed below

var.imp = data.frame(importance(rf, type=2))
# make row names as columns
var.imp$Variables = row.names(var.imp)  
print(var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),])



# Prediction and Model Evaluation

# I decided to use the model to attempt to predict the satisfaction level 
# based off of the training data set. It predicted the response variable
# perfectly - having zero false positives or false negatives.

# Predicting response variable
data.dev$predicted.response = predict(rf , data.dev)

# Create Confusion Matrix
print(confusionMatrix(data = data.dev$predicted.response,  
                  reference = data.dev$satisfaction,
                  positive ='Very Satisfied'))

# Model Testing

# Now it was time to see how the model did with data it had not seen before- making
# predictions on the test data.
# Predicting response variable
data.val$predicted.response <- predict(rf ,data.val)

# Create Confusion Matrix
print(confusionMatrix(data=data.val$predicted.response,  
                  reference=data.val$satisfaction,
                  positive='Very Satisfied'))

