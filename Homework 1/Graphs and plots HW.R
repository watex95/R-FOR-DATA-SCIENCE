
# Question 1
library(ggplot2)
data("diamonds")
attach(diamonds)
table(diamonds$clarity)

# Question 2
unique(cut)
table(diamonds$cut)

# Question 3
library(dplyr)
diamonds %>%
  group_by(color) %>%
  summarise_at(vars(carat), funs(mean(., na.rm=TRUE)))


# Question 4
barchart(table(diamonds$cut),main="Number of diamonds per cut")
bar


mean(carat)
median(carat)



library(dplyr)
diamonds %>%
  group_by(color) %>%
  summarise_at(vars(carat), funs(mean(., na.rm=TRUE)))






# Question 5

# Histogram plot
hist(diamonds$carat,main = "Histogram for carat size")

# Boxplot
boxplot(diamonds$carat,main="Boxplot for carat size")

#Density plot
d <- density(diamonds$carat)
plot(d, type="n", main="Density plot for carat size")
polygon(d, col="red", border="gray")




# Question 6
ggplot(diamonds, aes(carat,price)) + geom_point(alpha=0.3,col="#0000ff22", pch=16,cex=3)

# Question 7
qplot(color,price,data=diamonds, geom = "boxplot")





