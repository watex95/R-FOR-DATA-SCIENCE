

dat <- iris
dat$size <-ifelse(dat$Sepal.Length < median(dat$Sepal.Length),
           "small", "big")
# We now create a contingency table of the two variables Species and size with the table() function:
table(dat$Species, dat$size)


#The contingency table gives the observed number of cases in each
# subgroup. For instance, there is only one big setosa flower,
# while there are 49 small setosa flowers in the dataset.

# It is also a good practice to draw a barplot representing the data:
library(ggplot2)

ggplot(dat) +
  aes(x = Species, fill = size) +
  geom_bar() +
  scale_fill_hue() +
  theme_minimal()


# Chi-square test of independence
# For this example, we are going to test in R if there is a relationship
# between the variables Species and size. For this, the chisq.test()
# function is used:
  
test <- chisq.test(table(dat$Species, dat$size))
test


test$statistic # test statistic

test$p.value # p-value

# In our context, rejecting the null hypothesis for the Chi-square test
# of independence means that there is a significant relationship between
# the species and the size. Therefore, knowing the value of one variable
# helps to predict the value of the other variable























