install.packages("RDocumentation")


data(iris)

backpropagation(darch, trainData, targetData,
bp.learnRate = getParameter(".bp.learnRate", rep(1, times =
length(darch@layers))),
bp.learnRateScale = getParameter(".bp.learnRateScale"),
nesterovMomentum = getParameter(".darch.nesterovMomentum"),
dropout = getParameter(".darch.dropout", rep(0, times = length(darch@layers)
 + 1), darch), dropConnect = getParameter(".darch.dropout.dropConnect"),
matMult = getParameter(".matMult"), debugMode = getParameter(".debug", F),
    )


# NOT RUN {
data(iris)
model <- darch(Species ~ ., iris, darch.fineTuneFunction = "backpropagation")
# }



# Training a Neural Network Model using neuralnet

#Neural Network
library(neuralnet)
nn <- neuralnet(dividend ~ fcfps + earnings_growth + de + mcap + current_ratio, data=trainset, hidden=c(2,1), linear.output=FALSE, threshold=0.01)
nn$result.matrix
plot(nn)

nn$result.matrix

# Testing The Accuracy Of The Model
#Test the resulting output
temp_test <- subset(testset, select = c("fcfps","earnings_growth", "de", "mcap", "current_ratio"))
head(temp_test)
nn.results <- compute(nn, temp_test)
results <- data.frame(actual = testset$dividend, prediction = nn.results$net.result)

# Confusion Matrix
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)


















