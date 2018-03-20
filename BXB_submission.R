setwd("E:/R")
trainbxb = read.csv("bxbtrain.csv")
trainbxb = as.data.frame(trainbxb)

trainbxb[[1]] = as.character(trainbxb[[1]])
for (i in 1:nrow(trainbxb))
{
  trainbxb[[1]][i] = substr(trainbxb[[1]][i],1,nchar(trainbxb[[1]][i])-8)
  if(i!=1)
  {
    trainbxb$PrevTransfer[i] = trainbxb$TransfersInMonth[i-1]
    trainbxb$PrevPrevTransfer[i] = trainbxb$TransfersInMonth[i-2]
    
  }
}
trainbxb = na.omit(trainbxb)

#one-hot-encoding categorical features
ohe_feats = c('Month')
dummies = dummyVars(~ Month, data = trainbxb)
trainbxb_ohe = as.data.frame(predict(dummies, newdata = trainbxb))
trainbxb = cbind(trainbxb[,-c(which(colnames(trainbxb) %in% ohe_feats))],trainbxb_ohe)


testbxb = read.csv("bxbtest.csv")
testbxb1 = testbxb

testbxb[[1]] = as.character(testbxb[[1]])
for (i in 1:nrow(testbxb))
{
  testbxb[[1]][i] = substr(testbxb[[1]][i],1,nchar(testbxb[[1]][i])-8)
  if(i==1)
  {
    testbxb$PrevTransfer[i] = trainbxb$TransfersInMonth[nrow(trainbxb)]
    testbxb$PrevPrevTransfer[i] = trainbxb$TransfersInMonth[nrow(trainbxb)-1]
  }
  else if(i==2)
  {
    testbxb$PrevTransfer[i] = trainbxb$TransfersInMonth[i-1]
    testbxb$PrevPrevTransfer[i] = trainbxb$TransfersInMonth[nrow(trainbxb)]
  }
  else
  {
    testbxb$PrevTransfer[i] = testbxb$TransfersInMonth[i-1]
    testbxb$PrevPrevTransfer[i] = testbxb$TransfersInMonth[i-2]
  }
}
testbxb = na.omit(testbxb)

n = nrow(testbxb)
for (i in 1:12)
{
  testbxb[n+i,] = testbxb[1,]
  testbxb[n+i,1] = i
}


ohe_feats = c('Month')
dummies = dummyVars(~ Month, data = testbxb)
testbxb_ohe = as.data.frame(predict(dummies, newdata = testbxb))
testbxb = cbind(testbxb[,-c(which(colnames(testbxb) %in% ohe_feats))],testbxb_ohe)

testbxb = testbxb[1:n,]


#preprocessParams = preProcess(trainbxb[,-3], method=c("range","pca"))
preprocessParams = preProcess(trainbxb[,-3], method=c("range"))
# summarize transform parameters
print(preprocessParams)
# transform the dataset using the parameters
trainbxb = predict(preprocessParams, trainbxb)

preprocessParams = preProcess(testbxb, method=c("range"))
testbxb = predict(preprocessParams, testbxb)

library(caTools)
set.seed(10)
split = sample.split(trainbxb$IssuesInMonth,SplitRatio = 0.70)
trainingbxb = subset(trainbxb, split == TRUE)
testingbxb = subset(trainbxb, split == FALSE)

#model1 = lm(IssuesInMonth ~ .,data = trainingbxb)
model1 = lm(IssuesInMonth ~ Month3+Month4+Month5+Month6+TransfersInMonth+PrevTransfer+PrevPrevTransfer, data=trainingbxb)
summary(model1)

predicttest = predict(model1,newdata = testingbxb)

mae = function(error)
{
  mean(abs(error))
}
error = (testingbxb$IssuesInMonth - predicttest)/(testingbxb$IssuesInMonth)

mae(error)

predicttestfinal = predict(model1, newdata = testbxb)

submitbxb = data.frame(Month = testbxb1$Month, IssuesInMonth = predicttestfinal)
write.csv(submitbxb,file = "BXB_Submission_2.csv", row.names = FALSE)

DMwR::regr.eval(testingbxb$IssuesInMonth, predicttest)

