library(glmnet)

############# LOAD THE DATA  ####################
#Step1: Load data into matrix, X
#Step2: Load outcome into matrix, Y
#Step3: Load sampling weights into matrix, weights

############## APPLY SAMPLING WEIGHTS TO DATA ###
#Ref: McConville, Kelly. Improved estimation for complex surveys using modern regression. Doctoral dissertation, 2011.Colorado state
#university. http://bit.ly/1mqgQky
#BASED ON PAGE 63 of Equation 3.8
 
library(Matrix)
library(SparseM)

W <- Diagonal(x = (weights))
Ws<- solve(sqrt(W))
Ystar <- Ws%*%as.matrix(as.numeric(Y))
Xstar <- Ws%*%as.matrix(X)

################################################

##LASSO Regression 

library(caret)
library(glmnet)



###Cross validation to select the best tuning parameters
MultiControl <- trainControl(method = 'repeatedcv',number = 1,repeats = 10, classProbs = TRUE,returnResamp = "all",summaryFunction = twoClassSummary)


model <- train(Xstar,Ystar,method='glmnet',tuneGrid = expand.grid(.alpha=c(1),
.lambda=seq(0.1,1,length=5)))

####Choose best alpha and lambda based on the ouput, model

alpha <- 1
lambda <- 0.1

myglmnet =glmnet(Xstar,Ystar,alpha=alpha,lambda=lambda)
cfs <- coef(myglmnet)
length(cfs[which(cfs!=0)])

write.table(as.matrix(cfs),"LIST_OF_BEST_VARIABLES.txt",sep="\t",col.names=T,row.names=T)

