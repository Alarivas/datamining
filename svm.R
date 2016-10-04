library(e1071)
library(rpart)
library(SparseM)  


A <- read.table("mining.data", sep=",",
                col.names=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14"))
filas <- nrow(A)                                 # Count the rows in A



colums 	  <- 1:13
index     <- 1:nrow(A)
testindex <- sample(index, trunc(length(index)/3))
testset   <- A[testindex,]
trainset  <- A[-testindex,]

test_label <-testset[,14]
testset    <-testset[,colums]

train_label <- trainset[,14]
trainset    <- trainset[,colums]

for (i in 1:nrow(testset)){
	if(test_label[i]>0){
		test_label[i]=1
	}	
}

for (i in 1:nrow(trainset)){
	if(train_label[i]>0){
		train_label[i]=1
	}	
}



svm.model <- svm(trainset,train_label, type= "C-classification", kernel="polynomial", degree=2)
summary(svm.model)
svm.pred  <- predict(svm.model, testset)
table(svm.pred, test_label)
