library(e1071)
library(rpart) 
library(plyr)


A <- read.table("mining.data", sep=",",
                col.names=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14"))



for (i in 1:nrow(A)){
	if(A[i,14]>0){
		A[i,14]=1
	}	
}

formula = X14 ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13

k=10
average_svm=0
average_tree=0

folds <- split(A, cut(sample(1:nrow(A)),k))

for(i in 1:length(folds)){
	test <- ldply(folds[i])
	train <- ldply(folds[-i])
	svm.model <- svm(train[,2:14], train[,15], type= "C-classification", kernel="radial")
	svm.pred  <- predict(svm.model, test[,2:14])
	mat <- table(svm.pred, test[,15])
	print(table(svm.pred, test[,15]))
	accu_svm= sum(diag(mat)/sum(mat))
	print(accu_svm)
	average_svm= average_svm+ accu_svm


	dtree<- rpart(formula, data=train, method="class")
	pred <- predict(dtree, newdata= test,type="class")
	mat2 <- table(pred, test[,15])
	print(table(pred, test[,15]))
	accu_tree= sum(diag(mat2))/sum(mat2) 
	print(accu_tree)
	average_tree= average_tree+ accu_tree

}

print(sprintf("average svm: %f", average_svm/k))
print(sprintf("average tree: %f", average_tree/k))
