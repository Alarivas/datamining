library(rpart)


A <- read.table("mining.data", sep=",",
                col.names=c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n"))

b=0
for (i in 1:nrow(A)){
	if(A[i,14]>0){
		A[i,14]=1
		b=b+1
	}	
}


formula = n ~ a + b + c + d + e + f + g + h + i + j + k + l + m   


 
#creación dtree
dtree<- rpart(formula, data=A, method="class")

#output 
printcp(dtree) 
plotcp(dtree) # visualize cross-validation results
summary(dtree) # detailed summary of splits
plot(dtree, uniform= TRUE, main="HOLI")
text(dtree, use.n=TRUE, all=TRUE, cex=.8)


#matriz de confusión
pred = predict(dtree, type="class")
table(pred,A$n)
