library(AMORE)
library(boot)

data(beaver)
table(beaver$activ)

set.seed(100)
count=nrow(beaver)

idxTrain<-sample(1:count,2*count/3)
idxTest<-setdiff(1:count,idxTrain)

target<-function(x)
{
  n<-length(x)
  values<-unique(beaver[,"activ"])
  l<-length(values)
  T<-matrix(0,nrow=n,ncol=l)
  for(i in 1:l)
    T[,i]<-(x==values[i])
  colnames(T)<-values
  return(T)
}

setpoints<-target(beaver[,"activ"])
setpoints

network<-newff(n.neurons=c(3,3,2),
            learning.rate.global=0.05,
            momentum.global=0.01,
            hidden.layer="purelin",
            output.layer="sigmoid",
            method="ADAPTgdwm",
            error.criterium="LMS")

results<-train(network,
             beaver[idxTrain,],
             setpoints[idxTrain,],
             error.criterium="LMS",
             report=TRUE,
             show.step=5,
             n.shows=500)

plot(results$Merror,type="l",xlab="Ilerations (x5)",
     ylab="Error", col="darkred")

y<-sim(results$net,beaver[idxTest,])

classification_test<-function(s,res)
{
  set<-max.col(s)
  identified<-max.col(res)
  print(table(set,identified))
}
result<-classification_test(setpoints[idxTest,],y)

cat("Classification accuracy:",
    sum(diag(result))/sum(result)*100, "%\n")

