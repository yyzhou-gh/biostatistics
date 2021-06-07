##Perform Lasso  5mer
library(glmnet)
sum_y_top20<-rep(0,20)
sum_y_total<-rep(0,980)
for(m in 1:10){
  cat("m=",m,'\t')
  y.test.pred.total<-c()
  for(i in 1:10){
    Lasso.cv<-cv.glmnet(x.train[[i]],y.train[[i]])
    bestlambda<-Lasso.cv$lambda.min
    Lasso.model<-Lasso.cv$glmnet.fit
    y.test.pred.Lasso<-predict(Lasso.model,newx=x.test[[i]],s=bestlambda)
    y.test.pred.total<-c(y.test.pred.total,y.test.pred.Lasso)
  }
  names(y.test.pred.total)<-tmp
  sum_y_top20 <- y.test.pred.total[sgRNA.top20.id] + sum_y_top20
  sum_y_total <- y.test.pred.total+sum_y_total
}
y.test.pred.top20 <- sum_y_top20/10
y.test.pred.total.Lasso.mean <- sum_y_total/10
PCC.Lasso.5mer.top20<-cor(y.test.total[sgRNA.top20.id],y.test.pred.top20)
PCC.Lasso.5mer.all <- cor(y.test.total,y.test.pred.total.Lasso.mean)
save(y.test.total,y.test.pred.total.Lasso.mean,PCC.Lasso.5mer.top20,PCC.Lasso.5mer.all,file="C:\\Users\\56881\\Desktop\\生统课程作业\\regression\\result\\PCC.Lasso.5mer.Rdata")

##Perfrom Ridge Regression
sum_y_top20<-rep(0,20)
sum_y_total<-rep(0,980)
for(m in 1:10){
  cat("m=",m,'\t')
  y.test.pred.total<-c()
  for(i in 1:10){
    Ridge.cv<-cv.glmnet(x.train[[i]],y.train[[i]],alpha=0)
    bestlambda<-Ridge.cv$lambda.min
    Ridge.model<-Ridge.cv$glmnet.fit
    y.test.pred.Ridge<-predict(Ridge.model,newx=x.test[[i]],s=bestlambda)
    y.test.pred.total<-c(y.test.pred.total,y.test.pred.Ridge)
  }
  names(y.test.pred.total)<-tmp
  sum_y_top20 <- y.test.pred.total[sgRNA.top20.id] + sum_y_top20
  sum_y_total <- y.test.pred.total+sum_y_total
}
y.test.pred.top20 <- sum_y_top20/10
y.test.pred.total.Ridge.mean <- sum_y_total/10
PCC.Ridge.7mer.top20<-cor(y.test.total[sgRNA.top20.id],y.test.pred.top20)
PCC.Ridge.7mer.all <- cor(y.test.total,y.test.pred.total.Ridge.mean)
save(y.test.total,y.test.pred.total.Ridge.mean,PCC.Ridge.7mer.top20,PCC.Ridge.7mer.all,file="C:\\Users\\56881\\Desktop\\生统课程作业\\regression\\result\\PCC.Ridge.5mer.Rdata")


##Perform SVR
library(e1071)
rbf.par=c( c(0.00001,0.00002,0.00005,0.0001,0.0002,0.0005,0.001,0.002,0.005,0.1,0.2,0.5,1), seq(5,100,20),seq(100,1000,200))
PCC.SVR.RBF.top20<-matrix(,2,length(rbf.par))
PCC.SVR.RBF.top20[1,]<-rbf.par
PCC.SVR.RBF.all<-matrix(,2,length(rbf.par))
PCC.SVR.RBF.all[1,]<-rbf.par
y.test.pred.SVR.total<-list()
for(m in 1:length(rbf.par)){
  cat("m=",m)
  y.test.pred.total<-c()
  SVR.kernel="radial"
  rbf.kpar=rbf.par[m]
  for(i in 1:10){
    SVR.model<-svm(x.train[[i]],y.train[[i]],kernel=SVR.kernel,gamma=rbf.kpar);
    y.test.pred.SVR<-predict(SVR.model,x.test[[i]]);
    y.test.pred.total<-c(y.test.pred.total,y.test.pred.SVR);
  }
  names(y.test.pred.total)<-tmp
  PCC.SVR.RBF.top20[2,m]<-cor(y.test.total[sgRNA.top20.id],y.test.pred.total[sgRNA.top20.id])
  PCC.SVR.RBF.all[2,m] <- cor(y.test.total,y.test.pred.total)
  y.test.pred.SVR.total[[m]] <- y.test.pred.total
}
save(PCC.SVR.RBF.top20,PCC.SVR.RBF.all,y.test.pred.SVR.total,y.test.total,file="C:\\Users\\56881\\Desktop\\生统课程作业\\regression\\result\\SVR_RBF_PCC_5mer_100bp.Rdata")

##Perform RandomForest
library(randomForest)
PCC.RF.5mer.100bp.top20<-matrix(,10,50)
PCC.RF.5mer.100bp.all<-matrix(,10,50)
rownames(PCC.RF.5mer.100bp.top20)<-rep("ntree",10)
rownames(PCC.RF.5mer.100bp.all)<-rep("ntree",10)
for(n in 1:10){
  rownames(PCC.RF.5mer.100bp.top20)[n]=paste("ntree=",50+50*(n-1),sep="")
  rownames(PCC.RF.5mer.100bp.all)[n]=paste("ntree=",50+50*(n-1),sep="")
}
colnames(PCC.RF.5mer.100bp.top20)<-rep("mtry",50)
colnames(PCC.RF.5mer.100bp.all)<-rep("mtry",50)
for(m in 1:50){
  colnames(PCC.RF.5mer.100bp.top20)[m]=paste("mtry=",10+10*(m-1),sep="")
  colnames(PCC.RF.5mer.100bp.all)[m]=paste("mtry=",10+10*(m-1),sep="")
}
y.test.pred.RF.total<-list();length(y.test.pred.RF.total)<-10
for(j in 1:10){
  y.test.pred.RF.total[[j]]<-list();length(y.test.pred.RF.total[[j]])<-50
}

for(n in 1:10){
  for(m in 1:50){
    y.test.pred.RF.total[[n]][[m]]<-c(1)
  }
}
##Perform CV to determine optimal parameters of ntree and mtry
for(n in 1:10){
  ntree<-50+50*(n-1)
  for(m in 1:50){
    mtry<-10+10*(m-1)
    cat("mtry=",mtry)
    for(i in 1:10){
      cat("\r",i);
      RF.cv<-randomForest(x.train[[i]],y.train[[i]],ntree=ntree,mtry=mtry)
      y.test.pred.RF<-predict(RF.cv,x.test[[i]])
      y.test.pred.RF.total[[n]][[m]]<-c(y.test.pred.RF.total[[n]][[m]],y.test.pred.RF)
    }
    y.test.pred.RF.total[[n]][[m]]<-y.test.pred.RF.total[[n]][[m]][-1]
    names(y.test.pred.RF.total[[n]][[m]])<-tmp
    PCC.RF.5mer.100bp.top20[n,m]<-cor(y.test.total[sgRNA.top20.id],y.test.pred.RF.total[[n]][[m]][sgRNA.top20.id])
    PCC.RF.5mer.100bp.all[n,m]<-cor(y.test.total,y.test.pred.RF.total[[n]][[m]])
  }
}
##Perform CV to determine optimal parameters of ntree and mtry
save(PCC.RF.5mer.100bp.top20,PCC.RF.5mer.100bp.all,y.test.total,y.test.pred.RF.total,file="PCC.RF.5mer.100bp.50ntree.Rdata")

