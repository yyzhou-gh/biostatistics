#在训练集做十折交叉验证的划分
neg_train_5mer<-read.table('C:/Users/56881/Desktop/生统课程作业/classification/data/neg_train_5mer.txt',sep=' ',header = FALSE)
pos_train_5mer<-read.table('C:/Users/56881/Desktop/生统课程作业/classification/data/pos_train_5mer.txt',sep=' ',header = FALSE)
neg_test_5mer<-read.table('C:/Users/56881/Desktop/生统课程作业/classification/data/neg_test_5mer.txt',sep=' ',header = FALSE)
pos_test_5mer<-read.table('C:/Users/56881/Desktop/生统课程作业/classification/data/pos_test_5mer.txt',sep=' ',header = FALSE)
test=rbind(neg_test_5mer,pos_test_5mer)
test[1:335,1025]<-'0'
test[336:712,1025]<-'1'

x.test1<-test[1:712,1:1024] #真实测试集
y.test1<-matrix(,nrow = 712,ncol=1)
y.test1<-test[,1025]

train=rbind(neg_train_5mer,pos_train_5mer)
train[1:1443,1025]<-'0'
train[1444:2844,1025]<-'1'
train.index<-split(sample(1:2844),rep((1:10),length=2844))
save(train.index,file = "C:/Users/56881/Desktop/生统课程作业/classification/result/train.index.Rdata")  #确定分组后不再改动

x<-train[,1:1024]
y<-matrix(,nrow = 2844,ncol=1)
y[,1]<-train[,1025]
y<-as.factor(y)
x.train<-list()
x.test<-list()
y.train<-list()
y.test<-list()
y.test.total<-c()


for(i in 1:10){
  x.test[[i]]<-x[train.index[[i]],]
  x.train[[i]]<-x[setdiff(1:2844,train.index[[i]]),]
  y.test[[i]]<-y[train.index[[i]]]
  y.train[[i]]<-y[setdiff(1:2844,train.index[[i]])]
  y.test.total<-c(y.test.total,y.test[[i]])
}
save.image(file="C:/Users/56881/Desktop/生统课程作业/classification/workspace/cv.group.Rdata")

####################SVM
library(e1071)
library("pROC")
#rbf.par=c(0.0001,0.001,0.1,1,10,100,500,1000)
rbf.par=c(0.00001,0.0001,0.0005,0.001,0.005,0.01,0.05,0.1)
ACC.SVM.RBF.all<-matrix(,2,length(rbf.par))
ACC.SVM.RBF.all[1,]<-rbf.par
recall.SVM.RBF.all<-matrix(,2,length(rbf.par))
recall.SVM.RBF.all[1,]<-rbf.par
precision.SVM.RBF.all<-matrix(,2,length(rbf.par))
precision.SVM.RBF.all[1,]<-rbf.par
F1.SVM.RBF.all<-matrix(,2,length(rbf.par))
F1.SVM.RBF.all[1,]<-rbf.par
y.test.pred.SVM.total<-list()
auc.result<-list()

library(pROC)
for(m in 1:length(rbf.par)){
  cat("m=",m)
  #m=1
  y.test.pred.total<-c()
  prob_mult.total<-matrix(,,2)
  rbf.kpar=rbf.par[m]
  for(i in 1:10){
    #i=1
    #make two-class SVM
    SVM.model<-svm(x.train[[i]],y.train[[i]],kernel="radial",gamma=rbf.kpar,type='C-classification',probability = T);
    #Get related decidion values
    dec_vals_mult<-attr(predict(SVM.model,x.test[[i]],decision.values = T),"decision.values");
    #Get related probabilities
    prob_mult<-attr(predict(SVM.model,x.test[[i]],probability = T),"probabilities")
    prob_mult.total<-rbind(prob_mult.total,prob_mult)
    prob_mult.total<-na.omit(prob_mult.total)
    y.test.pred.SVM<-predict(SVM.model,x.test[[i]],decision.values = T)
    y.test.pred.total<-c(y.test.pred.total,y.test.pred.SVM);
  }
  roc.result<-roc(as.numeric(y.test.total),prob_mult.total[,2] , plot=TRUE, print.thres=TRUE, print.auc=TRUE)
  #取分类为1的probabilities
  auc.result[[m]]<-roc.result$auc
  confusionMatrix<-table(y.test.pred.total,y.test.total)
  TP.freq<-confusionMatrix[4]
  FP.freq<-confusionMatrix[2]
  FN.freq<-confusionMatrix[3]
  TN.freq<-confusionMatrix[1]
  ACC.SVM.RBF.all[2,m]=(TP.freq+TN.freq)/length(y.test.pred.total)
  recall.SVM.RBF.all[2,m]=TP.freq/(TP.freq+FN.freq)
  precision.SVM.RBF.all[2,m]=TP.freq/(TP.freq+FP.freq)
  F1.SVM.RBF.all[2,m]=2*TP.freq/(length(y.test.pred.total)+TP.freq-TN.freq)
  y.test.pred.SVM.total[[m]] <- y.test.pred.total
} #5.27,17:26开始跑

save(ACC.SVM.RBF.all,recall.SVM.RBF.all,precision.SVM.RBF.all,F1.SVM.RBF.all,y.test.pred.SVM.total,y.test.total,file="C:/Users/56881/Desktop/生统课程作业/classification/result/SVM.result.Rdata")
save.image("svm.workspace.Rdata")


#最优参数为gamma=0.0005,用训练集训练模型，再预测测试集
SVM.model<-svm(x,y,kernel="radial",gamma=0.0005,type='C-classification',probability = T);
prob_mult<-attr(predict(SVM.model,x.test1,probability = T),"probabilities")
y.test1.pred.SVM<-predict(SVM.model,x.test1,decision.values = T)
roc.result<-roc(as.numeric(y.test1),prob_mult[,2] , plot=TRUE, print.thres=TRUE, print.auc=TRUE)
auc.test1.result<-roc.result$auc
confusionMatrix<-table(y.test1.pred.SVM,y.test1)
y.test1
TP.freq<-confusionMatrix[4]
FP.freq<-confusionMatrix[2]
FN.freq<-confusionMatrix[3]
TN.freq<-confusionMatrix[1]
ACC.SVM.test1=(TP.freq+TN.freq)/length(y.test1)
recall.SVM.test1=TP.freq/(TP.freq+FN.freq)
precision.SVM.test1=TP.freq/(TP.freq+FP.freq)
F1.SVM.test1=2*TP.freq/(length(y.test1)+TP.freq-TN.freq)


#################logistic
glm.probs.total<-c()
x_y.test<-list()
for(i in 1:10){
  data<-x.train[[i]]
  data$label<-y.train[[i]]
  glm.fit=glm(label~.,data=data,family=binomial,control=list(maxit=100))
  glm.probs=predict(glm.fit,newdata=x.test[[i]], type="response")
  glm.probs.total<-c(glm.probs.total,glm.probs)
}
save(glm.fit,file="C:/Users/56881/Desktop/生统课程作业/classification/result/glm.fit.Rdata")
#合并十折，用训练集计算模型泛化能力
glm.pred=rep("0",length(glm.probs.total))
glm.pred[glm.probs.total>.5]="1"
roc.logi<-roc(as.numeric(y.test.total),glm.probs.total)
auc.logi<-roc.logi$auc
confusionMatrix<-table(glm.pred,y.test.total)
TP.freq<-confusionMatrix[4]
FP.freq<-confusionMatrix[2]
FN.freq<-confusionMatrix[3]
TN.freq<-confusionMatrix[1]
ACC.logi=(TP.freq+TN.freq)/length(y.test.total)
recall.logi=TP.freq/(TP.freq+FN.freq)
precision.logi=TP.freq/(TP.freq+FP.freq)
F1.logi=2*TP.freq/(length(y.test.total)+TP.freq-TN.freq)
save(auc.logi,ACC.logi,recall.logi,precision.logi,F1.logi,file="C:/Users/56881/Desktop/生统课程作业/classification/result/logi.result.Rdata")


##########lda
lda.probs.total<-c()
lda.class.total<-c()
for(i in 1:10){
  data<-x.train[[i]]
  data$label<-y.train[[i]]
  lda.fit <- lda(y.train[[i]]~as.matrix(x.train[[i]]),data=data)
  lda.fit <- lda(as.matrix(x.train[[i]]),y.train[[i]])
  lda.fit <- lda(label~.,data=data)
  lda.result <- predict(lda.fit,x.test[[i]])
  lda.class<- lda.result[[1]]
  lda.class.total<-c(lda.class.total,lda.class)
  lda.probs<-lda.result[[2]][,2]
  lda.probs.total<-c(lda.probs.total,lda.probs)
}
#合并十折，用训练集计算模型泛化能力
roc.lda<-roc(as.numeric(y.test.total),lda.probs.total)
auc.lda<-roc.lda$auc
#取分类为1的probabilities
# roc(as.numeric(y.test.total), as.numeric(y.test.pred.total), plot=TRUE, print.thres=TRUE, print.auc=TRUE,type='prob')#main="Smoothing"
confusionMatrix<-table(lda.class.total,y.test.total)
TP.freq<-confusionMatrix[4]
FP.freq<-confusionMatrix[2]
FN.freq<-confusionMatrix[3]
TN.freq<-confusionMatrix[1]
ACC.lda=(TP.freq+TN.freq)/length(y.test.total)
recall.lda=TP.freq/(TP.freq+FN.freq)
precision.lda=TP.freq/(TP.freq+FP.freq)
F1.lda=2*TP.freq/(length(y.test.total)+TP.freq-TN.freq)
save(auc.lda,ACC.lda,recall.lda,precision.lda,F1.lda,file="C:/Users/56881/Desktop/生统课程作业/classification/result/lda.result.Rdata")

#####RF
library(randomForest)
RF.assess<-matrix(0,5,25)#5行25列的总结模型性能的矩阵
rownames(RF.assess)<-c("AUC","ACC",'recall','precicion','F1')
RF.assess.col.name.list<-c()
for(m in c("ntree=100","ntree=200","ntree=300","ntree=400","ntree=500")){
  for(n in c("mtry=10","mtry=30","mtry=50","mtry=70","mtry=90")){
    RF.assess.col.name.list<-c(RF.assess.col.name.list,paste0(m,",",n))
  }
}
RF.assess.col.name.list<-RF.assess.col.name.list[1:25]
colnames(RF.assess)<-RF.assess.col.name.list
  
##Perform CV to determine optimal parameters of ntree and mtry

counts=0
for(n in 1:5){
  #n=1
  ntree<-100+100*(n-1)
  for(m in 1:5){
    #m=1
    counts=counts+1
    mtry<-10+20*(m-1)
    cat("mtry=",mtry)
    prob.rf.total<-c()
    y.test.pred.label.RF.total<-c()
    for(i in 1:10){
      cat("\r",i);
      data<-x.train[[i]]
      data$label<-y.train[[i]]

      RF.fit<-randomForest(label~.,data=data,ntree=ntree,mtry=mtry,importance=TRUE ,
                           proximity=TRUE);
      y.test.pred.rf<-predict(RF.fit,newdata=as.matrix(x.test[[i]]),type='prob')
      y.test.pred.label<-predict(RF.fit,newdata=as.matrix(x.test[[i]]))
      prob.rf<-y.test.pred.rf[,2]
      prob.rf.total<-c(prob.rf.total,prob.rf)
      y.test.pred.label.RF.total<-c(y.test.pred.label.RF.total,y.test.pred.label)
      
    }
        
    roc.RF<-roc(as.numeric(y.test.total),prob.rf.total)
    auc.RF<-roc.RF$auc
    confusionMatrix<-table(y.test.pred.label.RF.total,y.test.total)
    TP.freq<-confusionMatrix[4]
    FP.freq<-confusionMatrix[2]
    FN.freq<-confusionMatrix[3]
    TN.freq<-confusionMatrix[1]
    ACC.RF=(TP.freq+TN.freq)/length(y.test.total)
    recall.RF=TP.freq/(TP.freq+FN.freq)
    precision.RF=TP.freq/(TP.freq+FP.freq)
    F1.RF=2*TP.freq/(length(y.test.total)+TP.freq-TN.freq)
    
    RF.assess[1,counts]<-auc.RF
    RF.assess[2,counts]<-ACC.RF
    RF.assess[3,counts]<-recall.RF
    RF.assess[4,counts]<-precision.RF
    RF.assess[5,counts]<-F1.RF
        
  }
}
save(RF.assess,prob.rf.total,y.test.pred.label.RF.total,file="C:/Users/56881/Desktop/生统课程作业/classification/result/RF.result.Rdata")

#模型用到测试集上
data<-x
data$label<-y
RF.fit1<-randomForest(label~.,data=data,ntree=300,mtry=40,importance=TRUE ,
                     proximity=TRUE);
y.test1.pred.rf<-predict(RF.fit1,newdata=as.matrix(x.test1),type='prob')
y.test1.pred.label<-predict(RF.fit1,newdata=as.matrix(x.test1))
roc.RF.test1<-roc(as.numeric(y.test1),y.test1.pred.rf[,2])
auc.RF.test1<-roc.RF.test1$auc
confusionMatrix<-table(y.test1.pred.label,y.test1)
TP.freq<-confusionMatrix[4]
FP.freq<-confusionMatrix[2]
FN.freq<-confusionMatrix[3]
TN.freq<-confusionMatrix[1]
ACC.RF.test1=(TP.freq+TN.freq)/length(y.test1)
recall.RF.test1=TP.freq/(TP.freq+FN.freq)
precision.RF.test1=TP.freq/(TP.freq+FP.freq)
F1.RF.test1=2*TP.freq/(length(y.test1)+TP.freq-TN.freq)

#######KNN
library(kknn)
k.list=c(5,10,15,20)
knn.assess<-matrix(,5,4)
rownames(knn.assess)<-c("auc","acc","recall","precision","F1")
colnames(knn.assess)<-c("k=5","k=10","k=15","k=20")

for(j in k.list){
  cat("k=",j)
  y.test.pred.knn.total<-c()
  y.test.prob.knn.total<-c()
  for(i in 1:10){
    
    data<-x.train[[i]]
    data$label<-y.train[[i]]
    knn.fit <- kknn(label~.,data,x.test[[i]],k=j,distance = 1)
    y.test.pred.knn<- knn.fit[[1]]
    y.test.pred.knn.total<-c(y.test.pred.knn.total,y.test.pred.knn)
    y.test.prob.knn<-knn.fit[[6]][,2]
    y.test.prob.knn.total<-c(y.test.prob.knn.total,y.test.prob.knn)
  }
#合并十折，用训练集计算模型泛化能力
  roc.knn<-roc(as.numeric(y.test.total),y.test.prob.knn.total)
  auc.knn<-roc.knn$auc
  #取分类为1的probabilities
  # roc(as.numeric(y.test.total), as.numeric(y.test.pred.total), plot=TRUE, print.thres=TRUE, print.auc=TRUE,type='prob')#main="Smoothing"
  confusionMatrix<-table(y.test.pred.knn.total,y.test.total)
  #confusionMatrix<-table(y.test.pred.knn.total,y.test[[1]])
  TP.freq<-confusionMatrix[4]
  FP.freq<-confusionMatrix[2]
  FN.freq<-confusionMatrix[3]
  TN.freq<-confusionMatrix[1]
  ACC.knn=(TP.freq+TN.freq)/length(y.test.total)
  recall.knn=TP.freq/(TP.freq+FN.freq)
  precision.knn=TP.freq/(TP.freq+FP.freq)
  F1.knn=2*TP.freq/(length(y.test.total)+TP.freq-TN.freq)
  knn.assess[1,j/5]<-auc.knn
  knn.assess[2,j/5]<-ACC.knn
  knn.assess[3,j/5]<-recall.knn
  knn.assess[4,j/5]<-precision.knn
  knn.assess[5,j/5]<-F1.knn
}
save(knn.assess,file="C:/Users/56881/Desktop/生统课程作业/classification/result/knn.result.Rdata")


