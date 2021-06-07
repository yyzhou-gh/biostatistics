###Load the data of input and output
x<-as.matrix(read.table(file="C:\\Users\\56881\\Desktop\\生统课程作业\\regression\\data\\target_5mer.txt"))
sgRNA.1130<-read.csv(file="C:\\Users\\56881\\Desktop\\生统课程作业\\regression\\result\\1130sgRNA.csv",header=T)
sgRNA.1130.names<-as.character(sgRNA.1130[,1])
sgRNA.top20.id<-sgRNA.1130.names[1:20]
rownames(x)<-sgRNA.1130.names
y<-sgRNA.1130[,5]
names(y)<-sgRNA.1130.names
###Load the data of input and output

##Construct the training groups and testing groups
train.groups.id<-list()
test.groups.id<-list()
x.train<-list()
x.test<-list()
y.train<-list()
y.test<-list()
y.test.total<-c()
tmp<-c()
for(i in 1:10){
	test.groups.id[[i]]<-sgRNA.1130.names[which(sgRNA.1130[1:980,6]==i)]
	train.groups.id[[i]]<-sgRNA.1130.names[which(sgRNA.1130[,6]!=i)]
	x.train[[i]]<-x[train.groups.id[[i]],]
	x.test[[i]]<-x[test.groups.id[[i]],]
	y.train[[i]]<-y[train.groups.id[[i]]]
	y.test[[i]]<-y[test.groups.id[[i]]]
	tmp<-c(tmp,test.groups.id[[i]])
	y.test.total<-c(y.test.total,y.test[[i]])
}
names(y.test.total)<-tmp
##Construct the training groups and testing groups
save.image("C:/Users/56881/Desktop/生统课程作业/regression/workspace/data.prep.Rdata")