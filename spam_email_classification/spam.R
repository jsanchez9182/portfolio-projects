library(rpart)	
library(rpart.plot)
spam.data <- read.table("https://edoras.sdsu.edu/~jjfan/sta702/spamdata.txt", 
                        sep=",", na.strings="NA")
dim(spam.data)  # 4601   58
names(spam.data) <- c("wfmake", "wfaddress", "wfall", "wf3d", "wfour", 
                      "wfover", "wfremove", "wfinternet", "wforder", "wfmail", 
                      "wfreceive", "wfwill", "wfpeople", "wfreport", "wfaddresses", 
                      "wffree", "wfbusiness", "wfemail", "wfyou", "wfcredit", "wfyour", 
                      "wffont", "wf000", "wfmoney", "wfhp", "wfhpl", "wfgeorge", "wf650", 
                      "wflab", "wflabs", "wftelnet", "wf857", "wfdata", "wf415", "wf85", 
                      "wftechnology", "wf1999", "wfparts", "wfpm", "wfdirect", "wfcs", 
                      "wfmeeting", "wforiginal", "wfproject", "wfre", "wfedu", "wftable", 
                      "wfconference", "cfsc", "cfpar", "cfbrack", "cfexc", "cfdollar", 
                      "cfpound", "crlaverage", "crllongest", "crltotal", "spam")
table(spam.data$spam)  #0:2788 (nonspam); 1:1813 (spam) = data for spam column
spam.data$spam <- factor(spam.data$spam, levels=0:1, 
                         labels=c("No", "Spam"))

####### First Classifier #######
# split data into a training sample and a test sample, using stratified sampling
set1<-spam.data[spam.data$spam=="Spam",] #spam dataset
set0<-spam.data[spam.data$spam=="No",] #nonspam dataset
dim(set1)  # 1813   58
dim(set0)  # 2788   58
1813*2/3 #1208.667 emails = spam training
2788*2/3 #1858.667 emails = nonspam training

set.seed(858)  
training1<-sample(1:1813,1208) #samples w/out replacement 1208 items from 1:1813
test1<-(1:1813)[-training1] #takes (1813*1/3) items from 1:1813
sum((1:1813)==sort(c(training1,test1))) # sort data to see if sum is same as 1:1813

training0<-sample(1:2788,1858)
test0<-(1:2788)[-training0]
sum((1:2788==sort(c(training0,test0)))) #2788


train<-rbind(set1[training1,],set0[training0,]) #combines training data for 0 and 1
test<-rbind(set1[test1,], set0[test0,])
dim(train)  # 3066   58
dim(test)   # 1535   58
3066+1535 #4601 = total
1208+1858 #3066 = training total
4601 - 3066 #1535 = testing total

# tree growing and pruning with training data (aka fit classification tree to spam data using 10fold xvalidation)
my.control <- rpart.control(cp=0, xval=10) #cp = 0 means form largest tree; 10-fold xvalid
fit1<- rpart(spam ~ ., data=train, method="class",
             control=my.control)  #this makes our largest tree and gives subtree sequence

# plot the tree that corresponds to cp=0 (the largest, unpruned tree):
plot(fit1, margin=0.1)
text(fit1, use.n=T) #gives text to tree
title("Largest Tree for Classifier1")
printcp(fit1)  #recall, cp = cost complexity parameter
plotcp(fit1)

#tree 16 has absolutely smallest xerror (0.22765)
#0.22765 + 0.013098 (get 0.240748; look for xerror smaller than this)
#strictly following the 1SE rule, the optimal tree is tree 14; but if want smaller, could go with tree 10

#plot of xval estimates of error and training error against tree complexity
numsplits <- fit1$cptable[,2] #assigns the respective cptable values to variables names
trainerror <- fit1$cptable[,3]
xerror <- fit1$cptable[,4]
xstd <- fit1$cptable[,5]

plot(numsplits, trainerror, ylim=c(.05, 1.2), type="l") #plot training error (solid line)
lines(numsplits, xerror, lty =2) #plot xvalid error (dashed lines)
lines(numsplits, xerror-xstd, lty=2) #lower error bar for xvalid error
lines(numsplits, xerror+xstd, lty=2) #higher error bar for xvalid error
title("Cross-validation Error Estimates and Training Error for Classifier1")
legend(.02, .15, c("trainerror","xerror"), lty=c(1,2)) #coordinates are for top left corner of legend

#get optimal tree by pruning (tree 10)
fit1pruned <- prune(fit1, cp=0.007)
print(fit1pruned)
plot(fit1pruned, margin=0.1)
text(fit1pruned, use.n=T)
title("Optimal Tree for Classifier1")
summary(fit1pruned)
summary(fit1, cp=0.007) #gives same as above!

#get subtree of optimal tree by pruning (tree 7)
fit1bpruned <- prune(fit1, cp=0.0135)
printcp(fit1bpruned)
plot(fit1bpruned, margin=0.1)
text(fit1bpruned, use.n=T)
title("Pruned Optimal Tree for Classifier1")
summary(fit1bpruned)
summary(fit1, cp=0.0135)

#running test data down pruned optimal tree
pred1<-predict(fit1bpruned,newdata=test,type="class")
error1<-table(test$spam,pred1)[1,2]+table(test$spam,pred1)[2,1]
error1 #total misclassification error
errorrate1<-error1/length(test$spam)
errorrate1 # total misclassification error rate
#details of the misclassification error table (extra)
falserror1 <- table(test$spam,pred1) #table with number of false positive and false negative
falserror1 #930 non and 605 spam (test); 1011 non and 524 spam (pred1), where test and pred1 have total size 1535
falserrorate1 <- falserror1/length(test$spam)
falserrorate1 #divided false errors above by sample size 1535
#0.03452769 is the false positive error rate
#0.08729642 is the false negative error rate

####### Second Classifier #######
# tree growing and pruning with training data (aka fit classification tree to spam data using 10fold xvalidation)
my.control2 <- rpart.control(cp=0, xval=10) #cp = 0 means form largest tree; 10-fold xvalid
lmat <- matrix(c(0,10,1,0), byrow=T, nrow=2)
fit2<- rpart(spam ~ ., data=train, method="class", 
             parm=list(loss=lmat),
             control=my.control2)  #this makes our largest tree and gives subtree sequence

# plot the tree that corresponds to cp=0 (the largest, unpruned tree):
plot(fit2, margin=0.1)
title("Largest Tree for Classifier2")
text(fit2, use.n=T) #gives text to tree
printcp(fit2)  #recall, cp = cost complexity parameter
plotcp(fit2)
#tree 16 has absolutely smallest xerror (2.9007)
#2.9007 + 0.14490 (get 3.0456; look for xerror smaller than this)
#strictly following the 1SE rule, the optimal tree is tree 15; but if want smaller, could go with tree 10

#plot of xval estimates of error and training error against tree complexity
numsplits2 <- fit2$cptable[,2] #assigns the respective cptable values to variables names
trainerror2 <- fit2$cptable[,3]
xerror2 <- fit2$cptable[,4]
xstd2 <- fit2$cptable[,5]

plot(numsplits2, trainerror2, ylim=c(.05, 12), type="l") #plot training error (solid line)
lines(numsplits2, xerror2, lty =2) #plot xvalid error (dashed lines)
lines(numsplits2, xerror2-xstd2, lty=2) #lower error bar for xvalid error
lines(numsplits2, xerror2+xstd2, lty=2) #higher error bar for xvalid error
title("Cross-validation Error Estimates and Training Error for Classifier2")
legend(.02, 3, c("trainerror2","xerror2"), lty=c(1,2)) #coordinates are for top left corner of legend

#get optimal tree by pruning (tree 10)
fit2pruned <- prune(fit2, cp=0.0135)
print(fit2pruned)
plot(fit2pruned, margin=0.1)
text(fit2pruned, use.n=T)
title("Optimal Tree for Classifier2")
summary(fit2pruned)
summary(fit2, cp=0.0135) #gives same as above!

#get subtree of optimal tree by pruning (tree 6)
fit2bpruned <- prune(fit2, cp=0.035)
print(fit2bpruned)
plot(fit2bpruned, margin=0.1)
text(fit2bpruned, use.n=T)
title("Pruned Optimal Tree for Classifier2")
summary(fit2bpruned)
summary(fit2, cp=0.035)

#running test data down pruned optimal tree
pred2<-predict(fit2bpruned,newdata=test,type="class")
error2<-table(test$spam,pred2)[1,2]+table(test$spam,pred2)[2,1]
error2 #total misclassification error
errorrate2<-error2/length(test$spam)
errorrate2 #total misclassification error rate
#details of the misclassification error table (extra)
falserror2 <- table(test$spam,pred2) #table with number of false positive and false negative
falserror2 #930 non and 605 spam (test); 1167 non and 368 spam (pred1), where test and pred1 have total size 1535
falserrorate2 <- falserror2/length(test$spam)
falserrorate2 #divided false errors above by sample size 1535
#0.00781759 is the false positive error rate
#0.16221498 is the false negative error rate

