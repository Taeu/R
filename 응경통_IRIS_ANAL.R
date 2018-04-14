# ????????? ??????
# iris ?????? ?????? ????????? ??? ???????????? ??? ?????????
names(iris)=c("SL","SW","PL","PW","SP");
newSP=iris$SP;
newSP[101:150]=iris$SP[51:100];
newSP = newSP[,drop=TRUE];
library(MASS)
# iris??? names ???????????? ????????? ??????????????????
# iris ????????? ???????????? ??????!
iris2a = lda(newSP~PW+PL,prior=c(1,1)/2,data=iris);
iris2b = lda(newSP~PW+PL,data=iris);

par(mfrow=c(1,2));
# versiclolor , virginica = c
# setosa =s;
plot.iris.lda(iris2a,c("s","c"),newSP,line=1,border=1);
plot.iris.lda(iris2a,c("s","c"),newSP,line=1,border=1);

# LDA, QDA & LRA
iris3a = lda(SP~PW+PL,data=iris); 
iris3q = qda(SP~PW+PL,data=iris);
iris3m = multinom(SP~PW+PL,data=iris);
par(mfrow=c(1,3));

plot.iris.lda(iris3a,border=1)
plot.iris.lda(iris3q,border=1)
plot.iris.mnom(iris3m,border=1);

iris3a;
iris3q;
library(nnet);
summary(iris3m);
table(iris$SP,predict(iris3a)$class);
table(iris$SP,predict(iris3q)$class);
table(iris$SP,predict(iris3m));

# 4??????
iris4a = lda(SP~.,data=iris);
iris4q=qda(SP~.,data=iris);
iris4m=multinom(SP~.,data=iris);
table(iris$SP,predict(iris4a)$class);
table(iris$SP,predict(iris4q)$class);
table(iris$SP,predict(iris4m));
iris4a;
iris4q;
iris4m;
plot.iris.discriminant(iris3a,border=1)
plot.iris.discriminant(iris4a,border=1)

# ?????????
irx = iris[,5];
spc = class.ind(iris$SP);
irx3n1=nnet(spc~PW+PL, data=irx,size=1,rang=0.1,decay=5e-4,maxit=300);
irx3n2=nnet(spc~PW+PL, data=irx,size=2,rang=0.1,decay=5e-4,maxit=300);
par(mfrow=c(1,2));
plot.iris.mnom(irix3n1);
plot.iris.mnom(irix3n2);
# ?????????
irx3n5=nnet(spc~PW+PL, data=irx,size=5,rang=0.1,decay=5e-4,maxit=300);
irx3n6=nnet(spc~PW+PL, data=irx,size=5,rang=0.1,decay=0.01,maxit=300);
par(mfrow=c(1,2));
plot.iris.mnom(irix3n5);
plot.iris.mnom(irix3n6);