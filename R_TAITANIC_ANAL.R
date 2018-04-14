data.a=dir("C:/Users/Taeu/Desktop/응용경영통계/Titanic.csv");
dfile.a=paste("C:/Users/Taeu/Desktop/응용경영통계/Titanic.csv",data.a,sep="");
da=read.csv(dfile.a[1],head=TRUE,);

names(da);
nda = da[,-c(1,2,5)];
names(nda)=c("cl","age","s","sex")

f1=glm(s~.,family=binomial,data=nda);
f2=glm(s~.^2,family=binomial,data=nda);
stepAIC(f1,direction="both");
stepAIC(f2,direction="both");

f1r=f1;
f2r=f2;

summary(f1r);
summary(f2r);


tmp=nda$cl;levels(tmp) = c(0,1,2,3);
tmp =as.numeric(levels(tmp)[tmp]);
nda$cl=ordered(nda$cl)
is.ordered(nda$cl)

nda$cl=tmp;
win.graph();
pairs(nda,panel=panel.smooth,cex=0.7);
