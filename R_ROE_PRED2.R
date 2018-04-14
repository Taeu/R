#?????????_??????
data.a=dir("C:/Users/Taeu/Desktop/응용경영통계/a.txt");
dfile.a=paste("C:/Users/Taeu/Desktop/응용경영통계/a.txt",data.rey,sep="");
da=read.table(dfile.a[1],head=TRUE,sep="\t");
da=da[,-1];#name 
f1= glm(return ~.,family=poisson,data=da);
f2=glm(return~.^2,family=poisson,data=da);
stepAIC(f2,direction="both");
stepAIC(f1,direction="both");

f1r=lm(return ~ debtratio + industry, data = da);
f2r=glm(return ~ margin + debtratio + industry + margin:debtratio + margin:industry,family=quasipoisson(link=log), data = da)
# names(da) "return"    "log(sales)"     "margin"    "debtratio" "industry" 
# scale ??????
f1s=lm(return~log(sales)+margin+debtratio+industry,data=da);
f2s=lm(return~(log(sales)+margin+debtratio+industry)^2,data=da);
# same

dabank=da[da$industry=="bank",];dabank=dabank[,-5];
dacomp=da[da$industry=="comp",];dacomp=dacomp[,-5];
dacons=da[da$industry=="cons",];dacons=dacons[,-5];
daenergy=da[da$industry=="energy",];daenergy=daenergy[,-5];


outlierTest(f2r);
outlierTest(f1r);
# 56??? outlier

# 43, 

b1f=glm(return~.,family=quasipoisson(link=log),data=dabank);
cp1f=glm(return~.,family=poisson,data=dacomp);
cs1f=glm(return~.,family=poisson,data=dacons);
e1f=glm(return~.,family=poisson,data=daenergy);

b1fg=glm(return~.,family=poisson,data=dabank);
cp1fg=glm(return~.,family=poisson,data=dacomp);
cs1fg=glm(return~.,family=poisson,data=dacons);
e1fg=glm(return~.,family=poisson,data=daenergy);



b2f=glm(return~.^2,data=dabank);
cp2f=glm(return~.^2,data=dacomp);
cs2f=glm(return~.^2,data=dacons);
e2f=glm(return~.^2,data=daenergy);
stepAIC(b1f,direction="both");
stepAIC(cp1f,direction="both");
stepAIC(cs1f,direction="both");
stepAIC(e1f,direction="both");
stepAIC(b2f,direction="both");
stepAIC(cp2f,direction="both");
stepAIC(cs2f,direction="both");
stepAIC(e2f,direction="both");
b1fr=glm(formula = return ~ 1, data = dabank);
cp1r=glm(formula = return ~ margin, data = dacomp);
cs1r=glm(formula = return ~ debtratio, data = dacons);
e1fr=glm(formula = return ~ sales, data = daenergy);
predict(b1fr, newdata=data.frame(0),interval="prediction");
predict(cp1r, newdata=data.frame(sales=2000,margin=3.5,debtratio=3.5),interval="prediction");
predict(cs1r, newdata=data.frame(debtratio=50),interval="prediction");
predict(e1fr, newdata=data.frame(sales=2000),interval="prediction");



b2f=lm(return~.^2,data=dabank);
cp2f=lm(return~.^2,data=dacomp);
cs2f=lm(return~.^2,data=dacons);
e2f=lm(return~.^2,data=daenergy);
stepAIC(b2f,direction="both");
stepAIC(cp2f,direction="both");
stepAIC(cs2f,direction="both");
stepAIC(e2f,direction="both");
extractAIC(b1fr);
extractAIC(cp1r);
extractAIC(cs1r);
extractAIC(e1fr);

extractAIC(f2r);

b2fr=lm(formula = return ~ 1, data = dabank);
cp2f=lm(formula = return ~ margin, data = dacomp);
cs2f=lm(formula = return ~ debtratio, data = dacons)
e2rs=lm(formula = return ~ sales, data = daenergy)
# ??? ??????

attach(da);
win.graph();
pairs(da,panel=panel.smooth,cex=0.7);
detach(da);

attach(dabank);
win.graph();
pairs(dabank,panel=panel.smooth,cex=0.7);
detach(dabank);

attach(dacomp);
win.graph();
pairs(dacomp,panel=panel.smooth,cex=0.7);
detach(dacomp);

attach(dacons);
win.graph();
pairs(dacons,panel=panel.smooth,cex=0.7);
detach(dacons);

attach(daenergy);
win.graph();
pairs(daenergy,panel=panel.smooth,cex=0.7);
detach(daenergy);