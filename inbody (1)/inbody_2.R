#data 추출
library(MASS)

MD2 = read.table("inbody.csv",head = TRUE,sep=",");
new = MD2[,c(9:16)]
new = new[c(-31),]
new = new[,-c(7)]
colnames(new)


# full model
f1= lm(per~.,data=new) 
stepAIC(f1,direction="both");
f1r =lm(formula = per ~ REV_g + payout + leverage + country, data = new)
f1r2 =lm(formula = per ~ REV_g + payout + leverage + country+Beta_52w, data = new)
# fulmodel AIC 결과
f1r =lm(formula = per ~ 
          REV_g + 
          payout + 
          leverage + 
          country + 
          , 
   data = new) 

# multiple corr
f2 = lm(per~.^2,data = new)
# full model^2 AIC 결과
stepAIC(f2,direction="both");
f2r =lm(formula = per ~ REV_g + NI_G + payout + leverage + Beta_52w + 
          country + REV_g:NI_G + REV_g:payout + REV_g:leverage + REV_g:country + 
          NI_G:payout + NI_G:leverage + NI_G:Beta_52w + payout:leverage + 
          payout:Beta_52w + leverage:Beta_52w + leverage:country, data = new)
summary(f2r)


##데이터 시각화
attach(new);
win.graph();
pairs(new,panel=panel.smooth,cex=0.7);
detach(new);

# 여차저차해서 나온 최종 모델
final = lm (per~REV_g+payout+leverage+Beta_52w+country,data= new3);
mul1 = lm(per~.^2,data=new3)
stepAIC(mul1,direction="both")
final_2 = lm(formula = per ~ REV_g + NI_G + payout + leverage + Beta_52w + 
               country + REV_g:NI_G + REV_g:payout + REV_g:leverage + REV_g:country + 
               NI_G:payout + NI_G:leverage + NI_G:Beta_52w + payout:leverage + 
               payout:Beta_52w + leverage:Beta_52w + leverage:country, data = new3)
summary(final_2)
summary(final)

inbody = new[1,]
inbody = inbody[,c(1,2,3,4,5)]
predict(final,newdata=inbody,interval="prediction")
# 결과 36
predict(final )
