#data 추출
library(MASS)
MD = read.table("biodataset1.csv",head = TRUE,sep=",");
MD2 = read.table("biodataset2.csv",head = TRUE,sep=",");
new = MD2[,c(9:16)]
new = new[c(-31),]
colnames(new)


# full model
f1= lm(per~.data=new) 
stepAIC(f1,direction="both");
# fulmodel AIC 결과
f1r =lm(formula = per ~ 
          REV_g + 
          payout + 
          leverage + 
          country + 
          roe, 
   data = new) 

# multiple corr
f2 = lm(per~.^2,data = new)
# full model^2 AIC 결과
stepAIC(f2,direction="both");
f2r =lm(formula = per ~ REV_g + NI_G + payout + leverage + Beta_52w + 
         country + roe + REV_g:NI_G + REV_g:payout + REV_g:leverage + 
         REV_g:Beta_52w + REV_g:roe + NI_G:payout + NI_G:leverage + 
         NI_G:Beta_52w + NI_G:country + NI_G:roe + payout:leverage + 
         payout:Beta_52w + payout:country + payout:roe + leverage:country + 
         leverage:roe + Beta_52w:country + Beta_52w:roe + country:roe, 
       data = new)
summary(f2r)


new3 = new[,c(1,3,4,5,6,8)]
##데이터 시각화
attach(new3);
win.graph();
pairs(new3,panel=panel.smooth,cex=0.7);
detach(new3);

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

inbody = new3[1,]
inbody = inbody[,c(1,2,3,4,5)]
predict(final,newdata=inbody,interval="prediction")
# 결과 36
predict(final )
