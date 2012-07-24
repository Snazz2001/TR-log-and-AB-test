setwd("C:/My Projects/Zheng - TR logic on business and criteria for AB test/")
loan3<-read.csv("3loan2012Am.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
colnames(loan3)[1]<-"ProposalID"
loan3$FirstApplicationDate<-as.Date(as.character(loan3$FirstApplicationDate),format="%Y%m%d");
loan3$SecondApplicationDate<-as.Date(as.character(loan3$SecondApplicationDate),format="%Y%m%d");
loan3$FirstPaybackDate<-as.Date(as.character(loan3$FirstPaybackDate),format="%Y%m%d");
loan3$SecondPaybackDate<-as.Date(as.character(loan3$SecondPaybackDate),format="%Y%m%d");
loan3$ThirdApplicationDate<-as.Date(as.character(loan3$ThirdApplicationDate),format="%Y%m%d");
loan3$SecondDaysSinceLastPayOff<-loan3$SecondApplicationDate-loan3$FirstPaybackDate;
loan3$SecondDaysSinceLastPayOff<-as.integer(loan3$SecondDaysSinceLastPayOff)
loan3$AmountOrigDiff<-loan3$SecondLoanAmountOriginal - loan3$FirstLoanAmountOriginal;
loan3$AmountAgreeDiff<-loan3$SecondLoanAmountAgreed - loan3$FirstLoanAmountAgreed;
loan3$FirstAnnualIncome<-ifelse(loan3$FirstAnnualIncome==0,12,loan3$FirstAnnualIncome)
loan3<-loan3[complete.cases(loan3$SecondTrustRating),];
loan3<-loan3[complete.cases(loan3$FirstTrustRating),];
loan3<-loan3[complete.cases(loan3$FirstMixedScore),];
loan3<-loan3[complete.cases(loan3$SecondMixedScore),];
loan3<-loan3[complete.cases(loan3$ThirdTrustRating),];

loan3$FirstTotalExt<-ifelse(is.na(loan3$FirstTotalExt),0,loan3$FirstTotalExt);
loan3$SecondTotalExt<-ifelse(is.na(loan3$SecondTotalExt),0,loan3$SecondTotalExt);
loan3$FirstTotalTop<-ifelse(is.na(loan3$FirstTotalTop),0,loan3$FirstTotalTop);
loan3$SecondTotalTop<-ifelse(is.na(loan3$SecondTotalTop),0,loan3$SecondTotalTop);
loan3$AmCollected<-ifelse(is.na(loan3$AmCollected),0,loan3$AmCollected);

loan3$ScoreDiff<-loan3$SecondMixedScore - loan3$FirstMixedScore;
loan3$utilization1<-loan3$FirstLoanAmountAgreed/loan3$FirstTrustRating;
loan3$utilization2<-loan3$SecondLoanAmountAgreed/loan3$SecondTrustRating;

loan3$utilization1<-ifelse(loan3$utilization1>1,1,loan3$utilization1)
loan3$utilization2<-ifelse(loan3$utilization2>1,1,loan3$utilization2)
loan3$utilization3<-loan3$ThirdLoanAmountAgreed/loan3$ThirdTrustRating;
loan3$utilization3<-ifelse(loan3$utilization3>1,1,loan3$utilization3)

loan3$utilRatio<-loan3$utilization2/loan3$utilization1;
loan3$utilDiff<-loan3$utilization2 - loan3$utilization1;
loan3$IncomeDiff<-loan3$SecondAnnualIncome - loan3$FirstAnnualIncome;
loan3<-loan3[!is.na(loan3$utilRatio),];

loan3$trratio<-loan3$SecondTrustRating/loan3$FirstTrustRating;
loan3$utitrratio<-loan3$utilRatio/loan3$trratio;
loan3$FirstLoanIncome<-loan3$FirstLoanAmountOriginal/(loan3$FirstAnnualIncome/12);
loan3$SecondLoanIncome<-loan3$SecondLoanAmountOriginal/(loan3$SecondAnnualIncome/12);

nn.pred<-predict(nn,loan3)
pred<-prediction(nn.pred,loan3$ThirdBad);
auc<-performance(pred,"auc");
auc;
loan3<-data.frame(loan3,nn.pred)

loan3$TrUpdate<-ifelse((loan3$ThirdTrustRating-loan3$SecondTrustRating)>0,1,ifelse((loan3$ThirdTrustRating-loan3$SecondTrustRating)<0,-1,0));
data20122<-loan3[which(loan3$TrUpdate>=0),];
data20122$highscore<-ifelse(data20122$ThirdMixedScore>975,1,0);
sum(data20122$highscore==1&data20122$TrUpdate==0)
sum(data20122$highscore==1&data20122$TrUpdate==0&data20122$ThirdBad==1)
sum(data20122$highscore==1&data20122$TrUpdate==1)
sum(data20122$highscore==1&data20122$TrUpdate==1&data20122$ThirdBad==1)
mean(data20122$ThirdMixedScore[which(data20122$highscore==1&data20122$TrUpdate==1&data20122$ThirdBad==1)])
mean(data20122$ThirdMixedScore[which(data20122$highscore==1&data20122$TrUpdate==0&data20122$ThirdBad==1)])

t.test(data20122$ThirdMixedScore[which(data20122$highscore==1&data20122$TrUpdate==1&data20122$ThirdBad==1)],data20122$ThirdMixedScore[which(data20122$highscore==1&data20122$TrUpdate==0&data20122$ThirdBad==1)])

ggplot(data=data20122[which(data20122$highscore==1&data20122$ThirdBad==1),],aes(as.factor(TrUpdate),ThirdMixedScore))+geom_boxplot()

-----------next i will look at the loan amount affect by the proposed trust rating-------------------
datafeb<-data20122[which(data20122$ThirdApplicationDate>'2012-02-01'&data20122$ThirdApplicationDate<'2012-03-01'),];#only look at the Feb data

median(datafeb$utilization3[which(datafeb$TrUpdate==1)])
median(datafeb$utilization3[which(datafeb$TrUpdate==0)])
mean(datafeb$utilization3[which(datafeb$TrUpdate==1)])
mean(datafeb$utilization3[which(datafeb$TrUpdate==0)])
ggplot(datafeb,aes(utilization3,colour=as.factor(TrUpdate)))+geom_density()#came cross "Error in xyz %*% solve(M) : non-conformable arguments" use as.factor to fix it.

library(e1071)
skewness(datafeb$utilization3[which(datafeb$TrUpdate==1)])
skewness(datafeb$utilization3[which(datafeb$TrUpdate==0)])

sum(datafeb$TrUpdate==1)
[1] 107355
sum(datafeb$TrUpdate==1&datafeb$utilization3==1)
[1] 10534
sum(datafeb$TrUpdate==0)
[1] 62149
sum(datafeb$TrUpdate==0&datafeb$utilization3==1)
[1] 6389

ggplot(datafeb,aes(ThirdLoanAmountAgreed,colour=as.factor(TrUpdate)))+geom_density()
median(datafeb$ThirdLoanAmountAgreed[which(datafeb$TrUpdate==1)])
median(datafeb$ThirdLoanAmountAgreed[which(datafeb$TrUpdate==0)])
mean(datafeb$ThirdLoanAmountAgreed[which(datafeb$TrUpdate==1)])
mean(datafeb$ThirdLoanAmountAgreed[which(datafeb$TrUpdate==0)])
sd(datafeb$ThirdTrustRating[which(datafeb$TrUpdate==1)])
[1] 165.3985116
sd(datafeb$ThirdTrustRating[which(datafeb$TrUpdate==0)])
[1] 244.3634187
sd(datafeb$ThirdLoanAmountAgreed[which(datafeb$TrUpdate==1)])
[1] 214.7710915
sd(datafeb$ThirdLoanAmountAgreed[which(datafeb$TrUpdate==0)])
[1] 292.3102704

ggplot(datafeb,aes(ThirdTrustRating,colour=as.factor(TrUpdate)))+geom_density()

datafeb$trincrease<-datafeb$ThirdTrustRating-datafeb$SecondTrustRating;
ggplot(datafeb,aes(trincrease,colour=as.factor(TrUpdate)))+geom_density()

ggplot(datafeb,aes(trincrease))+geom_histogram(binwidth=5)+facet_grid(TrUpdate ~ .)

median(datafeb$trincrease[which(datafeb$TrUpdate==1)])
[1] 40
mean(datafeb$trincrease[which(datafeb$TrUpdate==1)])
[1] 52.76130595

threshold<-seq(0.001,1,by=0.001);
frozenSize<-rep(0,length(threshold));
loanAmount4f<-rep(0,length(threshold));
loanAmount4i<-rep(0,length(threshold));
maxloanAmount4f<-rep(0,length(threshold));
maxloanAmount4i<-rep(0,length(threshold));

meanloanAmount4f<-rep(0,length(threshold));
meanloanAmount4i<-rep(0,length(threshold));

loanAmountPreUti4f<-rep(0,length(threshold));
loanAmountPreUti4i<-rep(0,length(threshold));
meanLoanAmountPreUti4f<-rep(0,length(threshold));
meanLoanAmountPreUti4i<-rep(0,length(threshold));

loanAmountMinTrAmount4f<-rep(0,length(threshold));
loanAmountMinTrAmount4i<-rep(0,length(threshold));
meanLoanAmountMinTrAmount4f<-rep(0,length(threshold));
meanLoanAmountMinTrAmount4i<-rep(0,length(threshold));

loanAmountMinTrAmount4f2<-rep(0,length(threshold));
loanAmountMinTrAmount4i2<-rep(0,length(threshold));
meanLoanAmountMinTrAmount4f2<-rep(0,length(threshold));
meanLoanAmountMinTrAmount4i2<-rep(0,length(threshold));

trscore<-rep(0,length(threshold));


trIncreaseMean<-53;
for(i in 1:length(threshold))
 {
     datafeb$PredOutcome<-ifelse(datafeb$nn.pred<threshold[i],0,1);
	 
	 trscore[i]<-(1-threshold[i])*1000;
	
	datafeb$newTR3<-ifelse(datafeb$PredOutcome==0&datafeb$TrUpdate==1,datafeb$ThirdTrustRating,ifelse(datafeb$PredOutcome==1&datafeb$TrUpdate==0,datafeb$ThirdTrustRating,ifelse(datafeb$PredOutcome==0&datafeb$TrUpdate==0,datafeb$SecondTrustRating+trIncreaseMean,datafeb$SecondTrustRating)));#4 senario, if pred no risk and true tr increase or pred risk and true tr no change, keep the current rate, if pred n risk and tru tr frozen, add tr by 53, if pred risk and true tr change, use second tr as frozen.
	 #datafeb$newTR3<-ifelse(datafeb$PredOutcome==0,datafeb$SecondTrustRating+trIncreaseMean,datafeb$SecondTrustRating);
	 frozenSize[i]<-sum(datafeb$PredOutcome==1);
	 maxloanAmount4f[i]<-sum(datafeb$newTR3[which(datafeb$PredOutcome==1)]);
	 maxloanAmount4i[i]<-sum(datafeb$newTR3[which(datafeb$PredOutcome==0)]);
	 
	 meanloanAmount4f[i]<-mean(datafeb$newTR3[which(datafeb$PredOutcome==1)]);
	 meanloanAmount4i[i]<-mean(datafeb$newTR3[which(datafeb$PredOutcome==0)]);
	 
	 datafeb$newLoanPreUtil<-datafeb$newTR3*datafeb$utilization3;
	 
	 loanAmountPreUti4f[i]<-sum(datafeb$newLoanPreUtil[which(datafeb$PredOutcome==1)]);
	 loanAmountPreUti4i[i]<-sum(datafeb$newLoanPreUtil[which(datafeb$PredOutcome==0)]);
	 
	 meanLoanAmountPreUti4f[i]<-mean(datafeb$newLoanPreUtil[which(datafeb$PredOutcome==1)]);
	 meanLoanAmountPreUti4i[i]<-mean(datafeb$newLoanPreUtil[which(datafeb$PredOutcome==0)]);
	 

	 
	 increase2util3<-mean(datafeb$utilization3[which(datafeb$TrUpdate==1&datafeb$PredOutcome==0)])#we may replace the datafeb$utilization3 with this one.
	
	 datafeb$newLoanMin<-apply(data.frame(datafeb$newTR3,datafeb$ThirdLoanAmountAgreed),1,min);
	 
	 datafeb$newLoanMin<-ifelse(datafeb$PredOutcome==0&datafeb$TrUpdate==1,datafeb$ThirdLoanAmountAgreed,ifelse(datafeb$PredOutcome==1&datafeb$TrUpdate==0,datafeb$ThirdLoanAmountAgreed,ifelse(datafeb$PredOutcome==0&datafeb$datafeb$TrUpdate==0,datafeb$newTR3*datafeb$utilization3,datafeb$newLoanMin)));
	 
	 datafeb$newLoanMin2<-ifelse(datafeb$PredOutcome==0&datafeb$TrUpdate==1,datafeb$ThirdLoanAmountAgreed,ifelse(datafeb$PredOutcome==1&datafeb$TrUpdate==0,datafeb$ThirdLoanAmountAgreed,ifelse(datafeb$PredOutcome==0&datafeb$datafeb$TrUpdate==0,datafeb$newTR3*increase2util3,datafeb$newLoanMin)));
	  
	 loanAmountMinTrAmount4f[i]<-sum(datafeb$newLoanMin[which(datafeb$PredOutcome==1)]);
	 loanAmountMinTrAmount4i[i]<-sum(datafeb$newLoanMin[which(datafeb$PredOutcome==0)]);

	 meanLoanAmountMinTrAmount4f[i]<-mean(datafeb$newLoanMin[which(datafeb$PredOutcome==1)]);
	 meanLoanAmountMinTrAmount4i[i]<-mean(datafeb$newLoanMin[which(datafeb$PredOutcome==0)]);
	 
	 loanAmountMinTrAmount4f2[i]<-sum(datafeb$newLoanMin2[which(datafeb$PredOutcome==1)]);
	 loanAmountMinTrAmount4i2[i]<-sum(datafeb$newLoanMin2[which(datafeb$PredOutcome==0)]);

	 meanLoanAmountMinTrAmount4f2[i]<-mean(datafeb$newLoanMin2[which(datafeb$PredOutcome==1)]);
	 meanLoanAmountMinTrAmount4i2[i]<-mean(datafeb$newLoanMin2[which(datafeb$PredOutcome==0)]);
}
results2<-data.frame(trscore,frozenSize,maxloanAmount4f,maxloanAmount4i,meanloanAmount4f,meanloanAmount4i,loanAmountPreUti4f,loanAmountPreUti4i,meanLoanAmountPreUti4f,meanLoanAmountPreUti4i,loanAmountMinTrAmount4f,loanAmountMinTrAmount4i,meanLoanAmountMinTrAmount4f,meanLoanAmountMinTrAmount4i);

results3<-data.frame(trscore,loanAmountMinTrAmount4f,loanAmountMinTrAmount4i,meanLoanAmountMinTrAmount4f,meanLoanAmountMinTrAmount4i);

write.csv(results2,file="cutoffloansize.csv");

datafeb$PredOutcome<-ifelse(datafeb$nn.pred<0.08,0,1);
datafeb$PredOutcome<-ifelse(datafeb$nn.pred<0.08,0,1);
sum(datafeb$TrUpdate==1&datafeb$PredOutcome==0&datafeb$ThirdBad==1)
sum(datafeb$TrUpdate==1&datafeb$PredOutcome==0)

sum(datafeb$TrUpdate==1&datafeb$PredOutcome==1&datafeb$ThirdBad==1)
[1] 6350
sum(datafeb$TrUpdate==1&datafeb$PredOutcome==1)
[1] 30837
sum(datafeb$TrUpdate==0&datafeb$PredOutcome==0&datafeb$ThirdBad==1)
[1] 1577
sum(datafeb$TrUpdate==0&datafeb$PredOutcome==0)
[1] 50785
sum(datafeb$TrUpdate==0&datafeb$PredOutcome==1&datafeb$ThirdBad==1)
[1] 2373
sum(datafeb$TrUpdate==0&datafeb$PredOutcome==1)
[1] 11364

threshold<-seq(0.01,0.2,by=0.01);
threshold<-seq(0.005,0.2,by=0.005);
frozenSize<-rep(0,length(threshold));
loanAmount4f<-rep(0,length(threshold));
loanAmount4i<-rep(0,length(threshold));
maxloanAmount4f<-rep(0,length(threshold));
maxloanAmount4i<-rep(0,length(threshold));

meanloanAmount4f<-rep(0,length(threshold));
meanloanAmount4i<-rep(0,length(threshold));

loanAmountPreUti4f<-rep(0,length(threshold));
loanAmountPreUti4i<-rep(0,length(threshold));
meanLoanAmountPreUti4f<-rep(0,length(threshold));
meanLoanAmountPreUti4i<-rep(0,length(threshold));

loanAmountMinTrAmount4f<-rep(0,length(threshold));
loanAmountMinTrAmount4i<-rep(0,length(threshold));
meanLoanAmountMinTrAmount4f<-rep(0,length(threshold));
meanLoanAmountMinTrAmount4i<-rep(0,length(threshold));
loanAmountMinTrAmount4f2<-rep(0,length(threshold));
loanAmountMinTrAmount4i2<-rep(0,length(threshold));
meanLoanAmountMinTrAmount4f2<-rep(0,length(threshold));
meanLoanAmountMinTrAmount4i2<-rep(0,length(threshold));
trscore<-rep(0,length(threshold));

tr1rk1update1<-rep(0,length(threshold));
tr0rk0update1<-rep(0,length(threshold));

tr1rk1update2<-rep(0,length(threshold));
tr0rk0update2<-rep(0,length(threshold));

trIncreaseMean<-53;
for(i in 1:length(threshold))
 {
     datafeb$PredOutcome<-ifelse(datafeb$nn.pred<threshold[i],0,1);
	 
	 trscore[i]<-(1-threshold[i])*1000;
	
	datafeb$newTR3<-ifelse(datafeb$PredOutcome==0&datafeb$TrUpdate==1,datafeb$ThirdTrustRating,ifelse(datafeb$PredOutcome==1&datafeb$TrUpdate==0,datafeb$ThirdTrustRating,ifelse(datafeb$PredOutcome==0&datafeb$TrUpdate==0,datafeb$SecondTrustRating+trIncreaseMean,datafeb$SecondTrustRating)));
	
	datafeb$newTR3<-ifelse(datafeb$newTR3>1000,1000,datafeb$newTR3);
	
	 increase2util3<-mean(datafeb$utilization3[which(datafeb$TrUpdate==1&datafeb$PredOutcome==0)])#we may replace the datafeb$utilization3 with this one.
	
	 datafeb$newLoanMin<-apply(data.frame(datafeb$newTR3,datafeb$ThirdLoanAmountAgreed),1,min);
	 datafeb$newLoanMin2<-apply(data.frame(datafeb$newTR3,datafeb$ThirdLoanAmountAgreed),1,min);
	 
	 datafeb$newLoanMin<-ifelse(datafeb$PredOutcome==0&datafeb$TrUpdate==1,datafeb$ThirdLoanAmountAgreed,ifelse(datafeb$PredOutcome==1&datafeb$TrUpdate==0,datafeb$ThirdLoanAmountAgreed,ifelse(datafeb$PredOutcome==0&datafeb$TrUpdate==0,datafeb$newTR3*datafeb$utilization3,datafeb$newLoanMin)));
	 
	 datafeb$newLoanMin2<-ifelse(datafeb$PredOutcome==0&datafeb$TrUpdate==1,datafeb$ThirdLoanAmountAgreed,ifelse(datafeb$PredOutcome==1&datafeb$TrUpdate==0,datafeb$ThirdLoanAmountAgreed,ifelse(datafeb$PredOutcome==0&datafeb$TrUpdate==0,datafeb$newTR3*increase2util3,datafeb$newLoanMin2)));
	  
	 loanAmountMinTrAmount4f[i]<-sum(datafeb$newLoanMin[which(datafeb$PredOutcome==1)]);
	 loanAmountMinTrAmount4i[i]<-sum(datafeb$newLoanMin[which(datafeb$PredOutcome==0)]);

	 tr1rk1update1[i]<-sum(datafeb$newLoanMin[which(datafeb$PredOutcome==1&datafeb$TrUpdate==1)])-sum(datafeb$ThirdLoanAmountAgreed[which(datafeb$PredOutcome==1&datafeb$TrUpdate==1)]);
	 tr0rk0update1[i]<-sum(datafeb$newLoanMin[which(datafeb$PredOutcome==0&datafeb$TrUpdate==0)])-sum(datafeb$ThirdLoanAmountAgreed[which(datafeb$PredOutcome==0&datafeb$TrUpdate==0)]);
	 
	 meanLoanAmountMinTrAmount4f[i]<-mean(datafeb$newLoanMin[which(datafeb$PredOutcome==1)]);
	 meanLoanAmountMinTrAmount4i[i]<-mean(datafeb$newLoanMin[which(datafeb$PredOutcome==0)]);
	 
	 loanAmountMinTrAmount4f2[i]<-sum(datafeb$newLoanMin2[which(datafeb$PredOutcome==1)]);
	 loanAmountMinTrAmount4i2[i]<-sum(datafeb$newLoanMin2[which(datafeb$PredOutcome==0)]);

	 meanLoanAmountMinTrAmount4f2[i]<-mean(datafeb$newLoanMin2[which(datafeb$PredOutcome==1)]);
	 meanLoanAmountMinTrAmount4i2[i]<-mean(datafeb$newLoanMin2[which(datafeb$PredOutcome==0)]);
	 
	 tr1rk1update2[i]<-sum(datafeb$newLoanMin2[which(datafeb$PredOutcome==1&datafeb$TrUpdate==1)])-sum(datafeb$ThirdLoanAmountAgreed[which(datafeb$PredOutcome==1&datafeb$TrUpdate==1)]);
	 tr0rk0update2[i]<-sum(datafeb$newLoanMin2[which(datafeb$PredOutcome==0&datafeb$TrUpdate==0)])-sum(datafeb$ThirdLoanAmountAgreed[which(datafeb$PredOutcome==0&datafeb$TrUpdate==0)]);
}
results4<-data.frame(trscore,loanAmountMinTrAmount4f,loanAmountMinTrAmount4i,meanLoanAmountMinTrAmount4f,meanLoanAmountMinTrAmount4i,loanAmountMinTrAmount4f2,loanAmountMinTrAmount4i2,meanLoanAmountMinTrAmount4f2,meanLoanAmountMinTrAmount4i2,tr1rk1update1,tr0rk0update1,tr1rk1update2,tr0rk0update2);

results4$trrk1<-results4$tr1rk1update1+results4$tr0rk0update1
results4$trrk2<-results4$tr1rk1update2+results4$tr0rk0update2

mean(datafeb$ThirdLoanAmountAgreed[which(datafeb$TrUpdate==1&datafeb$PredOutcome==1)]);
mean(datafeb$newLoanMin[which(datafeb$TrUpdate==1&datafeb$PredOutcome==1)]);

datafeb$newLoanMin2<-ifelse(datafeb$PredOutcome==0&datafeb$TrUpdate==1,datafeb$ThirdLoanAmountAgreed,ifelse(datafeb$PredOutcome==1&datafeb$TrUpdate==0,datafeb$ThirdLoanAmountAgreed,ifelse(datafeb$PredOutcome==0&datafeb$TrUpdate==0,datafeb$newTR3*increase2util3,datafeb$newLoanMin)));

-------the following is to analyse the fronzen part change---------------
threshold<-seq(0.01,0.2,by=0.01);
threshold<-seq(0.005,0.2,by=0.005);
frozenprop<-rep(0,length(threshold));

meanLoanAmount<-rep(0,length(threshold));
trueLoanAmount<-rep(0,length(threshold));
loanAmount<-rep(0,length(threshold));
trscore<-rep(0,length(threshold));
arrearrate<-rep(0,length(threshold));
arrearrateall<-rep(0,length(threshold));
decreaseratio<-rep(0,length(threshold));
trIncreaseMean<-53;
arrearfrozen<-0.064;
for(i in 1:length(threshold))
 {
     datafeb$PredOutcome<-ifelse(datafeb$nn.pred<threshold[i],0,1);
	 
	 trscore[i]<-(1-threshold[i])*1000;
	
	datafeb$newTR3<-ifelse(datafeb$PredOutcome==0&datafeb$TrUpdate==1,datafeb$ThirdTrustRating,ifelse(datafeb$PredOutcome==1&datafeb$TrUpdate==0,datafeb$ThirdTrustRating,ifelse(datafeb$PredOutcome==0&datafeb$TrUpdate==0,datafeb$SecondTrustRating+trIncreaseMean,datafeb$SecondTrustRating)));
	
	datafeb$newTR3<-ifelse(datafeb$newTR3>1000,1000,datafeb$newTR3);
	
	 datafeb$newLoanMin<-apply(data.frame(datafeb$newTR3,datafeb$ThirdLoanAmountAgreed),1,min);
	 #datafeb$newLoanMin2<-apply(data.frame(datafeb$newTR3,datafeb$ThirdLoanAmountAgreed),1,min);
	 datafeb$decreaseratio<-(datafeb$AmCollected-datafeb$newLoanMin)/datafeb$AmCollected;
	 decreaseratio[i]<-mean(datafeb$decreaseratio[which(datafeb$PredOutcome==1&datafeb$TrUpdate==1)]);
	 loanAmount[i]<-sum(datafeb$newLoanMin[which(datafeb$PredOutcome==1&datafeb$TrUpdate==1)]);
	 meanLoanAmount[i]<-mean(datafeb$newLoanMin[which(datafeb$PredOutcome==1&datafeb$TrUpdate==1)]);
	 trueLoanAmount[i]<-sum(datafeb$ThirdLoanAmountAgreed[which(datafeb$PredOutcome==1&datafeb$TrUpdate==1)]);
	 frozenprop[i]<-dim(datafeb[which(datafeb$PredOutcome==1&datafeb$TrUpdate==1),])[1]/dim(datafeb[which(datafeb$TrUpdate==1),])[1]
	 if(length(datafeb$newLoanMin[which(datafeb$PredOutcome==1&datafeb$TrUpdate==1)])>0)
	 {arrearrate[i]<-length(datafeb$newLoanMin[which(datafeb$PredOutcome==1&datafeb$ThirdBad==1&datafeb$TrUpdate==1)])/length(datafeb$newLoanMin[which(datafeb$PredOutcome==1&datafeb$TrUpdate==1)]);
	  arrearrateall[i]<-(sum(datafeb$TrUpdate==0&datafeb$ThirdBad==1)+sum(datafeb$TrUpdate==1&datafeb$ThirdBad==1&datafeb$PredOutcome==0)+sum(datafeb$TrUpdate==1&datafeb$PredOutcome==1)*arrearfrozen)/dim(datafeb)[1];
	 }
	 else
	 {arrearrate[i]<-0;
	arrearrateall[i]<-0;	
	 }
}
results6<-data.frame(trscore,loanAmount,trueLoanAmount,meanLoanAmount,arrearrate,frozenprop,arrearrateall)


select 
t.agreementreference, 
 sum(isnull(-t.amount,0)) as Am_Collected 
from redline.transactions t
left join redline.Transaction_Types tt
on tt.transactiontype = t.transactiontype
inner join #temp te on te.AgreementReference = t.AgreementReference
where transactiontypegroup = 2 and posted = 1 group by t.AgreementReference


------------Estimate the collected Amount vs Pricinple Out-----------------

loanbin<-c(50,99,116,152,200,260,2000);
weightsbin<-c(-1.39078008542745,-0.976685538270457,-0.884872443666457,-0.731901063935573,-0.649597719144209,-0.549783318707196,0)

scorebin<-c(600,650,700,750,800,850,900,950,1000);
collectedpratio<-c(1.074714768,1.002852664,1.021439181,1.142469586,1.195870951,1.216786728,1.288965395,1.361027762,1.331448534)

threshold<-seq(0.01,0.2,by=0.01);
threshold<-seq(0.005,0.2,by=0.005);
frozenprop<-rep(0,length(threshold));

meanLoanAmount<-rep(0,length(threshold));
trueLoanAmount<-rep(0,length(threshold));
loanAmount<-rep(0,length(threshold));
trscore<-rep(0,length(threshold));
arrearrate<-rep(0,length(threshold));
arrearrateall<-rep(0,length(threshold));
decreaseratio<-rep(0,length(threshold));
trIncreaseMean<-53;
arrearfrozen<-0.064;
collectRatio<-rep(0,length(threshold));
datafeb2$weightAmount<-sapply(datafeb2$ThirdLoanAmountAgreed, function(x)weightsbin[min(which(loanbin>=x))]);
for(i in 1:length(threshold))
 {
     datafeb$PredOutcome<-ifelse(datafeb$nn.pred<threshold[i],0,1);
	 
	 trscore[i]<-(1-threshold[i])*1000;
	
	#weightsbin[min(which(loanbin>s))];
	
	datafeb$newTR3<-ifelse(datafeb$PredOutcome==0&datafeb$TrUpdate==1,datafeb$ThirdTrustRating,ifelse(datafeb$PredOutcome==1&datafeb$TrUpdate==0,datafeb$ThirdTrustRating,ifelse(datafeb$PredOutcome==0&datafeb$TrUpdate==0,datafeb$SecondTrustRating+trIncreaseMean,datafeb$SecondTrustRating)));
	
	datafeb$newTR3<-ifelse(datafeb$newTR3>1000,1000,datafeb$newTR3);

	datafeb$newLoanMin<-apply(data.frame(datafeb$newTR3,datafeb$ThirdLoanAmountAgreed),1,min);
	
	datafeb$value<-log(1000/datafeb$ThirdMixedScore-1);
	
	datafeb2$weightAmount<-sapply(datafeb2$ThirdLoanAmountAgreed, function(x)weightsbin[min(which(loanbin>=x))]);
	
	datafeb2$weightAmount2<-sapply(datafeb2$newLoanMin, function(x)weightsbin[min(which(loanbin>=x))]);
	
	#datafeb$weightAmount<-weightsbin[min(which(loanbin>=datafeb$ThirdLoanAmountAgreed))];
	#datafeb$weightAmount2<-weightsbin[min(which(loanbin>datafeb$newLoanMin))];
	datafeb$tmp<-datafeb$value-datafeb$weightAmount+datafeb$weightAmount2;
	datafeb$newMixedScore<-1000/(1+exp(datafeb$value-datafeb$weightAmount+datafeb$weightAmount2));
	datafeb$newCollected<-collectedpratio[min(which(scorebin>=datafeb$newMixedScore))]*datafeb$newLoanMin;
	 
	 collectRatio[i]<-sum(datafeb$newCollected[which(datafeb$PredOutcome==1&datafeb$TrUpdate==1)]/sum(datafeb$newLoanMin[which(datafeb$PredOutcome==1&datafeb$TrUpdate==1)]);
	 #datafeb$newLoanMin2<-apply(data.frame(datafeb$newTR3,datafeb$ThirdLoanAmountAgreed),1,min);
	 #datafeb$decreaseratio<-(datafeb$AmCollected-datafeb$newLoanMin)/datafeb$AmCollected;
	 #decreaseratio[i]<-mean(datafeb$decreaseratio[which(datafeb$PredOutcome==1&datafeb$TrUpdate==1)]);
	 loanAmount[i]<-sum(datafeb$newLoanMin[which(datafeb$PredOutcome==1&datafeb$TrUpdate==1)]);
	 meanLoanAmount[i]<-mean(datafeb$newLoanMin[which(datafeb$PredOutcome==1&datafeb$TrUpdate==1)]);
	 trueLoanAmount[i]<-sum(datafeb$ThirdLoanAmountAgreed[which(datafeb$PredOutcome==1&datafeb$TrUpdate==1)]);
	 frozenprop[i]<-dim(datafeb[which(datafeb$PredOutcome==1&datafeb$TrUpdate==1),])[1]/dim(datafeb[which(datafeb$TrUpdate==1),])[1]
	 if(length(datafeb$newLoanMin[which(datafeb$PredOutcome==1&datafeb$TrUpdate==1)])>0)
	 {arrearrate[i]<-length(datafeb$newLoanMin[which(datafeb$PredOutcome==1&datafeb$ThirdBad==1&datafeb$TrUpdate==1)])/length(datafeb$newLoanMin[which(datafeb$PredOutcome==1&datafeb$TrUpdate==1)]);
	  arrearrateall[i]<-(sum(datafeb$TrUpdate==0&datafeb$ThirdBad==1)+sum(datafeb$TrUpdate==1&datafeb$ThirdBad==1&datafeb$PredOutcome==0)+sum(datafeb$TrUpdate==1&datafeb$PredOutcome==1)*arrearfrozen)/dim(datafeb)[1];
	 }
	 else
	 {arrearrate[i]<-0;
	arrearrateall[i]<-0;	
	 }
}


sum(datafeb2$AmCollected[which(datafeb2$TrUpdate==0&datafeb2$ThirdMixedScore<=1000&datafeb2$ThirdMixedScore>950)])/sum(datafeb2$ThirdLoanAmountAgreed[which(datafeb2$TrUpdate==0&datafeb2$ThirdMixedScore<=1000&datafeb2$ThirdMixedScore>950)])
sum(datafeb2$AmCollected[which(datafeb2$TrUpdate==0&datafeb2$ThirdMixedScore<=950&datafeb2$ThirdMixedScore>900)])/sum(datafeb2$ThirdLoanAmountAgreed[which(datafeb2$TrUpdate==0&datafeb2$ThirdMixedScore<=950&datafeb2$ThirdMixedScore>900)])
sum(datafeb2$AmCollected[which(datafeb2$TrUpdate==0&datafeb2$ThirdMixedScore<=900&datafeb2$ThirdMixedScore>850)])/sum(datafeb2$ThirdLoanAmountAgreed[which(datafeb2$TrUpdate==0&datafeb2$ThirdMixedScore<=900&datafeb2$ThirdMixedScore>850)])
sum(datafeb2$AmCollected[which(datafeb2$TrUpdate==0&datafeb2$ThirdMixedScore<=850&datafeb2$ThirdMixedScore>800)])/sum(datafeb2$ThirdLoanAmountAgreed[which(datafeb2$TrUpdate==0&datafeb2$ThirdMixedScore<=850&datafeb2$ThirdMixedScore>800)])
sum(datafeb2$AmCollected[which(datafeb2$TrUpdate==0&datafeb2$ThirdMixedScore<=800&datafeb2$ThirdMixedScore>750)])/sum(datafeb2$ThirdLoanAmountAgreed[which(datafeb2$TrUpdate==0&datafeb2$ThirdMixedScore<=800&datafeb2$ThirdMixedScore>750)])
sum(datafeb2$AmCollected[which(datafeb2$TrUpdate==0&datafeb2$ThirdMixedScore<=750&datafeb2$ThirdMixedScore>700)])/sum(datafeb2$ThirdLoanAmountAgreed[which(datafeb2$TrUpdate==0&datafeb2$ThirdMixedScore<=750&datafeb2$ThirdMixedScore>700)])
sum(datafeb2$AmCollected[which(datafeb2$TrUpdate==0&datafeb2$ThirdMixedScore<=700&datafeb2$ThirdMixedScore>650)])/sum(datafeb2$ThirdLoanAmountAgreed[which(datafeb2$TrUpdate==0&datafeb2$ThirdMixedScore<=700&datafeb2$ThirdMixedScore>650)])
sum(datafeb2$AmCollected[which(datafeb2$TrUpdate==0&datafeb2$ThirdMixedScore<=650&datafeb2$ThirdMixedScore>600)])/sum(datafeb2$ThirdLoanAmountAgreed[which(datafeb2$TrUpdate==0&datafeb2$ThirdMixedScore<=650&datafeb2$ThirdMixedScore>600)])
sum(datafeb2$AmCollected[which(datafeb2$TrUpdate==0&datafeb2$ThirdMixedScore<=600)])/sum(datafeb2$ThirdLoanAmountAgreed[which(datafeb2$TrUpdate==0&datafeb2$ThirdMixedScore<=600)])


sum(datafeb2$AmCollected[which(datafeb2$TrUpdate==0&datafeb2$ThirdMixedScore<=600&datafeb2$ThirdMixedScore>550)])/sum(datafeb2$ThirdLoanAmountAgreed[which(datafeb2$TrUpdate==0&datafeb2$ThirdMixedScore<=600&datafeb2$ThirdMixedScore>550)])
sum(datafeb2$AmCollected[which(datafeb2$TrUpdate==0&datafeb2$ThirdMixedScore<=550&datafeb2$ThirdMixedScore>500)])/sum(datafeb2$ThirdLoanAmountAgreed[which(datafeb2$TrUpdate==0&datafeb2$ThirdMixedScore<=550&datafeb2$ThirdMixedScore>500)])
sum(datafeb2$AmCollected[which(datafeb2$TrUpdate==0&datafeb2$ThirdMixedScore<=500)])/sum(datafeb2$ThirdLoanAmountAgreed[which(datafeb2$TrUpdate==0&datafeb2$ThirdMixedScore<=500)])


sum(datafeb2$AmCollected[which(datafeb2$TrUpdate==0&datafeb2$ThirdMixedScore<=600)])/sum(datafeb2$ThirdLoanAmountAgreed[which(datafeb2$TrUpdate==0&datafeb2$ThirdMixedScore<=600)])