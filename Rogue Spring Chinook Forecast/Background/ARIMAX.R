clear all
cd('N:\docs\Projects\RogueCHS\May2020')
S=xlsread('Copy of Copy of RogueCHS_summary1','Master','B5:B43');
PDO=xlsread('Copy of Copy of RogueCHS_summary1','Master','M5:M43');
NPGO1=xlsread('Copy of Copy of RogueCHS_summary1','Master','N5:N43');
NPGO2=xlsread('Copy of Copy of RogueCHS_summary1','Master','O5:O43');
LOGRWL=xlsread('Copy of Copy of RogueCHS_summary1','Master','P5:P43');
FLOW=xlsread('Copy of Copy of RogueCHS_summary1','Master','Q5:Q43');
%Align ocean vars with dominant age class (4yro)
X=[PDO NPGO1 NPGO2 LOGRWL FLOW];
[Xz,mu,sig]=zscore(X);
Y=S;

scatter(X(:,1),Y)



%ARIMAX1=arima(1,0,0);
%out1=estimate(ARIMAX1,X(2:end,1),'X',X(1:end,2:4))

%LOOCV
hold=[length(X)-5 length(X)-4 length(X)-3 length(X)-2 length(X)-1];
p=1;
D=0;
q=2;
%Don't use any covs
for i=1:5
    Yest=Y(2:hold(i));
    Mod1=arima(p,D,q);   
    %EstMod1=estimate(Mod1,Yest,'Y0',Y(1:2));
    EstMod1=estimate(Mod1,Yest);
    yhat(i)=forecast(EstMod1,1,'Y0',Yest);
 MAPE0(i)=abs(Y(hold(i)+1)-yhat(i))/Y(hold(i)+1) ;
end
MAPE0 ;
mean(MAPE0) ;
[yhat',Y((end-3):end)] ;
%scatter(yhat',Y((end-3):end))
%This is garbage

%Use all predictors aligned with 4 yro.
[Xz mu, sig]=zscore(X);
for i=1:5
    Yest=Y(2:hold(i));
    Xest=Xz(2:hold(i),:);
    Mod1=arima(p,D,q);   
    EstMod1=estimate(Mod1,Yest,'X',Xest,'Y0',Y(1));
    yhat=forecast(EstMod1,1,'X0',Xz(1:hold(i),:),'Y0',Yest,'XF',Xz(hold(i)+1,:));
    %yhat=forecast(EstMod1,1,'X0',Xest,'Y0',Y(1:hold(i)),'XF',X(hold(i)+1,:))%same as above
 MAPE1(i)=abs(Y(hold(i)+1)-yhat)/Y(hold(i)+1) ;
end
MAPE1 ;
mean(MAPE1) ;

%Use just PDO aligned with 4 yro.
for i=1:5
    Yest=Y(2:hold(i));
    Xest=Xz(2:hold(i),1);
    Mod1=arima(p,D,q);   
    EstMod1=estimate(Mod1,Yest,'X',Xest,'Y0',Y(1));
    yhat=forecast(EstMod1,1,'X0',Xz(1:hold(i),1),'Y0',Yest,'XF',Xz(hold(i)+1,1));
 MAPE2(i)=abs(Y(hold(i)+1)-yhat)/Y(hold(i)+1) ;
end
MAPE2 ;
mean(MAPE2) ;

%Use just NPGO aligned with 4 yro.
for i=1:5
    Yest=Y(2:hold(i));
    Xest=Xz(2:hold(i),2);
    Mod1=arima(p,D,q);   
    EstMod1=estimate(Mod1,Yest,'X',Xest,'Y0',Y(1));
    yhat=forecast(EstMod1,1,'X0',Xz(1:hold(i),2),'Y0',Yest,'XF',Xz(hold(i)+1,2));
 MAPE3(i)=abs(Y(hold(i)+1)-yhat)/Y(hold(i)+1) ;
end
MAPE3 ;
mean(MAPE3) ;

%Use just LOGRWL aligned with 4 yro.
for i=1:5
    Yest=Y(2:hold(i));
    Xest=Xz(2:hold(i),4);
    Mod1=arima(p,D,q);   
    EstMod1=estimate(Mod1,Yest,'X',Xest,'Y0',Y(1));
    yhat=forecast(EstMod1,1,'X0',Xz(1:hold(i),4),'Y0',Yest,'XF',Xz(hold(i)+1,4));
 MAPE4(i)=abs(Y(hold(i)+1)-yhat)/Y(hold(i)+1) ;
end
MAPE4 ;
mean(MAPE4) ;

%Use just FLOW aligned with 4 yro.
for i=1:5
    Yest=Y(2:hold(i));
    Xest=Xz(2:hold(i),5);
    Mod1=arima(p,D,q);   
    EstMod1=estimate(Mod1,Yest,'X',Xest,'Y0',Y(1));
    yhat=forecast(EstMod1,1,'X0',Xz(1:hold(i),5),'Y0',Yest,'XF',Xz(hold(i)+1,5));
 MAPE5(i)=abs(Y(hold(i)+1)-yhat)/Y(hold(i)+1) ;
end
MAPE5 ;
mean(MAPE5) ;

[MAPE0', MAPE1', MAPE2', MAPE3', MAPE4', MAPE5']
[mean(MAPE0),mean(MAPE1),mean(MAPE2), mean(MAPE3), mean(MAPE4), mean(MAPE5)]

%% PREDICT CURRENT YEAR
Xzprime=(1479-mu(5))/sig(5)
Yest=Y(2:end);
Xest=Xz(2:end,5);
Mod1=arima(p,D,q);   
EstMod1=estimate(Mod1,Yest,'X',Xest,'Y0',Y(1));
yhat=forecast(EstMod1,1,'X0',Xz(1:end,5),'Y0',Yest,'XF',Xzprime)

%% ReDo with covariates averaged over age comp.
%covs in spreadsheet are aligned on 4 yr olds.
clear all
Year=xlsread('RogueCHS_summary1','Master','B5:B41');
N=xlsread('RogueCHS_summary1','Master','L5:L41');
PDO4=xlsread('RogueCHS_summary1','Master','N3:N43');
NPGO1_4=xlsread('RogueCHS_summary1','Master','O3:O43');
NPGO2_4=xlsread('RogueCHS_summary1','Master','P3:P43');
LOGRWL4=xlsread('RogueCHS_summary1','Master','Q3:Q43');
FLOW4=xlsread('RogueCHS_summary1','Master','R3:R43');
AGE=xlsread('RogueCHS_summary1','Master','E5:I41');

for t=3:(length(PDO4)-2)
wPDO(t-2)=sum(AGE(t-2,:)'.*PDO4(t-2:t+2));
wNPGO1(t-2)=sum(AGE(t-2,:)'.*NPGO1_4(t-2:t+2));
wNPGO2(t-2)=sum(AGE(t-2,:)'.*NPGO2_4(t-2:t+2));
wFLOW(t-2)=sum(AGE(t-2,:)'.*FLOW4(t-2:t+2));
end
X=[wPDO'  wNPGO1'  wNPGO2'  wFLOW'];
[Xz,mu,sig]=zscore(X);
Y=N;

%LOOCV
hold=[length(X)-4 length(X)-3 length(X)-2 length(X)-1];
p=1;
D=0;
q=2;
%Don't use any covs
for i=1:4
    Yest=Y(2:hold(i));
    Mod1=arima(p,D,q);   
    %EstMod1=estimate(Mod1,Yest,'Y0',Y(1:2));
    EstMod1=estimate(Mod1,Yest,'print',false);
    yhat(i)=forecast(EstMod1,1,'Y0',Yest);
 MAPE0(i)=abs(Y(hold(i)+1)-yhat(i))/Y(hold(i)+1) ;
end
MAPE0 ;
mean(MAPE0) ;
[yhat',Y((end-3):end)] ;

for i=1:4
    Yest=Y(2:hold(i));
    Xest=Xz(2:hold(i),:);
    Mod1=arima(p,D,q);   
    EstMod1=estimate(Mod1,Yest,'X',Xest,'Y0',Y(1));
    yhat=forecast(EstMod1,1,'X0',Xz(1:hold(i),:),'Y0',Yest,'XF',Xz(hold(i)+1,:));
    %yhat=forecast(EstMod1,1,'X0',Xest,'Y0',Y(1:hold(i)),'XF',X(hold(i)+1,:))%same as above
 MAPE1(i)=abs(Y(hold(i)+1)-yhat)/Y(hold(i)+1) ;
end
MAPE1 ;
mean(MAPE1) ;

%Use just PDO aligned with 4 yro.
for i=1:4
    Yest=Y(2:hold(i));
    Xest=Xz(2:hold(i),1);
    Mod1=arima(p,D,q);   
    EstMod1=estimate(Mod1,Yest,'X',Xest,'Y0',Y(1));
    yhat=forecast(EstMod1,1,'X0',Xz(1:hold(i),1),'Y0',Yest,'XF',Xz(hold(i)+1,1));
 MAPE2(i)=abs(Y(hold(i)+1)-yhat)/Y(hold(i)+1) ;
end
MAPE2 ;
mean(MAPE2) ;

%Use just NPGO aligned with 4 yro.
for i=1:4
    Yest=Y(2:hold(i));
    Xest=Xz(2:hold(i),2);
    Mod1=arima(p,D,q);   
    EstMod1=estimate(Mod1,Yest,'X',Xest,'Y0',Y(1));
    yhat=forecast(EstMod1,1,'X0',Xz(1:hold(i),2),'Y0',Yest,'XF',Xz(hold(i)+1,2));
 MAPE3(i)=abs(Y(hold(i)+1)-yhat)/Y(hold(i)+1) ;
end
MAPE3 ;
mean(MAPE3) ;

%Use just OTHER NPGO aligned with 4 yro.
for i=1:4
    Yest=Y(2:hold(i));
    Xest=Xz(2:hold(i),3);
    Mod1=arima(p,D,q);   
    EstMod1=estimate(Mod1,Yest,'X',Xest,'Y0',Y(1));
    yhat=forecast(EstMod1,1,'X0',Xz(1:hold(i),3),'Y0',Yest,'XF',Xz(hold(i)+1,3));
 MAPE4(i)=abs(Y(hold(i)+1)-yhat)/Y(hold(i)+1) ;
end
MAPE4 ;
mean(MAPE4) ;

%Use just FLOW aligned with 4 yro.
for i=1:4
    Yest=Y(2:hold(i));
    Xest=Xz(2:hold(i),4);
    Mod1=arima(p,D,q);   
    EstMod1=estimate(Mod1,Yest,'X',Xest,'Y0',Y(1));
    yhat=forecast(EstMod1,1,'X0',Xz(1:hold(i),4),'Y0',Yest,'XF',Xz(hold(i)+1,4));
 MAPE5(i)=abs(Y(hold(i)+1)-yhat)/Y(hold(i)+1) ;
end
MAPE5 ;
mean(MAPE5) ;

[MAPE0', MAPE1', MAPE2', MAPE3', MAPE4', MAPE5']
[mean(MAPE0),mean(MAPE1),mean(MAPE2), mean(MAPE3), mean(MAPE4), mean(MAPE5)]
