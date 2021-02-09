%% Load Data
cd('\\H:\Projects\Tucker Assignments\Rogue Spring Chinook Forecast\Background')
%[num,txt]=xlsread('RogueCHS_summary1.xlsx','Sheet1','A4:R41');
T=readtable('Copy of Copy of RogueCHS_summary1.xlsx','Sheet','N_Age_Harv','ReadRowNames',true);
Nage=(T.Wild./((1-T.HbelowGRD).*(1-T.HaboveGRD))).*[T.age2,T.age3,T.age4,T.age5,T.age6];

%% Plot Sibling Regressions
for A=1:4
subplot(2,2,A),scatter(Nage(1:(end-1),A),Nage(2:end,A+1))
end
figure
for A=1:4
subplot(2,2,A),scatter(log(Nage(1:(end-1),A)),log(Nage(2:end,A+1)))
xlim([0 max(max(log(Nage(:,A)),log(Nage(:,A+1))))])
ylim([0 max(max(log(Nage(:,A)),log(Nage(:,A+1))))])
end

%% Build Sibling Regression

%Calculate ratios
for A=1:4;
rat(:,A)=Nage(2:end,A+1)./Nage(1:(end-1),A);
lograt(:,A)=log(Nage(2:end,A+1))./log(Nage(1:(end-1),A));
end

%Autocorrelation in rats?
subplot(2,2,1),autocorr(rat(:,1))
subplot(2,2,2),autocorr(rat(:,2))
subplot(2,2,3),autocorr(rat(:,3))
subplot(2,2,4),autocorr(rat(:,4))
%nothing substantial

%Extract Ratios
rat23=median(rat(:,1));
rat34=median(rat(:,2));
rat45=median(rat(:,3));
rat56=median(rat(:,4));

lograt23=mean(lograt(:,1));
lograt34=mean(lograt(:,2));
lograt45=mean(lograt(:,3));
%lograt56=mean(lograt(:,4));%can't use logs bc log(0)=-Inf
rat56=median(rat(:,4));

%Get 2 year olds
figure
subplot(1,3,1),plot(1981:2019,Nage(:,1),'.-'),ylabel('Jacks'),xlabel('Year')
subplot(1,3,2),autocorr(Nage(:,1))
subplot(1,3,3),parcorr(Nage(:,1))
jack=arima(1,0,0);
%out1=estimate(jack,C(:,1,pop),'X',X1)%ARIMAX
out1=estimate(jack,Nage(:,1))%ARIMA
[Y, MSE]=forecast(out1,1,'Y0',Nage(:,1))
[Y-1.96*sqrt(MSE) Y+1.96*sqrt(MSE)]
Nage(:,1)
%{
%LOOCV
hold=[32 33 34 35 36];
three=arima(1,0,0);
for i=1:4
    out1=estimate(three,Nage(1:hold(i),2));
    [Y, MSE]=forecast(out1,1,'Y0',Nage(1:hold(i),2));
    RMSE(i)=sqrt((Nage(hold(i)+1,2)-Y)^2);
end
mean(RMSE)
%}

%Use arima(1,0,0) to get jacks
hold=[34 35 36 37 38]; % 1 less the prediction point.
jack=arima(1,0,0);
resid_pop=zeros(length(hold),1);
RMSE_pop=zeros(length(hold),1);
MAPE_pop=zeros(length(hold),1);
MAPE_mean=zeros(length(hold),1);
MAPE_prev=zeros(length(hold),1);
MAPE_log_pop=zeros(length(hold),1);

for i=1:length(hold)
  out1=estimate(jack,Nage(1:hold(i),1));
  [jack_hat, MSE]=forecast(out1,1,'Y0',Nage(1:hold(i),1));
  %five=Nage(hold(i),5);%What is this?

  %Untransformed ratio
  three_pop=jack_hat*rat23;
  four_pop=Nage(hold(i),2)*rat34;
  five_pop=Nage(hold(i),3)*rat45;
  six_pop=Nage(hold(i),4)*rat56;
  resid_pop(i)=sum([jack_hat,three_pop,four_pop,five_pop,six_pop])-sum(Nage(hold(i)+1,1:5));
  RMSE_pop(i)=sqrt((sum([jack_hat, three_pop,four_pop,five_pop,six_pop])-sum(Nage(hold(i)+1,1:5)))^2);
  MAPE_pop(i)= abs((sum([jack_hat, three_pop,four_pop,five_pop,six_pop])-sum(Nage(hold(i)+1,1:5)))/sum(Nage(hold(i)+1,1:5)));

  %logged ratio
  log_three_pop=exp(log(jack_hat)*lograt23);
  log_four_pop=exp(log(Nage(hold(i),2))*lograt34);
  log_five_pop=exp(log(Nage(hold(i),3))*lograt45);
  six_pop=Nage(hold(i),4)*rat56;
  MAPE_log_pop(i)= abs((sum([jack_hat, log_three_pop,log_four_pop,log_five_pop,six_pop])-sum(Nage(hold(i)+1,1:5)))/sum(Nage(hold(i)+1,1:5)));

  %Use historical mean as benchmark
  MAPE_mean(i)=abs((mean(sum(Nage(1:hold(i),1:5),2))-sum(Nage(hold(i)+1,1:5)))/sum(Nage(hold(i)+1,1:5)));

  %Use previous year's abundance as benchmark
  MAPE_prev(i)=abs(((sum(Nage(hold(i),1:5)))-sum(Nage(hold(i)+1,1:5)))/sum(Nage(hold(i)+1,1:5)));

end

%mean(mean(RMSE_pop))
%MAPE_log_pop(:,:)'

[mean(MAPE_mean) mean(MAPE_prev) mean(MAPE_pop)  mean(MAPE_log_pop)]


%% PREDICT CURRENT YEAR
jack=arima(1,0,0);
out1=estimate(jack,Nage(:,1));
[jack_hat, MSE]=forecast(out1,1,'Y0',Nage(:,1));
%Untransformed ratio
three_pop=jack_hat*rat23;
four_pop=Nage(end,2)*rat34;
five_pop=Nage(end,3)*rat45;
six_pop=Nage(end,4)*rat56;
sum([jack_hat,three_pop,four_pop,five_pop,six_pop])

