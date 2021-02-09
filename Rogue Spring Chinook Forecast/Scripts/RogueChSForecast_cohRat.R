# call packages -----------------------------------------------------------
packages <- c("openxlsx",
              "forecast",
              "ggplot2")

if (!require(install.load)) {
  install.packages("install.load")
}

install.load::install_load(packages)

# data steps --------------------------------------------------------------
## read-in raw data
rogueChSsibReg.dat <- read.xlsx("Data\\RogueChSForecastData.v.Working.xlsx",
                                sheet = 2,
                                colNames = TRUE)

## define new vars
### pre-harvest wild return
rogueChSsibReg.dat$preHarv_wildRet <- round(rogueChSsibReg.dat$wild_spn/((1-rogueChSsibReg.dat$harv_belGRD)*(1-rogueChSsibReg.dat$harv_abvGRD)),0)

### recruits
#### data frame
rogueChSsibReg.recDat <- data.frame(prodAge2 = c(tail(rogueChSsibReg.dat$age2_comp*rogueChSsibReg.dat$preHarv_wildRet,-2),rep(NA,2)),
                                    prodAge3 = c(tail(rogueChSsibReg.dat$age3_comp*rogueChSsibReg.dat$preHarv_wildRet,-3),rep(NA,3)),
                                    prodAge4 = c(tail(rogueChSsibReg.dat$age4_comp*rogueChSsibReg.dat$preHarv_wildRet,-4),rep(NA,4)),
                                    prodAge5 = c(tail(rogueChSsibReg.dat$age5_comp*rogueChSsibReg.dat$preHarv_wildRet,-5),rep(NA,5)),
                                    prodAge6 = c(tail(rogueChSsibReg.dat$age6_comp*rogueChSsibReg.dat$preHarv_wildRet,-6),rep(NA,6)))

rogueChSsibReg.recDat$recruits <- round(rowSums(rogueChSsibReg.recDat),0)

### combine data frames
rogueChSsibReg.dat <- cbind(rogueChSsibReg.dat,recruits=rogueChSsibReg.recDat[,ncol(rogueChSsibReg.recDat)])

## Nage (number at age)
rogueChSsibReg.dat$Nage2 <- round((rogueChSsibReg.dat$wild_spn/((1-rogueChSsibReg.dat$harv_belGRD)*(1-rogueChSsibReg.dat$harv_abvGRD)))*rogueChSsibReg.dat$age2_comp,0)
rogueChSsibReg.dat$Nage3 <- round((rogueChSsibReg.dat$wild_spn/((1-rogueChSsibReg.dat$harv_belGRD)*(1-rogueChSsibReg.dat$harv_abvGRD)))*rogueChSsibReg.dat$age3_comp,0)
rogueChSsibReg.dat$Nage4 <- round((rogueChSsibReg.dat$wild_spn/((1-rogueChSsibReg.dat$harv_belGRD)*(1-rogueChSsibReg.dat$harv_abvGRD)))*rogueChSsibReg.dat$age4_comp,0)
rogueChSsibReg.dat$Nage5 <- round((rogueChSsibReg.dat$wild_spn/((1-rogueChSsibReg.dat$harv_belGRD)*(1-rogueChSsibReg.dat$harv_abvGRD)))*rogueChSsibReg.dat$age5_comp,0)
rogueChSsibReg.dat$Nage6 <- round((rogueChSsibReg.dat$wild_spn/((1-rogueChSsibReg.dat$harv_belGRD)*(1-rogueChSsibReg.dat$harv_abvGRD)))*rogueChSsibReg.dat$age6_comp,0)

# prediction function -----------------------------------------------------
## container (this needs to be cleared before running the function)
rogueChSsibReg.predOut <- data.frame(predYear = numeric(),
                                     prediction = numeric(),
                                     stringsAsFactors = FALSE)

predFun <- function(predYear){
  ## reduced data set
  rogueChSsibReg.datRed <- subset(rogueChSsibReg.dat, ret_yr < predYear)


  # cohort ratios -----------------------------------------------------------
  ## age-3
  rogueChSsibReg.rat32dat <- cbind(head(subset(rogueChSsibReg.datRed,select = c(Nage2)),-1), tail(subset(rogueChSsibReg.datRed,select = c(Nage3)),-1))
  rogueChSsibReg.rat32dat$rat32 <- rogueChSsibReg.rat32dat$Nage3/rogueChSsibReg.rat32dat$Nage2
  rat32 <- median(rogueChSsibReg.rat32dat$rat32)

  ## age-4
  rogueChSsibReg.rat43dat <- cbind(head(subset(rogueChSsibReg.datRed,select = c(Nage3)),-1), tail(subset(rogueChSsibReg.datRed,select = c(Nage4)),-1))
  rogueChSsibReg.rat43dat$rat43 <- rogueChSsibReg.rat43dat$Nage4/rogueChSsibReg.rat43dat$Nage3
  rat43 <- median(rogueChSsibReg.rat43dat$rat43)

  ## age-5
  rogueChSsibReg.rat54dat <- cbind(head(subset(rogueChSsibReg.datRed,select = c(Nage4)),-1), tail(subset(rogueChSsibReg.datRed,select = c(Nage5)),-1))
  rogueChSsibReg.rat54dat$rat54 <- rogueChSsibReg.rat54dat$Nage5/rogueChSsibReg.rat54dat$Nage4
  rat54 <- median(rogueChSsibReg.rat54dat$rat54)

  ## age-6
  rogueChSsibReg.rat65dat <- cbind(head(subset(rogueChSsibReg.datRed,select = c(Nage5)),-1), tail(subset(rogueChSsibReg.datRed,select = c(Nage6)),-1))
  rogueChSsibReg.rat65dat$rat65 <- rogueChSsibReg.rat65dat$Nage6/rogueChSsibReg.rat65dat$Nage5
  rat65 <- median(rogueChSsibReg.rat65dat$rat65)

  # t+1 preditions ----------------------------------------------------------
  ## prediction data
  newdata <- tail(subset(rogueChSsibReg.datRed,select = c(Nage3, Nage4, Nage5, Nage6)),1)

  ## age-2 ARIMA
  rogueChSsibReg.age2ts<-ts(rogueChSsibReg.dat$Nage2,start = c(1981))
  rogueChSsibReg.age2ARIMAfit <- Arima(window(rogueChSsibReg.age2ts, end = predYear-1), order = c(1,0,0))
  rogueChSsibReg.age2ARIMAfore <- forecast(rogueChSsibReg.age2ARIMAfit, h = 1)

  ## age-3
  rogueChSsibReg.age3pred <- rat32*rogueChSsibReg.age2ARIMAfore$mean[1]

  ## age-4
  rogueChSsibReg.age4pred <- rat43*newdata$Nage3

  ## age-5
  rogueChSsibReg.age5pred <- rat54*newdata$Nage4

  ## age-6
  rogueChSsibReg.age6pred <- rat65*newdata$Nage5

  # output data frame -------------------------------------------------------
  rogueChSsibReg.out <- data.frame(class = c("age-3","age-4","age-5","age-6","currRet"),
                                   prediction = c(rogueChSsibReg.age3pred,
                                                  rogueChSsibReg.age4pred,
                                                  rogueChSsibReg.age5pred,
                                                  rogueChSsibReg.age6pred,
                                                  sum(rogueChSsibReg.age3pred,
                                                      rogueChSsibReg.age4pred,
                                                      rogueChSsibReg.age5pred,
                                                      rogueChSsibReg.age6pred)),
                                   row.names = NULL)
  ## output as data frame to global environment (for use in validation)
  rogueChSsibReg.predOut[nrow(rogueChSsibReg.predOut)+1,]<<-c(predYear,
                                                              round(rogueChSsibReg.out[5,2],0))

  print(rogueChSsibReg.out)  # uncomment line to view age-class breakdown in console
}

# function control --------------------------------------------------------
## runs the 'predFun' function according to year 't' (i.e., forecast year) specified in predFun(t)
predFun(2021)

## print predition output to console
print(rogueChSsibReg.predOut)

## clear prediction data frame (caution: will erase the output from previous steps)
rogueChSsibReg.predOut <- rogueChSsibReg.predOut[0,]

# predictive performance --------------------------------------------------
## generates predictions for the sequence of years between 'fromYear' and 'toYear' and tabulates with estimates generated directly
## data prior to 'fromYear' are used to initialize the routine
fromYear <- 2007
toYear <- 2018

for (predYear in seq(fromYear,toYear,1)){
  predFun(predYear)
}

# print(rogueChSsibReg.predOut)

rogueChSsibReg.validDat <- data.frame(predYear = rogueChSsibReg.predOut$predYear,
                                      prediction = rogueChSsibReg.predOut$prediction,
                                      estimated = rowSums(subset(rogueChSsibReg.dat, select = c(Nage3,
                                                                                                Nage4,
                                                                                                Nage5,
                                                                                                Nage6),
                                                                 ret_yr>=fromYear&ret_yr<=toYear)))

## MAPE for use in estimating weights for ensamble prediction (can be used when other models are developed)
# mape <- mean(abs((rogueChSsibReg.validDat$estimated-rogueChSsibReg.validDat$prediction)/rogueChSsibReg.validDat$estimated)) * 100

## validation plot
rogueChSsibReg.predPlot<-ggplot(data = rogueChSsibReg.validDat, aes(x=prediction, y = estimated))+                                           # plot output
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title.y = element_text(face = "bold", size = 14,vjust = 1,color = "black"),
        axis.title.x = element_text(face = "bold", size = 14,vjust = -1,color = "black"),
        axis.text.x = element_text(face = "bold",size = 12,vjust = 0.5,color = "black"),
        axis.text.y = element_text(face = "bold",size = 12,color = "black"),
        legend.position = "none")+
  geom_abline(intercept = 0, slope = 1)+
  labs(title = "Cohort Ratios", y = "Estimated", x = "Predicted") +
  geom_point(size = 3)+
  scale_y_continuous(limits=c(min(min(rogueChSsibReg.validDat$estimated),min(rogueChSsibReg.validDat$prediction)),max(max(rogueChSsibReg.validDat$estimated),max(rogueChSsibReg.validDat$prediction))),
                     breaks = seq(min(min(rogueChSsibReg.validDat$estimated),min(rogueChSsibReg.validDat$prediction)),max(max(rogueChSsibReg.validDat$estimated),max(rogueChSsibReg.validDat$prediction)),(max(max(rogueChSsibReg.validDat$estimated),max(rogueChSsibReg.validDat$prediction))-min(min(rogueChSsibReg.validDat$estimated),min(rogueChSsibReg.validDat$prediction)))/10),
                     labels = function(x) sprintf("%.0f", x))+
  scale_x_continuous(limits=c(min(min(rogueChSsibReg.validDat$estimated),min(rogueChSsibReg.validDat$prediction)),max(max(rogueChSsibReg.validDat$estimated),max(rogueChSsibReg.validDat$prediction))),
                     breaks = seq(min(min(rogueChSsibReg.validDat$estimated),min(rogueChSsibReg.validDat$prediction)),max(max(rogueChSsibReg.validDat$estimated),max(rogueChSsibReg.validDat$prediction)),(max(max(rogueChSsibReg.validDat$estimated),max(rogueChSsibReg.validDat$prediction))-min(min(rogueChSsibReg.validDat$estimated),min(rogueChSsibReg.validDat$prediction)))/10),
                     labels = function(x) sprintf("%.0f", x))


png(filename="Output\\Figures\\rogueChSsibRegPredPlot.png",     # plot is output to the project directory
    type="cairo",
    units="in",
    width=8,
    height=6,
    res=300)

print(rogueChSsibReg.predPlot)
dev.off() #  turn device off
print(rogueChSsibReg.predPlot)

## clear prediction data frame
rogueChSsibReg.predOut <- rogueChSsibReg.predOut[0,]
