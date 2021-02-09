# call packages -----------------------------------------------------------
packages <- c("openxlsx",
              "forecast")

if (!require(install.load)) {
  install.packages("install.load")
}

install.load::install_load(packages)

# data steps --------------------------------------------------------------
## read-in raw data
rogueChSsibReg.dat <- read.xlsx("Data\\RogueChSForecastData.xlsx",
                                sheet = 6,
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

# model fits (zero-intercept sibling regressions) -------------------------
## age-3 model
rogueChSsibReg.age3mod <- lm(Nage3~Nage2 + 0, data = cbind(head(subset(rogueChSsibReg.dat,select = c(Nage2)),-1), tail(subset(rogueChSsibReg.dat,select = c(Nage3)),-1)))
summary(rogueChSsibReg.age3mod)

## age-4 model
rogueChSsibReg.age4mod <- lm(Nage4~Nage3 + 0, data = cbind(head(subset(rogueChSsibReg.dat,select = c(Nage3)),-1), tail(subset(rogueChSsibReg.dat,select = c(Nage4)),-1)))
summary(rogueChSsibReg.age4mod)

## age-5 model
rogueChSsibReg.age5mod <- lm(Nage5~Nage4 + 0, data = cbind(head(subset(rogueChSsibReg.dat,select = c(Nage4)),-1), tail(subset(rogueChSsibReg.dat,select = c(Nage5)),-1)))
summary(rogueChSsibReg.age5mod)

## age-6 model
rogueChSsibReg.age6mod <- lm(Nage~Nage5 + 0, data = cbind(head(subset(rogueChSsibReg.dat,select = c(Nage5)),-1), tail(subset(rogueChSsibReg.dat,select = c(Nage6)),-1)))
summary(rogueChSsibReg.age6mod)

# model predictions -------------------------------------------------------
## age-2 ARIMA
rogueChSsibReg.age2ts<-ts(rogueChSsibReg.dat$Nage2,start = c(1981))
rogueChSsibReg.age2ARIMAfit <- Arima(window(rogueChSsibReg.age2ts, end = 2018), order = c(1,0,0))
rogueChSsibReg.age2ARIMAfore <- forecast(rogueChSsibReg.age2ARIMAfit, h = 1)

## prediction data
newdata <- cbind(data.frame(Nage2=rogueChSsibReg.dat$Nage2,tail(subset(rogueChSsibReg.dat,select = c(Nage3, Nage4, Nage5, Nage6)),1))

## age-3
rogueChSsibReg.age3pred <- predict(rogueChSsibReg.age3mod, newdata = newdata, interval = "prediction")

## age-4
rogueChSsibReg.age4pred <- predict(rogueChSsibReg.age4mod, newdata = newdata, interval = "prediction")

## age-5
rogueChSsibReg.age5pred <- predict(rogueChSsibReg.age5mod, newdata = newdata, interval = "prediction")

## age-6
rogueChSsibReg.age6pred <- predict(rogueChSsibReg.age6mod, newdata = newdata, interval = "prediction")

# output data frame -------------------------------------------------------
rogueChSsibReg.out <- data.frame(class = c("age-3","age-4","age-5","age-6","currRet"),
                                 prediction = rbind(rogueChSsibReg.age3pred,
                                                    rogueChSsibReg.age4pred,
                                                    rogueChSsibReg.age5pred,
                                                    rogueChSsibReg.age6pred,
                                                    c(sum(rbind(rogueChSsibReg.age3pred,
                                                                rogueChSsibReg.age4pred,
                                                                rogueChSsibReg.age5pred,
                                                                rogueChSsibReg.age6pred)[,1]),NA,NA)),
                                 row.names = NULL)

print(rogueChSsibReg.out)
