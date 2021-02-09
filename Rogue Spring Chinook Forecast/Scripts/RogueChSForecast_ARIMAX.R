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
rogueChSARIMAX.dat <- read.xlsx("Data\\RogueChSForecastData.v.Working.xlsx",
                                sheet = 1,
                                startRow = 5,
                                cols = c(1,2,seq(13,17,1)),
                                colNames = FALSE)

colnames(rogueChSARIMAX.dat) <- c("ret_yr",
                                  "wild_spn",
                                  "PDOmay_sept",
                                  "NPGOjun_nov",
                                  "NPGOapr_sept",
                                  "spr_trans",
                                  "Flow_meanOct")

## create response time series for fitting
rogueChSARIMAX.respts <- ts(head(subset(rogueChSARIMAX.dat, select = c("wild_spn")),-1),start = c(1981))

## create exogenous regressor time series for fitting
rogueChSARIMAX.xregts <- ts(head(subset(rogueChSARIMAX.dat, select = c("Flow_meanOct")),-1),start = c(1981))

## create exogenous regrfessor time series for prediction
rogueChSARIMAX.xpredts <- ts(tail(subset(rogueChSARIMAX.dat, select = c("Flow_meanOct")),1),start = c(2021))

# ARIMAX Fit --------------------------------------------------------------
rogueChSARIMAX.fit <- Arima(rogueChSARIMAX.respts,xreg = rogueChSARIMAX.xregts, order = c(1,0,2))
rogueChSARIMAX.fore <- forecast(rogueChSARIMAX.fit, xreg = rogueChSARIMAX.xpredts, h = 1)
summary(rogueChSARIMAX.fore)
