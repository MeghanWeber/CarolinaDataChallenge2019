##--------------------------------------------------------------------##
## Hackathon - 2019 Carolina Data Challenge
## Date: 10/05/19 - 10/06/19
##--------------------------------------------------------------------##


#importing libraries for data munging and data exploration
library(data.table)
library(dplyr)
library(stringr)
library(plyr)

#importing libraries for time series
library(forecast)
library(haven)
library(fma)
library(expsmooth)
library(lmtest)
library(zoo)
library(seasonal)


#set working directory
setwd("C:/Users/taylo/OneDrive/Desktop/NCSU/Misc/Hackathon")


#reading in original csv data files into data frames
feed_grains = fread("FeedGrains.csv", stringsAsFactors = FALSE)
projections = fread("Projection2019.csv", stringsAsFactors = FALSE)



##------------------------##
## Exploring the data
##------------------------##

summary(feed_grains$Year_ID)
summary(feed_grains$SC_Commodity_Desc)

count(feed_grains, 'SC_Group_Desc')
count(feed_grains, 'SC_Commodity_Desc')
count(feed_grains, 'SC_GroupCommod_Desc')


#filtered for corn
sorghum_df = feed_grains %>%
  filter(SC_GroupCommod_Desc == "Sorghum") %>%
  filter(SC_Frequency_Desc == "Monthly") %>%
  filter(SC_Group_Desc == "Exports and imports")


#adding export variable
china_imports = sorghum_df %>%
  mutate(export = ifelse(substr(SC_Attribute_Desc, start=1, stop=1) == "E", 1, 0)) %>%
  filter(export == 1) %>%
  filter(SC_GeographyIndented_Desc == "China (Mainland)")




##---------------------------------------------##
## Starting with Chris' csv files from USDA
##---------------------------------------------##

#US sorghum exports - China
china_imports = fread("sorg_E_US_I_China.csv", stringsAsFactors = FALSE)

#US sorghum exports - Globally
us_exports = fread("sorg_E_US_I_World.csv", stringsAsFactors = FALSE)


#combining the two previous data sets on a left join - keep all rows of global exports and include extra data for exports to China
merged_df <- sqldf("select * 
                    from us_exports as ue
                    left join china_imports as ci on ci.frequency = ue.frequency and 
                          ci.year = ue.year")


#filtering exports to China for January 2014 - April 2018
#this data will be used to model time series
filtered_china = china_imports %>%
  filter((year >= 2014 & year <= 2017) | (year == 2018 & (frequency == "Jan" | frequency == "Feb" | frequency == "Mar" | frequency == "Apr"))) %>% 
  group_by(year, frequency)


#creating a time series object
china_ts = ts(filtered_china$amount, start=2014, frequency=12)


#STL Decomposition of original time series
#seasonality and trend components exist
decomp_stl = stl(china_ts, s.window = 7)
plot(decomp_stl)


#getting image of the data with trend piece overlaying it
plot(china_ts, col = "grey", main = "Sorghum Time Series", xlab = "Year", ylab = "", lwd = 2)
lines(decomp_stl$time.series[,2], col = "red", lwd = 2)


#using Fourier modeling to try to capture seasonality (3 <= k <= 6)
fourier_model_3 = Arima(china_ts, order=c(0, 0, 0), xreg=fourier(china_ts, K=3))
summary(fourier_model_3)

fourier_model_4 = Arima(china_ts, order=c(0, 0, 0), xreg=fourier(china_ts, K=4))
summary(fourier_model_4)

fourier_model_5 = Arima(china_ts, order=c(0, 0, 0), xreg=fourier(china_ts, K=5))
summary(fourier_model_5)

fourier_model_6 = Arima(china_ts, order=c(0, 0, 0), xreg=fourier(china_ts, K=6))
summary(fourier_model_6)

##--------------------------------------------------##
## Based on MAPE, k=6 captures the most seasonality
## Fit 6 sine and cosine variables
##--------------------------------------------------##




#residuals from fitting 6 sign and cosine variables
fourier_residuals = fourier_model_6$residuals


#STL decomposition of previous residuals
#trend still exists, but it's stationary because it's going up and down - converging towards a mean
decomp_stl = stl(fourier_residuals, s.window = 7)
plot(decomp_stl)


#looking at autocorrelation and partial autocorrelation to
#figure out how many arima terms to use
Acf(fourier_residuals, main="")
Pacf(fourier_residuals, main="")


#final arima model testing
arima_model = Arima(china_ts, order=c(6, 0, 6), xreg=fourier(china_ts, K=6))
summary(arima_model)

##-----------------------------##
## Final ARIMA Model
## ARIMA (6, 0, 6)
## with fourier transformation
##-----------------------------##



#residuals from final arima model
arima_residuals = arima_model$residuals


#exponential smoothing model testing using Holt Winters (Level, Trend, Seasonality)
winters = hw(china_ts, seasonal="additive")
winters_residuals = winters$residuals


#looking at residuals from arima model to check for white noise
Acf(arima_residuals, main="")
Pacf(arima_residuals, main="")


#checking to see which model auto arima selected
#not better than our model, stupid computer
auto_arima = auto.arima(china_ts, seasonal=TRUE, xreg=fourier(china_ts, K=3))
summary(auto_arima)

##----------------------##
## ARIMA(6, 0, 6)
##----------------------##


#forecasting using arima model starting in May 2018
year_forecast = forecast(arima_model, h=12, xreg=fourier(china_ts, K=6))
plot(year_forecast)


#forecasting using Holt Winters ESM just to verify forecasts
winters_forecast = forecast(winters, h=12)
plot(winters_forecast)


#writing csv file - actual values
write.csv(filtered_china, "filtered_china.csv")


#writing csv file - predicted values
write.csv(arima_model$fitted, "predicted_values.csv")


#writing csv file - full china actual values from January 2014 to June 2019
full_china = china_imports %>%
  filter((year >= 2014 & year <= 2018) | (year == 2019 & (frequency == "Jan" | frequency == "Feb" | frequency == "Mar" | frequency == "Apr" | frequency == "May" | frequency == "Jun"))) %>% 
  group_by(year, frequency)

write.csv(full_china, "full_china.csv")


