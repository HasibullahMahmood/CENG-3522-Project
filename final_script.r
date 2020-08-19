## Upload all required libraries
library(data.table) # For converting data into time series format
library(ggplot2)    # To plot various plots
library(fpp2)       # For examining seasonality graphically
library(forecast)   # For various functions related to Time series
library(stats)      # For applying tests like acf, Ljung-Box Tests
library(tseries)    # For applying Dickey Fuller test
library(xts)        # Extensible time series format

# Import data
sales.df <- read.csv("sales_train_validation.csv")
calendar.df <- read.csv("calendar.csv")
price.df <- read.csv("sell_prices.csv")

# Print length of calendar
length(calendar.df[, 1])

# Get subset of data from row 1 up-to 1913
calendar.df <- calendar.df[1:1913, c(1, 4, 8:length(calendar.df))]

# Show 10 rows of data
head(calendar.df, 10)
rm(price.df)

# Factorize(encode) categorical columns.
event.name.1 <- unique(calendar.df$event_name_1)
event.type.1 <- unique(calendar.df$event_type_1)
event.name.2 <- unique(calendar.df$event_name_2)
event.type.2 <- unique(calendar.df$event_type_2)

# Show an example of factorized columns.
data.frame(event.name.1)

# Apply factorization on data.
calendar.df$event_name_1 <- factor(calendar.df$event_name_1,
                                   levels = event.name.1,
                                   labels = 1:length(event.name.1))

calendar.df$event_type_1 <- factor(calendar.df$event_type_1,
                                   levels = event.type.1,
                                   labels = 1:length(event.type.1))

calendar.df$event_name_2 <- factor(calendar.df$event_name_2,
                                   levels = event.name.2,
                                   labels = 1:length(event.name.2))

calendar.df$event_type_2 <- factor(calendar.df$event_type_2,
                                   levels = event.type.2,
                                   labels = 1:length(event.type.2))

# Remove unnecessary variables
rm(event.name.1, event.type.1, event.name.2, event.type.2)


# Show 10 rows of data
head(calendar.df, 10)

# Get the subset of data from column 6 up-to end
ca.df <- sales.df[, 6:ncol(sales.df)]

# Get the rows where state-id is equal to CA(California)
ca.df <- ca.df[ca.df$state_id=="CA",]

# sum the number of items sold per day
ca.df <- data.frame(colSums(ca.df[, 2:ncol(ca.df)]))

# Change the column name
colnames(ca.df) <- "nItemSold"

# Print sample
head(ca.df)

# Print summary
summary(ca.df)

# Add date to dataset
ca.df$date <- calendar.df[, 1]
# order the columns
ca.df <- ca.df[, c(2, 1)] 
# Convert character to date format
ca.df$date <- strptime(as.character(ca.df$date), "%m/%d/%Y") 

summary(ca.df)

# Convert data into time seires formats
ca.xts <- xts(ca.df$nItemSold, ca.df$date)
ca.ts <- ts(ca.df[, 2], start = c(2011, 29), end = c(2016, 116), frequency = 365)
# Change column name
colnames(ca.xts) <- c("nItemSold")
length(ca.ts)

## Plot the Time series data
plot(ca.xts, xlab="Date", ylab = "# of Items sold",
     main = "Items Sales data")

# Existence of seasonality can be observed using various plots
# Plot 1: Seasonal plot Year-wise (using ggseasonalplot())
ggseasonplot(x = ca.ts) +
  ylab("nItemSold") + 
  ggtitle("Seasonal plot: California State Items Sales")  + 
  facet_wrap(~year)

## Plot 2: Polar Seasonal plot Year-wise (using ggseasonplot())
ggseasonplot(ca.ts, polar=TRUE) +
  ylab("# of Items Sold") +
  ggtitle("Polar seasonal plot: California state Items Sales") + 
  facet_wrap(~year)

## Plot 3: Seasonal plot Month-wise (using monthplot())
monthplot(ca.ts)

## Decomposition of TS using decompose()
TSDecmpose<-decompose(ca.ts, type = "multiplicative")
plot(TSDecmpose)

## Spliting data into training and test data sets
TS_Train <- window(ca.ts, start=c(2011,29), end=c(2014, 366), freq=365)
length(TS_Train)

TS_Test <- window(ca.ts, start=c(2015,2), freq=365)
length(TS_Test)


autoplot(TS_Train, series="Train") +
  autolayer(TS_Test, series="Test") +
  ggtitle("Items Sales Traning and Test data") +
  xlab("Year") + ylab("# of items sales") +
  guides(colour=guide_legend(title="Forecast"))

TSDecmpose_train_Log<-stl(log10(TS_Train), s.window='p')
#head(TSDecmpose_train_Log, 10)
TS_Train_stl<-forecast(TSDecmpose_train_Log, method="rwdrift", h=480)
plot(TS_Train_stl)

result <- data.frame(date=tail(ca.df$date, 480),
                     original=tail(ca.df$nItemSold, 480),
                     forecasted=10^TS_Train_stl$mean, 
                     lower80=10^TS_Train_stl$lower[,1], upper80=10^TS_Train_stl$upper[,1],
                     lower95=10^TS_Train_stl$lower[,2], upper95=10^TS_Train_stl$upper[,2])


head(result)

Vec2<- 10^(cbind(log10(TS_Test) ,as.data.frame(forecast(TSDecmpose_train_Log,
                                                        method="rwdrift", h=480))[,1]))

ts.plot(Vec2, col=c("blue", "red"), main="California State Items sales: Actual vs Forecast")

RMSE2 <- round(sqrt(sum(((Vec2[,1]-Vec2[,2])^2)/length(Vec2[,1]))),4)
MAPE2 <- round(mean(abs(Vec2[,1]-Vec2[,2])/Vec2[,1]),4)
paste("Accuracy Measures: RMSE:", RMSE2, "and MAPE:", MAPE2)

# Plot items sales
plot(ca.ts, xlab="Years", ylab = "# of Items sold",
     main = "California State Items Sales")

# Make the variance equal
plot(log10(TS_Train))

# Make the means equal
plot(diff(TS_Train))

# Combination of both functions
plot(diff(log10(TS_Train)))

# adf test before stationarization
adf.test(TS_Train, k=125)

# adf test after stationarization
adf.test(diff(log10(TS_Train)), k=125)

TS_Train_log = log10(TS_Train)

# Run auto arima
TS_AutoARIMA <- auto.arima(TS_Train_log, seasonal = TRUE)
TS_AutoARIMA

# Creating model
model <- arima(TS_Train_log, c(5, 1, 2), seasonal = list(order = c(0, 1, 0), period = 365))

## Forecast sales
SalesForecasts <- forecast(model, h=480)
plot(SalesForecasts, shadecols = "oldstyle")

## Accuracy measures: RMSE and MAPE using ARIMA
Vec1<- 10^(cbind(log10(TS_Test) ,as.data.frame(SalesForecasts)[,1]))
ts.plot(Vec1, col=c("blue", "red"), main="California state item sales: Actual vs Forecast")

RMSE1 <- round(sqrt(sum(((Vec1[,1]-Vec1[,2])^2)/length(Vec1[,1]))),4)
MAPE1 <- round(mean(abs(Vec1[,1]-Vec1[,2])/Vec1[,1]),4)
paste("Accuracy Measures: RMSE:", RMSE1, "and MAPE:", MAPE1)

## Linear Regression

# Joining calendar with nItemSold
ca.df <- cbind(calendar.df, ca.df$nItemSold)
# Renaming the column
colnames(ca.df)[length(ca.df)] <- "nItemSold"
# Remove unnecessary columns
ca.df <- ca.df[, c(-1, -8, -9)]

ca.df$wday <- as.factor(ca.df$wday)
ca.df$snap_CA <- as.factor(ca.df$snap_CA)


# Split the dataset into training-set and test-set
training.set <- ca.df[1:1433,]
test.set <- ca.df[1434:length(ca.df$nItemSold),]


# Fit multiple linear regression to the training-set
regressor <- lm(formula = nItemSold ~ .,
                data = training.set)
# show summary
summary(regressor)

reduced.regressor <-step(regressor)
summary(reduced.regressor)

# Predicting the test-set
pred <- predict(reduced.regressor, newdata = test.set)

# preparing data for plotting
data.for.plotting <- data.frame("date" = seq(as.Date("2015/01/01"), as.Date("2016/04/24"), "day"))
data.for.plotting$actual <- ca.df[1434:1913, length(ca.df)]
data.for.plotting$predicted <- pred

# Show 10 row
head(data.for.plotting, 10)

ggplot(data.for.plotting, aes(data.for.plotting, x = date)) + 
  geom_line(aes(y = actual, colour = "Actual")) + 
  geom_line(aes(y = predicted, colour = "Predicted")) + 
  labs(title = "Actual VS Predicted VS date", x = "Date", y = "nItemSold")



## Accuracy measures: RMSE and MAPE
RMSE1 <- round(sqrt(sum(((data.for.plotting[,2]-data.for.plotting[,3])^2)/length(data.for.plotting[,2]))),4)
MAPE1 <- round(mean(abs(data.for.plotting[,2]-data.for.plotting[,3])/data.for.plotting[,2]),4)
paste("Accuracy Measures: RMSE:", RMSE1, "and MAPE:", MAPE1)

