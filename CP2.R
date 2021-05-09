# Loading Libraries
rm(list=ls())
suppressMessages(library(data.table))
suppressMessages(library(DT))
suppressMessages(library(timeSeries))
suppressMessages(library(tidyverse))
suppressMessages(library(reshape))
suppressMessages(library(stringr))
suppressMessages(library(doBy))
suppressMessages(library(formattable))
suppressMessages(library(gridExtra))
suppressMessages(library(dplyr))

suppressMessages(library(ggplot2))
suppressMessages(library(directlabels))
suppressMessages(library(plotly))
suppressMessages(library(corrplot))
suppressMessages(library(wesanderson))
suppressMessages(library(RColorBrewer))
suppressMessages(library(gridExtra))
suppressMessages(library(zoo))

suppressMessages(library(forecast)) ### For ARIMA
suppressMessages(library(prophet))  ### For Prophet Forecasting
suppressMessages(library(nnfor))    ### For Neural Network Forecasting
suppressMessages(library(e1071))    ### For SVM
suppressMessages(library(caret))
set.seed(2020)

# Load Data , Structure & Summary of the Data
train=fread("C:/Users/tehya/OneDrive/Documents/R/train.csv")
sprintf("The train data set has %d rows and %d columns", nrow(train), ncol(train) )
str(train)

print("the summary of train sales is:")
summary(train$sales)

# Extraction of Year and Month of Year :
train$Year=year(train$date)        #returns the year from date i.e. 2013, 2014 etc.
train$Month=as.yearmon(train$date) #this yearmon() function is coming from zoo package returns the month of an year i.e Jan 2013, Feb 2015 etc

# Missing Values Detection
colSums(is.na(train))
# Function 1 : For plotting missing value
plot_missing <- function(data, title = NULL, ggtheme = theme_gray(), theme_config = list("legend.position" = c("bottom"))) {
  ## Declare variable first to pass R CMD check
  feature <- num_missing <- pct_missing <- group <- NULL
  ## Check if input is data.table
  is_data_table <- is.data.table(data)
  ## Detect input data class
  data_class <- class(data)
  ## Set data to data.table
  if (!is_data_table) data <- data.table(data)
  ## Extract missing value distribution
  missing_value <- data.table(
    "feature" = names(data),
    "num_missing" = sapply(data, function(x) {sum(is.na(x))})
  )
  missing_value[, feature := factor(feature, levels = feature[order(-rank(num_missing))])]
  missing_value[, pct_missing := num_missing / nrow(data)]
  missing_value[pct_missing < 0.05, group := "Good"]
  missing_value[pct_missing >= 0.05 & pct_missing < 0.4, group := "OK"]
  missing_value[pct_missing >= 0.4 & pct_missing < 0.8, group := "Bad"]
  missing_value[pct_missing >= 0.8, group := "Remove"][]
  ## Set data class back to original
  if (!is_data_table) class(missing_value) <- data_class
  ## Create ggplot object
  output <- ggplot(missing_value, aes_string(x = "feature", y = "num_missing", fill = "group")) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(round(100 * pct_missing, 2), "%"))) +
    scale_fill_manual("Group", values = c("Good" = "#1a9641", 
                                          "OK" = "#a6d96a", 
                                          "Bad" = "#fdae61", "Remove" = "#d7191c"), 
                                           breaks = c("Good", "OK", "Bad", "Remove")) +
    scale_y_continuous(labels = comma) +
    coord_flip() +
    xlab("Features") + ylab("Number of missing rows") +
    ggtitle(title) +
    ggtheme + theme_linedraw()+
    do.call(theme, theme_config)
  ## Print plot
  print(output)
  ## Set return object
  return(invisible(missing_value))
}
plot_missing(train) 

# Feature Visualizations 
# Histogram of Sales Price
gbp1<-wes_palette("GrandBudapest2")[1]

ggplot(train, aes(x=sales))+
  geom_histogram(fill="#a6d96a", alpha=.9)+
  labs(x=NULL, y=NULL, title = "Histogram of Sales")+
  theme_minimal() + theme(plot.title=element_text(vjust=3, size=15) )

# Growth by Date
MSP <- aggregate(sales ~date, train, mean)

sl1 <-ggplot(MSP, aes(x=as.factor(date), y=sales))+
  geom_line(color=gbp1, aes(group=1), size=1.5)+
  geom_point(colour=gbp1, size = 3.5, alpha=0.5)+
  labs(title="The Growth of Sales by Date", x=NULL, y="sales")+
  theme( plot.title=element_text(vjust=3, size=15) ) + theme_minimal()

MSP$rate = c(0, 100*diff(MSP$sales)/MSP[-nrow(MSP),]$sales)

sl2 <-ggplot(MSP, aes(x=as.factor(date), y=rate))+
  geom_line(color= "gray50", aes(group=1), size=1)+
  labs(title="Change Rate of Sales", x="date", y="rate of change")+
  geom_hline(yintercept = 0, color = gbp1 )+
  theme(plot.title=element_text(size=15))+ theme_minimal()

grid.arrange(sl1,sl2) 

# Growth by Month in Different Years
MSP <- aggregate(sales ~Month, train, mean)

sl1 <-ggplot(MSP, aes(x=as.factor(Month), y=sales))+
  geom_line(color=gbp1, aes(group=1), size=1.5)+
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
  geom_point(colour=gbp1, size = 3.5, alpha=0.5)+
  labs(title="The Growth of Sales by Month", x=NULL, y="sales")+
  theme(plot.title=element_text(vjust=3, size=15)) + theme_minimal()

MSP$rate = c(0, 100*diff(MSP$sales)/MSP[-nrow(MSP),]$sales)

sl2 <-ggplot(MSP, aes(x=as.factor(Month), y=rate))+
  geom_line(color= "gray50", aes(group=1), size=1)+
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
  labs(title="Change Rate of Sales", x="Month", y="rate of change")+
  geom_hline(yintercept = 0, color = gbp1 )+
  theme(plot.title=element_text(size=15)) + theme_minimal()

grid.arrange(sl1,sl2) 

# Growth by Year
MSP <- aggregate(sales ~Year, train, mean)

sl1 <-ggplot(MSP, aes(x=as.factor(Year), y=sales))+
  geom_line(color=gbp1, aes(group=1), size=1.5)+
  geom_point(colour=gbp1, size = 3.5, alpha=0.5)+
  labs(title="The Growth of Sales by Year", x=NULL, y="Sales")+
  theme( plot.title=element_text(vjust=3, size=15) ) + theme_minimal()

MSP$rate = c(0, 100*diff(MSP$sales)/MSP[-nrow(MSP),]$sales)

sl2 <-ggplot(MSP, aes(x=as.factor(Year), y=rate))+
  geom_line(color= "gray50", aes(group=1), size=1)+
  labs(title="Change Rate of Sales", x="Year", y="rate of change")+
  geom_hline(yintercept = 0, color = gbp1 )+
  theme(plot.title=element_text(size=15))+ theme_minimal()

grid.arrange(sl1,sl2)

# Yearly Growth by Store
unique(train$store)
Year_state<-aggregate(sales ~store+Year, train, mean)
pal<-rep(brewer.pal(10, "BrBG"),5)

ggplot(Year_state, aes(group = store ))+
  geom_line(aes(x=Year,y=sales, color=factor(store)), alpha=0.5, show.legend=F)+
  labs(title="The Growth of Sales by Store from 2013 - 2017", x=NULL
  )+ 
  theme(panel.background=element_rect(fill = "White"),
        plot.title=element_text(vjust=3, size=15),
        panel.grid.major=element_line(color = pal))+
  geom_dl(aes(x=Year,y=sales,label = store), method = list(dl.combine("first.qp", "last.qp")))

# Yearly Growth by Item
unique(train$item)
Year_state<-aggregate(sales ~item+Year, train, mean)
pal<-rep(brewer.pal(10, "BrBG"),5)

ggplot(Year_state, aes(group = item ))+
  geom_line(aes(x=Year,y=sales,color=factor(item)), alpha=0.5, show.legend=F)+
  labs(title="The Growth of Sales by Item from 2013 - 2017", x=NULL
  )+
  theme(panel.background=element_rect(fill = "White"),
        plot.title=element_text(vjust=3, size=15),
        panel.grid.major=element_line(color = pal))+
  geom_dl(aes(x=Year,y=sales,label = item), method = list(dl.combine("last.qp")))

# PROPHET Baseline Model
train = fread("C:/Users/tehya/OneDrive/Documents/R/train.csv")
training = train[date %between% c("2013-01-01", "2016-12-31")]
sprintf("The training set has %d rows and %d columns", nrow(training), ncol(training))
str(training)

train_final_store_item=subset(training)
stats = data.frame(y = log1p(train_final_store_item$sales), 
                  ds = train_final_store_item$date)
stats = aggregate(stats$y, by = list(stats$ds), FUN = sum) 
head(stats)
colnames(stats) = c("ds", "y") 

model_prophet = prophet(stats)
summary(model_prophet)
future = make_future_dataframe(model_prophet, periods = 365)
forecast = predict(model_prophet, future)
predicted = data.frame(forecast[1462:1826, 19])
colnames(predicted) = c('predicted')

# Changepoints & Model Components Visualization 
add_changepoints_to_plot <- function(m, threshold = 0.01, cp_color = "red",
                                     cp_linetype = "dashed", trend = TRUE, ...) {
  layers <- list()
  if (trend) {
    trend_layer <- ggplot2::geom_line(
      ggplot2::aes_string("ds", "trend"), color = cp_color, ...)
    layers <- append(layers, trend_layer)
  }
  signif_changepoints <- m$changepoints[abs(m$params$delta) >= threshold]
  cp_layer <- ggplot2::geom_vline(
    xintercept = as.integer(signif_changepoints), color = cp_color,
    linetype = cp_linetype, ...)
  layers <- append(layers, cp_layer)
  return(layers)
}
plot(model_prophet, forecast) + add_changepoints_to_plot(model_prophet)

# Inspecting Model Components
prophet_plot_components(model_prophet, forecast)

# Prophet Baseline Model Evaluation 
testing = train[date %between% c("2017-01-01", "2017-12-31")]
sprintf("The test data set has %d rows and %d columns", nrow(testing), ncol(testing))
str(testing)

test_final_store_item = subset(testing)
stats1 = data.frame(y = log1p(test_final_store_item$sales), 
                    ds = test_final_store_item$date)
stats1 = aggregate(stats1$y, by = list(stats1$ds), FUN = sum)
head(stats1)
actual = data.frame(stats1[ , 2])
colnames(actual) = c('actual') 
head(actual)

# SMAPE CALCULATION
smape_cal = function(outsample, forecasts){
  outsample = as.numeric(outsample)
  forecasts = as.numeric(forecasts)
  smape = (abs(outsample - forecasts)) / ((abs(outsample) + abs(forecasts)) / 2)
  return(smape)
}
SMAPE_ERR = smape_cal(outsample = as.numeric(unlist(actual)), 
                      forecasts = as.numeric(unlist(predicted)))
SMAPE = mean(SMAPE_ERR, na.rm = T)
sprintf("The value of SMAPE for Baseline Prophet is %f ", round(SMAPE, digits = 4 ))
prophetb_evl = postResample(predicted, actual)
print(round(prophetb_evl, digits = 4)) 

################################################################################

# PROPHET with Customized Holidays and Events 
train = fread("C:/Users/tehya/OneDrive/Documents/R/train.csv")
training = train[date %between% c("2013-01-01", "2016-12-31")]
sprintf("The training set has %d rows and %d columns", nrow(training), ncol(training))
str(training)

train_final_store_item = subset(training)
stats = data.frame(y = log1p(train_final_store_item$sales), 
                   ds = train_final_store_item$date)
stats = aggregate(stats$y, by = list(stats$ds), FUN = sum)
head(stats)
colnames(stats) <- c("ds","y")

playoffs <- data.frame(
  holiday = 'playoff',
  ds = as.Date(c('2013-01-12', '2014-01-12', '2014-01-19',
                 '2014-02-02', '2015-01-11', '2016-01-17',
                 '2016-01-24', '2016-02-07')),
  lower_window = 0,
  upper_window = 1
) 

Xmas <- data.frame(
  holiday = 'Xmas',
  ds = as.Date(c('2013-01-01', '2013-12-25', '2014-01-01', '2014-12-25',
                 '2015-01-01', '2015-12-25','2016-01-01', '2016-12-25')),
  lower_window = 0,
  upper_window = 1
)

holidays <- bind_rows(playoffs, Xmas)

# Including Additional Regressor e.g. NFL Sundays 
nfl_sunday <- function(ds) {
  dates <- as.Date(ds)
  month <- as.numeric(format(dates, '%m'))
  as.numeric((weekdays(dates) == "Sunday") & (month > 8 | month < 2))
}
stats$nfl_sunday <- nfl_sunday(stats$ds)

model_prophet <- prophet()
model_prophet <- add_regressor(model_prophet, 'nfl_sunday')
model_prophet <- add_seasonality(model_prophet, name = 'daily', period = 60, fourier.order = 5)
model_prophet <- prophet(stats, holidays = holidays, holidays.prior.scale = 0.5, 
                         yearly.seasonality = 4, interval.width = 0.95, 
                         changepoint.prior.scale = 0.006, daily.seasonality = T) 
summary(model_prophet) 
future = make_future_dataframe(model_prophet, periods = 365, freq = 'days') 
forecast = predict(model_prophet, future)
predicted = data.frame(forecast[1462:1826, 31]) 
colnames(predicted) = c('predicted') 

# Changepoints & Model Components Visualization 
add_changepoints_to_plot <- function(m, threshold = 0.01, cp_color = "red",
                                     cp_linetype = "dashed", trend = TRUE, ...) {
  layers <- list()
  if (trend) {
    trend_layer <- ggplot2::geom_line(
      ggplot2::aes_string("ds", "trend"), color = cp_color, ...)
    layers <- append(layers, trend_layer)
  }
  signif_changepoints <- m$changepoints[abs(m$params$delta) >= threshold]
  cp_layer <- ggplot2::geom_vline(
    xintercept = as.integer(signif_changepoints), color = cp_color,
    linetype = cp_linetype, ...)
  layers <- append(layers, cp_layer)
  return(layers)
}
plot(model_prophet, forecast) + add_changepoints_to_plot(model_prophet)

# Inspecting Model Components
prophet_plot_components(model_prophet, forecast)

# Enhanced Prophet Model Evaluation 
testing = train[date %between% c("2017-01-01", "2017-12-31")]
sprintf("The test data set has %d rows and %d columns", nrow(testing), ncol(testing))
str(testing)

test_final_store_item = subset(testing)
stats1 = data.frame(y = log1p(test_final_store_item$sales), 
                    ds = test_final_store_item$date)
stats1 = aggregate(stats1$y, by = list(stats1$ds), FUN = sum)
head(stats1)
actual = data.frame(stats1[ , 2])
colnames(actual) = c('actual') 
head(actual)

# SMAPE CALCULATION
smape_cal = function(outsample, forecasts){
  outsample = as.numeric(outsample)
  forecasts = as.numeric(forecasts)
  smape = (abs(outsample - forecasts)) / ((abs(outsample) + abs(forecasts)) / 2)
  return(smape)
}
SMAPE_ERR = smape_cal(outsample = as.numeric(unlist(actual)), 
                      forecasts = as.numeric(unlist(predicted)))
SMAPE = mean(SMAPE_ERR, na.rm = T)
sprintf("The SMAPE for Enhanced Prophet is %f ", round(SMAPE, digits = 4))  
prophet_evl = postResample(predicted, actual)
print(round(prophet_evl, digits = 4)) 

################################################################################

# NN MLP Model
train = fread("C:/Users/tehya/OneDrive/Documents/R/train.csv")
training = train[date %between% c("2013-01-01", "2016-12-31")]
sprintf("The training set has %d rows and %d columns", nrow(training), ncol(training))
str(training)

train_final_store_item = subset(training)
stats = data.frame(y = log1p(train_final_store_item$sales), 
                   ds = train_final_store_item$date)
stats = aggregate(stats$y, by = list(stats$ds), FUN = sum)
head(stats)
colnames(stats) <- c("ds", "y") 

y <- ts(stats$y, start = c(2013, 1, 1), frequency = 365)
head(y)
plot(y)

# MLP NN Model with Auto Hidden Nodes
fit1 <- mlp(y, hd.auto.type = "valid", hd.max = 8) 
print(fit1)
plot(fit1)
frc <- forecast(fit1, h = 365) 
plot(frc)
predicted = data.frame(frc$mean)
colnames(predicted) = c('predicted')

# Evaluation of NN Model
testing = train[date %between% c("2017-01-01", "2017-12-31")]
sprintf("The test data set has %d rows and %d columns", nrow(testing), ncol(testing))
str(testing)

test_final_store_item = subset(testing)
stats1 = data.frame(y = log1p(test_final_store_item$sales), 
                   ds = test_final_store_item$date)
stats1 = aggregate(stats1$y, by = list(stats1$ds), FUN = sum) 
head(stats1)
actual = data.frame(stats1[,2])
colnames(actual) = c('actual') 
head(actual)

smape_cal = function(outsample, forecasts){
  outsample = as.numeric(outsample)
  forecasts = as.numeric(forecasts)
  smape = (abs(outsample - forecasts)) / ((abs(outsample) + abs(forecasts)) / 2)
  return(smape)
}
SMAPE_ERR = smape_cal(outsample = as.numeric(unlist(actual)), 
                      forecasts = as.numeric(unlist(predicted)))
SMAPE = mean(SMAPE_ERR, na.rm = T)
sprintf("The SMAPE for NN Model is %f ", round(SMAPE, digits = 4))
nn_evl = postResample(predicted, actual)  
print(round(nn_evl, digits = 4))

################################################################################

# SVM REGRESSION MODEL
train = fread("C:/Users/tehya/OneDrive/Documents/R/train.csv")
training = train[date %between% c("2013-01-01", "2016-12-31")]
sprintf("The training set has %d rows and %d columns", nrow(training), ncol(training))
str(training)

train_final_store_item = subset(training)
stats = data.frame(y = log1p(train_final_store_item$sales), 
                  ds = train_final_store_item$date)
stats = aggregate(stats$y, by = list(stats$ds), FUN = sum)
colnames(stats) <- c("ds","y")
head(stats)

daily_data <- stats$y
days <- 1:1461
DF <- data.frame(days, daily_data)
colnames(DF) <- c("x", "y")

svm_tune <- tune(svm, train.x = DF$x, train.y = DF$y, kernel = "radial", 
                 ranges = list(cost = 10^(-2:2), gamma = 2^(-2:2)))

svmodel <- svm(y ~ x, data = DF, type = "eps-regression",
               kernel = "radial", cost = 100, gamma = 4)

nd = 1:1826

prognoza <- predict(svmodel, newdata = data.frame(x = nd))

# SVM Forecast Plot
ylim <- c(min(DF$y), max(DF$y))
xlim <- c(min(nd), max(nd))
plot(DF$y, col = "blue", ylim = ylim, xlim = xlim, type = 'l')
par(new = TRUE)
plot(prognoza, col = "red", ylim = ylim, xlim = xlim)
full_predicted = data.frame(prognoza)
predicted = data.frame(full_predicted[1462:1826, 1])
colnames(predicted) = c('predicted')

# SVM Model Evaluation
testing = train[date %between% c("2017-01-01", "2017-12-31")]
sprintf("The test data set has %d rows and %d columns", nrow(testing), ncol(testing))
str(testing)

test_final_store_item = subset(testing)
stats1 = data.frame(y = log1p(test_final_store_item$sales), 
                   ds = test_final_store_item$date)
stats1 = aggregate(stats1$y, by = list(stats1$ds), FUN = sum)
head(stats1)
actual = data.frame(stats1[,2])
colnames(actual) = c('actual') 
head(actual)

# SMAPE CALCULATION
smape_cal <- function(outsample, forecasts){
  outsample <- as.numeric(outsample)
  forecasts <- as.numeric(forecasts)
  smape <- (abs(outsample - forecasts)) / ((abs(outsample) + abs(forecasts)) / 2)
  return(smape)
}
SMAPE_ERR <- smape_cal(outsample = as.numeric(unlist(actual)), 
                       forecasts = as.numeric(unlist(predicted)))
SMAPE <- mean(SMAPE_ERR, na.rm = T)
sprintf("The SMAPE for SVM model is %f ", round(SMAPE, digits = 4))
svm_evl = postResample(predicted, actual)
print(round(svm_evl, digits = 4)) 

################################################################################

# Auto-ARIMA Model
library(parallel) # To enhance computation speed as R uses single thread only.

# Reading the train input file
train = fread("C:/Users/tehya/OneDrive/Documents/R/train.csv")
training = train[date %between% c("2013-01-01", "2016-12-31")]
train_final_store_item = subset(training) 
df = data.frame(y = log1p(train_final_store_item$sales), 
               ds = train_final_store_item$date)
df = aggregate(df$y, by = list(df$ds), FUN = sum)
colnames(df) <- c("ds", "y") 

# Converting datetime column to datetime format
df[,1] = as.POSIXct(df[,1])

# Initializing available cores form the system  
cl = makeCluster(getOption('cl.cores', 7))

# Splitting data in list
l_store_item = list(df) 

# Storing models in list that is applied on input data
l_models = clusterMap(cl, function(x){library(forecast);auto.arima(x$y)}, l_store_item)

# Applying forecast method to predict next 365 days. 
l_forecast = clusterMap(cl, function(x){library(forecast);
             as.data.frame(forecast(x, 365))}, l_models)

# Appending all results to get back test size
result = do.call('rbind', l_forecast)
predicted = data.frame(result[,1]) 
colnames(predicted) = c('predicted') 

# Auto-ARIMA Model Evaluation 
testing = train[date %between% c("2017-01-01", "2017-12-31")]
sprintf("The test data set has %d rows and %d columns", nrow(testing), ncol(testing))
str(testing)

test_final_store_item = subset(testing)
stats1 = data.frame(y = log1p(test_final_store_item$sales), 
                   ds = test_final_store_item$date)
stats1 = aggregate(stats1$y, by = list(stats1$ds), FUN = sum)
head(stats1)
actual = data.frame(stats1[,2])
colnames(actual) = c('actual') 
head(actual)

# SMAPE CALCULATION
smape_cal <- function(outsample, forecasts){
  outsample <- as.numeric(outsample)
  forecasts <- as.numeric(forecasts)
  smape <- (abs(outsample - forecasts)) / ((abs(outsample) + abs(forecasts)) / 2)
  return(smape)
}
SMAPE_ERR <- smape_cal(outsample = as.numeric(unlist(actual)), 
                       forecasts = as.numeric(unlist(predicted)))
SMAPE <- mean(SMAPE_ERR, na.rm = T)
sprintf("The SMAPE for Auto-ARIMA model is %f ", round(SMAPE, digits = 4))
arima_evl = postResample(predicted, actual)
print(round(arima_evl, digits = 4)) 