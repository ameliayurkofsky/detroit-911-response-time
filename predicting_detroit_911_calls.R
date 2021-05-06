

##Package Check
##If a package is installed, load it; 
##If not, install package from CRAN and then load.
packages = c("readr", 
             "ggplot2",
             "caret",
             "RCurl",
             "knitr",
             "randomForest",
             "gbm",
             "dplyr")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)


#Import data set from the City of Detroit's Open Data Portal and take a quick peak at the variable types structure
download.file("https://www.arcgis.com/sharing/rest/content/items/db5bd090a156471ea0acf65bff327fd1/data", "cad_ago_lat_month.csv")
cad_ago_lat_month <- read_csv("cad_ago_lat_month.csv")
glimpse(cad_ago_lat_month)
poverty_ratios_2013 <- read_csv("~/911_calls/Income_to_Poverty_Ratios_in_Michigan_by_Zip_Code_Tabulation_Area,_2013.csv")
povratio <- poverty_ratios_2013[,c(3,14)] 

#subset to only include non-officer initated calls with non-missing response times
data <- subset(cad_ago_lat_month, cad_ago_lat_month$officerinitiated=="No" & 
               cad_ago_lat_month$priority<=2 & !is.na(cad_ago_lat_month$totalresponsetime) &  
                 !is.na(cad_ago_lat_month$zip_code) & cad_ago_lat_month$zip_code!=0)

#merge in poverty data by zipcode
data <- merge(data, povratio, by.x = "zip_code", by.y = "ZCTA5CE10", all.x = TRUE)
which(is.na(data$Pct_U100))
data <- data[-(13143),] #removing next 60 rows

#create time of day variable
data$call_hour <- as.numeric(substr(data$call_timestamp, 12, 13))

data$call_timeofday <- ifelse(data$call_hour>5 & data$call_hour<12, "Morning", 
                            ifelse(data$call_hour>11 & data$call_hour<17 , "Afternoon", 
                                   ifelse(data$call_hour>=17 & data$call_hour<=21, "Evening", "Night")))
table(data$call_hour, data$call_timeofday, useNA = "always")


#create new outcome variable
data$respond_atorbelowavg <- ifelse((data$totalresponsetime<=13 & data$priority==1) |
                                  (data$totalresponsetime<=33 & data$priority==2), "Y", "N")
table(data$respond_atorbelowavg , useNA = "always")


##Exploratory Data Analysis
str(data)
par(mfrow=c(1,2)) 
boxplot(data$totalresponsetime,data=data, main="Total Response Time in Minutes")
summary(data$totalresponsetime)

data %>%
  group_by(priority) %>%
  summarise(A_mean = mean(totalresponsetime))

#priority
ggplot(data = data, aes(x=as.factor(priority), y=totalresponsetime,
                        color=as.factor(priority))) +
  geom_boxplot() +
  xlab('Priority') +
  ylab('Response Time') +
  ggtitle('Priority vs Response TIme')  + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

#most common 911 calls descriptions by priority 
data %>% filter(priority==1) %>% count(calldescription) %>% arrange(desc(n))
data %>% filter(priority==2) %>% count(calldescription) %>% arrange(desc(n))

#zipcode
ggplot(data = data, aes(x=as.factor(zip_code), y=totalresponsetime,
                        color=as.factor(zip_code))) +
  geom_boxplot() +
  xlab('Zip Code') +
  ylab('Response Time') +
  ggtitle('Zip Code vs Response TIme')  + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#time of day
ggplot(data = data, aes(x=as.factor(call_timeofday), y=totalresponsetime,
                        color=as.factor(call_timeofday))) +
  geom_boxplot() +
  xlab('Time of Day') +
  ylab('Response Time') +
  ggtitle('Time of Day vs Response TIme')  + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")


#hour
ggplot(data = data, aes(x=as.factor(call_hour), y=totalresponsetime,
                        color=as.factor(call_hour))) +
  geom_boxplot() +
  xlab('Hour Day (0=12AM, 1=1AM, etc.)') +
  ylab('Response Time') +
  ggtitle('Hour of Day vs Response TIme')  + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")


#list of variables to drop
vars_remove <- names(data) %in% c("agency", "incident_id", "longitude", "latitude", "time_on_scene", 
                                  "incident_address", "block_id", "respondingunit",  
                                  "intaketime", "dispatchtime", "traveltime", "totaltime", "officerinitiated", 
                                  "totalresponsetime", "precinct_sca",
                                  "calldescription", "neighborhood", "council_district")

#drop unneeded variables
data2 <- data[!vars_remove]


##Training and Testing
## Split to training and testing subset 
total <- nrow(data2)
set.seed(123)
flag <- sort(sample(total*0.8,total*0.2, replace = FALSE))
datatrain <- data2[-flag,]
datatest <- data2[flag,]
dim(datatrain)
dim(datatest)
y1    <- datatrain$respond_atorbelowavg
y2    <- datatest$respond_atorbelowavg

#Random Forest Model
##random forest with the default parameters
rf1 <- randomForest(as.factor(datatrain$respond_atorbelowavg) ~ . , data= datatrain,  importance=TRUE)
importance(rf1) #importance 1=mean decrease in accuracy
importance(rf1, type=2) #importance 2= mean decrease in node impurity)
varImpPlot(rf1)
#Prediction on the training data set
rf.predtrain = predict(rf1, datatrain, type='class')
table(rf.predtrain, y1)
#testing error
rf1.testerror <- ifelse(rf.predtrain == y1, 0, 1)
sum(rf1.testerror) / length(y1)

#Prediction on the testing data set
rf.pred = predict(rf1, datatest, type='class')
table(rf.pred, y2)
#testing error
rf1.testerror <- ifelse(rf.pred == y2, 0, 1)
sum(rf1.testerror) / length(y2)

#Boosting Model 
##boosting model with default parameters
datatrain$zip_code <- as.factor(datatrain$zip_code)
datatrain$priority <- as.factor(datatrain$priority)
datatrain$callcode <- as.factor(datatrain$callcode)
datatrain$category <- as.factor(datatrain$category)
datatrain$call_timeofday <- as.factor(datatrain$call_timeofday)
datatrain$call_timestamp <- as.numeric(datatrain$call_timestamp)

datatrain$respond_atorbelowavg <- ifelse(datatrain$respond_atorbelowavg=="Y", 1, 0)

gbm1 <- gbm(respond_atorbelowavg ~ ., data = datatrain, distribution = "bernoulli")

# print results
print(gbm1)
summary(gbm1)

#Prediction 
datatest$zip_code <- as.factor(datatest$zip_code)
datatest$priority <- as.factor(datatest$priority)
datatest$callcode <- as.factor(datatest$callcode)
datatest$category <- as.factor(datatest$category)
datatest$call_timeofday <- as.factor(datatest$call_timeofday)
datatest$call_timestamp <- as.numeric(datatest$call_timestamp)
#training
gbm.pred1 = predict(gbm1, newdata = datatrain, type = "response")
gbm.pred1 <- ifelse(gbm.pred1 >=0.5, "Y","N")
table(gbm.pred1, y1)
#testing error
gbm.pred1_error <- ifelse(gbm.pred1 == y1, 0, 1)
sum(gbm.pred1_error) / length(y1)

#test set
gbm.pred = predict(gbm1, newdata = datatest, type = "response")
gbm.pred2 <- ifelse(gbm.pred >=0.5, "Y","N")
table(gbm.pred2, y2)
#testing error
gbm.pred2_error <- ifelse(gbm.pred2 == y2, 0, 1)
sum(gbm.pred2_error) / length(y2)


#TUNING
best.iter <- gbm.perf(gbm1, method = "OOB")
print(best.iter)
best.iter <- gbm.perf(gbm1, method = "test")
print(best.iter)

#random forest: tuning parameters with repeated cross-validation
mtry <- sqrt(ncol(x))
#ntree: Number of trees to grow.
ntree <- 3
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=1,
                        search = 'random')

#Random generate 15 mtry values with tuneLength = 15
set.seed(1)
rf2 <- train(as.factor(datatrain$respond_atorbelowavg) ~ .,
                   data = datatrain,
                   method = 'rf',
                   metric = 'Accuracy',
                   tuneLength  = 15, 
                   trControl = control)
print(rf_random)


