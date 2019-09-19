#==============================================================================================
#                                Importing Required Libraries
#==============================================================================================

library(pscl)
library(InformationValue)
library(fmsb)
library(woeBinning)
library(car)

#==============================================================================================
#                   Importing Data (with text as strings and not factors)
#==============================================================================================

adult_try_logistic <- read.csv("adult.csv",stringsAsFactors = FALSE)
str(adult_try_logistic)

#==============================================================================================
#                  Replacing "?" with NA and Encoding Dependent Variable
#                        1 = Preferred Event | 0 = Alternate Event
#==============================================================================================

adult_try_logistic[adult_try_logistic == "?"] <- NA
adult_try_logistic$income[adult_try_logistic$income == ">50K"] <- 1
adult_try_logistic$income[adult_try_logistic$income == "<=50K"] <- 0

#==============================================================================================
#                            Converting text to lower case
#==============================================================================================

adult_try_logistic$workclass <- tolower(adult_try_logistic$workclass)
adult_try_logistic$occupation <- tolower(adult_try_logistic$occupation)
adult_try_logistic$education <- tolower(adult_try_logistic$education)
adult_try_logistic$race <- tolower(adult_try_logistic$race)
adult_try_logistic$sex<- tolower(adult_try_logistic$sex)
adult_try_logistic$native.country <- tolower(adult_try_logistic$native.country)
adult_try_logistic$marital.status <- tolower(adult_try_logistic$marital.status)
adult_try_logistic$relationship <- tolower(adult_try_logistic$relationship)

#==============================================================================================
#                       Checking for % of Missing Values
# Decide whether to impute or not. If yes, with or without category wise median / mean / mode
#                        If not, encode Missing Values
#==============================================================================================

colMeans(is.na(adult_try_logistic)) # gives col wise %age
str(adult_try_logistic)

#==============================================================================================
#                                 Check for Imbalanced Data
#==============================================================================================
table(adult_try_logistic$income)

#==============================================================================================
#                                 Missing Value Replacement
#==============================================================================================

adult_try_logistic$workclass[is.na(adult_try_logistic$workclass)]<- "missing_workclass"
adult_try_logistic$occupation[is.na(adult_try_logistic$occupation)]<- "missing_occupation"
adult_try_logistic$native.country[is.na(adult_try_logistic$native.country)]<- "missing_country"

#==============================================================================================
#                         Checking for remaining missing values
#==============================================================================================

sum(is.na(adult_try_logistic))

#==============================================================================================
#                          Converting back to Factor Variables
#==============================================================================================
adult_try_logistic$workclass <- as.factor(adult_try_logistic$workclass)
adult_try_logistic$occupation <- as.factor(adult_try_logistic$occupation)
adult_try_logistic$native.country <- as.factor(adult_try_logistic$native.country)
adult_try_logistic$race <- as.factor(adult_try_logistic$race)
adult_try_logistic$sex <- as.factor(adult_try_logistic$sex)
adult_try_logistic$relationship <- as.factor(adult_try_logistic$relationship)
adult_try_logistic$income <- as.factor(adult_try_logistic$income)
adult_try_logistic$education <- as.factor(adult_try_logistic$education)
adult_try_logistic$marital.status <- as.factor(adult_try_logistic$marital.status)
str(adult_try_logistic)

#==============================================================================================
#                              Check Information Value (IV)
#==============================================================================================

binning<-woe.binning(adult_try_logistic, 'income', adult_try_logistic)
binning_df = as.data.frame(binning[,c(1,3)])
binning_df$V3 = ""
for(k in c(1:nrow(binning_df))){
  if(binning_df$V2[k] < 0.02){
    binning_df$V3[k] = "Useless"
  }else if(binning_df$V2[k] >= 0.02 & binning_df$V2[k] < 0.1){
    binning_df$V3[k] = "Weak"
  }else if(binning_df$V2[k] >= 0.1 & binning_df$V2[k] < 0.3){
    binning_df$V3[k] = "Medium"
  }else if(binning_df$V2[k] >= 0.3 & binning_df$V2[k] < 0.5){
    binning_df$V3[k] = "Strong"
  }else if(binning_df$V2[k] >= 0.5){
    binning_df$V3[k] = "Suspicious"
  }
}
binning_df

#==============================================================================================
#                                    Thumb Rule for IV
#==============================================================================================
#Information Value	Predictive Power
#< 0.02	useless for prediction
#0.02 to 0.1	Weak predictor
#0.1 to 0.3	Medium predictor
#0.3 to 0.5	Strong predictor
#>0.5	Suspicious or too good to be true

#==============================================================================================
#                       Choosing Relevant Independent Variables
#==============================================================================================

adult_try_logistic <- adult_try_logistic[,-c(3,9,12,14)]
binning<-woe.binning(adult_try_logistic, 'income', adult_try_logistic)

#==============================================================================================
#                      Checking for Weight of Evidence and Plots
#     (to identify which levels to combine together for each Independent Variable)
#==============================================================================================

df.with.binned.vars.added <- woe.binning.deploy(adult_try_logistic, binning, add.woe.or.dum.var='woe')
woe.binning.plot(binning, multiple.plots=FALSE)

#==============================================================================================
#                        Reducing Levels in Factor Variables (<10)
#==============================================================================================

# DIY
adult_try_logistic$workclass<-as.character(adult_try_logistic$workclass)
adult_try_logistic$workclass[adult_try_logistic$workclass=="without-pay"| adult_try_logistic$workclass=="never-worked"]<-"unemployed"
adult_try_logistic$workclass[adult_try_logistic$workclass=="state-gov"| adult_try_logistic$workclass=="local-gov"|adult_try_logistic$workclass=="federal-gov"]<-"Gov"
adult_try_logistic$workclass<-as.factor(adult_try_logistic$workclass)
table(adult_try_logistic$workclass)
adult_try_logistic$education<-as.character(adult_try_logistic$education)
adult_try_logistic$education[adult_try_logistic$education=="10th"| adult_try_logistic$education=="11th"|adult_try_logistic$education=="12th"|adult_try_logistic$education=="1st-4th"|adult_try_logistic$education=="5th-6th"|adult_try_logistic$education=="7th-8th"|adult_try_logistic$education=="9th"|adult_try_logistic$education=="preschool"|adult_try_logistic$education=="prof-school"]<-"school"
adult_try_logistic$education[adult_try_logistic$education=="some-college"| adult_try_logistic$education=="bachelors"]<-"Bachelors"
adult_try_logistic$education<-as.factor(adult_try_logistic$education)
table(adult_try_logistic$education)
adult_try_logistic$occupation<-as.character(adult_try_logistic$occupation)
adult_try_logistic$occupation[adult_try_logistic$occupation=="machine-op-inspct"| adult_try_logistic$occupation=="adm-clerical"| adult_try_logistic$occupation=="craft-repair"| adult_try_logistic$occupation=="sales"| adult_try_logistic$occupation=="transport-moving"]<-"work"
adult_try_logistic$occupation<-as.factor(adult_try_logistic$occupation)
table(adult_try_logistic$occupation)
#==============================================================================================
#                           Create Training Data (Bootstrapping)
#==============================================================================================
input_ones <- adult_try_logistic[which(adult_try_logistic$income == "1"), ]  # all 1's code (encoding) of whichever level is lower in frequency
input_zeroes <- adult_try_logistic[which(adult_try_logistic$income == "0"), ]  # all 0's
set.seed(100)  # for repeatability of samples
input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_ones))  # 1's for training
input_zeroes_training_rows <- sample(1:nrow(input_zeroes), 0.7*nrow(input_ones))  # 0's for training. Pick as many 1's as 0's
training_ones <- input_ones[input_ones_training_rows, ]  
training_zeroes <- input_zeroes[input_zeroes_training_rows, ]
trainingData <- rbind(training_ones, training_zeroes)  # row bind the 1's and 0's 

#==============================================================================================
#                                     Create Test Data
#==============================================================================================
test_ones <- input_ones[-input_ones_training_rows, ]
test_zeroes <- input_zeroes[-input_zeroes_training_rows, ]
testData <- rbind(test_ones, test_zeroes)  # row bind the 1's and 0's

#==============================================================================================
#                             Modeling (Tuning based on p-values)
#                        Number of Fisher Iterations should be less
#==============================================================================================

logitMod <- glm(income ~ age + workclass + education + occupation + hours.per.week, data=trainingData, family=binomial(link="logit"))
summary(logitMod)
pR2(logitMod)

#==============================================================================================
#                                    Testing (Prediction)
#==============================================================================================

predicted <- predict(logitMod, testData, type="response")

#==============================================================================================
#                             Check if system removes unused levels
#==============================================================================================

usedlevels = levels(logitMod$model$workclass)
unusedlevels = levels(trainingData$workclass)[!levels(trainingData$workclass) %in% usedlevels]
unusedlevels

#==============================================================================================
#         If system removes unused levels, create a new updated test data (Optional)
#==============================================================================================

test_updated = testData[testData$workclass %in% usedlevels,]

#==============================================================================================
#                             Testing (Prediction) (Optional)
#==============================================================================================

predicted <- predict(logitMod, test_updated, type="response")

#==============================================================================================
#                               Deciding Optimal Cut-off
#==============================================================================================

optCutOff <- optimalCutoff(testData$income, predicted)[1]
optCutOff

#==============================================================================================
#                                    Checking VIF (<2)
#==============================================================================================

VIF(logitMod)

#==============================================================================================
#                           Checking Multicollinearity (Optional)
#==============================================================================================

# If aliased coefficients
alias(logitMod)

#==============================================================================================
#                               Checking Accuracy of the Model
#==============================================================================================

fitted.results <- predict(logitMod,testData[,-12],type='response')
fitted.results <- ifelse(fitted.results > optCutOff,1,0)
misClasificError <- mean(fitted.results != testData$income)
print(paste('Accuracy',1-misClasificError))

#==============================================================================================
#                              Area Under Curve (AUC) (>80%)
#==============================================================================================

plotROC(testData$income, predicted)

#==============================================================================================
#                                        Concordance
#==============================================================================================

Concordance(testData$income, predicted)

#==============================================================================================
#                                        Sensitivity
#==============================================================================================

sensitivity(testData$income, predicted, threshold = optCutOff)

#==============================================================================================
#                                        Specificity
#==============================================================================================

specificity(testData$income, predicted, threshold = optCutOff)

#==============================================================================================
#                                     Confusion Matrix
#==============================================================================================

confusionMatrix(testData$income, predicted, threshold = optCutOff)

#==============================================================================================
#                                    Ensemble (Optional)
#==============================================================================================
