library(plumber)

load('AdultWorkspace.RData')

#* @param age:int values between 1 to 100
#* @param workclass:character values
#* @param education:character values
#* @param occupation:character value
#* @param hours.per.week:int value between 1 to 1000

#* @post /logistic



function(age , workclass , education , occupation , hours.per.week)
{
  df = setNames(data.frame(matrix(ncol = 5, nrow = 1)), colnames(trainingData[,c(1:5)]))
  #df <- trainingData("ApplicantIncome"=ApplicantIncome, "LoanAmount"=LoanAmount,"Credit_History"=Credit_History)
  df$age = as.integer(df$age)
  df$workclass = as.character(df$workclass)
  df$education = as.character(df$education)
  df$occupation = as.character(df$occupation)
  df$hours.per.week = as.integer(df$hours.per.week)
  #print(str(df))
  return(predict(logitMod,newdata=df,type='response'))
}

