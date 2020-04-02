getwd()
setwd("/Users/harshitmehta/Desktop/ISBF_course/Machine_Learning/UoL_assignment")

# loading all the libraries

library(dplyr)  
library(ggplot2)
library(lattice)
library(glmnet)
library(ROSE)

############################ PART 2 ###############################################

#R code to import and prepare the student performance dataset
school1=read.table("student-mat.csv",sep=";",header=TRUE)
school2=read.table("student-por.csv",sep=";",header=TRUE)

################### Understanding the Data #######################################

table(school1$school)

head(school1)

colnames(school1)

summary(school1)

################## Data Cleaning & Preparation ######################################

# to check if there are any missing values
any(is.na(school1))
# Thus we have no missing values in the data set.

# As per the requirements of assignment, we have to predict G3
# without G1 and G2 so dropping them
# dropping G1 and G2 from school1 (math)
df_math = subset(school1, select = -c(G1,G2))
colnames(df_math)

glimpse(df_math)

# The following variables need to be converted to categorical type:

# Medu - denotes Mother's eductaion - 5 levels
df_math$Medu = factor(df_math$Medu, levels=c("0","1","2","3","4"), ordered=TRUE)
summary(df_math$Medu)

# Fedu - denotes Father's eductaion - 5 levels
df_math$Fedu = factor(df_math$Fedu, levels=c("0","1","2","3","4"), ordered=TRUE)
summary(df_math$Fedu)

# famrel - denotes - quality of family relationships 
# 1 - very bad to 5 - excellent 
df_math$famrel = factor(df_math$famrel, levels=1:5, ordered=TRUE)
summary(df_math$famrel)

# traveltime - denotes home to school travel time  
# 0 to 4
df_math$traveltime = factor(df_math$traveltime, levels=0:4, ordered=TRUE)
summary(df_math$traveltime)

# studytime - denotes weekly study time   
# 1 to 4
df_math$studytime = factor(df_math$studytime, levels=1:4, ordered=TRUE)
summary(df_math$studytime)

# freetime - free time after school (1 - very low to 5 - very high)
df_math$freetime = factor(df_math$freetime, levels=1:5, ordered=TRUE)
summary(df_math$freetime)

# goout - going out with friends ( 1 - very low to 5 - very high) 
df_math$goout = factor(df_math$goout, levels=1:5, ordered=TRUE)
summary(df_math$goout)

# Dalc - workday alcohol consumption (from 1 - very low to 5 - very high) 
df_math$Dalc = factor(df_math$Dalc, levels=1:5, ordered=TRUE)
summary(df_math$Dalc)

# Walc - weekend alcohol consumption ( 1 - very low to 5 - very high) 
df_math$Walc = factor(df_math$Walc, levels=1:5, ordered=TRUE)
summary(df_math$Walc)

# health - current health status ( 1 - very bad to 5 - very good) 
df_math$health = factor(df_math$health, levels=1:5, ordered=TRUE)
summary(df_math$health)

# failures - number of past class failures (n if 1<=n<3, else 4) 
df_math$failures = factor(df_math$failures, levels=0:4, ordered=TRUE)
summary(df_math$failures)

summary(df_math)

########################### Exploratory Data Analysis(EDA) ###########################

# Creating box-plots for categorical data
suppressMessages(attach(df_math))

plot(higher,G3, xlab = "Wants to take Higher education", ylab = "Grades", main = "Figure 2.1")
summary(df_math[df_math$higher=="yes",]$G3)
summary(df_math[df_math$higher=="no",]$G3) 

plot(Mjob,G3, xlab = "Mother's Job", ylab = "Grades", main = "Figure 2.2")
summary(df_math[df_math$Mjob=="at_home",]$G3)
summary(df_math[df_math$Mjob=="health",]$G3)
summary(df_math[df_math$Mjob=="other",]$G3)
summary(df_math[df_math$Mjob=="services",]$G3)
summary(df_math[df_math$Mjob=="teacher",]$G3)

plot(Fjob, G3, xlab = "Father's Job", ylab = "Grades", main = "Figure 2.3")
summary(df_math[df_math$Fjob=="at_home",]$G3)
summary(df_math[df_math$Fjob=="health",]$G3)
summary(df_math[df_math$Fjob=="other",]$G3)
summary(df_math[df_math$Fjob=="services",]$G3)
summary(df_math[df_math$Fjob=="teacher",]$G3)

plot(guardian,G3, xlab = "Student's guardian", ylab = "Grades", main = "Figure 2.4")
summary(df_math[df_math$guardian=="father",]$G3)
summary(df_math[df_math$guardian=="mother",]$G3)
summary(df_math[df_math$guardian=="other",]$G3)

plot(internet,G3, xlab = "Internet access at home", ylab = "Grades", main = "Figure 2.5")
summary(df_math[df_math$internet=="yes",]$G3)
summary(df_math[df_math$internet=="no",]$G3) 

plot(activities,G3, xlab = "Extra-curricular activities", ylab = "Grades", main = "Figure 2.6")
summary(df_math[df_math$activities=="yes",]$G3)
summary(df_math[df_math$activities=="no",]$G3) 

plot(school, G3, xlab = "Schools", ylab = "Grades", main = "Figure 2.7")
summary(df_math[df_math$school=="GP",]$G3)
summary(df_math[df_math$school=="MS",]$G3)

plot(reason,G3, xlab = "Reason to choose a school", ylab = "Grades", main = "Figure 2.8")
summary(df_math[df_math$reason=="course",]$G3)
summary(df_math[df_math$reason=="home",]$G3)
summary(df_math[df_math$reason=="other",]$G3)
summary(df_math[df_math$reason=="reputation",]$G3)

plot(schoolsup,G3, xlab = "Extra educational support", ylab = "Grades", main = "Figure 2.9")
summary(df_math[df_math$schoolsup=="yes",]$G3)
summary(df_math[df_math$schoolsup=="no",]$G3)

plot(paid, G3, xlab = "Extra paid classes", ylab = "Grades", main = "Figure 2.10")
summary(df_math[df_math$paid=="yes",]$G3)
summary(df_math[df_math$paid=="no",]$G3)

plot(sex,G3, xlab = "Gender", ylab = "Grades", main = "Figure 2.11")
summary(df_math[df_math$sex=="F",]$G3)
summary(df_math[df_math$sex=="M",]$G3)

plot(address,G3, xlab = "Address", ylab = "Grades", main = "Figure 2.12")
summary(df_math[df_math$address=="U",]$G3)
summary(df_math[df_math$address=="R",]$G3)

plot(famsize, G3, xlab = "Family Size", ylab = "Grades", main = "Figure 2.13")
summary(df_math[df_math$famsize=="GT3",]$G3)
summary(df_math[df_math$famsize=="LE3",]$G3)

plot(romantic,G3, xlab = "Romantic relationship", ylab = "Grades", main = "Figure 2.14" )
summary(df_math[df_math$romantic=="yes",]$G3)
summary(df_math[df_math$romantic=="no",]$G3)

plot(Pstatus,G3, xlab = "Parent's cohabitation status", ylab = "Grades", main = "Figure 2.15")
summary(df_math[df_math$Pstatus=="A",]$G3)
summary(df_math[df_math$Pstatus=="T",]$G3)

plot(famsup,G3, xlab = "Family educational support", ylab = "Grades", main = "Figure 2.16")
summary(df_math[df_math$famsup=="yes",]$G3)
summary(df_math[df_math$famsup=="no",]$G3)

plot(nursery,G3, xlab = "Attended nursery school", ylab = "Grades", main = "Figure 2.17")
summary(df_math[df_math$activities=="yes",]$G3)
summary(df_math[df_math$activities=="no",]$G3) 

plot(traveltime,G3, xlab = "Traveltime", ylab = "Grades", main = "Figure 2.18")
summary(df_math[df_math$traveltime=="1",]$G3)
summary(df_math[df_math$traveltime=="2",]$G3) 
summary(df_math[df_math$traveltime=="3",]$G3)
summary(df_math[df_math$traveltime=="4",]$G3)

plot(studytime,G3, xlab = "Study time", ylab = "Grades", main = "Figure 2.19")
summary(df_math[df_math$studytime=="1",]$G3)
summary(df_math[df_math$studytime=="2",]$G3) 
summary(df_math[df_math$studytime=="3",]$G3)
summary(df_math[df_math$studytime=="4",]$G3)

plot(failures,G3, xlab = "Number of past Failures", ylab = "Grades", main = "Figure 2.20")
summary(df_math[df_math$failures=="0",]$G3)
summary(df_math[df_math$failures=="1",]$G3) 
summary(df_math[df_math$failures=="2",]$G3)
summary(df_math[df_math$failures=="3",]$G3)

plot(famrel,G3, xlab = "Quality of family relationships", ylab = "Grades", main = "Figure 2.21")
summary(df_math[df_math$famrel=="1",]$G3) 
summary(df_math[df_math$famrel=="2",]$G3)
summary(df_math[df_math$famrel=="3",]$G3)
summary(df_math[df_math$famrel=="4",]$G3)
summary(df_math[df_math$famrel=="5",]$G3)

plot(freetime,G3, xlab = "Free time after school ", ylab = "Grades", main = "Figure 2.22")
summary(df_math[df_math$freetime=="1",]$G3) 
summary(df_math[df_math$freetime=="2",]$G3)
summary(df_math[df_math$freetime=="3",]$G3)
summary(df_math[df_math$freetime=="4",]$G3)
summary(df_math[df_math$freetime=="5",]$G3)

plot(goout,G3, xlab = "Going out with friends", ylab = "Grades", main = "Figure 2.23")
summary(df_math[df_math$goout=="1",]$G3) 
summary(df_math[df_math$goout=="2",]$G3)
summary(df_math[df_math$goout=="3",]$G3)
summary(df_math[df_math$goout=="4",]$G3)
summary(df_math[df_math$goout=="5",]$G3)

plot(Dalc,G3, xlab = "Workday alcohol consumption", ylab = "Grades", main = "Figure 2.24")
summary(df_math[df_math$Dalc=="1",]$G3) 
summary(df_math[df_math$Dalc=="2",]$G3)
summary(df_math[df_math$Dalc=="3",]$G3)
summary(df_math[df_math$Dalc=="4",]$G3)
summary(df_math[df_math$Dalc=="5",]$G3)

plot(Walc,G3, xlab = "Workday alcohol consumption", ylab = "Grades", main = "Figure 2.25")
summary(df_math[df_math$Walc=="1",]$G3) 
summary(df_math[df_math$Walc=="2",]$G3)
summary(df_math[df_math$Walc=="3",]$G3)
summary(df_math[df_math$Walc=="4",]$G3)
summary(df_math[df_math$Walc=="5",]$G3)

plot(health,G3, xlab = "Health Status", ylab = "Grades", main = "Figure 2.26")
summary(df_math[df_math$health=="1",]$G3) 
summary(df_math[df_math$health=="2",]$G3)
summary(df_math[df_math$health=="3",]$G3)
summary(df_math[df_math$health=="4",]$G3)
summary(df_math[df_math$health=="5",]$G3)

# Creating Scatter plots for numerical data
plot(df_math$absences,df_math$G3, xlab = "Absences", ylab = "Grades", main = "Figure 2.27")

plot(df_math$age,df_math$G3, xlab = "Age", ylab = "Grades", main = "Figure 2.28")

pairs(~df_math$age+df_math$absences+df_math$G3, main = "Figure 2.29")


############################ Train / Test Split ######################################

set.seed(1)
train = sample(1:nrow(df_math), 320)
actual_g3 = df_math[-train,31]

##############################   Modeling   ############################################


### Subset Selection
# Stepwise Selection
# Linear Model

full_model_fit <- lm(G3~.,data = df_math[train,])
summary(full_model_fit)

# Backward AIC
library(leaps)
backward_aic_fit = MASS::stepAIC(full_model_fit, direction = "backward", trace = FALSE)
backward_aic_fit$anova
summary(backward_aic_fit)
backward_aic_pred = predict(backward_aic_fit, newdata = df_math[-train,1:30])
mean((backward_aic_pred-actual_g3)^2)


# Lasso Regression
library(glmnet)
x_train = model.matrix(G3~., df_math[train,])[,-1]
x_test = model.matrix(G3~., df_math[-train,])[,-1]

y_train = df_math[train,] %>%
  dplyr::select(G3) %>%
  unlist() %>%
  as.numeric()

y_test = df_math[-train,] %>%
  dplyr::select(G3) %>%
  unlist() %>%
  as.numeric()

lasso_mod = glmnet(x_train, 
                   y_train, 
                   alpha = 1) # Fit lasso model on training data

plot(lasso_mod)    # Draw plot of coefficients

set.seed(1)
cv.out = cv.glmnet(x_train, y_train, alpha = 1) # Fit lasso model on training data
plot(cv.out) # Draw plot of training MSE as a function of lambda
best_lambda = cv.out$lambda.min # Select lamda that minimizes training MSE
lasso_pred = predict(lasso_mod, s = best_lambda, newx = x_test) # Use best lambda to predict test data
mean((lasso_pred - y_test)^2) # Calculate test MSE

lasso_best <- glmnet(x_train, y_train, alpha = 1, lambda = best_lambda)
coef(lasso_best)


############### TREES ##################

library(ISLR)
library(tree)
library(MASS)

tree_g3 = tree(G3~., data = df_math , subset = train)
summary(tree_g3)
plot(tree_g3)
text(tree_g3)

cv.tree_g3 = cv.tree(tree_g3)
plot(cv.tree_g3$size, cv.tree_g3$dev, type = 'b')

prune.tree_g3 = prune.tree(tree_g3, best = 4)
plot(prune.tree_g3)
text(prune.tree_g3)

yhat = predict(tree_g3, newdata = df_math[-train,1:30])
plot(yhat, actual_g3)
abline(0,1)
mean((yhat-actual_g3)^2)

yhat_pruned = predict(prune.tree_g3, newdata = df_math[-train,1:30])
plot(yhat_pruned, actual_g3)
abline(0,1)
mean((yhat_pruned-actual_g3)^2)

############# RANDOM FOREST ############

library(randomForest)

# We are performing bagging - by considering all the predictors i.e. mtry = 30
set.seed(-1)
bagging_g3 = randomForest(G3~., data = df_math[train,], mtry = 30, ntree= 1000, importance = TRUE)
bagging_g3
yhat.bag1 = predict(bagging_g3, newdata = df_math[-train,1:30])
bagging_test = df_math[-train,"G3"]
plot(yhat.bag1, bagging_test)
abline(0,1)
mean((yhat.bag1-bagging_test)^2)
importance(bagging_g3)
varImpPlot(bagging_g3, main = "Variable Importance plot - Bagged DT")

# RF - that is with m != p. mtry = p/3
set.seed(-1)
rf_g3 = randomForest(G3~., data = df_math[train,], mtry = 10, ntree= 1000, importance = FALSE)
rf_g3
yhat.rf = predict(rf_g3, newdata = df_math[-train,])
rf_test = df_math[-train,"G3"]
plot(yhat.rf, rf_test)
abline(0,1)
mean((yhat.rf-rf_test)^2)
varImpPlot(rf_g3, main = "Variable Importance plot - Random Forest")

##################################  RESULTS  ##########################################

cat("\n\n Model Performance : \n\n")
cat("RMSE of Backward Step wise : ", sqrt(mean((backward_aic_pred-actual_g3)^2)),"\n")
cat("RMSE of Lasso : ", sqrt(mean((lasso_pred - y_test)^2)),"\n")
cat("RMSE of Decision Tree : ", sqrt(mean((yhat_pruned-actual_g3)^2)),"\n")
cat("RMSE of Bagged Decision Trees : ", sqrt(mean((yhat.bag1-bagging_test)^2)),"\n")
cat("RMSE of RF : ", sqrt(mean((yhat.rf-rf_test)^2)),"\n")


######################################################################################

############################# Portugese Performance Analysis #########################


############################## Understanding the Data #############################

table(school2$school)

head(school2)

colnames(school2)

summary(school2)

############################# Data Cleaning & Preparation ############################

# to check if there are any missing values
any(is.na(school2))
# Thus we have no missing values in the data set.

# As per the requirements of assignment, we have to predict G3
# without G1 and G2 so dropping them
# dropping G1 and G2 from school1 (math)
df_port = subset(school2, select = -c(G1,G2))
colnames(df_port)

glimpse(df_port)

# The following variables need to be converted to categorical type:

# Medu - denotes Mother's eductaion - 5 levels
df_port$Medu = factor(df_port$Medu, levels=c("0","1","2","3","4"), ordered=TRUE)
summary(df_port$Medu)

# Fedu - denotes Father's eductaion - 5 levels
df_port$Fedu = factor(df_port$Fedu, levels=c("0","1","2","3","4"), ordered=TRUE)
summary(df_port$Fedu)

# famrel - denotes - quality of family relationships 
# 1 - very bad to 5 - excellent 
df_port$famrel = factor(df_port$famrel, levels=1:5, ordered=TRUE)
summary(df_port$famrel)

# traveltime - denotes home to school travel time  
# 0 to 4
df_port$traveltime = factor(df_port$traveltime, levels=0:4, ordered=TRUE)
summary(df_port$traveltime)

# studytime - denotes weekly study time   
# 1 to 4
df_port$studytime = factor(df_port$studytime, levels=1:4, ordered=TRUE)
summary(df_port$studytime)

# freetime - free time after school (1 - very low to 5 - very high)
df_port$freetime = factor(df_port$freetime, levels=1:5, ordered=TRUE)
summary(df_port$freetime)

# goout - going out with friends ( 1 - very low to 5 - very high) 
df_port$goout = factor(df_port$goout, levels=1:5, ordered=TRUE)
summary(df_port$goout)

# Dalc - workday alcohol consumption (from 1 - very low to 5 - very high) 
df_port$Dalc = factor(df_port$Dalc, levels=1:5, ordered=TRUE)
summary(df_port$Dalc)

# Walc - weekend alcohol consumption ( 1 - very low to 5 - very high) 
df_port$Walc = factor(df_port$Walc, levels=1:5, ordered=TRUE)
summary(df_port$Walc)

# health - current health status ( 1 - very bad to 5 - very good) 
df_port$health = factor(df_port$health, levels=1:5, ordered=TRUE)
summary(df_port$health)

# failures - number of past class failures (n if 1<=n<3, else 4) 
df_port$failures = factor(df_port$failures, levels=0:4, ordered=TRUE)
summary(df_port$failures)

summary(df_port)



############################# Exploratory Data Analysis(EDA) #########################

# Creating Scatter plots for numerical data
plot(df_port$absences,df_port$G3)
plot(df_port$age,df_port$G3)
pairs(~df_port$age+df_port$absences+df_port$G3)

# Creating box-plots for categorical data
suppressMessages(attach(df_port))
plot(school, G3, xlab = "School", ylab = "Grades")
plot(sex,G3, xlab = "Sex", ylab = "Grades")
plot(address,G3, xlab = "Address", ylab = "Grades")
plot(famsize, G3, xlab = "Family Size", ylab = "Grades")
plot(Pstatus,G3, xlab = "Pstatus", ylab = "Grades")
plot(Mjob,G3, xlab = "Mother's Job", ylab = "Grades")
plot(Fjob, G3, xlab = "Father's Job", ylab = "Grades")
plot(reason,G3, xlab = "Reason for choosing school", ylab = "Grades")
plot(guardian,G3, xlab = "Gaurdian", ylab = "Grades")
plot(schoolsup,G3, xlab = "School Support", ylab = "Grades")
plot(famsup,G3, xlab = "Family Support", ylab = "Grades")
plot(paid, G3, xlab = "Extra Paid classes", ylab = "Grades")
plot(activities,G3, xlab = "Extra-Curricular", ylab = "Grades")
plot(nursery,G3, xlab = "Attended nursery", ylab = "Grades")
plot(higher,G3, xlab = "Wants to go for Higher education", ylab = "Grades")
plot(internet,G3, xlab = "Internet Access at Home", ylab = "Grades")
plot(romantic,G3, xlab = "Romantic Relationship", ylab = "Grades")

############################ Train / Test Split ##################################

set.seed(0)
train = sample(1:nrow(df_port), 520)
actual_g3 = df_port[-train,31]

############################### Modeling #########################################


### Subset Selection
# Stepwise Selection
# Linear Model

full_model_fit <- lm(G3~.,data = df_port[train,])
summary(full_model_fit)

# Backward AIC
library(leaps)
backward_aic_fit = MASS::stepAIC(full_model_fit, direction = "backward", trace = FALSE)
backward_aic_fit$anova
summary(backward_aic_fit)
backward_aic_pred = predict(backward_aic_fit, newdata = df_port[-train,1:30])
mean((backward_aic_pred-actual_g3)^2)
coef(backward_aic_fit)


# Lasso Regression
library(glmnet)
x_train = model.matrix(G3~., df_port[train,])[,-1]
x_test = model.matrix(G3~., df_port[-train,])[,-1]

y_train = df_port[train,] %>%
  dplyr::select(G3) %>%
  unlist() %>%
  as.numeric()

y_test = df_port[-train,] %>%
  dplyr::select(G3) %>%
  unlist() %>%
  as.numeric()

lasso_mod = glmnet(x_train, 
                   y_train, 
                   alpha = 1) # Fit lasso model on training data

plot(lasso_mod)    # Draw plot of coefficients

set.seed(1)
cv.out = cv.glmnet(x_train, y_train, alpha = 1) # Fit lasso model on training data
plot(cv.out) # Draw plot of training MSE as a function of lambda
best_lambda = cv.out$lambda.min # Select lamda that minimizes training MSE
lasso_pred = predict(lasso_mod, s = best_lambda, newx = x_test) # Use best lambda to predict test data
mean((lasso_pred - y_test)^2) # Calculate test MSE

lasso_best <- glmnet(x_train, y_train, alpha = 1, lambda = best_lambda)
coef(lasso_best)

############### TREES ##############

library(ISLR)
library(tree)
library(MASS)

tree_g3 = tree(G3~., data = df_port , subset = train)
summary(tree_g3)
plot(tree_g3)
text(tree_g3)

cv.tree_g3 = cv.tree(tree_g3)
plot(cv.tree_g3$size, cv.tree_g3$dev, type = 'b')

prune.tree_g3 = prune.tree(tree_g3, best = 3)
plot(prune.tree_g3)
text(prune.tree_g3)

yhat = predict(tree_g3, newdata = df_port[-train,1:30])
plot(yhat, actual_g3)
abline(0,1)
mean((yhat-actual_g3)^2)

yhat_pruned = predict(prune.tree_g3, newdata = df_port[-train,1:30])
plot(yhat_pruned, actual_g3)
abline(0,1)
mean((yhat_pruned-actual_g3)^2)

############## RANDOM FOREST ###############

library(randomForest)

# We are performing bagging - by considering all the predictors i.e. mtry = 30
set.seed(-1)
bagging_g3 = randomForest(G3~., data = df_port[train,], mtry = 30, ntree= 1000, importance = TRUE)
bagging_g3
yhat.bag1 = predict(bagging_g3, newdata = df_port[-train,1:30])
bagging_test = df_port[-train,"G3"]
plot(yhat.bag1, bagging_test)
abline(0,1)
mean((yhat.bag1-bagging_test)^2)
importance(bagging_g3)
varImpPlot(bagging_g3)

# trying RF - that is with m != p, and setting importance = False
set.seed(-1)
bagging_g3 = randomForest(G3~., data = df_port[train,], mtry = 10, ntree= 1000, importance = FALSE)
bagging_g3
yhat.bag3 = predict(bagging_g3, newdata = df_port[-train,])
bagging_test = df_port[-train,"G3"]
plot(yhat.bag3, bagging_test)
abline(0,1)
mean((yhat.bag3-bagging_test)^2)
varImpPlot(bagging_g3)

##################################  RESULTS  ##########################################

cat("\n\n Model Performance : \n\n")
cat("RMSE of Backward Step wise : ", sqrt(mean((backward_aic_pred-actual_g3)^2)),"\n")
cat("RMSE of Lasso : ", sqrt(mean((lasso_pred - y_test)^2)),"\n")
cat("RMSE of Decision Tree : ", sqrt(mean((yhat_pruned-actual_g3)^2)),"\n")
cat("RMSE of Bagged Decision Trees : ", sqrt(mean((yhat.bag1-bagging_test)^2)),"\n")
cat("RMSE of RF : ", sqrt(mean((yhat.bag3-bagging_test)^2)),"\n")

