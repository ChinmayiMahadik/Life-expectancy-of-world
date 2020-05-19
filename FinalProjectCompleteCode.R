#...............................................General Look up....................................................................

install.packages("DataExplorer")
library(readr)
LifeExp <- read_csv("C:/Users/vedan/OneDrive/Desktop/Statistics/Homework/Final Project/LifeExp.csv")
library(tidyverse)
library(funModeling)
library(Hmisc)
library(DataExplorer)

glimpse(LifeExp)
df_status(LifeExp)
freq(LifeExp)
profiling_num(LifeExp)
plot_num(LifeExp)
describe(LifeExp)

create_report

#...............................................Imputing Missing Value...............................................................

install.packages("mice")
library(mice)

md.pattern(LifeExp)

pMiss = function(x){sum(is.na(x))/length(x)*100}
apply(DataNullValues,2,pMiss)


#Considering data variables having Null values
DataNullValues = cbind(LifeExp$Alcohol, LifeExp$`percentage expenditure`, LifeExp$`Hepatitis B`,
                       LifeExp$BMI, LifeExp$Polio,
                       LifeExp$`Total expenditure`,LifeExp$Diphtheria,
                       LifeExp$GDP, LifeExp$Population, LifeExp$`thinness  1-19 years`,
                       LifeExp$`thinness 5-9 years`)


#Predicting data and imputing for null values using MICE-Cart
ImputeData = mice(DataNullValues,m=1,maxit=50,method='cart',seed=500)

#Saving Imputed Data Set Varibales
completeData <- complete(ImputeData,1); completeData

#Verifying Imputed Variables- Null Values availibility - should be 0
apply(completeData,2,pMiss)

#Exporting Imputed Variables
write.csv(completeData,"C:/Users/vedan/OneDrive/Desktop/Statistics/Homework/Final Project/ToBeImputed.csv")

#Importing Updated DataSet
library(readr)
LifeExpImputed = read_csv("C:/Users/vedan/OneDrive/Desktop/Statistics/Homework/Final Project/LifeExpImputed.csv")

#Verifying Missing values count - should be 0
apply(LifeExpImputed,2,pMiss)


#..............................................................................................................
#........................................................Boxplots......................................................


#..............................................Boxplot part 1.........................................................

par(mfrow=c(2,2))
boxplot(LifeExpImputed$Year, main = "Year Boxplot")
boxplot(LifeExpImputed$`Life.expectancy` , main = "Life Expectancy Boxplot")
boxplot(LifeExpImputed$Adult.Mortality, main = "Adult Mortality Boxplot")
boxplot(LifeExpImputed$infant.deaths, main = " Infant Deaths Boxplot")

#..............................................Boxplot part 2.........................................................
par(mfrow=c(2,2))
boxplot(LifeExpImputed$Alcohol, main = "Alcohol Boxplot")
boxplot(LifeExpImputed$percentage.expenditure, main = "Percentage expenditure Boxplot")
boxplot(LifeExpImputed$Hepatitis.B, main = "Hepatitis Boxplot" )
boxplot(LifeExpImputed$Measles, main = "Measles Boxplot")


#..............................................Boxplot part 3.........................................................
par(mfrow=c(2,2))
boxplot(LifeExpImputed$BMI, main = "BMI Boxplot" )
boxplot(LifeExpImputed$under.five.deaths, main = "Under-five Deaths Boxplot" )
boxplot(LifeExpImputed$Polio, main = "Polio Boxplot" )
boxplot(LifeExpImputed$Total.expenditure, main = "Total expenditure Boxplot" )

#..............................................Boxplot part 4.........................................................
par(mfrow=c(2,2))
boxplot(LifeExpImputed$Diphtheria, main = "Diptheria Boxplot" )
boxplot(LifeExpImputed$HIV.AIDS, main = "HIV/Aids Boxplot" )
boxplot(LifeExpImputed$GDP, main = "GDP Boxplot" )
boxplot(LifeExpImputed$Population, main = "Population Boxplot" )

#..............................................Boxplot part 5.........................................................
par(mfrow=c(2,2))
boxplot(LifeExpImputed$thinness..1.19.years, main = "Thinness 1 - 19 years Boxplot" )
boxplot(LifeExpImputed$thinness.5.9.years, main = "Thinness 5 - 9 Years Boxplot" )
boxplot(LifeExpImputed$Income.composition.of.resources, main = "Income Composition of resources Boxplot" )
boxplot(LifeExpImputed$Schooling, main = "Schooling Boxplot" )


#..............................................Histograms.............................................................
par(mfrow=c(2,2))
hist(LifeExpImputed$Year, main = "Year Histogram")
hist(LifeExpImputed$`Life.expectancy` , main = "Life Expectancy Histogram")
hist(LifeExpImputed$Adult.Mortality, main = "Adult Mortality Histogram")
hist(LifeExpImputed$infant.deaths, main = " Infant Deaths Histogram")

par(mfrow=c(2,2))
hist(LifeExpImputed$Alcohol, main = "Alcohol Histogram")
hist(LifeExpImputed$percentage.expenditure, main = "Percentage expenditure Histogram")
hist(LifeExpImputed$Hepatitis.B, main = "Hepatitis Histogram" )
hist(LifeExpImputed$Measles, main = "Measles Histogram")

par(mfrow=c(2,2))
hist(LifeExpImputed$BMI, main = "BMI Histogram" )
hist(LifeExpImputed$under.five.deaths, main = "Under-five Deaths Histogram" )
hist(LifeExpImputed$Polio, main = "Polio Histogram" )
hist(LifeExpImputed$Total.expenditure, main = "Total expenditure Histogram" )

par(mfrow=c(2,2))
hist(LifeExpImputed$Diphtheria, main = "Diptheria Histogram" )
hist(LifeExpImputed$HIV.AIDS, main = "HIV/Aids Histogram" )
hist(LifeExpImputed$GDP, main = "GDP Histogram" )
hist(LifeExpImputed$Population, main = "Population Histogram" )

par(mfrow=c(2,2))
hist(LifeExpImputed$thinness..1.19.years, main = "Thinness 1 - 19 years Histogram" )
hist(LifeExpImputed$thinness.5.9.years, main = "Thinness 5 - 9 Years Histogram" )
hist(LifeExpImputed$Income.composition.of.resources, main = "Income Composition of resources Histogram" )
hist(LifeExpImputed$Schooling, main = "Schooling Histogram" )


#..............................................Bivariate Analysis.............................................................
par(mfrow=c(2,2))
plot(LifeExpImputed$Year,LifeExpImputed$`Life expectancy`,main = "Year  Life Expectancy")
plot(LifeExpImputed$`Adult Mortality`, LifeExpImputed$`Life expectancy`,main = "Adult Mortality vs Life Expectancy")
plot(LifeExpImputed$`infant deaths`, LifeExpImputed$`Life expectancy`,main = "infant deaths vs Life Expectancy")
plot(LifeExpImputed$Alcohol,LifeExpImputed$`Life expectancy`,main = "Alcohol vs Life Expectancy")

par(mfrow=c(2,2))
plot(LifeExpImputed$`percentage expenditure`, LifeExpImputed$`Life expectancy`,main = " Percentage Expenditure vs Life Expectancy")
plot(LifeExpImputed$`Hepatitis B`, LifeExpImputed$`Life expectancy`,main = " Hepatitis Bvs Life Expectancy")
plot(LifeExpImputed$Measles, LifeExpImputed$`Life expectancy`)
plot(LifeExpImputed$BMI, LifeExpImputed$`Life expectancy`,main = "BMI vs Life Expectancy")

par(mfrow=c(2,2))
plot(LifeExpImputed$`under-five deaths`, LifeExpImputed$`Life expectancy`,main = "under-five deaths vs Life Expectancy")
plot(LifeExpImputed$Polio, LifeExpImputed$`Life expectancy`,main = "Polio vs Life Expectancy")
plot(LifeExpImputed$`Total expenditure`, LifeExpImputed$`Life expectancy`,main = "Total expenditure vs Life Expectancy")
plot(LifeExpImputed$Diphtheria, LifeExpImputed$`Life expectancy`,main = "Diphtheria vs Life Expectancy")

par(mfrow=c(1,3))
plot(LifeExpImputed$`HIV/AIDS`, LifeExpImputed$`Life expectancy`,main = "Year vs Life Expectancy")
plot(LifeExpImputed$GDP, LifeExpImputed$`Life expectancy`)
plot(LifeExpImputed$Population, LifeExpImputed$`Life expectancy`,main = "Year vs Life Expectancy")

par(mfrow=c(2,2))
plot(LifeExpImputed$`thinness  1-19 years`, LifeExpImputed$`Life expectancy`,main = "HIV/AIDS vs Life Expectancy")
plot(LifeExpImputed$`thinness 5-9 years`, LifeExpImputed$`Life expectancy`,main = "thinness 5-9 years vs Life Expectancy")
plot(LifeExpImputed$`Income composition of resources`, LifeExpImputed$`Life expectancy`,main = "Income composition of resources vs Life Expectancy")
plot(LifeExpImputed$Schooling, LifeExpImputed$`Life expectancy`,main = "Schooling vs Life Expectancy")


par(mfrow=c(1,1))

library(corrplot)
Lifecor = data.frame(LifeExpImputed$Year, LifeExpImputed$`Life expectancy`, LifeExpImputed$`Adult Mortality`, 
                     LifeExpImputed$`infant deaths`, LifeExpImputed$Alcohol, LifeExpImputed$`percentage expenditure`, 
                     LifeExpImputed$`Hepatitis B`,LifeExpImputed$Measles, LifeExpImputed$BMI,
                     LifeExpImputed$`under-five deaths`, LifeExpImputed$Polio, LifeExpImputed$`Total expenditure`,
                     LifeExpImputed$Diphtheria, LifeExpImputed$`HIV/AIDS`, LifeExpImputed$GDP, LifeExpImputed$Population,
                     LifeExpImputed$`thinness  1-19 years`, LifeExpImputed$`thinness 5-9 years`, 
                     LifeExpImputed$`Income composition of resources`, LifeExpImputed$Schooling)

names(Lifecor) = c("Year","Life Expectancy","Adult Mort","Inf Death",
                   "Alcohol","Perc Expe.","Hep B","Measles","BMI",
                   "<5 Death", "Polio", "Tot Exp.", "Dipth.", 
                   "HIV", "GDP", "Popul.", "1-19 thin", "5-9thin", "Inc Comp Res.", "Schooling" )

lifecorrelation = cor(Lifecor)
corrplot(lifecorrelation)




#..............................................Multivariate Analysis.............................................................


library(ggplot2)
#Relationship between population and Life Expectency with BMI in Color
ggplot(LifeExpImputed, aes(x=LifeExpImputed$Life.expectancy , y=LifeExpImputed$Population)) + 
  geom_bar(aes(fill =LifeExpImputed$BMI ), stat="identity",position=position_dodge()) +
  labs(title = "Relationship between Population and Life Expentency  with  BMI in Color", 
       x= " Life Expectency ",
       y = "Population", 
       colour="BMI") +
  theme(axis.text.x = element_text(angle = 90))



#Relationship between GDP and Life Expentency  with  percentage expenditure in Color
ggplot(LifeExpImputed, aes(x=LifeExpImputed$Life.expectancy , y=LifeExpImputed$GDP)) + 
  geom_bar(aes(fill =LifeExpImputed$percentage.expenditure ), stat="identity",position=position_dodge()) +
  labs(title = "Relationship between GDP and Life Expentency  with  percentage expenditure in Color", 
       x= "Life Expectency",
       y = "GDP", 
       colour="Percentage expenditure") +
  theme(axis.text.x = element_text(angle = 90))


#Relationship between Diptheria count and infant deaths with population
qplot(LifeExpImputed$infant.deaths,LifeExpImputed$Population, 
      color =LifeExpImputed$Diphtheria ,geom = c("point","smooth"), 
      main = "Relationship between Diptheria count and infant deaths with population ") 

#Relationship between Life expectancy and Alcohol with populationr
qplot(LifeExpImputed$Life.expectancy,LifeExpImputed$Population, 
      color =LifeExpImputed$Alcohol ,geom = c("point","smooth"), 
      main = "Relationship between Life expectancy and Alcohol with population ") 


#..............................................Model Finding Step wise.............................................................



library(openxlsx)
setwd("G:/University of Denver Life!/C U Denver sem 2/BANA 6610 R programming/Final project/life-expectancy-who")
getwd()
lifeEx = read.csv("LifeExpImputed.csv")
summary(lifeEx)

########### Linear regression ###########
####### FUll model ####################
model1 = lm(lifeEx$Life.expectancy ~ lifeEx$Adult.Mortality + lifeEx$infant.deaths + lifeEx$Alcohol + lifeEx$percentage.expenditure + lifeEx$Hepatitis.B + lifeEx$Measles + lifeEx$BMI + lifeEx$under.five.deaths +lifeEx$Polio + lifeEx$Total.expenditure + lifeEx$Diphtheria + lifeEx$HIV.AIDS + lifeEx$GDP + lifeEx$Population + lifeEx$thinness..1.19.years + lifeEx$thinness.5.9.years + lifeEx$Income.composition.of.resources + lifeEx$Schooling)
summary(model1)

#Performing stepwise AIC to find the best combination of independent variable
library(MASS)
library(car)
model1_Step = stepAIC(model1)

#Model 2 with result of stepwise AIC

model2 =lm(lifeEx$Life.expectancy ~ lifeEx$Adult.Mortality + lifeEx$infant.deaths + 
             lifeEx$percentage.expenditure + lifeEx$BMI + lifeEx$under.five.deaths + 
             lifeEx$Polio + lifeEx$Diphtheria + lifeEx$HIV.AIDS + lifeEx$GDP + 
             lifeEx$thinness..1.19.years + lifeEx$Income.composition.of.resources + 
             lifeEx$Schooling)
summary(model2)
anova(model2)
vif(model2)

#Model 3 without correleated independent variable

model3 =lm(lifeEx$Life.expectancy ~ lifeEx$Adult.Mortality +  lifeEx$BMI +
             lifeEx$Polio + lifeEx$Diphtheria + (lifeEx$HIV.AIDS) + 
             (lifeEx$thinness..1.19.years) + (lifeEx$Income.composition.of.resources) + 
             (lifeEx$Schooling))
summary(model3)
AIC(model3)
BIC(model3)
vif(model3)
durbinWatsonTest(model3)
residualPlots(model3)
plot(model3)

############## Robust methods ###########################
library(MASS)
library(quantreg)

#Huber loss

model_hl = rlm(lifeEx$Life.expectancy ~ lifeEx$Adult.Mortality +  lifeEx$BMI +lifeEx$Polio + lifeEx$Diphtheria + lifeEx$HIV.AIDS + 
                 lifeEx$thinness..1.19.years + lifeEx$Income.composition.of.resources + lifeEx$Schooling ,k2 = 1.345)
summary(model_hl)

#least mean square 

model_lms = lqs(lifeEx$Life.expectancy ~ lifeEx$Adult.Mortality +  lifeEx$BMI +lifeEx$Polio + lifeEx$Diphtheria + lifeEx$HIV.AIDS + 
                  lifeEx$thinness..1.19.years + lifeEx$Income.composition.of.resources + lifeEx$Schooling, method = 'lms')
print(model_lms)

#Least Trimmed squares

model_lts = lqs(lifeEx$Life.expectancy ~ lifeEx$Adult.Mortality +  lifeEx$BMI +lifeEx$Polio + lifeEx$Diphtheria + lifeEx$HIV.AIDS + 
                  lifeEx$thinness..1.19.years + lifeEx$Income.composition.of.resources + lifeEx$Schooling, method = 'lts')
print(model_lts)

#Least Absolute Deviations(LAD)

model_lad = rq(lifeEx$Life.expectancy ~ lifeEx$Adult.Mortality +  lifeEx$BMI +lifeEx$Polio + lifeEx$Diphtheria + lifeEx$HIV.AIDS + 
                 lifeEx$thinness..1.19.years + lifeEx$Income.composition.of.resources + lifeEx$Schooling, tau = 0.5)
summary(model_lad)

#S-estimator

model_s = lqs(lifeEx$Life.expectancy ~ lifeEx$Adult.Mortality +  lifeEx$BMI +lifeEx$Polio + lifeEx$Diphtheria + lifeEx$HIV.AIDS + 
                lifeEx$thinness..1.19.years + lifeEx$Income.composition.of.resources + lifeEx$Schooling, method = 'S')
print(model_s)

#MM-estimator
model_mm = rlm(lifeEx$Life.expectancy ~ lifeEx$Adult.Mortality +  lifeEx$BMI +lifeEx$Polio + lifeEx$Diphtheria + lifeEx$HIV.AIDS + 
                 lifeEx$thinness..1.19.years + lifeEx$Income.composition.of.resources + lifeEx$Schooling, method = 'MM')
summary(model_mm)


############### CROSS VALIDATION ################################

training_rows = sample(1:nrow(lifeEx), 0.8 * nrow(lifeEx))  
training_data = lifeEx[training_rows, ] 
test_data = lifeEx[-training_rows, ] 

lm_Predicted <- predict(model3, test_data)
rob_Predicted <- predict(model_hl, test_data)

lm_actuals_pred <- cbind(lm_Predicted, test_data$Life.expectancy)
rob_actuals_pred <- cbind(rob_Predicted, test_data$Life.expectancy)

# Linear regression
mean(apply(lm_actuals_pred, 1, min)/
       apply(lm_actuals_pred, 1, max)) 

#Robust regression
mean(apply(rob_actuals_pred, 1, min)/ 
       apply(rob_actuals_pred, 1, max))

# R-square value of linear regression
summary(model3)$r.squared

# R-square value of robust regression
library(robustbase)
model_lts1 = ltsReg(covMcd(lifeEx$Life.expectancy ~ lifeEx$Adult.Mortality +  lifeEx$BMI +lifeEx$Polio + lifeEx$Diphtheria + lifeEx$HIV.AIDS + 
                             lifeEx$thinness..1.19.years + lifeEx$Income.composition.of.resources + lifeEx$Schooling, control = rrcov.control()))
summary(model_lts1)


##### KFOLD CROSS VALIDATION
library(caret)
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)

#for linear model
cv_model <- train(Life.expectancy ~ Adult.Mortality +  BMI +Polio + Diphtheria + HIV.AIDS + thinness..1.19.years + Income.composition.of.resources + Schooling, data = lifeEx ,method = "lm",trControl = train.control)
# Summarize the results
print(cv_model)
#Rsquare value of every fold
cv_model$resample


#for robust model
cv_rob_model <- train(Life.expectancy ~ Adult.Mortality +  BMI +Polio + Diphtheria + HIV.AIDS + thinness..1.19.years + Income.composition.of.resources + Schooling, data = lifeEx ,method = "rlm",trControl = train.control)
print(cv_rob_model)
cv_rob_model$resample





#..............................................Model Finding from Subset Algorithm.............................................................

library(tidyverse)
library(caret)
library(leaps)

#Best Model finding by not considering Countries

modelSub = lm(LifeExpImputed$`Life expectancy` ~ LifeExpImputed$Year + LifeExpImputed$Status
              + LifeExpImputed$`Adult Mortality` + LifeExpImputed$`infant deaths` + LifeExpImputed$Alcohol 
              + LifeExpImputed$`percentage expenditure` + LifeExpImputed$`Hepatitis B` + LifeExpImputed$Measles 
              + LifeExpImputed$BMI + LifeExpImputed$`under-five deaths` + LifeExpImputed$Polio
              + LifeExpImputed$`Total expenditure` +LifeExpImputed$Diphtheria + LifeExpImputed$`HIV/AIDS`
              +LifeExpImputed$GDP + LifeExpImputed$Population + LifeExpImputed$`thinness  1-19 years`
              + LifeExpImputed$`thinness 5-9 years` + LifeExpImputed$`Income composition of resources` 
              + LifeExpImputed$Schooling, data = LifeExpImputed)

summary(modelSub)
anova(modelSub)

#Using regsubsets to find out the top  best subset model to predice selling price - Not Considering Countries
Submodels = regsubsets(LifeExpImputed$`Life expectancy` ~ LifeExpImputed$Year + LifeExpImputed$Status
                       + LifeExpImputed$`Adult Mortality` + LifeExpImputed$`infant deaths` + LifeExpImputed$Alcohol 
                       + LifeExpImputed$`percentage expenditure` + LifeExpImputed$`Hepatitis B` + LifeExpImputed$Measles 
                       + LifeExpImputed$BMI + LifeExpImputed$`under-five deaths` + LifeExpImputed$Polio
                       + LifeExpImputed$`Total expenditure` +LifeExpImputed$Diphtheria + LifeExpImputed$`HIV/AIDS`
                       + LifeExpImputed$GDP + LifeExpImputed$Population + LifeExpImputed$`thinness  1-19 years`
                       + LifeExpImputed$`thinness 5-9 years` + LifeExpImputed$`Income composition of resources` 
                       + LifeExpImputed$Schooling, data = LifeExpImputed, nvmax = 20)
summary(Submodels)

#considering parameters and finding best possible subset
Var_sum = summary(Submodels)
data.frame(
  Adj_R_Sq = which.max(Var_sum$adjr2),
  CP_Value = which.min(Var_sum$cp),
  BIC_Value = which.min(Var_sum$bic)
)

#Considering 12 Variables we get (Principle of Parsimonious)

SubModel1 = lm(LifeExpImputed$`Life expectancy` ~ LifeExpImputed$Status + LifeExpImputed$`Adult Mortality` + LifeExpImputed$`infant deaths` 
               +  LifeExpImputed$BMI + LifeExpImputed$`under-five deaths` + LifeExpImputed$Polio 
               + LifeExpImputed$Diphtheria + LifeExpImputed$`HIV/AIDS` + LifeExpImputed$GDP 
               + LifeExpImputed$`thinness  1-19 years`+ LifeExpImputed$`Income composition of resources` + LifeExpImputed$Schooling,
               data = LifeExpImputed)

summary(SubModel1)
anova(SubModel1)
library(car)
vif(SubModel1)


#Removing  LifeExpImputed$`under-five deaths` having 167.41 vif value


SubModel2 = lm(LifeExpImputed$`Life expectancy` ~ LifeExpImputed$Status + LifeExpImputed$`Adult Mortality`  + LifeExpImputed$`infant deaths`
               +  LifeExpImputed$BMI + LifeExpImputed$Polio + LifeExpImputed$Diphtheria + LifeExpImputed$`HIV/AIDS` + LifeExpImputed$GDP 
               + LifeExpImputed$`thinness  1-19 years`+ LifeExpImputed$`Income composition of resources` + LifeExpImputed$Schooling,
               data = LifeExpImputed)
summary(SubModel2)

#got decreased significance of independent variable infant deaths

#Poor residual plots



# now  we have 10 independent variables, and performed some transformation to refine residual plots

SubModel3 = lm(LifeExpImputed$`Life expectancy` ~  LifeExpImputed$Status + LifeExpImputed$`Adult Mortality` + sqrt(sqrt(LifeExpImputed$`infant deaths`) )
               + LifeExpImputed$Polio + LifeExpImputed$Diphtheria + sqrt(sqrt(LifeExpImputed$`HIV/AIDS`)) + LifeExpImputed$GDP 
               + LifeExpImputed$`thinness  1-19 years`+ LifeExpImputed$`Income composition of resources` + LifeExpImputed$Schooling,
               data = LifeExpImputed)

summary(SubModel3)
anova(SubModel3)
library(car)
vif(SubModel3)
qqnorm(SubModel3$residuals) 

AIC(SubModel3)
BIC(SubModel3)
residualPlots(SubModel3)

durbinWatsonTest(SubModel3)



library(caret)
set.seed(123)
train.control <- trainControl(method = "cv", number = 10)
cv_model <- train (`Life expectancy` ~  Status + `Adult Mortality` 
                  + sqrt(sqrt(`infant deaths`) ) + Polio + Diphtheria 
                  + sqrt(sqrt(`HIV/AIDS`)) + GDP +`thinness  1-19 years`
                  + `Income composition of resources` +Schooling, data = LifeExpImputed,
                   method = "lm", trControl = train.control)
# Summarize the results
print(cv_model)
#Rsquare value of every fold
cv_model$resample

#.............................................Considering few Significant countries................................



SubModel4 = lm(LifeExpLimitedCountries$`Life expectancy` ~ LifeExpLimitedCountries$Country 
              + LifeExpLimitedCountries$Year
               + LifeExpLimitedCountries$`Adult Mortality`
               + sqrt(sqrt(LifeExpLimitedCountries$`infant deaths`) )
               + LifeExpLimitedCountries$Polio + LifeExpLimitedCountries$Diphtheria 
               + sqrt(sqrt(LifeExpLimitedCountries$`HIV/AIDS`)) + LifeExpLimitedCountries$GDP 
               + LifeExpLimitedCountries$`thinness  1-19 years`
               + LifeExpLimitedCountries$`Income composition of resources`
               + LifeExpLimitedCountries$Schooling,
               data = LifeExpLimitedCountries)

summary(SubModel4)

#When adding few countries in Level the significance of other imp. variable is going down
#and the prediction scope is limited for fewer countires only.
#for each year the Life Exp of each cuntry is nearly the same.


#.........................................Advance Machine Learning approach..............................................

library(caret)
library(rpart)

SubModelFit = rpart(LifeExpImputed$`Life expectancy` ~ LifeExpImputed$Status + LifeExpImputed$`Adult Mortality` + LifeExpImputed$`infant deaths` 
                    + LifeExpImputed$Polio + LifeExpImputed$Diphtheria +LifeExpImputed$`HIV/AIDS` + LifeExpImputed$GDP 
                    + LifeExpImputed$`Income composition of resources` + LifeExpImputed$Schooling,
                    method="class", data = LifeExpImputed)
printcp(SubModelFit) # display the results
plotcp(SubModelFit) # visualize cross-validation results
summary(SubModelFit) # detailed summary of splits

SubModelFit$cptable

#................................................Decision tree Analysis........................................................................

install.packages("matrixStats")
library(party)
library("matrixStats")
#considerd top most correlatd and significant independent variables to build a decision tree

Ctree.LifeExp = ctree(LifeExpImputed$`Life expectancy` ~  `infant deaths` 
                      + BMI + `HIV/AIDS` + GDP + `Income composition of resources` 
                      + Schooling, data = LifeExpImputed)
plot(Ctree.LifeExp)

dev.off()


#................................................Random Forest Analysis........................................................................

install.packages("randomForest")

library(randomForest)

Forest.LifeEXp = randomForest(`Life expectancy` ~  LifeExpImputed$`Adult Mortality`
                              + LifeExpImputed$`infant deaths`+ Polio + Diphtheria + LifeExpImputed$`HIV/AIDS`
                              + LifeExpImputed$GDP + LifeExpImputed$`thinness  1-19 years`
                              + LifeExpImputed$`Income composition of resources`
                              + Schooling,data = LifeExpImputed)
print(Forest.LifeEXp)


predict(Forest.LifeEXp)
plot(Forest.LifeEXp$mse)
Forest.LifeEXp$importance
Forest.LifeEXp$ntree
mean(Forest.LifeEXp$rsq)



#.....................................................Regularization approach to see change in selcted Model...............................


SubModel3 = lm(LifeExpImputed$`Life expectancy` ~  LifeExpImputed$Status + LifeExpImputed$`Adult Mortality` + sqrt(sqrt(LifeExpImputed$`infant deaths`) )
               + LifeExpImputed$Polio + LifeExpImputed$Diphtheria + sqrt(sqrt(LifeExpImputed$`HIV/AIDS`)) + LifeExpImputed$GDP 
               + LifeExpImputed$`thinness  1-19 years`+ LifeExpImputed$`Income composition of resources` + LifeExpImputed$Schooling,
               data = LifeExpImputed)

summary(SubModel3)



#Code Reference-class Notes
#For ridge and lasso regression, we will be using the `glmnet` library.  
#Remember, we need to tune our hyperparameter, $\lambda$ to find the 'best' ridge or lasso model to implement.  
library(glmnet)
x = model.matrix(LifeExpImputed$`Life expectancy` ~., LifeExpImputed)[,-1]
y = LifeExpImputed$`Life expectancy`
#Ridge regression
#Then we would want to build in a cross-validation process to choose our 'best' $\lambda$.  We can do this using `cv.glmnet,` 
cv_ridge = cv.glmnet(x, y, alpha = 0)
#ridge regression is performed by default  using `alpha = 0`
cv_ridge$lambda.min
#We see that the cross-validated model with a $\lambda = 1.975$ provides the optimal model in terms of minimizing MSE
predict(cv_ridge, type="coefficients", s=4.15826)

#We can compare our ridge coefficient values to those of our original `lm` to see the difference

#Lasso
cv_lasso = cv.glmnet(x, y, alpha = 1)
bestlam = cv_ridge$lambda.min
predict(cv_lasso, type="coefficients", s=bestlam)



#elastic net
cv_en = cv.glmnet(x, y, alpha = 0.5)
bestlam = cv_en$lambda.min
predict(cv_en, type="coefficients", s=bestlam)











