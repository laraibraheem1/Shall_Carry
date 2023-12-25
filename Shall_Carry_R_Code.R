# Empty dataset
rm(list=ls())

# Libraries to be installed first
install.packages("gdata")
install.packages( "AER" )
install.packages( "car")
install.packages( "lmtest" )
install.packages( "zoo" )

# And, even after they are installed, they need to be called every time before they are used
library(gdata)
library(AER)
library(car)
library(lmtest)
library(zoo)

#set working directory
setwd("C:/Users/larai/OneDrive/Desktop/ARE 107/week 1")
load("Guns.RData") #loads data
View(mydata)
summary(mydata)

####PART 1#####
#Pooled OLS
#vioi,t = β1 + β2 Shallit +uit
regpols<-lm(vio~shall,data=mydata)

#Fixed Effects Model (State FE Only)
#vioi,t = β1 + β2 Shallit +αstate +uit
regfes<-lm(vio~shall+factor(stateid),data=mydata)

#Fixed Effects Model (State + Year FE)
#vioi,t = β1 + β2 Shallit +αstate +λt +u
regfesy<-lm(vio~shall+factor(year)+factor(stateid),data=mydata) 

#Fixed Effects Model (State + Year FE and State-level Time Trends)
#vioi,t = β1 + β2 Shallit +αstate +γstate ×t +λt +uit
regfesyt<-lm(vio~shall+factor(year)+factor(stateid)+factor(stateid):year,data=mydata) 

#Fixed Effects Model (State + Year FE and Additional Regressors)
#vioi,t = β1 + β2 Shallit +∑5 j=1 βj ×Controlsi,t +αstate +λt +uit
regfesytmulti<-lm(vio~shall+density+avginc+incarc_rate+pm1029+factor(year)+factor(stateid),data=mydata)

#do an intial comparison of resulting coefficients between models
compareCoefs(regpols,regfes,digits=4) 
compareCoefs(regfesy,regfesyt,digits=4) 
compareCoefs(regfesytmulti,digits=4)

#Summarize results
# Extract coefficients and standard errors
#first install broom to access tidy function
#which helps clean and visualize data 
install.packages("broom")
library(broom)
coefs_regpols <- tidy(regpols)
coefs_regfes <- tidy(regfes)
coefs_regfesy <- tidy(regfesy)
coefs_regfesyt <- tidy(regfesyt)
coefs_regfesytmulti <- tidy(regfesytmulti)
# Print the results with stargazer
stargazer(
  regpols, regfes, regfesy, regfesyt, regfesytmulti,
  title = "Summary of Results",
  type = "text",
  coef = list(coefs_regpols$estimate, coefs_regfes$estimate, coefs_regfesy$estimate, coefs_regfesyt$estimate, coefs_regfesytmulti$estimate),
  se = list(coefs_regpols$std.error, coefs_regfes$std.error, coefs_regfesy$std.error, coefs_regfesyt$std.error, coefs_regfesytmulti$std.error),
  p.auto = FALSE,
  single.row = TRUE,
  digits = 3
)



####PART 2:Now using plm package instead of lm #######
install.packages("Formula")
install.packages("plm")
library(plm)
library(Formula)
mydatap<-pdata.frame(mydata,c("stateid","year"))

#Pooled OLS, no fixed effects
#each step includes calculating robust standard errrors
pregpols<-plm(vio~shall,model="pooling",data=mydatap)
rvpols<-vcovHC(pregpols,method="arellano",cluster="group")
coeftest(pregpols,vcov=rvpols)

#Fixed Effects Model (State Fixed Effects Only)
pregfes<-plm(vio~shall,model="within",data=mydatap)
rvfes<-vcovHC(pregfes,method="arellano",cluster="group")
coeftest(pregfes,vcov=rvfes)

#Fixed Effects Model (State+Year FE)
pregfesy<-plm(vio~shall,effect="twoway",model="within",data=mydatap)
rvfesy<-vcovHC(pregfesy,,method="arellano",cluster="group")
coeftest(pregfesy,vcov=rvfesy)

#FE Model(state +Year FE and Additional Regressors)
pregfesymulti<-plm(vio~shall+density+avginc+incarc_rate+pm1029,effect="twoway",model="within",data=mydatap)
rvfesymulti<-vcovHC(pregfesymulti,method="arellano",cluster="group")
coeftest(pregfesymulti,vcov=rvfesymulti)

# Extract coefficients and standard errors
library(broom)
coefs_pregpols <- tidy(pregpols)
coefs_pregfes <- tidy(pregfes)
coefs_pregfesy <- tidy(pregfesy)
coefs_pregfesymulti <- tidy(pregfesymulti)

# Print the results with stargazer
stargazer(
  pregpols, pregfes, pregfesy, pregfesymulti,
  title = "Summary of Results",
  type = "text",
  coef = list(coefs_pregpols$estimate, coefs_pregfes$estimate, coefs_pregfesy$estimate, coefs_pregfesymulti$estimate),
  se = list(coefs_pregpols$std.error, coefs_pregfes$std.error, coefs_pregfesy$std.error, coefs_pregfesymulti$std.error),
  p.auto = FALSE,
  single.row = TRUE,
  digits = 3
)