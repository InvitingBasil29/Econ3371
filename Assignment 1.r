# Linear Regression Model - Capital Asset Pricing Model variables
# 3273 daily observations from 02/01/2004 to 30/12/2016 
# Date 		 day/month/year 
# Mkt.RF   Market minus Risk free return
# SMB	     Small Minus Big == return for investing in a low market capitalization firm
# HML	     High  Minus Low == return for companies with high book-to-market values
# RMW      robust-minus-weak profitability
# CMA      conservative-minus-aggressive (low-minus-high) investment
# RF	     Risk Free return

# CONTENT PERTAINING TO ASSIGNMENT 1
# SAM      The Boston Beer Company, Inc.  


rm(list=ls(all=TRUE))  # Remove all objects in the memory
graphics.off();        # Close all graphs

#install.packages(c("car"),dependencies=TRUE)       #package 
#install.packages(c("sandwich"),dependencies=TRUE)
#install.packages(c("lmtest")  ,dependencies=TRUE)
# Alternatively
#list.packages = c("car","sandwich","lmtest")
#install.packages(c("list.packages")  ,dependencies=TRUE)
#install.packages(c("foreign"), dependencies=TRUE)
library(foreign)
library(car)           # implement tests in linear models
library(sandwich)      # compute the variance for Heteroskedastic and/or Autocorrelated Residuals
library(lmtest)        # confidence intervals with heteroskedastic variance
library(huxtable)      # Side-by-side regression tables
library(tidyverse)

capm = read.csv("C:/Users/Joe Bonadeo/ECON3371/Week 1/data_sm.csv",header= TRUE)

View(capm)	
capm$SAM.RF = capm$SAM - capm$RF ## Why this line important wald test doesn't work without

# QUESTION 1a Testing Regressions

        reg.SAM = lm(SAM.RF ~ Mkt.RF, data = capm)
        reg.SAM.Mod2 = lm(SAM.RF ~ Mkt.RF + HML + SMB, data = capm)
        reg.SAM.Mod3 = lm(SAM.RF ~ Mkt.RF + HML + SMB + RMW + CMA, data = capm)
        
        huxreg("SAM Mod1"= reg.SAM,
               "SAM Mod2"= reg.SAM.Mod2,
               "SAM Mod3"= reg.SAM.Mod3,
               stars = c('*'=0.1, '**' =0.05, '***'=0.01))


# Question 1B Testing if intercepts should be included + Assumption of SAM neutrality

        test.mod1= linearHypothesis(reg.SAM, c("(Intercept)=0","Mkt.RF=1"), vcov=vcovHC(reg.SAM, type= "HC1"))
        test.mod2= linearHypothesis(reg.SAM.Mod2, c("(Intercept)=0","Mkt.RF=1"), vcov=vcovHC(reg.SAM.Mod2, type= "HC1"))
        test.mod3= linearHypothesis(reg.SAM.Mod3, c("(Intercept)=0","Mkt.RF=1"), vcov=vcovHC(reg.SAM.Mod3, type= "HC1"))
        
        test.mod4= linearHypothesis(reg.SAM, c("(Intercept)=0"), vcov=vcovHC(reg.SAM, type= "HC1"))
        test.mod5= linearHypothesis(reg.SAM.Mod2, c("(Intercept)=0"), vcov=vcovHC(reg.SAM.Mod2, type= "HC1"))
        test.mod6= linearHypothesis(reg.SAM.Mod3, c("(Intercept)=0"), vcov=vcovHC(reg.SAM.Mod3, type= "HC1"))
        
        test.mod7= linearHypothesis(reg.SAM, c("Mkt.RF=1"), vcov=vcovHC(reg.SAM, type= "HC1"))
        test.mod8= linearHypothesis(reg.SAM.Mod2, c("Mkt.RF=1"), vcov=vcovHC(reg.SAM.Mod2, type= "HC1"))
        test.mod9= linearHypothesis(reg.SAM.Mod3, c("Mkt.RF=1"), vcov=vcovHC(reg.SAM.Mod3, type= "HC1"))
        
        
                                    
        test.mod1 # Pair wise Test
        test.mod2 # Pair wise Test
        test.mod3 # Pair wise Test
        test.mod4 # Intercept = 0 
        test.mod5 # Intercept = 0
        test.mod6 # Intercept = 0
        test.mod7 # Beta1 Coeff = 1
        test.mod8 # Beta1 Coeff = 1
        test.mod9 # Beta1 Coeff = 1

# Question 1C: Rejecting Models in favour of others

        
        # Wald test for comparing nested models
        waldtest(reg.SAM  , reg.SAM.Mod2 , vcov = vcovHC) # Model 1 vs 2
        waldtest(reg.SAM.Mod2  , reg.SAM.Mod3 , vcov = vcovHC) # Model 2 vs 3
        waldtest(reg.SAM, reg.SAM.Mod3 , vcov = vcovHC) # Model 1 vs 3

# Question 1D: Dummy Variable

# Creating Dataset for dates during GFC 1/08/2008 to 31/12/2010
        
        GFC.D=c(rep(0, 1006), rep(1, 757), rep(0, 1510))
        capm$GFC.D=GFC.D
        GFC.A=c(rep(0,2265),rep(1,1008))
        capm$GFC.A=GFC.A
        view(capm)
        
        ## Defining new dummy variables
        
        capm$Mkt.RF.D = capm$Mkt.RF*capm$GFC.D
        capm$Mkt.RF.A = capm$Mkt.RF*capm$GFC.A
        capm$HML.D = capm$HML*capm$GFC.D
        capm$HML.A = capm$HML*capm$GFC.A
        capm$SMB.D = capm$SMB*capm$GFC.D
        capm$SMB.A = capm$SMB*capm$GFC.A
        
        # Model 1
        
        SAM.Model1.dummy   = lm(SAM.RF ~ Mkt.RF + Mkt.RF.D + Mkt.RF.A, data =capm)
        SAM.Model1.dummy.rob = coeftest(SAM.Model1.dummy, vcov = vcovHC, save=TRUE)
        SAM.Model2.dummy   = lm(SAM.RF ~ Mkt.RF + Mkt.RF.D + Mkt.RF.A + HML + HML.D + HML.A +
                                SMB +SMB.D + SMB.A, data =capm)
        SAM.Model2.dummy.rob = coeftest(SAM.Model2.dummy, vcov = vcovHC, save=TRUE)
        
        summary(SAM.Model1.dummy)
        summary(SAM.Model2.dummy)
        
        huxreg("Model 1" = SAM.Model1.dummy,
               "Model 2" = SAM.Model2.dummy,
               stars = c('*'=0.1, '**' =0.05, '***'=0.01))
        
        ## Question 1E 
        
        test.mod10= linearHypothesis(SAM.Model1.dummy, c("Mkt.RF.D=0", "Mkt.RF.A=0"),vcov = vcovHC(SAM.Model1.dummy,type = "HC1"))
        test.mod11= linearHypothesis(SAM.Model2.dummy, c("Mkt.RF.D=0", "Mkt.RF.A=0"), cov = vcovHC(SAM.Model2.dummy,type = "HC1"))
        test.mod12= linearHypothesis(SAM.Model2.dummy, c("HML.D=0", "HML.A=0","SMB.D=0", "SMB.A=0"),vcov = vcovHC(SAM.Model2.dummy,type = "HC1"))
        test.mod13= linearHypothesis(SAM.Model2.dummy, c("Mkt.RF.D=0", "Mkt.RF.A=0","HML.D=0", "HML.A=0","SMB.D=0", "SMB.A=0"),vcov = vcovHC(SAM.Model2.dummy,type = "HC1"))
        
        test.mod10 # Question 1E model comparisons
        test.mod11
        test.mod12
        test.mod13
    
        waldtest(SAM.Model1.dummy,SAM.Model2.dummy)

## Question 2

solow=read.dta("solow.dta")
View(solow)
solowinter = subset(solow,solow$inter==1 & solow$oecd==0)
View(solowinter)
attach(solowinter)

ln.y = log(gdp85)
ln.sk  = log(save/100)
ln.hk  = log(hk/100)
ln.ngd = log(n/100 +0.05)

nrow(solowinter)
detach(solowinter)
storage_list = data.frame(
  beta2.r=integer(),
  beta3.r=integer(),
  std.error.b2=integer(),
  std.error.b3=integer(),
  alpha=integer(),
  beta=integer()
)

for (i in 1:nrow(solowinter)) {
  temp.solow = solowinter[-i, ]
  attach(temp.solow)
  ln.y = log(gdp85)
  ln.sk  = log(save/100)
  ln.hk  = log(hk/100)
  ln.ngd = log(n/100 +0.05)
  
  ln.skalt = ln.sk-ln.ngd
  ln.hkalt = ln.hk-ln.ngd
  #reg.3  = nls(ln.y ~  c + (a/(1-a-b))*ln.sk + (b/(1-a-b))*ln.hk -((a+b)/(1-a-b))*ln.ngd,
               #start = list(c= 6, a = .3, b = .3))
  
  solow.reg = lm(ln.y ~ ln.skalt + ln.hkalt, data = temp.solow)
  summary(solow.reg)
  beta2.r = summary(solow.reg)$coef[2, 1]
  beta3.r = summary(solow.reg)$coef[3, 1]
  std.error.b2 = summary(solow.reg)$coef[2, 2]
  std.error.b3 = summary(solow.reg)$coef[3, 2]
    
  #alpha = (beta2.r*(1+beta3.r)-beta2.r*beta3.r)/((1+beta2.r)*(1+beta3.r)-beta2.r*beta3.r)
  alpha = beta2.r/(beta2.r+beta3.r+1)
  beta = beta3.r/(1+beta2.r+beta3.r)                            
    
  storage_list[i,] <- c(beta2.r, beta3.r, std.error.b2, std.error.b3, alpha, beta)  # append new values to the list
  }


# storage_matrix = array(unlist(storage_list),dim=c(6,23))
#rownames(storage_matrix) = c("beta2.r","beta3.r","std.error.b2","std.error.b3","alpha","beta")
#print(solowinter$country)

storage_list$Country_Removed=c("Algeria",           "Botswana",          "Cameroon",          "Ethiopia",         
                        "Ivory Coast",       "Kenya",             "Madagascar",        "Malawi",           
                        "Mali",              "Morocco",           "Nigeria",           "Senegal",          
                        "S. Africa",         "Tanzania",          "Tunisia",           "Zambia",           
                        "Zimbabwe",          "Bangladesh",        "Burma",             "Hong Kong",        
                        "India",             "Israel",            "Jordan",            "Korea, Rep. of",   
                        "Malaysia",          "Pakistan",          "Philippines",       "Singapore",        
                        "Sri Lanka",         "Syrian Arab Rep.",  "Thailand",          "Costa Rica",       
                        "Dominican Rep.",    "El Salvador",       "Guatemala",         "Haiti",            
                        "Honduras",          "Jamaica",           "Mexico",            "Nicaragua",        
                        "Panama",            "Trinidad + Tobago", "Argentina",         "Bolivia",          
                        "Brazil",            "Chile",             "Colombia",          "Ecuador",          
                        "Paraguay",          "Peru",              "Uruguay",           "Venezuela",        
                        "Indonesia"
  
)
View(storage_list)

beta2.graph = ggplot(storage_list, aes(x=Country_Removed,y=beta2.r,group=Country_Removed,color=Country_Removed))+
  geom_pointrange(aes(ymin=beta2.r-1.96*std.error.b2,ymax=beta2.r+1.96*std.error.b2), show.legend = FALSE)+
  geom_hline(yintercept=mean(storage_list[["beta2.r"]]))+
  theme(axis.text.x = element_text(angle = 90))

beta2.graph

beta3.graph = ggplot(storage_list, aes(x=Country_Removed,y=beta3.r,group=Country_Removed,color=Country_Removed))+
  geom_pointrange(aes(ymin=beta3.r-1.96*std.error.b3,ymax=beta3.r+1.96*std.error.b3), show.legend = FALSE)+
  geom_hline(yintercept=mean(storage_list[["beta3.r"]]))+
  theme(axis.text.x = element_text(angle = 90))  

beta3.graph

alpha.graph = ggplot(storage_list, aes(x=Country_Removed,y=alpha,group=Country_Removed,color=Country_Removed))+
  geom_hline(yintercept=mean(storage_list[["alpha"]]))+geom_point()+ theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 90))

alpha.graph

beta.graph = ggplot(storage_list, aes(x=Country_Removed,y=beta,group=Country_Removed,color=Country_Removed))+
  geom_hline(yintercept=mean(storage_list[["beta"]]),show.legend = FALSE)+geom_point()+ theme(legend.position="none")+
  theme(axis.text.x = element_text(angle = 90))

beta.graph

#ggplot(storage_list, aes(x=Country,y=Estimate_b2))+
  #geom_pointrange(aes)(ymin=beta2.r-1.96*std.error.b2,ymax=beta2.r+1.96*std.error.b2)

mean()


       