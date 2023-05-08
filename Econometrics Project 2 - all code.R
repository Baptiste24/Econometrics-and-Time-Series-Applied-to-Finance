#Introduction

pack=pack<-c("car","readxl","ggplot2", "vtable","tidyverse", "jtools", "e1071", "tseries", 
             "ggplot2", "plotly", "fRegression","e1071","forecast") # Here we list the packages we want to install or add to the library  

pack2<-c("lmtest", "fGarch", "vars", "FinTS", "moments", "rugarch", "sandwich", "rmgarch",
         "urca", "xts") 


install.packages(pack,pack2)
lapply(pack2, require, character.only = TRUE) 

library(readxl)
Macro <- read_excel("C:/Users/Baptiste/Downloads/Macro.xlsx", 
                    col_names = TRUE, col_types =c('date', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
                                                   'numeric','numeric','numeric','numeric','numeric')) 
View(Macro)

#Task 1.1
#print the variables' names
cnames=(colnames(Macro))
cnames
#plot the log time series
MCSFT_log_serie = log(Macro$MICROSOFT)
SP500_log_serie = log(Macro$'S&P500')
CPI_log_serie = log(Macro$CPI)
IDPR_log_serie = log(Macro$INDPRO)
M1SUPP_log_serie = log(Macro$M1SUPPLY)
CCRED_log_serie = log(Macro$CCREDIT)
BMINUS_log_serie = log(Macro$BMINUSA)
US3M_log_serie = log(Macro$USTB3M)
US6M_log_serie = log(Macro$USTB6M)
US10Y_log_serie = log(Macro$USTB10Y)

plot(MCSFT_log_serie)#It starts with a postive trend and then it becomes stationary
plot(SP500_log_serie)
plot(CPI_log_serie)
plot(IDPR_log_serie)
plot(M1SUPP_log_serie)
plot(CCRED_log_serie)
plot(BMINUS_log_serie)
plot(US3M_log_serie)
plot(US6M_log_serie)
plot(US10Y_log_serie)

#most of the series have obvious trends, whether it is upward or downward. We can spot heteroscedasticity on Microsoft serie or US3 and 6m series. However the Credit spread time serie seems to be stationary relatives to the other. 

#Task 1.2

#Log returns calcul
LR_MCSFT <- c(NA, 100* diff (MCSFT_log_serie)) #log-returns
LR_SP500 <- c(NA, 100* diff (SP500_log_serie))
View(data.frame(LR_MCSFT, LR_SP500))


#scatter plot of Microsoft against S&P500 returns
Date=Macro$Date

par(mfrow=c(1,2))
startDate <- Date[1]
endDate <- Date[length(Date)]  
par(cex.axis = 1.5, cex.lab = 1.5, lwd = 2)
plot(Date, LR_MCSFT, type = 'l', xlab =" Date ",ylab ="Returns Microsoft",
     xlim=c(startDate, endDate))

startDate <- Date[1]
endDate <- Date[length(Date)]  
par(cex.axis = 1.5, cex.lab = 1.5, lwd = 2)
plot(Date, LR_SP500, type = 'l', xlab =" Date ",ylab ="Returns S&P500",
     xlim=c(startDate, endDate))

par(cex.axis = 1.5, cex.lab = 1.5, lwd = 2) 
plot(LR_MCSFT, LR_SP500, xlab ="S&P500",
     ylab ="Microsoft",
     pch = 20, cex = 1, col="red") 
#We can see that the point form a strong cluster with few outliers. Hence our hypothesis is that these series are correlated.Also noting the outliers don't occur at the same time for each serie. A possible explanation as that the correlation is not perfect (1)

#Task 1.3

#boxplot for Microsoft and S&P500
MCSFT_SP500 <- cbind(MCSFT=LR_MCSFT,SP500=LR_SP500)
boxplot(MCSFT_SP500,main="Log-returns of Microsoft and S&P500", frame.plot=TRUE)

# we can see that the outliers of Microsoft and the S&P500 are not on the same side of the chart. They seem to move in opposite direction. Furthermore, the data are more spread below the mediane for the S&P500 while Microsoft returns lie more above the mediane.

#Task 1.4

#log-returns
LR_US3M <- c(NA, 100* diff (US3M_log_serie)) 
LR_US10Y <- c(NA, 100* diff (US10Y_log_serie))
View(data.frame(LR_US3M, LR_US10Y))

#scatter plot of US3m T-bills against US10Y T-bills returns

par(mfrow=c(1,2))
startDate <- Date[1]
endDate <- Date[length(Date)]  
par(cex.axis = 1.5, cex.lab = 1.5, lwd = 2)
plot(Date, LR_US3M, type = 'l', xlab =" Date ",ylab ="Returns US3M T-bills",
     xlim=c(startDate, endDate))

startDate <- Date[1]
endDate <- Date[length(Date)]  
par(cex.axis = 1.5, cex.lab = 1.5, lwd = 2)
plot(Date, LR_US10Y, type = 'l', xlab =" Date ",ylab ="Returns US10Y T-bills",
     xlim=c(startDate, endDate))


par(cex.axis = 1.5, cex.lab = 1.5, lwd = 2) 
plot(LR_US3M, LR_US10Y, xlab ="10Y T-bills",
     ylab ="3M T-bills",
     pch = 20, cex = 1, col="red") 

#We can see a strong cluster highlighting a possible correlation between these series. There are several outliers which in that case seem to happend at the same time highlighting a possible correlation there too. A possible explanation is that the explanatory variables may be the same.

#boxplot for US3M and US10Y

US3M_US10Y <- cbind(US3M=LR_US3M,US10Y=LR_US10Y)
boxplot(US3M_US10Y,main="Log-returns of US3M and US10Y", frame.plot=TRUE)

#We can see from the boxplot that most of the value are very close to the mediane, however there are much more outliers than for the US10Y serie. 

#comparison with Microsoft and S&P500 : At this stage, we can spot some key differences. excess return correlation seems to be negative for Microsoft and the S&P500 whereas it seems that the excess return between the US3M and US10Y T-bills are perfectly correlated as we can see as much outliers in proportion below the mediane and above it.

#Task 1.5

#Mean
mean(na.omit(LR_MCSFT))
mean(na.omit(LR_SP500))
mean(na.omit(LR_US3M))
mean(na.omit(LR_US10Y))
#Standard deviation
sd(na.omit(LR_MCSFT))
sd(na.omit(LR_SP500))
sd(na.omit(LR_US3M))
sd(na.omit(LR_US10Y))
#Skewness
skewness(na.omit(LR_MCSFT))
skewness(na.omit(LR_SP500))
skewness(na.omit(LR_US3M))
skewness(na.omit(LR_US10Y))
#Kurtosis
kurtosis(na.omit(LR_MCSFT))
kurtosis(na.omit(LR_SP500))
kurtosis(na.omit(LR_US3M))
kurtosis(na.omit(LR_US10Y))
#Median
median(na.omit(LR_MCSFT))
median(na.omit(LR_SP500))
median(na.omit(LR_US3M))
median(na.omit(LR_US10Y))
#1st and 3rd quartiles, we use the quantile function.The 0% and 100% values are our min and max.
quantile(na.omit(LR_MCSFT))
quantile(na.omit(LR_SP500))
quantile(na.omit(LR_US3M))
quantile(na.omit(LR_US10Y))

#The normal distribution is characterized by a 0 skewness and a kurtosis of 3. However, when trying to assess normality, it is acceptable to come across skewness between -2 and 2+, as for kurtosis, it is acceptable to find kurtosis between 7- and 7+.
#In regard to our data it seems that our series Microsoft, S&P500 and US10Y T-bills could be normaly distributed. However the kurtosis of the US3M T-bill is way above the 7+ range which highlights that we can have much more outliers (because of fatter tails) than for normal distributed series.

#2.1
#price series(log series)
acf(MCSFT_log_serie) #non stationary
pacf(MCSFT_log_serie) #stationary

acf(SP500_log_serie) #non stationary
pacf(SP500_log_serie)#stationary

acf(US3M_log_serie) #non stationary
pacf(US3M_log_serie) #non stationary

acf(US10Y_log_serie)#non stationary
pacf(US10Y_log_serie) #stationary

acf(CPI_log_serie)#non stationary
pacf(CPI_log_serie)# stationary

acf(IDPR_log_serie)#non stationary
pacf(IDPR_log_serie)# stationary

acf(M1SUPP_log_serie)#non stationary
pacf(M1SUPP_log_serie)# stationary

acf(CCRED_log_serie)#non stationary
pacf(CCRED_log_serie)# stationary

acf(BMINUS_log_serie)#non stationary
pacf(BMINUS_log_serie )# non stationary

#return series (Microsoft, SP500, US3M, US10Y) #verify
acf(na.omit(LR_MCSFT))
pacf(na.omit(LR_MCSFT))

#2.2 
#(To do 14 test of each)
#ADF test (non stationary)
library(urca) # This package contains the ADF test we need to check the stationarity of our series 

UnitRootTest<-ur.df(MCSFT_log_serie, type = "trend", selectlags = c("BIC")) 
summary(UnitRootTest)
UnitRootTest<-ur.df(SP500_log_serie, type = "trend", selectlags = c("BIC"))
summary(UnitRootTest)
UnitRootTest<-ur.df(CPI_log_serie, type = "trend", selectlags = c("BIC")) 
summary(UnitRootTest)
UnitRootTest<-ur.df(IDPR_log_serie, type = "trend", selectlags = c("BIC"))
summary(UnitRootTest)
UnitRootTest<-ur.df(M1SUPP_log_serie, type = "trend", selectlags = c("BIC")) 
summary(UnitRootTest)
UnitRootTest<-ur.df(CCRED_log_serie, type = "trend", selectlags = c("BIC")) 
summary(UnitRootTest)
UnitRootTest<-ur.df(BMINUS_log_serie, type = "trend", selectlags = c("BIC")) 
summary(UnitRootTest)
UnitRootTest<-ur.df(US3M_log_serie , type = "trend", selectlags = c("BIC")) 
summary(UnitRootTest)
UnitRootTest<-ur.df(US6M_log_serie  , type = "trend", selectlags = c("BIC")) 
summary(UnitRootTest)
UnitRootTest<-ur.df(US10Y_log_serie , type = "trend", selectlags = c("BIC")) 
summary(UnitRootTest)

#Breusch G test (non autocorrelated)
library(lmtest)
bgtest(MCSFT_log_serie~SP500_log_serie+CPI_log_serie +IDPR_log_serie +M1SUPP_log_serie+CCRED_log_serie+
       BMINUS_log_serie+US3M_log_serie +US3M_log_serie +US6M_log_serie+US10Y_log_serie,data=Macro,order=1)
#We reject H0, Microsoft is autocorrelated 
bgtest(SP500_log_serie~MCSFT_log_serie+CPI_log_serie +IDPR_log_serie +M1SUPP_log_serie+CCRED_log_serie+
         BMINUS_log_serie+US3M_log_serie +US3M_log_serie +US6M_log_serie+US10Y_log_serie,data=Macro,order=1)
bgtest(CPI_log_serie~MCSFT_log_serie+SP500_log_serie +IDPR_log_serie +M1SUPP_log_serie+CCRED_log_serie+
         BMINUS_log_serie+US3M_log_serie +US3M_log_serie +US6M_log_serie+US10Y_log_serie,data=Macro,order=1)
bgtest(IDPR_log_serie~MCSFT_log_serie+SP500_log_serie +CPI_log_serie +M1SUPP_log_serie+CCRED_log_serie+
         BMINUS_log_serie+US3M_log_serie +US3M_log_serie +US6M_log_serie+US10Y_log_serie,data=Macro,order=1)
bgtest(M1SUPP_log_serie~MCSFT_log_serie+SP500_log_serie +CPI_log_serie +IDPR_log_serie+CCRED_log_serie+
         BMINUS_log_serie+US3M_log_serie +US3M_log_serie +US6M_log_serie+US10Y_log_serie,data=Macro,order=1)


#ARCH (homoskedasticity)
# Create our ARMA-GARCH model structure
arma.garch= ugarchspec(mean.model=list(armaOrder=c(1,0)),
                       variance.model=list(garchOrder=c(1,1)), 
                       distribution.model = "norm")

MCSFT.arma.garch = ugarchfit(data=MCSFT_log_serie, spec=arma.garch)
show(MCSFT.arma.garch) #We reject H0, it is heteroskedastic

SP500.arma.garch = ugarchfit(data=SP500_log_serie, spec=arma.garch)
show(SP500.arma.garch) #We reject H0, it is heteroskedastic

CPI.arma.garch = ugarchfit(data=CPI_log_serie, spec=arma.garch)
show(CPI.arma.garch) #We accept H0, it is homoskedastic

IDPR.arma.garch = ugarchfit(data=IDPR_log_serie, spec=arma.garch)
show(IDPR.arma.garch) #we accept H0 but not for alpha

arma.dcc = ugarchspec(mean.model = list(armaOrder = c(0,1)), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), distribution.model = "norm")
M1SUPP.arma.garch = ugarchfit(data=M1SUPP_log_serie, spec=arma.dcc)
show(M1SUPP.arma.garch) #we accept H0

CCRED.arma.garch = ugarchfit(data=CCRED_log_serie, spec=arma.garch)
show(CCRED.arma.garch) #We reject H0, it is heteroskedastic

BMINUS.arma.garch  = ugarchfit(data=BMINUS_log_serie , spec=arma.garch)
show(BMINUS.arma.garch)#we accept H0

US3M.arma.garch  = ugarchfit(data=US3M_log_serie , spec=arma.garch)
show(US3M.arma.garch) #we accept H0

US6M.arma.garch  = ugarchfit(data=US6M_log_serie , spec=arma.garch)
show(US6M.arma.garch)#we accept H0

US10Y.arma.garch  = ugarchfit(data=US10Y_log_serie , spec=arma.garch)
show(US10Y.arma.garch)#we accept H0

#Jaque-Bera (normality)
jarque.test(MCSFT_log_serie) #We reject H0, it is not normally distributed
jarque.test(SP500_log_serie) 
jarque.test(CPI_log_serie) 
jarque.test(IDPR_log_serie)
jarque.test(M1SUPP_log_serie)
jarque.test(CCRED_log_serie)
jarque.test(BMINUS_log_serie)
jarque.test(US3M_log_serie)
jarque.test(US6M_log_serie)
jarque.test(US10Y_log_serie)

#2.3
'presence of multicollinearity among the 
macroeconomic (SP500,CPI,INDPRO,M1SUPPLY) and financial variables.'

#2.3.1
#estimating the OLS models
fit<- lm(MCSFT_log_serie ~SP500_log_serie + CPI_log_serie +IDPR_log_serie+M1SUPP_log_serie+
            CCRED_log_serie+BMINUS_log_serie +US3M_log_serie+US6M_log_serie +US10Y_log_serie)
summary(fit)
ErrorTerms<-fit$residuals #Residuals from OLS
 

#2.3.2
#Testing OLS ASSUMPTIONS
  # 1) E(Res)=0
par(mfrow=c(1,2)) 
plot(ErrorTerms)
hist(ErrorTerms) #breaks = 96
mean(ErrorTerms)
sd(ErrorTerms)

  # 2) Var(Res)=sigma^2 (constant & finite)
bptest(fit, data=Macro) # White's test
ArchTest(ErrorTerms, lags=60, demean = TRUE) # ARCH test 
coeftest(fit, vcov = vcovHC(fit)) # The cure to heteroscedasticity: Compute robust standard errors by using White's correction

# An alternative is to compute robust standard errors by using Newey-West procedure:
coeftest(fit, vcov = NeweyWest( fit))

  # 3) Residuals no autocorrelated
plot(ErrorTerms)

par(mfrow=c(1,2)) 
acf(ErrorTerms, lag=25)
pacf(ErrorTerms, lag=25)

bgtest(fit, order=5) # Breusch-Godfrey Test
Box.test(ErrorTerms,lag =5) # Ljung-Box test

  # 4) X(t) and e(t) non-correlated
Check <- lm(MCSFT_log_serie ~ErrorTerms) #Regression, the sizes are dif and I cant compute
summary(Check)

cor(ErrorTerms,MCSFT_log_serie)

  # 5) Residuals are normally distributed
jarque.test(ErrorTerms) # Bera-Jarque test

#2.3.3
library(strucchange)
# Chow Test: We test the null hypothesis of NO STRUCTURAL BREAKS starting from obs. 1500
# KNOWN STRUCTURAL BREAK
sctest(fit, type = "Chow", point = 1500) 

# CUSUM test: We test the null hypothesis of NO STRUCTURAL BREAKS
# UNKNOWN STRUCTURAL BREAK
par(mfrow=c(1,1))
SB <- efp(MCSFT_log_serie ~SP500_log_serie + CPI_log_serie +IDPR_log_serie+M1SUPP_log_serie+
            CCRED_log_serie+BMINUS_log_serie +US3M_log_serie+US6M_log_serie +US10Y_log_serie)
plot(SB)


#2.4
library(rugarch)
arma.eg = ugarchspec(variance.model = list(garchOrder = c(1, 1), model = "eGARCH"), mean.model = list(armaOrder = c(0,0)))
LR_MCSFT2.arma.eg = ugarchfit(arma.eg, data = MCSFT_log_serie)
show(LR_MCSFT2.arma.eg)

#2.5
library(rmgarch) 
arma.dcc = ugarchspec(mean.model = list(armaOrder = c(1,1)), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), distribution.model = "norm")
LR_MCSFT2.arma.dcc = ugarchfit(arma.dcc, data = MCSFT_log_serie)
show(LR_MCSFT2.arma.dcc)

SP500.arma.dcc = ugarchfit(arma.dcc, data = SP500_log_serie)
show(SP500.arma.dcc)

#try1
library(rugarch)
#GARCH-DCC model
spec = ugarchspec(variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                  mean.model = list(armaOrder = c(0,0)))
# Estimate the model
fit = ugarchfit(data = cbind(MCSFT_log_serie,SP500_log_serie), spec = spec, solver = "hybrid")

# Extract the estimated conditional correlation
cor = fit@fit$ccov
show(fit)

#try2
# Load the data
msft_returns <- MCSFT_log_serie 
sp500_returns <-SP500_log_serie

# Fit a GARCH-DCC model
garch_spec_msft <- ugarchspec(variance.model = list(garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0)))
garch_spec_sp500 <- ugarchspec(variance.model = list(garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0)))
garch_msft <- ugarchfit(garch_spec_msft, msft_returns)
garch_sp500 <- ugarchfit(garch_spec_sp500, sp500_returns)
dcc_fit <- dccfit(garch_msft, garch_sp500)

# Print the summary of the model
print(summary(dcc_fit))


#try3
# load data
data1 <- MCSFT_log_serie 
data2 <- SP500_log_serie

# fit GARCH model for data1
spec1 <- ugarchspec(variance.model = list(garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0)))
fit1 <- ugarchfit(data = data1, spec = spec1)

# fit GARCH model for data2
spec2 <- ugarchspec(variance.model = list(garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0)))
fit2 <- ugarchfit(data = data2, spec = spec2)

# estimate DCC parameters
dcc_param <- dcc_parameter_estimation(fit1, fit2)

#Task 3.1

#The series could be cointegrated because when we plot the two series on the same charts
#we can see that at some point there is a shock that moves both series' mean down, not in the same degree but as we progress over time the series end up reconcile at the same point, hence we can spot kind of a long term equilibrium.

par(mfrow=c(1,2))
startDate <- Date[1]
endDate <- Date[length(Date)]  
par(cex.axis = 1.5, cex.lab = 1.5, lwd = 2)
plot(Date, US3M_log_serie, type = 'l', xlab =" Date ",ylab ="Returns US3M T-Bill",
     xlim=c(startDate, endDate))

startDate <- Date[1]
endDate <- Date[length(Date)]  
par(cex.axis = 1.5, cex.lab = 1.5, lwd = 2)
plot(Date, US6M_log_serie, type = 'l', xlab =" Date ",ylab ="Returns US6M T-Bill",
     xlim=c(startDate, endDate))

#Task 3.2

###########"Before doing lin reg and cointegration test we have to establish : 1-Non stationary 2- both variables are integrated of the same order I(1)

#from 2.1 seems that ACF of log series show that values don't decrease quickly hence it is a hint for non stationarity

unitrootTest(US3M_log_serie, lags = 50, type = c("c")) #Option of the unit root test with
acf(US3M_log_serie) #les valeurs ne descendent pas rapidement donc c'est certain que la série n'est pas stationnaire
adf.test(US3M_log_serie, k=25)# Selon la p-value, on accepte la nul hypothesis, la série n'est pas stationnaire
diff_US3 <- diff(US3M_log_serie)#on va tester si la serie différenciée 1 fois devient stationnaire ?
plot(diff_US3)
acf(diff_US3)
adf.test(diff_US3, k=2)#this test indicates that the serie is stationary in its first difference, hence I(1)

acf(US6M_log_serie)#les valeurs ne descendent pas rapidement donc c'est certain que la série n'est pas stationnaire
adf.test(US6M_log_serie, k=25)#Selon la p-value, on accepte la nul hypothesis, la série n'est pas stationnaire
diff_US6 <- diff(US6M_log_serie)#on va tester si la serie différenciée 1 fois devient stationnaire?
plot(diff_US6)
acf(diff_US6)
adf.test(diff_US6, k=2)# nos 2 séries sont I(1), elles deviennent stationnaires dans la première différence, on va pouvoir faire de la cointégration car il existe au moins 1 combinaison des 2 qui sera stationnaire.

################## We have established that both series are not stationary and are integrated of the same order I(1)


################## We can do lin reg and cointegration test.
#lin reg 
Coint_Eq = lm( US6M_log_serie ~ US3M_log_serie)
summary(Coint_Eq)#commentaire ?????

Resids <-  Coint_Eq$residuals#save the residuals
plot(Resids)

acf(Resids)#just a quick look at the number of lags, here it is 10.
unitrootTest(Resids, lags = 10, type = c("nc")) #Option of the unit root test with
#no constant and no trend (see the plot of the residuals above)
#Unitroot ou ADF.test , c'est le meme résultat...
adf.test(Resids, k=10)# on rejette la  nul hypothesis de non-cointégration, nos résidus sont stationnaires, donc nos séries sont cointégrées.

# Nos séries étant cointégrées, peu importe les écarts à cout terme entre elles, à long terme elle se rejoignent et s'équilibrent
# Par conséquent on a donc une combinaison linéaire de variables non stationnaire qui est stationnaire, on peut donc utiliser l'OLS.

#Task 3.3 

# do ECM based on above
#We demonstrated in the 3.2 that in long term the series are in equilibrium, however too describe in the short term the relation we have to use an error correction model (ECM)
# We can use this model because our series are cointegrated, meaning that in LT we can forecast but not in ST

ResidsAdj <-Resids[1:length(diff_US6)]
ECM <- lm(diff_US6~diff_US3+ResidsAdj)
summary(ECM)# commentaire ???

#Task 3.4

#lin reg 
Coint_Eq = lm( US3M_log_serie ~ US6M_log_serie)
summary(Coint_Eq)#commentaire ?????

Resids <-  Coint_Eq$residuals#save the residuals
plot(Resids)

acf(Resids)#just a quick look at the number of lags, here it is 10.
unitrootTest(Resids, lags = 10, type = c("nc")) #Option of the unit root test with
#no constant and no trend (see the plot of the residuals above)
adf.test(Resids, k=10)

ResidsAdj <-Resids[1:length(diff_US3)]
ECM <- lm(diff_US3~diff_US6+ResidsAdj)
summary(ECM)

#when we regress the three-month yields on the 6-month yields the results don't change, we still find the cointegration.
# This leads us also to do the ECM.
#Seems as the change of regression did nothing to results.

#Task 4.1
diff_US10 <- diff(US10Y_log_serie)
acf(diff_US10)
plot(diff_US10)
plot(diff_US6)
plot(diff_US3)
VARData <- data.frame(diff_US3, diff_US6, diff_US10)
VARselect(VARData, lag.max=5) #Apply the Information Criteria to select the order of the VAR

VAR_Model <- VAR(VARData, p = 3)
summary(VAR_Model)

causality(VAR_Model, cause = "diff_US3")#US3m yield Granger-cause diff_US6 and diff_US10 + instantaneous causality
causality(VAR_Model, cause = "diff_US6")#US6m yield Granger-cause diff_US3 and diff_US10 + instantaneous causality
causality(VAR_Model, cause = "diff_US10")#US10y yield Granger-cause diff_US3 and diff_US6 + instantaneous causality

# Task 4.2

ir = irf(VAR_Model ,n.ahead = 20)
plot (ir)

vd = fevd (VAR_Model ,n.ahead = 20)
plot (vd)

#Serie US3 seems to be much more imapcted by shocks in series than US6 and US10 series are

#4.3 
#cf When you use the cholesky decomposition the ordering of your variables matter.

VARData <- data.frame(diff_US6,diff_US10,diff_US3)
VARselect(VARData, lag.max=5) #Apply the Information Criteria to select the order of the VAR

VAR_Model <- VAR(VARData, p = 1)# 5 lags minimizes the AIC
summary(VAR_Model)

ir = irf(VAR_Model ,n.ahead = 20)
plot (ir)

vd = fevd (VAR_Model ,n.ahead = 20)
plot (vd)

# If we invert our variables order in the VAR model, there is no impact on the orthogonal impulse response.
#Economically speaking, that means that if we want to study the interdependence of series among each other, the order of our model has not a significant impact on our conclusion.

#Task 4.4

dset <- cbind(diff_US3,diff_US6,diff_US10)
VARselect(dset, lag.max=5)#we check the number of lag to input in the johanssen function

cointegration <- ca.jo(dset, type="trace",ecdet="trend", K=5)
summary(cointegration)
# we can see that for 95% conf interval, we reject the null hypothesis of having 0 cointegration. Futhermore, we reject the null hypothesis that there is less than or 2 cointegration relationship.
# hence there are more than 2 cointegration relationship.

#based on empirical evidence in Shea (1992), The cointegration The cointegration vectors that best describe the long-term impact of interest-rate levels on interest-rate changes can often be written as linear combinations of interest-rate spreads
#However, There is difficulty in keeping short-term yields in such a restricted cointegrated system with other interest rates. Short-term speculative returns from long-term bonds do not conform to the expectations hypothesis.
#This means that we could not use OLS as a systematic estimator to make forecast.
