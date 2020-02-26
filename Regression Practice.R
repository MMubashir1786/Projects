# Overview:
# Frequently used Libs: Tidyverse, ggplot2, olsrr, kernlab

# Task Completed:
#                 Basics of Linear Modelling (interpretation through visualization, checking correlation)
#                 Linear Modelling (Prediction & summary interpration and Residual explaination)
#                 Influence (outliers Interpration and handelling in Linear modelling)
#                 VIF & PCA (Principal Component Analysis)
#                 MultiColinearity ---- Problems with interpretation
#                 Machine Learning (Basics) 
#                 Introduction of Training ML Algorithms and Clusterring
#                 k-means
#                 Logistic Models, Data Cleaning


# *************************** Libraries*****************************************************************************

library(tidyverse)
library(ggplot2)
library(olsrr)
library(kernlab)
library(plotly)
library(statsr)
library(car)
library(animation)

getwd()


# =============================================================== Basics of Linear Modelling (interpretation through visualization, checking correlation b/w Response(Y) & predictors(X)) ========================================================================

# The first step towards Linear Model is basically the acknoledgement of equation of a straight line  "Y = m(x) + c" 



n <- 1000
x1 <- runif(n, 1, 5) #Random uniform Distn ---- to generate 1000 nos b/w 1 - 5 with almost equal probability
hist(x1)             #the frequency of each no generated is almost same, leads to a total of 1000 nos




#      Example 1 (intro to LM)

Y <- 5*(x1)          # Linear model (Y = b0 +b1(x1),  b0 =0 )
df_1 <- data.frame(y = Y, x = x1) # Assuming a Linear Model (without Error), avg weight of a wheat in a field depend upon the the 5 times the amount of sunlight it gets.
View(df_1)
str(df_1)
summary(df_1) # we can see 1< x <5 and for each x value, the corrosponding y-value is 5time that of x.

df_1 %>%      # simple Linear observation starts from little bit above y=5 
  ggplot(aes(x, y))+
  geom_point()      

lm(y ~ x, df_1) # For Linear model "Y = b0 +b1(x1)", a near perfect prediction for b1, since we did not incorporate error in our Linear Model



#      Example 2 (LM)

Y <- 10 + 5*(x1)    # New Linear Model with b0 & b1 both
df_2 <- data.frame(y = Y, x = x1)
View(df_2)
str(df_2)
summary(df_2) # we can see 1< x <5 and for each x value, the corrosponding y-value is 5time that of x +10.

df_2 %>%      # simple Linear observation starts from little bit above y=5 
  ggplot(aes(x, y))+
  geom_point()      

lm(y ~ x, df_2) # For Linear model "Y = b0 +b1(x1)", a near perfect prediction for b0 & b1, since we did not incorporate error in our Linear Model





#      Example 3 (LM with ERROR)

?rnorm
e <- rnorm(n) # we assume Error comes from "n" follows St Normal Distn
Y <- 10 + 5*(x1) + e    # New Linear Model with b0 & b1 both, including Error
df_3 <- data.frame(y =Y, x =x1)
View(df_3)
str(df_3)
summary(df_3) 

df_3 %>%      # simple Linear observation with Error 
  ggplot(aes(x, y))+
  geom_point()      

lm(y ~ x, df_3) # For Linear model "Y = b0 +b1(x1)", we incorporate error in our Linear Model




#      Example 4 (checking correlation)

data("anscombe")
View(anscombe)  # Every y is related to its corresponding x
#Correlation (-1 < r < 1) can show the strength (how strong or weak) but doesnt tell how much variation is explained by 1 variable due to variability in another
?with
with(anscombe, cor(y1, x1))
with(anscombe, cor(y2, x2))
with(anscombe, cor(y3, x3))
with(anscombe, cor(y4, x4))  # Correlation does not explain causation.... however, all y's explains almost 81.6% of variation caused by corresponding x's

with(anscombe, plot(x1, y1)) # We can see the +ve Linear relation
with(anscombe, plot(x2, y2)) # Despite being a Parabola, we have got a 86% +ve corr
with(anscombe, plot(x3, y3)) # +ve corr exists
with(anscombe, plot(x4, y4)) # No appearent relationship, but there we got 86% +ve corr




#      Example 4 (Interpretation of various features through visualization)

ggplot(anscombe, aes(y=y1, x=x1))+
  geom_point()+
  geom_smooth(method = "lm")    #the line does not cover each point, the verticle distance b/w line and points is residual  

lm_1 <- lm(y1 ~ x1, anscombe)   # Linear model for x1 & y1
residualPlots(lm_1)   #Residual plot from car Package, interpretation: It tells that appearently outliers exists but to what entent and how much influence they are exerting, is further explained by the Influence topic



ggplot(anscombe, aes(y=y2, x=x2))+
  geom_point()+
  geom_smooth(method = "lm")

lm_2 <- lm(y2 ~ x2, anscombe)   # Linear model for x2 & y2
residualPlots(lm_2)         #Residual plot from car Package, interpretation: Outliers exists but cant clearly shows the influence 



ggplot(anscombe, aes(y=y3, x=x3))+
  geom_point()+
  geom_smooth(method = "lm")   # Perfect linear Relationship, but the magnitude of the line id distrubed by the only outlier, pulling the line upwards, showing stronger impact

lm_3 <- lm(y3 ~ x3, anscombe)   # Linear model for x3 & y3
residualPlots(lm_3)         #Residual plot from car Package, interpretation: A single point seems to be influencing the whole trend 



ggplot(anscombe, aes(y=y4, x=x4))+
  geom_point()+
  geom_smooth(method = "lm")           # No Relationship --- Gradiant =0, but for an outlier a LM is defined

lm_4 <- lm(y4 ~ x4, anscombe)   # Linear model for x4 & y4
residualPlots(lm_4)         #Residual plot from car Package, interpretation:Cant Interpret it right now


# Uptill now, we know how linear model works (as they predict the variables of the Model) and how to interpret it (less error means Perfect Fit).






#================================================================  Linear Modelling ( Residual explaination, Prediction & summary interpration)====================================================================================================================

df_comprepair <- read.table("G:/Data Analysis with R (Certificate) SSUET/ClassFiles/Data/computerRepair.txt",
                            header = TRUE)
View(df_comprepair)

# initially eyeballing the Data: 
#                               less time taken to Repair 2 computers than 1, 
#                               there are 2 different times to repair 4 uts (10 min diff), 
#                               there are 2 different times to repair 6 uts (1 min diff), 
#                               there are 2 different times to repair 9 uts (-4 min diff),
#                               there are 2 different times to repair 10 uts (12 min diff),

boxplot(df_comprepair$Minutes) #Visualizing the variability in mins --- Random & Normally distributed

boxplot(df_comprepair$Units)   #Visualizing the variability in Uts  ---- Rightly skewed.. as whisker below is longer than the whisker above the fig (b/c more dual times are given in interval 5-10 uts)
hist(df_comprepair$Units)


with(df_comprepair, plot(Minutes ~ Units)) # To visualize the variability in both variables, we make scatter plot
# Apperantly a strong positive Relationship b/w uts repaired and min taken to repair them.

with(df_comprepair, cor(Minutes, Units)) # 99.4% correlation complements the scatter plot, we saw earlier
with(df_comprepair, cor(Units , Minutes))# same 99.4# correlation....


lm_comprepair <- lm(Minutes ~ ., data = df_comprepair) # Linear Model with Response "Minutes" & all other explainatory variable (i.e uts ONLY)
summary(lm_comprepair)
# the explanatory prediction value is highly significant (as p< 0.05) and....
# ...... the Model itself is highly significant as well (b/c p< 0.05) &.....
# .... explains a 98.6% (Adjusted R-squared:  0.9864) variability in Minutes for a variability in Uts.





# Observing fitted Value & Residual relationship

View(lm_comprepair)       # A comprehensive Summary about a Linear Model
lm_comprepair$fitted.values # fitted Values 
lm_comprepair$residuals    # Residuals 


#Observing the residuals for each fitted value
?cbind
df_crresid <- cbind(df_comprepair,lm_comprepair$fitted.values,
                    lm_comprepair$residuals)
View(df_crresid)

df_crresid %>% 
  mutate(Total = `lm_comprepair$fitted.values`+ `lm_comprepair$residuals`) %>% 
  View    # If we add the 2, than we will get the same values for Mins





#    Using the Model to make Predictions

# We train a Linear Model on a range of values b/w 1-10, ensure the prediction must be carried out for Data near training sample.
predict(lm_comprepair, data.frame(Units = 4))  # perfect -- as value inside training data-set
predict(lm_comprepair, data.frame(Units = 11)) # reasonable -- as value outside training data-set
predict(lm_comprepair, data.frame(Units = 40)) # Very far from Training Dataset -- Not recommended
















#---------------------------------------------------- Illustration (newspaper)---------------
#                         A newspaper agency hired you to assess if it is feasible
#                         for them to start a Sunday edition. Here's your dataset that contains data for
#                         34 newspapers, about their daily sales, and Sunday sales, in thousands of dollars
#                         The particular newspaper that is considering a Sunday edition has a daily circulation 
#                         of 500,000. Provide an interval estimate (based on 95% level) for the predicted Sunday 
#                         circulation of this paper.

df_newsppr <- read.table("G:/Data Analysis with R (Certificate) SSUET/ClassFiles/Data/newspapersData.txt", 
                         header = TRUE, sep = "\t")
View(df_newsppr)

# --------------------------------------------- DAILY interpretaion -----------------------------------------

boxplot(df_newsppr$Daily) # the data is highly concenterated in lower levels, means...
# ...most newspapers have daily turnover b/w 200-400 have lower spread than....
# ... newspapers daily turnover b/w 400-800. we can see it in our next figure
ggplot(df_newsppr, aes(x= Newspaper, y= Daily))+
  geom_point()              
# We can see that mostly daily sales of various newspapers  lies in the Interval 125-400,... 
# ...and have significantly less distance (b/w each othere --- variability) than points in the higher Interval

# --------------------------------------------- SUNDAY interpretaion -----------------------------------------

boxplot(df_newsppr$Sunday) # Almost same trend as of Daily Turnover,...
# ...as newspapers 
ggplot(df_newsppr, aes(x= Newspaper, y= Sunday))+
  geom_point()              
# A similar Pattern can be seen


# --------------------------------------------- overall interpretaion -----------------------------------------

# Therefore we can conclude that Sunday Distribution is directly influenced by the Daily Distribution.
ggplot(df_newsppr, aes(y = Sunday, x = Daily, text = Newspaper))+
  geom_point()     # our conclusion is Proved..!!
# Also we can observe a significant cluster b/w 125-400..
# ..means turnover is mostly in this Interval for newspapers except few, which shows higher nos of Daily and Sunday circulation..



# --------------------------------------------- Linear Model -----------------------------------------
lm_newsppr <- lm(Sunday ~ Daily, df_newsppr)
summary(lm_newsppr) 
# the coefficient of explanatory variable is highly significantly (as p-value < 0.05)
# The model explains the variability in Response variable 91.5% (Adjusted R-square)
# The model is also highly significant, since p-value <0.05

confint(lm_newsppr) # Confidence interval of the coefficient of explanatory variable...
# having confidence intervals for coefficients inherently means that there should
# be a confidence interval for the prediction as well.
?predict
predict(lm_newsppr, data.frame(Daily = 500),
        interval = "prediction")  # The prediction along with Prediction Interval

predict(lm_newsppr, data.frame(Daily = 500),
        interval = "confidence")  # The prediction along with confidence Interval
# The particular newspaper that is considering a Sunday edition has a daily circulation 
# of 500,000. Provide an interval estimate (based on 95% level) for the predicted Sunday 
# circulation of this paper.

















#----------------------------------------------------- Illustration (Supervisor performance Analysis) ------

df_suprvsr <- read.table("G:/Data Analysis with R (Certificate) SSUET/ClassFiles/Data/supervisorPerformance.txt", 
                         header = TRUE)
View(df_suprvsr)
# Initially there is 1 Response & 6 Explainatory Variables
# Y: Overall rating of job done by supervisor (our response variable)
# X1: Handles employee complains
# X2: Does not allow special priviliges 
# X3: Opportunity to learn new things
# X4: Raises based on performance
# X5: Too critical of poor performance
# X6: Rate of advancing to better jobs


# --------------------------------------------- Linear Model -----------------------------------------
lm_supvsr <- lm(Y ~ X1, df_suprvsr) # LM of Y against X1
summary(lm_supvsr) # coefficient & model are highly significant, as both have very low p-values <0.05
# However, the model only explains 67% of variability in Y as related to X1. 
# The residual of the above model is the part of Y not related to X1.


lm_supvsr1 <- lm(X2 ~ X1, df_suprvsr) # LM of X2 against X1
summary(lm_supvsr1) # Only 30% X2 is explained by X1, and residuals are related with X2
# In reality all predictors are not fully Independent

lm_supvsr2 <- lm(Y ~ X1 + X2, df_suprvsr)
summary(lm_supvsr2) # Model explains 66% variation in Y
# Model is highly significant, along with the coefficient of X1. 
# However, coefficient of X2 is highly insignificant, since it is way more than 0.05
# thus, we interpret the coefficients of:
#                                        X2 --- the rate of change of Y for each unit change in X2, after discounting the effect of X1
#                                        X1 --- the rate of change of Y for each unit change in X1, after discounting the effect of X2


lm_supvsrfull <- lm(Y ~ ., df_suprvsr)
summary(lm_supvsrfull) 
# Model & coefficient of X1 is highly significant (very less than p-value)
# Model explains the 66.3% variation in Y, as explanatory variable changes


















# ----------------------------------------------------Illustration (cigarette Data)---------------
df_cigratte <- read.table("G:/Data Analysis with R (Certificate) SSUET/ClassFiles/Data/cigaretteConsumption.txt", 
                          header = TRUE)
View(df_cigratte)
# A State-wise cigeratte Data includes Avg age in a state, HighSchool pop, avg Income per head, 
# Ratio of Black & Females, Price & Sale of cigeratte




# --------------------------------------------- Linear Model -----------------------------------------
lm_cigratte <- lm(Sales ~. - State, df_cigratte)
summary(lm_cigratte)
# Price is highly significant (p-value < 0.05), and puts a -ve impact on sales(if a ut in price increases, after discounting the affect of other variables)
# Model is also highly significant, but only explains a 22.8% response
# HS has -ve impact which is acceptable, also Females has -ve Impact on Sales
# We exclude a State column from our Model, since we do realize the sales vary on State level but we cannot logically acknowledge it.



lm_cigratte <- lm(Sales ~ Price + Income + Age, df_cigratte)
summary(lm_cigratte)
# For 3 explanatory variables, we can see 2 highly significant variables,
# # Model is also highly significant, but only explains a 25.8% response, little bit increased from previous one.




# Overall we have seen LM Summary Interpretation, when the Model & variable is significant and how much a Model explains a Response.
# Besides we now know how to make LM and what variables should make more sense through which we can procede further.
# We now cann Interpret the variables, their coefficients from LM summary.
# We got familier with the Prediction, its Importance and the Prediction Interval.
# We now know the terms training model and training dataset, and how to make a prediction from it.

























#===================================================================== Influence (outliers Interpration and handelling in Linear modelling) ==================

df_newYorkRivers <- read.table("G:/Data Analysis with R (Certificate) SSUET/ClassFiles/Data/newYorkRivers.txt",
                               header = TRUE, sep = '\t')

View(df_newYorkRivers) # Data-set for Rivers
# initially there are 20 Differennt Rivers, 
# Most polluted (with highest Nitrogen Levels) are Fishkill & Heckensack, and least polluted is Oswegatchie (lowest Nitrogen levels)  
# Heckensack has the highest Commercial-Industrial exposure, with Neversink there on 2nd
# Heckensack has the highest Residential exposure, followed by Fishkill & Wappinger
# Ausable	has the highest Forest exposure, followed by Neversink




# --------------------------------------------- Linear Model---------------

lm_newYorkRivers <- lm(Nitrogen ~., df_newYorkRivers[, 2:6] )  # exclude 1st colum b/c it is a name
summary(lm_newYorkRivers) # the model explains 63.2% Response variable, and the model is quite significant..
# ... ironey is.. not even a single explanatory variable is significant... we now check the Influence

lm_riversNotNeversink <- update(lm_newYorkRivers, subset = -4) # we now eliminate River Neversink
summary(lm_riversNotNeversink)
# The removal of Neversink not increases the response variable explaination by a model from previously 63.2% - 81.45%...
# .... Also the model becomes more significant, and 2 explanatory variables become significant.


# Since neversink was not the top one.. what if we eliminate one of the top ones...
lm_riversNotHackensack <- update(lm_newYorkRivers, subset = -5)
summary(lm_riversNotHackensack)
# no major changes can be observed, since the model explaination slightly increases....
# ... there is now 1 significant in the model unlike 2 in the previous one..
# .... however, the model is again highly significant...
# ... this insight shows we should stick with the previous model






# --------------------------------------------- Linear Model---------------
# We only want Commercial & Industrial Land as a predictor, since it is supposed to be a major polutor
with(df_newYorkRivers, plot(Nitrogen ~ ComIndl)) # here we observed that there are 02 outliers...
#... one near 2.0 & other one near 3.0

#for more comprehensive view
ggplot(df_newYorkRivers, aes(x = ComIndl, y = Nitrogen)) +
  geom_point() +
  geom_smooth(method = 'lm') # Apperently a cluster of points can be seen....
#... but the influence of one point seems to be much stronger that the line extended till that outlier



# LM (only one predictor ---- ComIndl)
lm_riversIndst <- lm( Nitrogen ~ ComIndl, df_newYorkRivers)
summary(lm_riversIndst) # Apperently the model explains only 24.4% variation in Response variable...
# ... else the model itself and explanatory variable are both significant..




# --------------------------------------------- influence detection Diagnostics (Must Do, whether relevant or not)---------------

residualPlots(lm_riversIndst) # By default against fitted values there must be a random scatter, which ......
#... in this case is not there. All the points on left side making a bunch and slight pattern can be observed, and only one point 
#...seems to be on the right side.Since there is no random scatter and a huge bend in the line, we assume there is a problem.
# the residual plot can only identify there is a problem... it can never gives u insight about the extremes of the data..
# So the residual plot is only a diagnostic measure which needs to be performed, b/c it slightly reveals the problem.


ols_plot_resid_lev(lm_riversIndst) #residual leverage Plot
#.. We say certain no of observations are "leverage" (in this particular plot), when there are unusual observations in explanatory variable
#.. We say certain no of observations are "Outliers" (in this particular plot), when there are unusual observations in response variable
# in this plot we can clearly see that 4 & 5 are leverage, and 3 & 7 are outliers

ols_plot_resid_stud(lm_riversIndst) #Studentized Residual Plot
# Reason for this plot : Does the order in which the observations were collected matter?? or the index no has any....
#... relationship with the behaviour of the plot??
# if order matters ... then automatically it is related to time.
# in this case this diagnostic is not very useful but certainly if index no is related to time then this plot can be of great importance.

ols_plot_resid_pot(lm_riversIndst) # Potential residual plot
# It shows the existence of outliers..
# Plot to aid in classifying unusual observations as high-leverage points, outliers, or a combination of both..



# --------------------------------------------- influence measure Diagnostics ---------------
# the following are the main methods to measure Influence in the given Model on the basis of where and how

ols_plot_cooksd_chart(lm_riversIndst) # Cook's D Chart (only identifies the influence existence, does not reveal its direction )
# We can see that the observation # 4 has the most Influence b/c it is way pass of the threshold (Red line)...
# the existence of this observation heavily affects the coefficient of the variable
# this chart measure the influence of each observation on the model's specification
# this chart primarily use to detect the Influence..

ols_plot_dffits(lm_riversIndst) # DFFits (influence diagnostic --- explains the direction of Influence)
# this chart also highlighting the Influence of same observation as Cook's D Chart...
# however, it also explains the Influence is negative (direction).
# means it drags the coefficient of a variable downwards..

ols_plot_hadi(lm_riversIndst) # hadi
# it reveals another pt (5th point, unlike only 4th from previous charts)




# --------------------------------------------- Linear Model(Remaining interpretation in context with Influence)---------------

ggplot(df_newYorkRivers[-c(4, 5), ], aes(x = ComIndl, y = Nitrogen)) +
  geom_point() +
  geom_smooth(method = 'lm')
 
# After influence measure diagnostics, we can eliminate the 4 & 5 observation, ultimately results in something decent
# Now we update our Linear Model..

lm_riversNot45 <- update(lm_newYorkRivers, subset = -c(4, 5))
summary(lm_riversNot45)
residualPlot(lm_riversNot45) # scatter can be observed, unlike from the last residual plot

# After 4 & 5 observation excluded, we realize that the explaination increases from 63.2% to 84.5%
# Model become more significant..
# Reason why even a single variable is not significant, is b/c of the problem of Multicolinearity



# Afterall, we learned that Influence can become more problematic if not treated and has a great deal of value...
# To cure the Influence problem, we first employ influence detection Diagnostics then influence measure diagnostics.



# --------------------------------------------- Linear Model (illustration Salary Data) -----------------
df_salary <- read.table("G:/Data Analysis with R (Certificate) SSUET/ClassFiles/Data/salary.txt", header = TRUE)
View(df_salary)
# X == experience (no of years)
# E == Education (3 = graduate, 2 = Inter, 1 = matric) 
# M == Sex (1 = Male, 0 = female)

class(df_salary$E)
class(df_salary$M)
df_salary$E <- as.character(df_salary$E)
df_salary$M <- as.character(df_salary$M)

lm_salary <- lm(S ~ X + E + M, data = df_salary)
summary(lm_salary)







#========================================== MultiColinearity ---- Problems with interpretation (VIF & PCA (Principal Component Analysis)) ==============================


#                     Illustration (hamilton)

df_hamilton <- read.table("G:/Data Analysis with R (Certificate) SSUET/ClassFiles/Data/hamilton.txt",
                          header = TRUE)
View(df_hamilton)
plot(Y ~ X1, data = df_hamilton)
plot(Y ~ X2, data = df_hamilton)

lm_hamilton <- lm(Y ~ ., data = df_hamilton)
summary(lm_hamilton)   #All var are highly significent, along with Model itsels. Also it explains 99.98% variation in Response
with(df_hamilton, cor(Y, X1))
with(df_hamilton, cor(Y, X2))

GGally::ggpairs(df_hamilton) # Marginal relationship b/w X1 & X2 is -0.9 (very strong)



#                     Illustration (Equal Opportunity)

df_equalOpp <- read.table("G:/Data Analysis with R (Certificate) SSUET/ClassFiles/Data/equalOpportunity.txt",
                          header = TRUE)

View(df_equalOpp)

lm_equalOpp1 <- lm(ACHV ~ SCHOOL + PEER + FAM, data = df_equalOpp)
summary(lm_equalOpp1)
# None of the explanatory variable is significant but the Model itself is significant...
#... Also the Model explains 17% variability in Response variable ------ Problem of MULTICOLINEARIETY

aov(lm_equalOpp1) # Fit Analysis of Variance

ols_plot_resid_fit(lm_equalOpp1) # No Influence exists --- Random Scatter
GGally::ggpairs(df_equalOpp[,2:4]) # this shows a high correlation among explanatory variable

lm_equalOpp2 <- lm(ACHV ~ SCHOOL, data = df_equalOpp) 
summary(lm_equalOpp2) # now the variable is significant, Model is significant, model explaination is still 16.3% 

lm_equalOpp3 <- lm(ACHV ~ SCHOOL + PEER, data = df_equalOpp)
summary(lm_equalOpp3) # When increase the no of explanatory variables, every variable becomes insignificant, ...
# .... though Model remains significant ---- Problem of Multicolineariety





#                     Illustration (French Import)


df_frenchImports <- read.table("G:/Data Analysis with R (Certificate) SSUET/ClassFiles/Data/frenchEconomy.txt",
                               header = TRUE)
View(df_frenchImports)

lm_frenchImports1 <- lm(IMPORT ~ DOPROD + STOCK + CONSUM, data = df_frenchImports)
summary(lm_frenchImports1) # Model is highly significant... Also variability explained in Response is 96.7%
# ... But none of the explanatory variable is significant.

aov(lm_frenchImports1) #ANOVA

ols_plot_resid_fit(lm_frenchImports1) # A pattern can be seen (U shape)

ols_plot_resid_stud(lm_frenchImports1) # Maybe a Time affect 

lm_frenchImports2 <- lm(IMPORT ~ DOPROD + STOCK + CONSUM,
                        data = subset(df_frenchImports, YEAR < 60))
summary(lm_frenchImports2) 
#When took Data for Yr less than 60, then we get 2 significant variable, significant Model & model explaination of 98.8%

ols_plot_resid_stud(lm_frenchImports2)
# coefficient of DOPROD -ve, when it should be positive

cor(df_frenchImports[2:4])
GGally::ggpairs(df_frenchImports[2:5]) # consumption to Doprod corr is 0.999 
# therefor we either include consumption or Doprod
lm_frenchImports3 <- lm(IMPORT ~ DOPROD + STOCK,
                        data = subset(df_frenchImports, YEAR < 60))
summary(lm_frenchImports3)
# Variables are significant, Model is significant at a cost of slight decrease in Model explaination from 98.8% to 97.8%






#                     Illustration (Advertising)


df_advertising <- read.table("G:/Data Analysis with R (Certificate) SSUET/ClassFiles/Data/advertising.txt",
                             header = TRUE)
View(df_advertising)

lm_advertising1 <- lm(St ~ At + Pt + Et + At.1 + Pt.1, data = df_advertising)
summary(lm_advertising1) # only 2 variables are significant (this Yr promotion expenditure & Et)
# all coefficients are +ve, Model is also significant and variability explained is 89.0%

aov(lm_advertising1) # ANOVA

GGally::ggpairs(df_advertising[2:6]) # cannot say Multicolinearity exists

# check change if one variable (this Yr Advert expenditure) is dropped
lm_advertising2 <- lm(St ~ Pt + Et + At.1 + Pt.1, data = df_advertising)
summary(lm_advertising2) # 2 variable are significant (A massive shift in position can be seen)
# change in Pt (coefficient decreases), At.1 and Pt.1 (coefficient becomes negative)

# check At with others(b/c we want to check the Partial Relationship of this explanatory var with others) 
lm_advertisingTestAt <- lm(At ~ Pt + At.1 + Pt.1, data = df_advertising)  
summary(lm_advertisingTestAt)
# All variables are highly significant, 
# Model is highly significant,
# 97% variability is explained (perfectly predictable or correlated)
# this yr's Advert exp can easily be predicted by "Pt + At.1 + Pt.1"
# And due to Multicolineariety (which is very hard to detect in this case), we have to discard the coefficients of previous Model since...
#... due to this the relationship is compromised... 




# Conclusion:
#            Up until now we have tackled multicolinariety by 2 Methods:
#             a)   Marginal Relationship (GGally)
#             b)   partial Relationship (through LM -- making explanatory variable a Response and test............ 
#                                     .... its relationship with other explanatory variables, e.g ==> lm_advertisingTestAt )
#            From nowonwards, we use the techenique of VIF (Variance Inflation Factor)




# --------------------------------------------- VIF  ------------------------------------------------------------
# When Multicolineariety exists, the variance b/w coefficients increases.
# The typical Interpretation of VIF -- how much variance of a variable inflates while other variables coexists..
# VIF > 5 --- Dangerous (to some extent it indicated some correlation)
# VIF >10 --- Highly Problematic 

# VIF==> VIF[j] = 1/(1 - R^2[j]), j = 1...p
# The value of VIF j also measures the amount by which the variance of the jth
# regression coefficient is increased due to the linear association of Xj with other
# predictor variables relative to the variance that would result if Xj were not related...
# ...to them linearly. This explains the naming of this particular diagnostic.



vif(lm_equalOpp1) # all are greater than 10
# the var of school coefficient inflates by 83 times b/c of the existence of peer & Family
# dropping one of the variables willnot eliminate collinearity
# in case of lm_equalOpp2, VIF does not holds any significance as it has only one var "School" 

vif(lm_frenchImports1)
# A regression equation containing either
# CONSUM or DOPROD along with STOCK will eliminate collinearity
# both var has almost same variance inflation.. we drop consumption
vif(lm_frenchImports3) # All is well


vif(lm_advertising1)
# except Et, all variables has inflated variance.
# we also checked At is perfectly predictable by the other 3
# when we apply marginal test, couldnt find any corr but in partial we have found the corr
vif(lm_advertising2) # we have eliminated At
# All is well

# Here the prescription might be to regress sales St against E t and three of the
# remaining four variables (At, Pt, At-I, St-t) and examine the resulting VIF/s to
# see if collinearity has been eliminated.





# Conclusion: 
#            we dont just eliminate the variable with highest var inflation... we first investigate the relationship first then we move to elimination round
#            we have to investigate through GGally (Marginal) than if not satisfied go for Partial








































































































































































































































































































































































































































































































































































































































































































































































































































































































