# *************************** Libraries*****************************************************************************
library(tidyverse) # for inclusion of multiple packages 
library(ggplot2)   # For Data Visualization
library(olsrr)     # for building OLS Regression Models
library(statsr)    # for statistical functions
library(car)       # for Applied Regression
library(skimr)     # for compact & flexible summaries 
library(caret)     # ML convinence functions
library(scales)    # Provide methods for automatically determining breaks and labels for axes and legends
library(corrplot)  # correlation plot
library(glmnet)    # ridge, lasso & elastinet
library(xgboost)   # gbm
library(Metrics)   # for RMSE
library(plyr)      # for Data manupilation
library(dplyr)     # for Data manupilation
library(gridExtra) # notably to arrange multiple grid-based plots on a page, and draw tables.
library(MASS)      # for step-AIC
library(lars)      # for Lasso regression
library(randomForest)
library(xgboost)
# ****************************************** Data **********************************************************************


df_amestrain <- read.csv("G:/Data Analysis with R (Certificate) SSUET/ClassFiles/Data/house/train.csv", stringsAsFactors = FALSE)
dim(df_amestrain) # 1460 x 81
df_amestest <-  read.csv("G:/Data Analysis with R (Certificate) SSUET/ClassFiles/Data/house/test.csv", stringsAsFactors = FALSE)
dim(df_amestest) # 1459 x 80


# Our goal is to transform all of our data into numeric, while preserving as much information from the categoric variables 
# as we’re going to use a Multiple Regression Regression Model, gradient boosting method (XGBoost), ridge, lasso and elastic-net regularization. 
# As XGBoost has trouble with extrapolation we will use , which will also account for a lot of the multicollinearity in the data. 
# we take avgs from the predictions of all 4 regressors to get an accurate estimate. 
# We’ll go more in depth on why we chose these methods and how we’ll use them later on.

?bind_rows
combine <- bind_rows(df_amestest, df_amestrain)  #combining both the datasets
dim(combine)





# --------------------------------------------------- Area Inside (areaI)---------------------------------

skim(combine)
# from skimming it is revealed that Alley, PoolQC, Fence, MiscFeature are the variables having completion rate below 0.5 or
# N/A's w.r.t column means is >0.5
?sapply
# Also max GarageYrBlt is 2207 (practically impossible), and character variables of Garage has some similar pattern..
# ... each variable is missing values b/w 157 - 159, somewhat similar --- might means there is No garage at all

# similar pattern might be seen with basement N/A's values lies b/w 79-82 (might be there is no basement)

# years are considered as numerics, rather than character & due to this a mathematical calc applied to it

# need to check Lot variables in both (character & numerics)

# check MasVnrType

# check FireplaceQu & PoolQC



# step -1 amend variables
#step-2 Amend the N/As
# step -3 amend the yrs
# step-3 correlation plot -- with saleprice


#----**********************************---------------- Step -1 (in Factors & Integers from Characters)--------------------******************************--------------------

# Foundation (change from chr to factor)
#                                      factors are used to work with categorical variables, 
#                                      variables that have a fixed and known set of possible values. 
#                                      They are also useful when you want to display character vectors in a non-alphabetical order. 
#                                      Historically, factors were much easier to work with than characters.
table(combine$Foundation)
str(combine$Foundation)
combine$Foundation <- as.factor(combine$Foundation)
str(combine$Foundation)
sum(table(combine$Foundation))  # checked and OK

# Heatingtype (change from chr to factor)
table(combine$Heating)
str(combine$Heating)
combine$Heating <- as.factor(combine$Heating)
str(combine$Heating)
sum(table(combine$Heating))  # checked and OK

# HeatingQC (making an Integer vector)
table(combine$HeatingQC)
str(combine$HeatingQC)
combine$HeatingQC <- as.integer(revalue(combine$HeatingQC, c("Ex" = 5, "Gd" = 4, "TA" = 3, "Fa"=2, "Po" = 1)))
table(combine$HeatingQC)


# Centeral AC (making an Integer vector)
table(combine$CentralAir)
combine$CentralAir <- as.integer(revalue(combine$CentralAir, c( "N"=0, "Y" = 1)))
table(combine$CentralAir)
sum(table(combine$CentralAir))

# Roofstyle (change from chr to factor)
table(combine$RoofStyle)
str(combine$RoofStyle)
combine$RoofStyle <- as.factor(combine$RoofStyle)
str(combine$RoofStyle)
sum(table(combine$RoofStyle))  # checked and OK


# RoofMat (change from chr to factor)
table(combine$RoofMatl)
str(combine$RoofMatl)
combine$RoofMatl <- as.factor(combine$RoofMatl)
str(combine$RoofMatl)
sum(table(combine$RoofMatl))  # checked and OK


# Landcontour (change from chr to factor)
table(combine$LandContour)
str(combine$LandContour)
combine$LandContour <- as.factor(combine$LandContour)
str(combine$LandContour)
sum(table(combine$LandContour))  # checked and OK


# Landslope (making an Integer vector)
table(combine$LandSlope)
combine$LandSlope <- as.integer(revalue(combine$LandSlope, c( "Sev"=0, "Mod" = 1, "Gtl" = 2)))
table(combine$LandSlope)
sum(table(combine$LandSlope))

# BuildingType (change from chr to factor)
table(combine$BldgType) 
str(combine$BldgType)
combine$BldgType <- as.factor(combine$BldgType)
str(combine$BldgType)
sum(table(combine$BldgType))  # checked and OK
table(combine$BldgType)

# house style (change from chr to factor)
table(combine$HouseStyle) 
str(combine$HouseStyle)
combine$HouseStyle <- as.factor(combine$HouseStyle)
str(combine$HouseStyle)
sum(table(combine$HouseStyle))  # checked and OK
table(combine$HouseStyle) 


# Neighbourhood (change from chr to factor)
table(combine$Neighborhood) 
str(combine$Neighborhood)
combine$Neighborhood <- as.factor(combine$Neighborhood)
str(combine$Neighborhood)
sum(table(combine$Neighborhood))  # checked and OK
table(combine$Neighborhood) 
sum(is.na(combine$Neighborhood))

# Condition1 (change from chr to factor)
table(combine$Condition1) 
str(combine$Condition1)
combine$Condition1 <- as.factor(combine$Condition1)
str(combine$Condition1)
sum(table(combine$Condition1))  # checked and OK
table(combine$Condition1) 


# Condition2 (change from chr to factor)
table(combine$Condition2) 
str(combine$Condition2)
combine$Condition2 <- as.factor(combine$Condition2)
str(combine$Condition2)
sum(table(combine$Condition2))  # checked and OK
table(combine$Condition2) 


# Street (making an Integer vector)
table(combine$Street)
combine$Street <- as.integer(revalue(combine$Street, c( "Grvl"=0, "Pave" = 1)))
table(combine$Street)
sum(table(combine$Street))


# Paved drive way (making an Integer vector)
table(combine$PavedDrive)
combine$PavedDrive <- as.integer(revalue(combine$PavedDrive, c( "N"=0, "P" = 1, "Y" = 2)))
table(combine$PavedDrive)
sum(table(combine$PavedDrive))




#----**********************************---------------- Step -2 (Amend N/As by changing their status & combining variables)--------------------******************************--------------------

# PoolQC (making an Integer vector) with PoolArea
table(combine$PoolQC) # from all the listed properties, only 10 properties seems to have pools
combine$PoolQC [is.na(combine$PoolQC)] <- "Pool Not Available" # if PoolQC is N/A then it means the property does not have the pool
table(combine$PoolQC)
qualities <- c('Pool Not Available' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5) # Quality vector defined for a catagorical variable, contains ordanility
combine$PoolQC <- as.integer(revalue(combine$PoolQC, qualities)) # converting the qualities into cardinality order w.r.t Quality vector
table(combine$PoolQC)
sum(table(combine$PoolQC))

combine[combine$PoolArea>0 &  combine$PoolQC == 0, c('PoolArea', 'PoolQC')] # In order to check the relationship, I have got 3 properties with
# positive poolarea but No pool exists there. so there might be a data entry error.. 
# lets check with Total quality of a property & analyze the relationship
combine[combine$PoolArea>0 &  combine$PoolQC == 0, c('PoolArea', 'PoolQC','OverallQual')] # it turns out that overall quality rate for these property..
# "Below Avg", "Above Avg" & "fair" therefore we will rate the discripency in entering Pool quality accordingly..
combine$PoolQC[961] <- 2
combine$PoolQC[1044] <- 3
combine$PoolQC[1140] <- 2
combine[combine$PoolArea>0 &  combine$PoolQC == 0, c('PoolArea', 'PoolQC')]  # error is ractified



# Miscfeatures (change from chr to factor) with amending N/As
table(combine$MiscFeature)
sum(is.na(combine$MiscFeature)) # means these properties does not have any miscellenous features
2814+5+4+95+1
combine$MiscFeature [is.na(combine$MiscFeature)] <- "Not Available" 
table(combine$MiscFeature)
combine$MiscFeature <- as.factor(combine$MiscFeature)
str(combine$MiscFeature)

ggplot(combine[!is.na(combine$SalePrice),], aes(x=MiscFeature, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))
# When looking at the frequencies, the variable seems irrelevant to me. Having a shed probably means ‘no Garage’, 
# which would explain the lower sales price for Shed. Also, while it makes a lot of sense that a house with a Tennis court is expensive, 
# there is only one house with a tennis court in the train (only) dataset.




# Alley (change from chr to factor) with amending N/As
table(combine$Alley)
sum(is.na(combine$Alley)) # means these properties does not have any Alley with the property
120+78+2721
combine$Alley [is.na(combine$Alley)] <- "Not Available" 
table(combine$Alley)
combine$Alley <- as.factor(combine$Alley)
str(combine$Alley)

?geom_bar
ggplot(combine[!is.na(combine$SalePrice), ], aes(x = Alley, y = SalePrice))+
  geom_bar(stat = "summary", fun.y = "median", fill = "orange")+
  scale_y_continuous(breaks = seq(0, 200000, by = 10000), labels = comma)+
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))
# It is evident that salePrice for property with paved data is highest, followed by no Alley available &
# lowest for property with gravel alley. A count is shown of no of property in each catagory for reference





# Fence (change from chr to factor) with amending N/As
table(combine$Fence)
sum(is.na(combine$Fence))
118+112+329+12+2348 # checked

combine$Fence [is.na(combine$Fence)] <- "None" 
# b4 we move forward with factoring the variable, lets check if there exists (any) ordinality in a variable

ggplot(combine[!is.na(combine$SalePrice), ], aes(x = Fence, y = SalePrice))+
  geom_bar(stat = "summary", fun.y = "median", fill = "orange")+
  scale_y_continuous(breaks = seq(0, 200000, by = 10000), labels = comma)+
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))
# Since cant find any ordinality.. therefore

combine$Fence <- as.factor(combine$Fence)
str(combine$Fence)





# FireplaceQu (making an Integer vector) with amending N/As
table(combine$FireplaceQu)
sum(is.na(combine$FireplaceQu))
43+74+744+46+592+1420 #checked

combine$FireplaceQu [is.na(combine$FireplaceQu)] <- "Fire Place Not Available" 
table(combine$FireplaceQu)
qualities <- c('Fire Place Not Available' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5) # Quality vector defined for a catagorical variable, contains ordanility
combine$FireplaceQu <- as.integer(revalue(combine$FireplaceQu, qualities)) 
table(combine$FireplaceQu)

table(combine$Fireplaces) #no of fire places
sum(table(combine$Fireplaces))






# Lot Variables
# For Lot frontage, there are around 486 N/As. if we can get these N/As go away, we can definitly inprove our model. 
# to this end i am plotting lot frontage w.r.t neighbourhood.
table(combine$Neighborhood)
combine$LotFrontage[is.na(combine$LotFrontage)] <- 0
# check lot shape  & lotconfig variable
sum(is.na(combine$LotFrontage))

ggplot(combine[!is.na(combine$SalePrice),], aes(x = Neighborhood, y = LotFrontage))+
  geom_bar(stat = "summary", fun.y = "median", fill = "black")+
  theme(axis.text = element_text(angle = 45, hjust = 1))
?geom_label
# we observe that wach nghod has almost different lot frontage, 





table(combine$LotShape) 
sum(is.na(combine$LotShape)) # no N/As to tackle

ggplot(combine[!is.na(combine$SalePrice),], aes(x =LotShape, y = SalePrice))+
  geom_bar(stat = "summary", fun.y = "median", fill = "red")+
  scale_y_continuous(breaks = seq(0, 250000, by = 20000), labels = comma)+
  geom_label(stat = "count", aes(label = ..count.., y = ..count..)) # we donot find any ordinality, thus making the variable factor

combine$LotShape <- as.factor(combine$LotShape)
str(combine$LotShape)


table(combine$LotConfig) 
sum(is.na(combine$LotConfig)) # no N/As to tackle
ggplot(combine[!is.na(combine$SalePrice),], aes(x =LotConfig, y = SalePrice))+
  geom_bar(stat = "summary", fun.y = "median", fill = "red")+
  scale_y_continuous(breaks = seq(0, 250000, by = 20000), labels = comma)+
  geom_label(stat = "count", aes(label = ..count.., y = ..count..)) # we donot find any ordinality, thus making the variable factor

combine$LotConfig <- as.factor(combine$LotConfig)
str(combine$LotConfig)






qualities <- c('Not Available' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5) # Quality vector defined for a catagorical variable, contains ordanility

# Garage variables (6 in Total)
table(combine$GarageType)    # change into factor
sum(is.na(combine$GarageType))  # 157 N/A's
table(combine$GarageFinish)  # change into factor
sum(is.na(combine$GarageFinish)) # 159 N/A's
table(combine$GarageCond)    # change into qualities
sum(is.na(combine$GarageCond))  # 159 N/A's
table(combine$GarageQual)   # change into qualities
sum(is.na(combine$GarageQual))  # 159 N/A's

sum(is.na(combine$GarageArea))  # 1 N/A's (b/c in case if N/A's in garage means No garage, then it can easily take a value of zero)
sum(is.na(combine$GarageCars))  # 1 N/A's (b/c in case if N/A's in garage means No garage, then it can easily take a value of zero)
# some diaparity can be seen 159-157-1
# First of all, I am going to replace all 159 missing GarageYrBlt: Year garage was built values with the values in....
# ....YearBuilt (this is similar to YearRemodAdd, which also defaults to YearBuilt if no remodeling or additions).


# To find if the assumption of N/A means No garage is true, then we need to check the id's or length of garage variable.. 
# .. if same then assumption is true

length(which(is.na(combine$GarageType) & is.na(combine$GarageFinish) & is.na(combine$GarageCond) & is.na(combine$GarageQual))) 
# since length is same.. it rightful to say that N/A's means no garage is available for 157 N/A's

combine[is.na(combine$GarageType) | is.na(combine$GarageFinish) | is.na(combine$GarageCond) | is.na(combine$GarageQual), c("Id", 
                                                                                                                           "GarageArea", "GarageType",
                                                                                                                           "GarageFinish","GarageCond",
                                                                                                                           "GarageQual", "GarageCars")]
# Except 1 observation (2127), every N/A means the garage does not exists
# for row (2577) garage type must be made not available & N/A's in garagearea must = 0 & garage car = 0

combine$GarageFinish[667] <- names(sort(-table(combine$GarageFinish)))[1] 
combine$GarageCond[667] <- names(sort(-table(combine$GarageCond)))[1]
combine$GarageQual[667] <- names(sort(-table(combine$GarageQual)))[1]

combine$GarageArea[1117] <- 0
combine$GarageCars[1117] <- 0
combine$GarageType[1117] <- "Not Available"

combine$GarageType [is.na(combine$GarageType)] <- "Not Available"
combine$GarageType <- as.factor(combine$GarageType)
table(combine$GarageType)
sum(table(combine$GarageType))

combine$GarageFinish [is.na(combine$GarageFinish)] <- "Not Available"
combine$GarageFinish <- as.factor(combine$GarageFinish)
table(combine$GarageFinish)
sum(table(combine$GarageFinish))

combine$GarageCond [is.na(combine$GarageCond)] <- "Not Available"
combine$GarageCond <- as.integer(revalue(combine$GarageCond, qualities))
table(combine$GarageCond)
sum(table(combine$GarageCond))


combine$GarageQual [is.na(combine$GarageQual)] <- "Not Available"
combine$GarageQual <- as.integer(revalue(combine$GarageQual, qualities))
table(combine$GarageQual)
sum(table(combine$GarageQual))


sum(table(combine$GarageArea))
sum(is.na(combine$GarageArea))


sum(table(combine$GarageCars))
sum(is.na(combine$GarageCars))




# Basement Variables (11 in Total; 5 catagorical & 6 Numerical)

sum(is.na(combine$BsmtCond)) # 82 N/A's
sum(is.na(combine$BsmtQual)) # 81 N/A's
sum(is.na(combine$BsmtExposure)) # 82 N/A's
sum(is.na(combine$BsmtFinType1)) # 79 N/A's
sum(is.na(combine$BsmtFinType2)) # 80 N/A's
sum(is.na(combine$BsmtFinSF1))   # 1 N/A
sum(is.na(combine$BsmtFinSF2))   # 1 N/A
sum(is.na(combine$BsmtUnfSF))    # 1 N/A
sum(is.na(combine$BsmtFullBath)) # 2 N/A's
sum(is.na(combine$BsmtHalfBath)) # 2 N/A's
sum(is.na(combine$TotalBsmtSF))  # 1 N/A


# Check if rows are same for 79 N/A's exists b/w variables having N/A's (79-82)
?which
which(is.na(combine$BsmtCond) & is.na(combine$BsmtQual) & is.na(combine$BsmtExposure) & is.na(combine$BsmtFinType1) 
      & is.na(combine$BsmtFinType2)) # this gives us similar observation nos (Row nos)  having N/A's and common among these 5 variables

length(which(is.na(combine$BsmtCond) & is.na(combine$BsmtQual) & is.na(combine$BsmtExposure) & is.na(combine$BsmtFinType1) 
             & is.na(combine$BsmtFinType2))) # all combine has same 79 rows


# Comprehensive look at which row are different in terms of having N/A's in catagorical variables (logigal Operator "|" -- element-wise)
combine[!is.na(combine$BsmtFinType1) & (is.na(combine$BsmtCond) | is.na(combine$BsmtQual) | is.na(combine$BsmtExposure)
                                        | is.na(combine$BsmtFinType2) ), c("BsmtCond", "BsmtQual", "BsmtExposure", 
                                                                         "BsmtFinType2", "BsmtFinType1")] # It seems the basement exists, but the data entry is not done
# We now know 79 houses are without basement
# Now assigning values to the fields with incomplete records
table(combine$BsmtExposure)
?names
?sort
?table
combine$BsmtExposure[c(28, 889, 2408)] <- names(sort(-table(combine$BsmtExposure)))[1] #taking the first value in table

table(combine$BsmtCond)
combine$BsmtCond[c(581, 726, 1065)] <- names(sort(-table(combine$BsmtCond)))[1]
combine$BsmtCond[c(581, 726, 1065)]


table(combine$BsmtQual)
combine$BsmtQual[c(758, 759)] <- names(sort(-table(combine$BsmtQual)))[1]
combine$BsmtQual[c(758, 759)]



table(combine$BsmtFinType2)
combine$BsmtFinType2[c(1792)] <- names(sort(-table(combine$BsmtFinType2)))[1]
combine$BsmtFinType2[c(1792)]



qualities <- c('Not Available' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5) # Quality vector defined for a catagorical variable, contains ordanility
qualities_1 <- c('Not Available' = 0, 'No' = 1, 'Mn' = 2, 'Av' = 3, 'Gd' = 4)
qualities_2 <- c('Not Available' = 0, 'Unf' = 1, 'LwQ' = 2, 'Rec' = 3, 'BLQ' = 4, 'ALQ' = 5, "GLQ" = 6)
# simply elimination N/A's and change the nature of a variable (Catagorical variable)
combine$BsmtCond[is.na(combine$BsmtCond)] <- 'Not Available'
combine$BsmtQual[is.na(combine$BsmtQual)] <- 'Not Available'
combine$BsmtExposure[is.na(combine$BsmtExposure)]<- 'Not Available'
combine$BsmtFinType1[is.na(combine$BsmtFinType1)]<- 'Not Available'
combine$BsmtFinType2[is.na(combine$BsmtFinType2)]<- 'Not Available'

table(combine$BsmtCond)
combine$BsmtCond <- as.integer(revalue(combine$BsmtCond, qualities))
79+5+104+2609+122

table(combine$BsmtQual)
combine$BsmtQual <- as.integer(revalue(combine$BsmtQual, qualities))
79+88+1285+1209+258


table(combine$BsmtExposure)
combine$BsmtExposure <- as.integer(revalue(combine$BsmtExposure, qualities_1))
79+1907+239+418+276

table(combine$BsmtFinType1)
combine$BsmtFinType1 <- as.integer(revalue(combine$BsmtFinType1, qualities_2))
79+851+154+288+269+429+849

table(combine$BsmtFinType2)
combine$BsmtFinType2 <- as.integer(revalue(combine$BsmtFinType2, qualities_2))
79+2494+87+105+68+52+34


# Dealing with Numerical variables, their N/A's and appropriate treatment
combine[is.na(combine$BsmtFinSF1) | is.na(combine$BsmtFinSF2)
        | is.na(combine$BsmtUnfSF) | is.na(combine$BsmtFullBath) | is.na(combine$BsmtHalfBath)
        | is.na(combine$TotalBsmtSF), c("BsmtQual", "BsmtFinSF1","BsmtFinSF2", "BsmtUnfSF", "BsmtFullBath",
                                        "BsmtHalfBath", "TotalBsmtSF")] # it is obvious that these basement does not exists

combine$BsmtFinSF1[is.na(combine$BsmtFinSF1)] <- 0
combine$BsmtFinSF2[is.na(combine$BsmtFinSF2)] <- 0
combine$BsmtUnfSF[is.na(combine$BsmtUnfSF)] <- 0
combine$BsmtFullBath[is.na(combine$BsmtFullBath)] <- 0
combine$BsmtHalfBath[is.na(combine$BsmtHalfBath)] <- 0
combine$TotalBsmtSF[is.na(combine$TotalBsmtSF)] <- 0


sum(is.na(combine$BsmtCond)) 
sum(is.na(combine$BsmtQual)) 
sum(is.na(combine$BsmtExposure)) 
sum(is.na(combine$BsmtFinType1)) 
sum(is.na(combine$BsmtFinType2)) 
sum(is.na(combine$BsmtFinSF1))   
sum(is.na(combine$BsmtFinSF2))   
sum(is.na(combine$BsmtUnfSF))    
sum(is.na(combine$BsmtFullBath)) 
sum(is.na(combine$BsmtHalfBath)) 
sum(is.na(combine$TotalBsmtSF))  










# Masonry Variables (1 Numerical & 1 catagorical)
sum(is.na(combine$MasVnrArea)) #23 N/A's
sum(is.na(combine$MasVnrType)) #24 N/A's

length(which(is.na(combine$MasVnrArea) & is.na(combine$MasVnrType))) # so it is revealed that 23 properties does not have Masonry
# if Masonry does not exists, then area must be zero

table(combine$MasVnrType)

# checking that extra N/A & its possible treatment
combine[!is.na(combine$MasVnrArea) & is.na(combine$MasVnrType), c("MasVnrType", "MasVnrArea")]
names(sort(-table(combine$MasVnrType))) # sorted table in descending order (count-wise)
combine$MasVnrType[c(1151)] <- names(sort(-table(combine$MasVnrType)))[2] # take 2nd value, as first one is None

# taking care of catagorical by changing the type into factor
combine$MasVnrType[is.na(combine$MasVnrType)] <- 'Not Available'
25+880+1742+23+249
combine$MasVnrType <- as.factor(combine$MasVnrType)
str(combine$MasVnrType)


# taking care of Numerical variable N/A's
combine$MasVnrArea[is.na(combine$MasVnrArea)] <- 0









# MSzoning 
table(combine$MSZoning) # 4 N/A's
combine$MSZoning[is.na(combine$MSZoning)] <- names(sort(-table(combine$MSZoning)))[2] # while applying Mode rule I should have gone for 1st but accidently I typed 2nd
combine$MSZoning <- as.factor(combine$MSZoning)
str(combine$MSZoning)
25+139+26+2265+464










# kitchen variables (1 catagorical & 1 numerical)
table(combine$KitchenQual) #only 1 N/A value
combine[!is.na(combine$KitchenAbvGr) & is.na(combine$KitchenQual), c("KitchenAbvGr","KitchenQual" )] # means kitchen exists, but not recorded
# now replacing with the most common value in my table
combine$KitchenQual[is.na(combine$KitchenQual)] <- names(sort(-table(combine$KitchenQual))) # most common value is "TA" (Applying Mode here)
combine$KitchenQual <- as.integer(revalue(combine$KitchenQual, qualities))
sum(table(combine$KitchenQual))










# Utilities (ordinal)
table(combine$Utilities) # 2 N/A's available'
combine[is.na(combine$Utilities) | (combine$Utilities == "NoSeWa"), c(1:10)]
# We do know that N/A utilities belongs to test dataset, and assuming a mode for these 2 which is "AllPub" make the variable useless for prediction
# we make this a Null variable
combine$Utilities <- NULL










# Home Functionality
table(combine$Functional) # 2 N/A's
combine[is.na(combine$Functional), c(1:10)] # since both N/A's lie in the test dataset, therefore we use the Mode criteria
combine$Functional[is.na(combine$Functional)] <- names(sort(-table(combine$Functional)))[1]
qualities_3 <- c('Sal'=0, 'Sev'=1, 'Maj2'=2, 'Maj1'=3, 'Mod'=4, 'Min2'=5, 'Min1'=6, 'Typ'=7)
combine$Functional <- as.integer(revalue(combine$Functional, qualities_3))
sum(table(combine$Functional))









# Exterior Variables (all 4 are catagorical variable)
table(combine$ExterCond)
sum(table(combine$ExterCond)) # no N/A's


table(combine$ExterQual)
sum(table(combine$ExterQual)) # no N/A's


table(combine$Exterior1st) # 1 N/A


table(combine$Exterior2nd) # 1 N/A
combine[is.na(combine$Exterior1st) | is.na(combine$Exterior2nd), c("Id", "ExterCond","ExterQual","Exterior1st","Exterior2nd")]
# there is only one property which is missing both the variables

combine$Exterior2nd[is.na(combine$Exterior2nd)] <- names(sort(-table(combine$Exterior2nd)))[1]
combine$Exterior1st[is.na(combine$Exterior1st)] <- names(sort(-table(combine$Exterior1st)))[1]

#changing the nature of Exterior variables

combine$ExterCond <- as.integer(revalue(combine$ExterCond, qualities))
str(combine$ExterCond)
combine$ExterQual <- as.integer(revalue(combine$ExterQual, qualities))
str(combine$ExterQual)
combine$Exterior2nd <- as.factor(combine$Exterior2nd)
str(combine$Exterior2nd)
combine$Exterior1st <- as.factor(combine$Exterior1st)
str(combine$Exterior1st)












# Electrical systems (catagorical variable)
table(combine$Electrical) #1 N/A
combine$Electrical[is.na(combine$Electrical)] <- names(sort(-table(combine$Electrical)))[1]
combine$Electrical <- as.factor(combine$Electrical)
str(combine$Electrical)
sum(table(combine$Electrical))










# Sale Type & Sale condition
table(combine$SaleType) # 1 N/A
combine[is.na(combine$Electrical)] <- names(sort(-table(combine$Electrical)))[1]
combine$Electrical <- as.factor(combine$Electrical)
str(combine$Electrical)
sum(table(combine$Electrical))



table(combine$SaleCondition)
combine$SaleCondition <- as.factor(combine$SaleCondition)
str(combine$SaleCondition)
sum(table(combine$SaleCondition))


















#----**********************************---------------- Step -3 (changing integers to Factors)--------------------******************************--------------------

table(combine$MSSubClass) #MSSubClass: Identifies the type of dwelling involved in the sale. These classes are coded as numbers, but really are categories.
combine$MSSubClass <- as.factor(combine$MSSubClass) 
combine$MSSubClass <- revalue(combine$MSSubClass,c('20'='1 story 1946+', '30'='1 story 1945-', '40'='1 story unf attic', 
                                                   '45'='1,5 story unf', '50'='1,5 story fin', '60'='2 story 1946+', 
                                                   '70'='2 story 1945-', '75'='2,5 story all ages', '80'='split/multi level', 
                                                   '85'='split foyer', '90'='duplex all style/age', '120'='1 story PUD 1946+', 
                                                   '150'='1,5 story PUD all', '160'='2 story PUD 1946+', '180'='PUD multilevel', 
                                                   '190'='2 family conversion'))
str(combine$MSSubClass)
sum(table(combine$MSSubClass))




table(combine$YearRemodAdd)
str(combine$YearRemodAdd)
combine$YearRemodAdd <- as.factor(combine$YearRemodAdd)




table(combine$GarageYrBlt)
str(combine$GarageYrBlt)
combine$GarageYrBlt[is.na(combine$GarageYrBlt)] <- combine$YearBuilt[is.na(combine$GarageYrBlt)]
combine$GarageYrBlt <- as.factor(combine$GarageYrBlt)
sum(table(combine$GarageYrBlt)) #No N/A left






table(combine$YearBuilt)
str(combine$YearBuilt)
combine$YearBuilt <- as.factor(combine$YearBuilt)



table(combine$MoSold)
combine$MoSold <- as.factor(combine$MoSold)
str(combine$MoSold)
sum(table(combine$MoSold))






table(combine$YrSold)
str(combine$YrSold)
combine$YrSold <- as.factor(combine$YrSold)
str(combine$YrSold)
sum(table(combine$YrSold))






table(combine$SaleType)
sum(is.na(combine$SaleType))
combine$SaleType[is.na(combine$SaleType)] <- names(sort(-table(combine$SaleType)))[1]
combine$SaleType <- as.factor(combine$SaleType)



median(df_amestrain$SalePrice) # median house price = $163000






ys <- ggplot(combine[!is.na(combine$SalePrice),], aes(x=YrSold, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='green')+
  scale_y_continuous(breaks= seq(0, 800000, by=10000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
  coord_cartesian(ylim = c(0, 200000)) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice


ms <- ggplot(combine[!is.na(combine$SalePrice),], aes(x=MoSold, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue')+
  scale_y_continuous(breaks= seq(0, 800000, by=25000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
  coord_cartesian(ylim = c(0, 200000)) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice


grid.arrange(ys, ms, widths = c(1,2))
# Although possible a bit less steep than expected, the effects of the Banking crises that took place at the end of 2007 can be seen indeed. 
# After the highest median prices in 2007, the prices gradually decreased. However, seasonality seems to play a bigger role, as you can see below.




nzv <- nearZeroVar(combine, saveMetrics = TRUE)
View(nzv) #Street, Alley	, LandContour, LandSlope, Condition2, RoofMatl, BsmtCond, BsmtFinType2, BsmtFinSF2, Heating,
# LowQualFinSF, KitchenAbvGr, Functional, OpenPorchSF, EnclosedPorch, X3SsnPorch, ScreenPorch, PoolArea, PoolQC, MiscFeature
# MiscVal,     21 variables out of 80 explanatory variables seems to have a non zero variance..
# therefore we exclude these variables from our combine data


combine[ , c("Street", "Alley"	, "LandContour", "LandSlope", "Condition2", "RoofMatl", "BsmtCond", "BsmtFinType2", "BsmtFinSF2", "Heating",
             "LowQualFinSF", "KitchenAbvGr", "Functional", "OpenPorchSF", "EnclosedPorch", "X3SsnPorch", "ScreenPorch", "PoolArea", "PoolQC",
             "MiscFeature","MiscVal" )] <- list(NULL)
skim(combine) # after removal of nzv's

#----**********************************---------------- Step -4 (Graphical visualization of Data)--------------------******************************--------------------

numericVars <- which(sapply(combine[!is.na(combine$SalePrice), ], is.numeric)) #index vector numeric variables of Training set only
View(numericVars)

all_numeric_variable <- combine[ , numericVars] 
View(all_numeric_variable[!is.na(all_numeric_variable$SalePrice),]) 
correlation <- cor(all_numeric_variable[!is.na(all_numeric_variable$SalePrice),], use = "pairwise.complete.obs") # correlation of all numeric variables


#sort on decreasing correlations with SalePrice (descending order)
cor_sorted <- as.matrix(sort(correlation[,'SalePrice'], decreasing = TRUE))

#select only high corelations
CorHigh_positive <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- correlation[CorHigh_positive, CorHigh_positive] # variables of high correlation on both sides
View(cor_numVar)
# correlation Plot for only those variables 
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)
?apply

# the following variable has high correlation with sale price: 
#                                                             "OverallQual", "ExterQual", "BsmtQual","KitchenQual", "FireplaceQu"
#                                                             "GarageCars", "GarageArea", "GrLivArea", "TotalBsmtSF", "1stFlrSF",
#                                                             "TotRmsAbvGrd", "FullBath"


# I have now finally reached the point where all character variables have been converted into categorical factors 
# or have been label encoded into numbers. In addition, 3 numeric variables have been converted into factors, and 
# I deleted one variable (Utilities). As you can see below, the number of numerical variables is now 56 (including 
# the response variable), and the remaining 23 variables are categorical.





# Above Ground Living Area, and other surface related variables (in square feet)

# As I have already visualized the relation between the Above Ground Living Area and SalePrice in my initial explorations, 
# I will now just display the distribution itself. As there are more ‘square feet’ surface measurements in the Top 20, I am taking the opportunity 
# to bundle them in this section. Note: GarageArea is taken care of in the Garage variables section.

s1 <- ggplot(data= combine, aes(x=GrLivArea)) +
  geom_density() + labs(x='Square feet living area')
s2 <- ggplot(data=combine, aes(x=as.factor(TotRmsAbvGrd))) +
  geom_histogram(stat='count') + labs(x='Rooms above Ground')
s3 <- ggplot(data= combine, aes(x=X1stFlrSF)) +
  geom_density() + labs(x='Square feet first floor')
s4 <- ggplot(data= combine, aes(x=X2ndFlrSF)) +
  geom_density() + labs(x='Square feet second floor')
s5 <- ggplot(data= combine, aes(x=TotalBsmtSF)) +
  geom_density() + labs(x='Square feet basement')
s6 <- ggplot(data = combine[combine$LotArea<100000,], aes(x=LotArea)) +
  geom_density() + labs(x='Square feet lot')
s7 <- ggplot(data= combine, aes(x=LotFrontage)) +
  geom_density() + labs(x='Linear feet lot frontage')
s8 <- ggplot(data= combine, aes(x=LowQualFinSF)) +
  geom_histogram() + labs(x='Low quality square feet 1st & 2nd')

grid.arrange(s1, s2, s3, s4, s5, s6, s7, s8)


# I will investigate several of these variables for outliers later on. For the lot visualization, 
# I have already taken out the lots above 100,000 square feet (4 houses).

# GrLivArea seemed to be just the total of square feet 1st and 2nd floor. However, in a later version, 
# I discovered that there is also a variable called: LowQualFinSF: Low quality finished square feet (all floors). 
#As you can see above (Low quality square feet 1st and 2nd) almost all houses have none of this (only 40 houses do have some). 
#It turns out that these square feet are actually included in the GrLivArea. The correlation between those 3 variables and GrLivArea is exactely 1.





#The most important categorical variable; Neighborhood

#Th first graph shows the median SalePrice by Neighorhood. The frequency (number of houses) of each Neighborhood in the train set is shown in the labels.

# The second graph below shows the frequencies across all data.


n1 <- ggplot(combine[!is.na(combine$SalePrice),], aes(x=Neighborhood, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice
n2 <- ggplot(data=combine, aes(x=Neighborhood)) +
  geom_histogram(stat='count')+
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
grid.arrange(n1, n2)  # Practically it is of no use b/c first table covers only train set values & 2nd fig covers the whole data






# quality variable
q1 <- ggplot(data=combine, aes(x=as.factor(OverallQual))) +
  geom_histogram(stat='count')
q2 <- ggplot(data=combine, aes(x=as.factor(ExterQual))) +
  geom_histogram(stat='count')
q3 <- ggplot(data=combine, aes(x=as.factor(BsmtQual))) +
  geom_histogram(stat='count')
q4 <- ggplot(data=combine, aes(x=as.factor(KitchenQual))) +
  geom_histogram(stat='count')
q5 <- ggplot(data=combine, aes(x=as.factor(GarageQual))) +
  geom_histogram(stat='count')
q6 <- ggplot(data=combine, aes(x=as.factor(FireplaceQu))) +
  geom_histogram(stat='count')
q7 <- ggplot(data=combine, aes(x=as.factor(PoolQC))) +
  geom_histogram(stat='count')

grid.arrange(q1, q2, q3, q4, q5, q6, q7)




#Overall Quality is very important, and also more granular than the other variables. 
# External Quality is also improtant, but has a high correlation with Overall Quality (0.73). 
# Kitchen Quality also seems one to keep, as all houses have a kitchen and there is a variance with some substance. 
# Garage Quality does not seem to distinguish much, as the majority of garages have Q3. Fireplace Quality is in the list of high correlations, 
#and in the important variables list. The PoolQC is just very sparse (the 13 pools cannot even be seen on this scale). 
#I will look at creating a ‘has pool’ variable later on.




# The second most important categorical variable; MSSubClass
#The first visualization shows the median SalePrice by MSSubClass. 
# The frequency (number of houses) of each MSSubClass in the train set is shown in the labels.
ms1 <- ggplot(combine[!is.na(combine$SalePrice),], aes(x=MSSubClass, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice
ms2 <- ggplot(data=combine, aes(x=MSSubClass)) +
  geom_histogram(stat='count')+
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Again not a very practical visualization
grid.arrange(ms1, ms2)


#Garage variables

g1 <- ggplot(data=combine[combine$GarageCars >0, ], aes(x=GarageYrBlt)) +
  geom_histogram(stat='count')
g2 <- ggplot(data=combine, aes(x=as.factor(GarageCars))) +
  geom_histogram(stat='count')
g3 <- ggplot(data= combine, aes(x=GarageArea)) +
  geom_density()
g4 <- ggplot(data=combine, aes(x=as.factor(GarageCond))) +
  geom_histogram(stat='count')
g5 <- ggplot(data=combine, aes(x=GarageType)) +
  geom_histogram(stat='count')
g6 <- ggplot(data=combine, aes(x=as.factor(GarageQual))) +
  geom_histogram(stat='count')
g7 <- ggplot(data=combine, aes(x=as.factor(GarageFinish))) +
  geom_histogram(stat='count')


grid.arrange(g1, g2, g3, g4, g5, g6, g7)
# As already mentioned in section 4.2, GarageCars and GarageArea are highly correlated. 
# Here, GarageQual and GarageCond also seem highly correlated, and both are dominated by level =3.



#Basement variables

b1 <- ggplot(data=combine, aes(x=BsmtFinSF1)) +
  geom_histogram() + labs(x='Type 1 finished square feet')
b2 <- ggplot(data=combine, aes(x=BsmtFinSF2)) +
  geom_histogram()+ labs(x='Type 2 finished square feet')
b3 <- ggplot(data=combine, aes(x=BsmtUnfSF)) +
  geom_histogram()+ labs(x='Unfinished square feet')
b4 <- ggplot(data=combine, aes(x=as.factor(BsmtFinType1))) +
  geom_histogram(stat='count')+ labs(x='Rating of Type 1 finished area')
b5 <- ggplot(data=combine, aes(x=as.factor(BsmtFinType2))) +
  geom_histogram(stat='count')+ labs(x='Rating of Type 2 finished area')
b6 <- ggplot(data=combine, aes(x=as.factor(BsmtQual))) +
  geom_histogram(stat='count')+ labs(x='Height of the basement')
b7 <- ggplot(data=combine, aes(x=as.factor(BsmtCond))) +
  geom_histogram(stat='count')+ labs(x='Rating of general condition')
b8 <- ggplot(data=combine, aes(x=as.factor(BsmtExposure))) +
  geom_histogram(stat='count')+ labs(x='Walkout or garden level walls')

grid.arrange(b1, b2, b3, b4, b5, b6, b7, b8)


# So it seemed as if the Total Basement Surface in square feet (TotalBsmtSF) is further broken down into finished areas 
# (2 if more than one type of finish), and unfinished area. I did a check between the correlation of total of those 3 variables,
# and TotalBsmtSF. The correlation is exactely 1, so that’s a good thing (no errors or small discrepancies)!

# Basement Quality is a confusing variable name, as it turns out that it specifically rates the Height of the basement.


# Adding a variable
combine$age <- as.numeric(combine$YrSold) - as.numeric(combine$YearRemodAdd)

ggplot(data=combine[!is.na(combine$SalePrice),], aes(x=age, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)
#As expected, the graph shows a negative correlation with Age (old house are worth less).


#----**********************************---------------- Step -5 (seprate train & Test, & make validation set from Train)--------------------******************************--------------------

train_dataset <- combine[!is.na(combine$SalePrice),] # 59 variables
test_dataset <-  combine[is.na(combine$SalePrice),]  # 58 variables


skim(train_dataset)


test_dataset$SalePrice <- NULL

skim(test_dataset)


 

inTrain <- createDataPartition(train_dataset$SalePrice, p = 0.7, list = FALSE) #indices of train Data set (70% training & 30% validation)
# it gives index nos of rows and classify them which goes to train & which goes to validation 
# Also it must also need to partition the response variable accordingly, therefore we use "train_dataset$SalePrice"
train <- train_dataset[inTrain, ]           # 70% rows specified by inTrain
test <- train_dataset[-inTrain,]      # Remaining 30% rows specified by inTrain, which were not in train & 
#                                      should eliminate the column "SalePrice", for validation purposes

skim(train)

#-------------------- Step -6a (Linear Modelling)----------------------------------------

lm_1 <- lm(SalePrice ~. , train)
summary(lm_1)

step_lm_1 <- stepAIC(lm_1, direction = 'both') # all significant variables with lowest AIC are found in lm_2
summary(step_lm_1) # with StepAic, we get the following model "lm_2" with lowest AIC


vif(step_lm_1)  # removing in lm_2---- GarageQual + GarageCond + X1stFlrSF +Foundation + Neighborhood +Exterior1st +




lm_2 <- lm(SalePrice ~ MSZoning + LotShape + LotConfig + 
             Condition1 + BldgType +  OverallQual + OverallCond + 
             RoofStyle + HouseStyle +  MasVnrArea + ExterQual + 
              BsmtQual + BsmtExposure + BsmtFinType1 + TotalBsmtSF + 
             HeatingQC + GrLivArea + BsmtFullBath + BsmtHalfBath + 
             FullBath + HalfBath + BedroomAbvGr + KitchenQual + Fireplaces + 
             GarageCars +  WoodDeckSF + SaleType, data = train) 

summary(lm_2) # 83.08% variation in Response is explained by model


plot(lm_2) #Removing rows 2642(1183), 1983 (524), 2758(1299), 2151, 2263, 2506 to avoid outlier influence
View(train)
which(train$Id == 1299)
lm_updat <- update(lm_2, subset = -c(377, 827, 915))
plot(lm_updat)






# Checking the performance of Linear Regression with RMSE value.
prediction_1 <- predict(lm_updat, newdata = test, type = 'response')
model_output <- cbind(log(test$SalePrice), log(prediction_1)) # our desired output
View(model_output) # seems quite wild predictions
sum(is.na(model_output)) # since there is no na


rmse(log(test$SalePrice), log(prediction_1)) #---------- thus rmse turns out to be 20.8% approx --- 
 






# PREDICTION
submission_predictions <- predict(lm_updat, newdata = test_dataset, type = 'response') # prediction prices
prediction_file <- tibble(Id = test_dataset$Id, SalePrice = submission_predictions)    # format of a csv file as per competition description
write_csv(prediction_file, "submission.csv")


#-------------------- Step -6b (LASSO Regression)----------------------------------------

# For the avoidance of multicollinearity, implementing LASSO regression is not a bad idea. 
# Transferring the variables into the form of matrix, we can automate
# the selection of variables by implementing 'lars' method in Lars package.


skim(train)
View(train)
independent_vars <- as.matrix(train[, 2:58]) # b/c 1st variable is ID & last variable is a Response variable
dependent_var <- as.matrix(train[ , 59]) 

View(independent_vars) 
View(dependent_var)

laa <- lars(independent_vars,dependent_var, type = 'lasso') ># cannot run lasso regression as it has srtings in the matrix
?lars


#-------------------- Step -6c (Random Forest)----------------------------------------

# The other model I chose to fit in the training set is Random Forest model. 
# The model, prediction and RMSE calculation can be found below:


rf <- randomForest(SalePrice ~., data = train ) # we have catagorical variables in our data, therefore it cannot handle it






#-------------------- Step -6d (XGBoost)----------------------------------------
# The first step of XGBoost is to transform the dataset into Sparse matrix.

train_inner <- as.matrix(train, rownames.force = NA)
test_inner <- as.matrix(test,rownames.force = NA)
train_inner <- as(train_inner, "sparseMatrix")
test_inner <- as(test_inner, "sparseMatrix")


























































































































































































