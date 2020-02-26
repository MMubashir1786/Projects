# Overview:
# Most used Lib ---> Tidyverse, ggplot2, "Openintro" for dataset, tree  rpart &  rattle for ML

# Task Completed:
#                Data Analysis (Through RFM and other Analysis---- first eyeball the Data itself)
#                Stats (Mean, SD, sampling dist, CLT, normal & T-dist, Confidence Interval )
#                Data Visualization (boxplot, Histogram)
#                Data Interpretation (Store Data, ncbirth)
#                ML (Only Decision Tree)
#                Data Cleaning and making it Tidy for work (using; filters, spread, gather, seperate, unite, arrange, mutate, unique)
#                Regression (Linear Modelling --- predicting the predictors and their relationship with Response variable)


install.packages("rjson")

# =============================================== Libraries necessary for the work ============================================================
library(tidyverse)
library(ggplot2)
library(openintro)
library(statsr)
library(jsonlite)
library(rjson)


 

getwd()  # to check working directory

?rjson
?jsonlite


# ============================================== Data Analysis ================================================================================


Data <- fromJSON("G:/Data Analysis with R (Certificate) SSUET/ClassFiles/Data/storeData.json")

Data <- read_csv("G:/Data Analysis with R (Certificate) SSUET/ClassFiles/Data/stordata.csv")
View(Data)


# Adding a Subtotal column in the Data frame along with removing extra rows without Customer ID & changing columns nature
Data %>% 
  mutate(Sub_Total = Quantity * UnitPrice) %>% 
  drop_na(CustomerID)-> Data     
View(Data)

str(Data)

?strptime
Data$CustomerID <- as.character(Data$CustomerID)
Data$InvoiceDate <- as.Date(Data$InvoiceDate, format = "%m/%d/%Y")

str(Data)


# Invoice Totals
Data %>% 
  group_by(InvoiceNo) %>% 
  summarise(Total = sum(Sub_Total)) %>% 
  View

# Country Totals along with the Invoice & Customer Counts along with avgs
Data %>% 
  group_by(Country) %>% 
  summarise(Total_Sales = sum(Sub_Total),
            Total_Invoices = n_distinct(InvoiceNo),
            Avg_Invoice = (Total_Sales/Total_Invoices),
            Total_Customers = n_distinct(CustomerID),
            Avg_Customer_Spending = (Total_Sales/Total_Customers)) %>% 
  arrange(desc(Total_Sales)) %>% 
  View

# Country Analysis : Avg Invoice and no of distinct products sold in each country

Data %>% 
  group_by(Country, InvoiceNo) %>% 
  summarise(Total = sum(Sub_Total),
            n_distinctProducts = n()) -> InvCon

InvCon %>% 
  ungroup() %>% 
  group_by(Country) %>% 
  summarise(AvgInvoice = mean(Total),
            n_Sales = n()) %>% View


  
# Country Analysis: first, last & max Invoice
Data %>% 
  group_by(Country, InvoiceNo) %>% 
  summarise(Total = sum(Sub_Total)) %>% 
  ungroup() %>% 
  group_by(Country) %>% 
  summarise(FirstInvoiceNo = first(InvoiceNo),
            FirstInvoiceTotal = first(Total),
            maxInvoice = max(Total)) %>% 
  View

# RMF Analysis
?filter
Data %>% 
  group_by(CustomerID) %>% 
  summarise(LastPurchaseDate_Recency = max(InvoiceDate),
            TotalSales_Monetary = sum(Sub_Total),
            noofVisits_Frequency = n_distinct(InvoiceNo),
            AvgRevperVisit = (TotalSales_Monetary/noofVisits_Frequency))-> rfm_1
summary(rfm_1)
# Since there might be some descripencies, therefore we can alter our rfm_1 DF
# no negative sales value, as it hurts our analysis
rfm_1 %>% 
  filter(TotalSales_Monetary >=0) ->rfm_1   # filter -- kill the rows which does not fulfill the given criteria
summary(rfm_1)

rfm_1 %>% 
  ggplot(aes(TotalSales_Monetary))+
  geom_histogram(binwidth = 100)    #the graph appears to be very small,

#Therefore, we filter the total sales till 10000

rfm_1 %>%
  filter(TotalSales_Monetary < 10000) %>% 
  ggplot(aes(TotalSales_Monetary))+
  geom_histogram(binwidth = 100)    #the graph appears to be skewed rightwards,

library(Hmisc)
?cut2
# Associate each customer with a bin according to the sales Revenue so we can perform a loyalty Test
vec_valueBins <- cut2(rfm_1$TotalSales_Monetary, g=5)
View(vec_valueBins)
levels(vec_valueBins)
table(vec_valueBins)     # Summary of a catagorical variable
rfm_1$TRBin <- vec_valueBins

class(rfm_1$TRBin)
levels(rfm_1$TRBin)

table(rfm_1$TotalSales_Monetary)


# Associate each customer witha bin according to the no of visits (frequency) so we can perform a loyalty Test

rfm_1$frequencyBin <- cut2(rfm_1$noofVisits_Frequency, g = 5)
table(rfm_1$frequencyBin)

View(rfm_1)


# Adding no of Days since Last Purchase and associate a Bin to it
max(rfm_1$LastPurchaseDate_Recency)

rfm_1$DaysSinceLastPurchase <- as.Date("2011-12-10")- rfm_1$LastPurchaseDate_Recency # this contains a lot of Details, as we only need no of Days

View(rfm_1)

class(rfm_1$DaysSinceLastPurchase)

library(lubridate)
class(days(rfm_1$DaysSinceLastPurchase)$day)

rfm_1$DaysSinceLastPurchase <- days(rfm_1$DaysSinceLastPurchase)$day # this way we can get our required numeric class as opposed to difftime

class(rfm_1$DaysSinceLastPurchase) # Numeric

rfm_1$LastPurBin <- cut2(rfm_1$DaysSinceLastPurchase, g=5) # Bin created
table(rfm_1$LastPurBin)



View(rfm_1)








# ======================================== Stats, Data Visualization & Interpretation ===============================================================



data("ncbirths")
View(ncbirths)

# Distribution of babyweight by Mother's habit
ggplot(ncbirths, aes(x = habit, y = weight))+
  geom_boxplot()                             # this shows that median weight of a baby is slightly larger than for NS than S, however almost data overlap except NS Moms has numerious outliers

# Distribution of baby weight (Histogram ---- to view the Distn of a continous variable)
ggplot(ncbirths, aes(x=weight))+
  geom_histogram()              # this suggest that max no baby weight lies b/w 5-10, also shape is slightly constituted to Normal Distn with a long left tail


data("diamonds")
View(diamonds)
summary(diamonds)

diamonds %>% 
  ggplot(aes(x=carat, y=price))+
  geom_point()
  
diamonds %>% 
  ggplot(aes(x=cut, y=price))+
  geom_boxplot()  # this box plot shows many outliers in each catagory, dipiciting a fact that there are numerious other factors for prices to be determined


# ------ CLT------ (if X1, X2, ..., Xn are the random samples drawn from a population which has a mean = mu & variance = (sigma)^2, then according to CLT distn of Samples follows a Normal distn ~ (mu, (sigma)^2/sqrt(n)), where n is a size of a Random Sample)   ------ sd = sqrt(variance)
data("yrbss")  # Population --- physically_active_7d
View(yrbss)
str(yrbss)
??na.rm

pop_mean <- mean(yrbss$physically_active_7d, na.rm = T)
pop_sd <- sd(yrbss$physically_active_7d, na.rm = T)


# We use "pnorm" for finding Normal probabilities & "qnorm" for finding quantile in Normal distn

# ------- Confidence Interval -------

# xbar +- z* * s.e.                s.e = sd of sample
#6.88 + c(-1, 1) * qnorm(0.975) * 0.94/sqrt(169)


# When we are given a sample, sometimes variance of a population is not given (so sd is impossible to calculate) therefore; in such a case we replace the sample variance (S) in place of sigma
# this makes (Xbar-mu/(S/sqrt(n))) and we use it for random samples from Normal distn.
# Thus t-distn is T=(Xbar-mu/(S/sqrt(n))) with degree of freedom n-1


# test statistic : 32, sd = 10 , n = 16
# t-statistic: (xbar - mu)/se | s.e = S/sqrt(n)


# confidence intervals: ####
# Load the data set mtcars in the datasets R package. 
# Calculate a 95% confidence interval to the nearest MPG 
# for the variable mpg.
data("mtcars")
str(mtcars)
Xbar <- mean(mtcars$mpg)
S <- sd(mtcars$mpg)
length(mtcars$mpg) # How many rows
z <- qnorm(0.975)  # usually we use Normal distn for CI but here we have se instead of sd, therefore we use t-distn
t <- qt(0.975, nrow(mtcars)-1)
Xbar + c(-1,1) * t * (S/sqrt(nrow(mtcars)))


# ncbirths: test if difference of weights between babies of S and NS is real, or because of sampling variability
?t.test
t.test(weight ~ habit, data = ncbirths)




# --------- binomial & poisson-----------------
?pbinom
?ppois # The time is an important factor in Poisson Distn







# ======================================== ML ==================================================================================================================
library(tree)
library(rpart)
library(rattle)



data("iris")
?iris
View(iris)
iris %>%
  summarise(avgSepalLength = mean(Sepal.Length),
            avgSepalWidth = mean(Sepal.Width),
            avgPetalLength = mean(Petal.Length),
            avgPetalWidth = mean(Petal.Width))

ggplot(iris, aes(y = Sepal.Length)) +
  geom_boxplot()

# grouped by species
iris %>%
  group_by(Species) %>% 
  summarise(avgSepalLength = mean(Sepal.Length),
            avgSepalWidth = mean(Sepal.Width),
            avgPetalLength = mean(Petal.Length),
            avgPetalWidth = mean(Petal.Width))

ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_boxplot()


# observing measurements in two dimensions
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                 color = Species)) +
  geom_point()

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                 color = Species)) +
  geom_point()

ggplot(iris, aes(x = Petal.Width, y = Sepal.Width, 
                 color = Species)) +
  geom_point()

ggplot(iris, aes(x = Petal.Width, y = Sepal.Width, 
                 color = Species)) +
  geom_point(size = 4)



tree1 <- tree(Species ~ Sepal.Width + Petal.Width, data = iris)
summary(tree1)
plot(tree1)
text(tree1)


plot(iris$Petal.Width, iris$Sepal.Width, pch=19, col=as.numeric(iris$Species))
partition.tree(tree1, label = "Species", add = TRUE)

# observe partitions creatd by decision tree

graph <- qplot(Petal.Width, Sepal.Width, data = iris, 
               color = Species, size = I(4))

graph + 
  geom_hline(aes(yintercept = 2.65)) +
  geom_vline(aes(xintercept = 0.8)) + 
  geom_vline(aes(xintercept=1.75)) + 
  geom_vline(aes(xintercept=1.35))

# using more variables
# 
tree2 <- tree(Species ~ Sepal.Width + Sepal.Length + 
                Petal.Length + Petal.Width, data = iris)
summary(tree2)

plot(tree2)
text(tree2)

# lets check
iris[iris$Petal.Length<2.45,5]
length(iris[iris$Petal.Length<2.45,5])

iris[iris$Petal.Length>2.45&iris$Petal.Width>1.75,5]
length(iris[iris$Petal.Length>2.45&iris$Petal.Width>1.75,5])

iris[iris$Petal.Length>2.45&iris$Petal.Width<1.75&iris$Petal.Length>4.95,5]

#viewing the Petal.Length of all species
boxplot(formula=Petal.Length ~ Species, data=iris, 
        xlab="Species", ylab="Petal length")

rpart <- rpart(Species ~ ., data=iris, method="class")
rpart

fancyRpartPlot(rpart, main="Iris")



















# =============================================== Data Cleaning ============================================================

# My WHO Assignment























































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































