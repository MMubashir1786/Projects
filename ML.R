# Overview:
# Frequently used Libs: Tidyverse, tree, rpart, rattle, kernlab, animation

# Task Completed:
#                 Machine Learning (Basics) 
#                 Overfitting_InOutSample_evaluation
#                 k-means and Clusterring
#                 Logistic Models, Data Cleaning
install.packages("caret")
install.packages("doParallel")

# *************************** Libraries*****************************************************************************

library(tidyverse)
library(kernlab)
library(animation)
library(tree)
library(rpart)
library(rattle)
library(broom)
library(caret)
library(doParallel)

getwd()



# =============================================================== Machine Learning (Basics --- Decision Trees) ========================================================================
data("iris")
?iris
View(iris)
iris %>%
  summarise(avgSepalLength = mean(Sepal.Length),
            avgSepalWidth = mean(Sepal.Width),
            avgPetalLength = mean(Petal.Length),
            avgPetalWidth = mean(Petal.Width))  # we get the average of 4

ggplot(iris, aes(y = Sepal.Length)) +
  geom_boxplot()  # A usual boxplot 


# grouped by species
iris %>%
  group_by(Species) %>% 
  summarise(avgSepalLength = mean(Sepal.Length),
            avgSepalWidth = mean(Sepal.Width),
            avgPetalLength = mean(Petal.Length),
            avgPetalWidth = mean(Petal.Width))

ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_boxplot()    # The Observation: Although averages are different, however, 
#                     few (b/c observations on whiskers complements the box) similar observation can be seen b/w  setosa & versicolor 
#                     many (b/c portion of boxes seems to have same observations)  similar observation can be seen b/w versicolor & virginica 

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                 color = Species)) +
  geom_point()                       # This complements our earlier interpretation, as a saperate pattern can be seen for setosa, however,
#                                      mixed observations in the interval of 5.5 - 7 b/w versicolor & virginica





ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                 color = Species)) +
  geom_point()

ggplot(iris, aes(x = Petal.Width, y = Sepal.Width, 
                 color = Species)) +
  geom_point()  # Here we can clearly differentiate b/w the 3

ggplot(iris, aes(x = Petal.Width, y = Sepal.Width, 
                 color = Species)) +
  geom_point(size = 4)



# Making a decision Tree for Petal.Width & Sepat.Width having Species as response variable

# Illustration -1
tree1 <- tree(Species ~ Sepal.Width + Petal.Width, data = iris)
summary(tree1)
plot(tree1)
text(tree1) # We are sharing our observation here with this basic ML techenique...
#            if petal.width <0.8, (as we have seen in the graph) ----- setosa (as it was showing a saperate trend for itself)
#            In other case, again 2 cases here ... if Petal.width is >= 1.75 then virginica (clearly seen from greph)
#            with petal.width <1.75, again 2 classifications.... (as can be observe from graph, where a section contains points of versicolor & virginica)


plot(iris$Petal.Width, iris$Sepal.Width, pch=19, col=as.numeric(iris$Species)) # graph for Illustration of the Decision Tree above
partition.tree(tree1, label = "Species", add = TRUE)                           # classification of decision tree (creating partitions) onto graph




# we have made our Decision tree manually
graph <- qplot(Petal.Width, Sepal.Width, data = iris, 
               color = Species, size = I(4))

graph + 
  geom_hline(aes(yintercept = 2.65)) +
  geom_vline(aes(xintercept = 0.8)) + 
  geom_vline(aes(xintercept=1.75)) + 
  geom_vline(aes(xintercept=1.35))







# Illustration -- 2
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




# Illustration -- 3 (Decision Trees using Rattle & Rpart)
#viewing the Petal.Length of all species
boxplot(formula=Petal.Length ~ Species, data=iris, 
        xlab="Species", ylab="Petal length")


rpart <- rpart(Species ~ ., data=iris, method="class")
rpart

fancyRpartPlot(rpart, main="Iris")





# here output is classification (predicting spicies)





# =============================================================== K-means and Clusterring (Unsupervised learning) ========================================================================



ggplot(iris, aes(x = Petal.Width, y = Sepal.Width)) +
  geom_point()
ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) +
  geom_point()
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point()
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point()




ggplot(iris, aes(x = Petal.Width, y = Sepal.Width, 
                 color = Species)) +
  geom_point(size = 4)              # with colors... 3 obvious colors
ggplot(iris, aes(x = Petal.Width, y = Sepal.Width)) +
  geom_point(size = 4)              # Appearently 2 obvious clusters... one large & other one is small

# In cluster Analysis, there is no definite answer....but it has several advantegous...
# we can segment the data into different clusters on the basis of behaviour
# e.g: is there a consumption of certain type of product is popular  in a cluster than other cluster


# Example (Practicle example by Sir): (Travel website selling experiences) 2 day trip to churna Island, 2 day trip to Gorakh Hill.. etc
#          hypothesis: travellers like trekking have certain type of behaviour, love water have their type of behaviour, and similarly for snow..
#          we didnot interfare with the group forming of making specific group, instead we let data to decide to make a cluster itself...based 
#          on the description of the products tey viewed..
#          we know since each visitor is not a buyer.. but each cluster has a combination of visitors & buyers...
#          our job is to analyze which products were sold max in each clusters... this lead us to know what experience was more popular and in which cluster??
#          Purpose: to make customer see what they wanted to buy and ultimatly convert the visitor into a customer



# Cluster Analysis : K-means algorithm is the most common and intutive method use for cluster Analysis 
# k-means is a built-in function

md=iris[,-5]   #excluding 5th column
set.seed(10)   # always starts from different positions, b/c of set.seed 
KM2=kmeans(as.matrix(md), centers = 2, iter.max = 35,nstart = 10) # need to make matrix (md) of relevent observations b4 kmeans
print(KM2)    # made 2 clusters
# we have excluded column 5 & store it in md...
KM3=kmeans(as.matrix(md), centers = 3, iter.max = 35,nstart = 10)
print(KM3)    # made 3 clusters

kmeans.ani(as.matrix(md), centers = 2) # makes atmost 35 iterations for 2 clusters, and for each iteration mean changes, cluster might changes...


kmeans.ani(as.matrix(md), centers = 3)# makes atmost 35 iterations for 3 clusters, and for each iteration mean changes, cluster might changes...

# all the poins remains intact, until the large point randomly starts from any given observation & since it is a random initialization, therefore...
#... it can choose how to initiate cluster, means everytime it initializes with different set of observations. Also each cluster has different.....
#.... figures & colors to (visually) identify different cluster


# this unsupervised learning is depend upon us giving it a no of class....








# =============================================================== Logistic Models, Data Cleaning (ml_caret) ========================================================================

# Logistic Regression: where output variable (in the boundries of regression techeniques) is a binary or catagorical variuable (e.g: pass/fail, male/female...)
# Agenda: Into classification section of Regression, we will take a ML problem (all the process defined in libraries) and analyze area ...
#         under the curve (and confusion matrix), with the convinience of libriries..


# For Article: b/c upuntil today, much work has been automated. due to this, making algorithm seems more expensive interms of time & ...
#              money as there is no need to revint the wheel again. also prior processes of making algorithms such as accuracy validation,...
#              ect, whenever a computer scientist develops any clissifier, they also made these processes.
#              Due to this they made libraries, through which we need to execute our processes ONLY, no need to reinvent a wheel...
#              therefore, manual work only exists where it is required to develop stastical logics, such as regression Analysis.
#              Although, multicollinearity can be checked with vif's, Influences can be checked with Cook's D chart, etc.. 
#              but we need to manually see  for the sake of analysis the charts and residual plots to identify Influence etc, 
#              However, when practicing ML in Real world, no need to learn algorithms & matrix calculations, almost every aspect...
#              in ML is up to maturing point and rapid prototyping is only a necessary tool which can be learn by individual ..
#              today to apply ML in real world (first of all learn to make appropriate features, to identify the process, model fitting & value interpretation)
#              b/c it requires intelligence..

# Y = a + b1x1 + b2x2+......+bnxn
# in a typical regression Model, a response variable is a numerical variable (on a continous scale) ---- 
# what we need??? response var is a catagorical variable --- which is not a number (for logistic regression)
# A logistic regression is a Generalized Linear Model (GLM) --- where a numerical response variable is transformed logically (log of odds)
# this made y --- P(y = 1 --- Positive/ Male/Pass ) & P(y = 0 -- neative/ female/ fail) --- however it is upto us where we want to decide 
# to make the y "0" or "1". 
# Recivers Operator characterstic (ROC) curve: It is to only be made on binary classification & use to evaluate an ML model
#                                             x-axis (threshold for making a positive response) & y-axis (1 - positive predicted value)  --- 
#                                             positive predicted value from confision matrix
# If auc> o.5, than ROC curve is not a diagonal line. it must be a curve with a bend




# Illustration ---01

data("PimaIndiansDiabetes2", package = "mlbench")

View(PimaIndiansDiabetes2)
# there are different set of readings in the dataset, and response variable is a catagorical variable (diabetes --- pos / neg) -- 
# a binary classification (whether a person got diabetes or not with the given variables)
# pregnant means, how many pregnancy occured.. appearently we can see several N/A's
# the data has actual test results, along with other variables, from which we can predict the existence of diesease 


# In order to predict, we need a model without N/A's
#PimaIndian <- na.omit(PimaIndiansDiabetes2)  OR
PimaIndiansDiabetes2 %>% 
  drop_na() -> pimaindian   # "drop.na" ---from tidyverser   ---- this name is camel case, there also a snake case (with "_")

# Fitting logistict model
model <- glm(diabetes ~., data = pimaindian,
             family = binomial)    # b/c response is a binary (b/c link function is logit (Burnolli) function)
# Why Binomial? use for counts of binary variables (this much +ve's & this much -ve's)
# when we are deciding whether the case is +ve or -ve -------- always use family = Binomial




#Predictions of the GLM (logistic model)
head(predict(model)) #log values --- we know the response is the probability b/w 0 & 1, but the outcome does not seems probability
# therefor we use the following function:
head(predict(model, type = "response")) # we specify here we need probability (interested in the response of probabilitiues)




head(fitted(model))




#   Unfinished.....




















# Illustration 02

df_trainData <- read.csv("G:/Data Analysis with R (Certificate) SSUET/ClassFiles/Data/pml-training.csv")  # less error 
# (less Root Mean Square Error (RMSE) in ML lingo)  ---- highly processed Dataset (no need to change anything in dataset)
#  Already features are given... b/c each variable might be according to timestamps, to which the distributionn of each variable is drived and..
# features of that distribution:Total 
#                               kurtosis (measuremen of the peak of distn), 
#                               skewness(tail of distn), 
#                               max, 
#                               min, 
#                               amplitude, 
#                               var,
#                               mean, 
#                               sd,  
# recorded, these features are describing the data, b/c it is a cleaned and shaped dataset
# millions of rows sumed up to 19k rows & each row represents a user's rawtimestamp2 with its related variables
# 
df_testData <-  read.csv("G:/Data Analysis with R (Certificate) SSUET/ClassFiles/Data/pml-testing.csv") # We never use Test Data unless we go through Training & then validation



View(head(df_trainData))
# Data extracted & cleaned, taken from fit-bit devices (Readings taken while walking, sitting, readings, 
# ect) which has accelorometer, ect
# If any pattern can be drived after traning a model, then a model can efficiently predicts in which position are u right now?

# We know from Regression models that fitted values are the prediction of our model based on the observations we have 
# (b/c betas are predicted) ---- upon which we have trained our  dataset onto & we define "error = observe - fitted"
# This is known as "in sample prediction" or "training set prediction" ---- however, usually prediction on training dataset 
# turn out to be very great!! b/c the model has been trained on that particular dataset.. RMSE is minimal, Accuracy is very good
# Therefore, we divide the whole dataset into 80-20 (60+20 for training + validation & 20 for Testing of model to evaluate model performance)
# Training: split that 80 into 60 & 20 (60 for training & 20 for validation) to avoid the biasness/ overfitting the model on test data
#           and in order to preserve the accuracy of the model on test dataset. there are lib to bifercate the training set into 2, for validation purpose


# if our Model is biased towards negative (means usually it predicts -ve and few times +ve), then model can be credible only if 
# our positive predictive value is high, b/c if model flags 5 out of 50 patients means they must have virus symptoms. this also means 
# the sensivity of our model is quite low (as it does not consider taking other 45 into its consideration). In other way round, if 
# sensitivity was high and positive predictive value is low then model is crap. 

# In ROC (Recievers Operators characterstic) curve: (x-axis "threshold" & y-axis "1 - Positive predictive value"), 
# we Interpret the importance of Model and auc > 0.5 to make it efficient (and better than the coin flip)
# In ROC curve we look for a bump above the diagonal line.


# to eliminate N/A's
View(colMeans(is.na(df_trainData)))  # This shows the proportion of N/A's bieng True(1) in certain column, after converting it to true & False 
#(if near to 1 then eliminate that column), also analyze the N/A's, as they migh convert it to credible info. though this is not a real dataset, 
# thereforewe drop these variables.
varsToDrop <- names(colMeans(is.na(df_trainData)))[colMeans(is.na(df_trainData)) > 0.9]

df_trainData %>% 
  select(-varsToDrop) -> df_trainData
df_testData %>% 
  select(-varsToDrop) -> df_testData
# Have eliminated from test & train both.




# Caret: useful for ML in R as it has all the convinience functions. (e.g Confusion Matrix, nearzero variance, etc)

# now we analyze near-zero var variables, and eliminate them..(var of that variable = 0)
nearZeroVar(df_trainData, saveMetrics = TRUE)
vars_nzv <- nearZeroVar(df_trainData, saveMetrics = TRUE)$nzv 
View(vars_nzv)# Vector for True & False
# I preserve only those columns (cells in this case) which has nzv = "FALSE" & eliminate "TRUE's" 

df_trainData <- df_trainData[, vars_nzv==FALSE] # eliminating nzv's from train Data'
df_testData <- df_testData[, vars_nzv==FALSE]   # eliminating nzv's from test Data
df_trainData <- df_trainData[,-1]               # eliminate first column, b/c it is row index nos
df_testData <- df_testData[,-1]                 # eliminate first column, b/c it is row index nos



# Now I am spliting the training dataset into train & validation, so I can check my model
# there is a separate test data there... No need to create one

inTrain <- createDataPartition(df_trainData$classe, p = 0.7, list = FALSE) #indices of train Data set (70% training & 30% validation)
# it gives index nos of rows and classify them which goes to train & which goes to validation 
# Also it must also need to partition the response variable accordingly, therefore we use "df_trainData$classe"
train <- df_trainData[inTrain, ]           # 70% rows specified by inTrain
test <- df_trainData[-inTrain,]      # Remaining 30% rows specified by inTrain, which were not in train & 
#                                      should eliminate the column "classe", for validation purposes



cl <- makePSOCKcluster(3)
registerDoParallel(cl) # It will Instruct caret to register the above no of cores (in this case - 3) & execute multiple process on these cores
 
mdl_rf <- train(classe ~ ., method = "rf", data = train) #Random Forest 
mdl_rf                                                   # it will train the model simultenously for evey given variable, and gives out the best parameters as a result       
confusionMatrix(test$classe, predict(mdl_rf, test))
save(mdl_rf, file = "mdl_rf.RData")

mdl_gbm <- train(classe ~ ., method = "gbm", data = train)
mdl_gbm
confusionMatrix(test$classe, predict(mdl_gbm, test))

confusionMatrix(test$classe, predict(mdl_xgb, test))
save(mdl_xgb, file = "mdl_xgb.RData")


















































































































































































































































































































































































