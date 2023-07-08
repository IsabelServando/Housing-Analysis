##CLASS PROJECT
## ISABEL SERVANDO

## 1. ACCESS THE DATA SET
## set working directory
setwd("/Users/isabelservando/Downloads/R FILES ")

## load data set
housing <- read.csv("housing.csv") 
## 20640 observations, 10 variables

## view variables of data set
str(housing)

## cast as factor
housing$ocean_proximity <- as.factor(housing$ocean_proximity)

## display levels of the variable
levels(housing$ocean_proximity)
## "<1H OCEAN"  "INLAND"     "ISLAND"     "NEAR BAY"   "NEAR OCEAN"

##2. EDA and Data Visualization

## a. check head of the dataset
head(housing)

##   longitude    latitude    housing_median_age total_rooms total_bedrooms population households median_income
## 1   -122.23    37.88                 41         880            129        322        126        8.3252
## 2   -122.22    37.86                 21        7099           1106       2401       1138        8.3014
## 3   -122.24    37.85                 52        1467            190        496        177        7.2574
## 4   -122.25    37.85                 52        1274            235        558        219        5.6431
## 5   -122.25    37.85                 52        1627            280        565        259        3.8462
## 6   -122.25    37.85                 52         919            213        413        193        4.0368

##    median_house_value ocean_proximity
## 1          452600        NEAR BAY
## 2          358500        NEAR BAY
## 3          352100        NEAR BAY
## 4          341300        NEAR BAY
## 5          342200        NEAR BAY
## 6          269700        NEAR BAY

## check tail of the dataset
tail(housing)
##           longitude latitude housing_median_age total_rooms total_bedrooms population households
##   20635   -121.56    39.27                 28        2332            395       1041        344
##   20636   -121.09    39.48                 25        1665            374        845        330
##   20637   -121.21    39.49                 18         697            150        356        114
##   20638   -121.22    39.43                 17        2254            485       1007        433
##   20639   -121.32    39.43                 18        1860            409        741        349
##   20640   -121.24    39.37                 16        2785            616       1387        530

##               median_income median_house_value ocean_proximity
##   20635        3.7125             116800          INLAND
##   20636        1.5603              78100          INLAND
##   20637        2.5568              77100          INLAND
##   20638        1.7000              92300          INLAND
##   20639        1.8672              84700          INLAND
##   20640        2.3886              89400          INLAND

## b.check summary
summary(housing) 

## longitude         latitude     housing_median_age  total_rooms    total_bedrooms     population   
## Min.   :-124.3   Min.   :32.54   Min.   : 1.00      Min.   :    2   Min.   :   1.0   Min.   :    3  
## 1st Qu.:-121.8   1st Qu.:33.93   1st Qu.:18.00      1st Qu.: 1448   1st Qu.: 296.0   1st Qu.:  787  
## Median :-118.5   Median :34.26   Median :29.00      Median : 2127   Median : 435.0   Median : 1166  
## Mean   :-119.6   Mean   :35.63   Mean   :28.64      Mean   : 2636   Mean   : 537.9   Mean   : 1425  
## 3rd Qu.:-118.0   3rd Qu.:37.71   3rd Qu.:37.00      3rd Qu.: 3148   3rd Qu.: 647.0   3rd Qu.: 1725  
## Max.   :-114.3   Max.   :41.95   Max.   :52.00      Max.   :39320   Max.   :6445.0   Max.   :35682  
##                                                                     NA's   :207       

##  households     median_income     median_house_value   ocean_proximity
## Min.   :   1.0   Min.   : 0.4999   Min.   : 14999     <1H OCEAN :9136  
## 1st Qu.: 280.0   1st Qu.: 2.5634   1st Qu.:119600     INLAND    :6551  
## Median : 409.0   Median : 3.5348   Median :179700     ISLAND    :   5  
## Mean   : 499.5   Mean   : 3.8707   Mean   :206856     NEAR BAY  :2290  
## 3rd Qu.: 605.0   3rd Qu.: 4.7432   3rd Qu.:264725     NEAR OCEAN:2658  
## Max.   :6082.0   Max.   :15.0001   Max.   :500001   

## c.Perform a correlation analysis on numeric variables in the data frame.

## install packages needed for subsetting
install.packages("tidyverse")
library(dplyr)

## create subset of all numerical variables for correlation
housingcor <- housing %>%     
  select (c("latitude", "longitude","housing_median_age","total_rooms",
            "total_bedrooms", "population", "households","median_income",
            "median_house_value")) 

## numeric correlation
cor(housingcor)

##                    latitude    longitude     housing_median_age total_rooms total_bedrooms   population
## latitude            1.00000000  -0.92466443         0.01117267 -0.03609960     NA          -0.108784747
## longitude          -0.92466443   1.00000000        -0.10819681  0.04456798     NA          0.099773223
## housing_median_age  0.01117267  -0.10819681         1.00000000 -0.36126220     NA          -0.296244240
## total_rooms        -0.03609960   0.04456798        -0.36126220  1.00000000     NA          0.857125973
## total_bedrooms              NA          NA                 NA          NA      1           NA
## population         -0.10878475   0.09977322        -0.29624424  0.85712597     NA          1.000000000
## households         -0.07103543   0.05531009        -0.30291601  0.91848449     NA          0.907222266
## median_income      -0.07980913  -0.01517587        -0.11903399  0.19804965     NA          0.004834346
## median_house_value -0.14416028  -0.04596662         0.10562341  0.13415311     NA          -0.024649679

##                  households median_income median_house_value
## latitude           -0.07103543  -0.079809127        -0.14416028
## longitude           0.05531009  -0.015175865        -0.04596662
## housing_median_age -0.30291601  -0.119033990         0.10562341
## total_rooms         0.91848449   0.198049645         0.13415311
## total_bedrooms      NA            NA                 NA
## population          0.90722227   0.004834346        -0.02464968
## households          1.00000000   0.013033052         0.06584265
## median_income       0.01303305   1.000000000         0.68807521
## median_house_value  0.06584265   0.688075208         1.00000000

##high negative correlation: latitude to longitude
##high positive correlation: households to population
##high positive correlation: total_rooms to population and households
## moderate positive correlation: median_income to median_house_value
## everything else doesn't have a high correlation

## remove scientific notation, since data set has large values
options(scipen = 999) 

## d) Create histograms for each numeric variable
## HISTOGRAM 1: LATITUDE
## USING BASE R
hist(housing$latitude, xlab = "Latitude", main ="Histogram: Latitude", col='darkolivegreen') 

## USING GGPLOT
## install.packages
library(ggplot2)
ggplot(housing, aes(x=latitude)) + 
  geom_histogram(fill="darkolivegreen4", alpha=10, position="identity",bins=40) +  
  theme_minimal()  + 
  labs(x="LATITUDE", title="HISTOGRAM: LATITUDE",y="COUNT") + 
  scale_y_continuous(breaks=seq(0, 6000, by = 500)) + 
  scale_x_continuous(breaks=seq(30, 50, by = .5)) +  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

## HISTOGRAM 2: LONGITUDE
## USING BASE R
hist(housing$longitude,xlab = "Longitude", main ="Histogram: Longitude", col="darkblue") 

## USING GGPLOT
ggplot(housing, aes(x=longitude)) + 
  geom_histogram(fill="darkblue", alpha=10, position="identity", bins=40) +  
  theme_minimal() +
  labs(x="LONGITUDE", title="HISTOGRAM: LONGITUDE",y="COUNT") +
  scale_y_continuous(breaks=seq(0, 4000, by = 500)) +
  scale_x_continuous(breaks=seq(-125, -110, by = .5))+  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## HISTOGRAM 3: HOUSING MEDIAN AGE
## USING BASE R
hist(housing$housing_median_age,xlab = "Median Age (in months)", main ="Histogram: Housing Median Age", col="deeppink")

## USING GGPLOT
ggplot(housing, aes(x=housing_median_age)) + 
  geom_histogram(fill="deeppink2", alpha=10, position="identity", bins=40) +  
  theme_minimal() +
  labs(x="HOUSING MEDIAN AGE (in months)", title="HISTOGRAM: HOUSING MEDIAN AGE",y="COUNT") +
  scale_x_continuous(breaks=seq(0, 55, by = 5)) + 
  scale_y_continuous(breaks=seq(0, 1500, by = 100)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

## HISTOGRAM 4: TOTAL ROOMS 
## USING BASE R
hist(housing$total_rooms,xlab = "Total Rooms", main ="Histogram: Total Rooms", col="darkgreen")

## USING GGPLOT
ggplot(housing, aes(x=total_rooms)) + 
  geom_histogram(fill="darkgreen", alpha=10, position="identity", bins=40) +  
  theme_minimal() +
  labs(x="TOTAL ROOMS", title="HISTOGRAM: TOTAL ROOMS",y="COUNT") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(breaks=seq(0, 8000, by = 500)) + 
  scale_x_continuous(breaks=seq(0, 40000, by = 2000)) 

## HISTOGRAM 5: TOTAL BEDROOMS 
## USING BASE R
hist(housing$total_bedrooms,xlab = "Total Bedrooms", main ="Histogram: Total Bedrooms", col="deepskyblue")

## USING GGPLOT
ggplot(housing, aes(x=total_bedrooms)) + 
  geom_histogram(fill="deepskyblue", alpha=10, position="identity", bins=40) +  
  theme_minimal() +
  labs(x="TOTAL BEDROOMS", title="HISTOGRAM: TOTAL BEDROOMS",y="COUNT") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(breaks=seq(0, 15000, by = 500)) +
  scale_x_continuous(breaks=seq(0, 7000, by = 500)) 

## HISTOGRAM 6: POPULATION 
## BASE R
hist(housing$population, xlab = "Population (in thousands)", main ="Histogram: Population",col="purple")

## USING GGPLOT
ggplot(housing, aes(x=population)) + 
  geom_histogram(fill="purple", alpha=10, position="identity", bins=40) +  
  theme_minimal() +
  labs(x="POPULATION", title="HISTOGRAM: POPULATION", y="COUNT") +
  scale_y_continuous(breaks=seq(0, 12000, by = 1000)) + 
  scale_x_continuous(breaks=seq(0, 30000, by = 2000)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## HISTOGRAM 7: HOUSEHOLDS
## USING BASE R
hist(housing$households, xlab = "Households", main ="Histogram: Households", col="darkgoldenrod1")

## USING GGPLOT
ggplot(housing, aes(x=households)) + 
  geom_histogram(fill="darkgoldenrod1", alpha=10, position="identity", bins=40) +  
  theme_minimal() +
  labs(x="HOUSEHOLDS", title="HISTOGRAM: HOUSEHOLDS",y="COUNT") +
  scale_y_continuous(breaks=seq(0, 6500, by = 500)) + 
  scale_x_continuous(breaks=seq(0, 6000, by = 500)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## HISTOGRAM 8: MEDIAN INCOME
## USING BASE R
hist(housing$median_income, xlab = "Median Income (in thousands of dollars)", main ="Histogram: Median Income",col="chartreuse3")

## USING GGPLOT
ggplot(housing, aes(x=median_income)) + 
  geom_histogram(fill="chartreuse3", alpha=10, position="identity", bins=40) +  
  theme_minimal() +
  labs(x="MEDIAN INCOME (in thousands of dollars)", title="HISTOGRAM: MEDIAN INCOME", y="COUNT") +
  scale_y_continuous(breaks=seq(0, 2000, by = 100)) + 
  scale_x_continuous(breaks=seq(0, 20, by = 1)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## HISTOGRAM 9: MEDIAN HOUSE VALUE
##  USING BASE R
hist(housing$median_house_value, xlab = "Median House Value (in dollars)", main ="Histogram: Median House Value", col="red")

## USING GGPLOT
ggplot(housing, aes(x=median_house_value)) + 
  geom_histogram(fill="red", alpha=10, position="identity", bins=40) +  
  theme_minimal() +
  labs(x="MEDIAN HOUSE VALUE (in dollars)", title="HISTOGRAM: MEDIAN HOUSE VALUE", y="COUNT") +
  scale_y_continuous(breaks=seq(0, 1500, by = 100)) + 
  scale_x_continuous(breaks=seq(10000,500000 , by = 20000)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## e) Produce bloxplots for each numeric variable
##BOXPLOT 1: LATITUDE
## USING BASE R
boxplot(housing$latitude, ylab="Latitude", main ="Boxplot: Latitude", col="darkolivegreen4")

## USING GGPLOT
ggplot(housing, aes( x=latitude, varwidth=TRUE))+
  geom_boxplot() + theme_minimal() + coord_flip()+
  labs(x="LATITUDE",title="BOXPLOT: LATITUDE") + 
  geom_boxplot(fill="darkolivegreen4", color="darkgrey")

##BOXPLOT 2: LONGITUDE
## USING BASE R
boxplot(housing$longitude, ylab="Latitude", main ="Boxplot: Longitude", col="darkblue")

## USING GGPLOT
ggplot(housing, aes( x=longitude, varwidth=TRUE))+
  geom_boxplot() + theme_minimal() + coord_flip()+
  labs(x="LONGITUDE",title="BOXPLOT: LONGITUDE") + 
  geom_boxplot(fill="darkblue", color="darkgrey")

##BOXPLOT 3: HOUSING MEDIAN AGE
## USING BASE R
boxplot(housing$housing_median_age, ylab="Housing Median Age (in months)", main ="Boxplot: Housing Median Age", col="deeppink2")

## USING GGPLOT
ggplot(housing, aes(x=housing_median_age, varwidth=TRUE))+
  geom_boxplot() + theme_minimal() + coord_flip()+
  labs(x="HOUSING MEDIAN AGE (in months)",title="BOXPLOT: HOUSING MEDIAN AGE") + 
  geom_boxplot(fill="deeppink2", color="darkgrey")

##BOXPLOT 4: TOTAL ROOMS
## USING BASE R
boxplot(housing$total_rooms, ylab="Total Rooms", main ="Boxplot: Total Rooms", col="darkgreen")

## USING GGPLOT
ggplot(housing, aes( x=total_rooms, varwidth=TRUE))+
  geom_boxplot() + theme_minimal() + coord_flip()+
  labs(x="TOTAL ROOMS",title="BOXPLOT: TOTAL ROOMS") + 
  geom_boxplot(fill="darkgreen", color="darkgrey")

##BOXPLOT 5: TOTAL BEDROOMS
## USING BASE R
boxplot(housing$total_bedrooms, ylab="Total Bedrooms", main ="Boxplot: Total Bedrooms", col="deepskyblue")

## USING GGPLOT
ggplot(housing, aes(x=total_bedrooms, varwidth=TRUE))+
  geom_boxplot() + theme_minimal() + coord_flip()+
  labs(x="TOTAL BEDROOMS",title="BOXPLOT: TOTAL BEDROOMS") + 
  geom_boxplot(fill="deepskyblue", color="darkgrey")

##BOXPLOT 6: POPULATION
## USING BASE R
boxplot(housing$population, ylab="Population", main ="Boxplot: Population", col="purple")

## USING GGPLOT
ggplot(housing, aes(x=population, varwidth=TRUE))+
  geom_boxplot() + theme_minimal() + coord_flip()+
  labs(x="POPULATION",title="BOXPLOT: POPULATION") + 
  geom_boxplot(fill="purple", color="darkgrey")

##BOXPLOT 7: HOUSEHOLDS
## USING BASE R
boxplot(housing$households, ylab="Households", main ="Boxplot: Households", col="darkgoldenrod1")

## USING GGPLOT
ggplot(housing, aes(x=households, varwidth=TRUE))+
  geom_boxplot() + theme_minimal() + coord_flip()+
  labs(x="HOUSEHOLDS",title="BOXPLOT: HOUSEHOLDS") + 
  geom_boxplot(fill="darkgoldenrod1", color="darkgrey")

##BOXPLOT 8: MEDIAN INCOME
## USING BASE R
boxplot(housing$median_income, ylab="Median Income", main ="Boxplot: Median Income", col="chartreuse3")

## USING GGPLOT
ggplot(housing, aes(x=median_income, varwidth=TRUE))+
  geom_boxplot() + theme_minimal() + coord_flip()+
  labs(x="MEDIAN INCOME (in thousands of dollars)",title="BOXPLOT: MEDIAN INCOME") + 
  geom_boxplot(fill="chartreuse3", color="darkgrey")

##BOXPLOT 9: MEDIAN HOUSE VALUE
## USING BASE R
boxplot(housing$median_house_value, ylab="Median House Value", main ="Boxplot: Median House Value", col="red")

## USING GGPLOT
ggplot(housing, aes( x=median_house_value, varwidth=TRUE))+
  geom_boxplot() + theme_minimal() + coord_flip()+
  labs(x="MEDIAN HOUSE VALUE (in dollars)",title="BOXPLOT: MEDIAN HOUSE VALUE") + 
  geom_boxplot(fill="red", color="darkgrey")

## f) Produce boxplots for the variables: housing_median_age, median_income, and median_house_value 
## “with respect” to the factor variable ocean_proximity.

##BOXPLOT 1: Median Age by Ocean Proximity
## USING BASE R
boxplot(housing$housing_median_age ~ housing$ocean_proximity, ylab= "Median Age (in months)",xlab="Ocean Proximity", main ="Boxplot: Median Age by Ocean Proximity", varwidth=TRUE)

## USING GGPLOT
ggplot(housing, aes(y=ocean_proximity, x=housing_median_age,fill=ocean_proximity, varwidth=TRUE))+
  geom_boxplot() + theme_minimal() + coord_flip()+
  labs(x="HOUSING MEDIAN AGE", y="OCEAN PROXIMITY",title="BOXPLOT: HOUSING MEDIAN AGE BY OCEAN PROXIMITY") +
  theme(legend.title=element_blank()) 

##BOXPLOT 2: Median Income by Ocean Proximity
## USING BASE R
boxplot(housing$median_income ~ housing$ocean_proximity,ylab="Median Income (in thousands of dollars)",xlab = "Ocean Proximity", main ="Boxplot: Median Income by Ocean Proximity")

## USING GGPLOT
ggplot(housing, aes(y=ocean_proximity, x=median_income,fill=ocean_proximity, varwidth=TRUE))+
  geom_boxplot() + theme_minimal() + coord_flip()+
  labs(x="MEDIAN INCOME (in thousands of dollars)", y="OCEAN PROXIMITY",title="BOXPLOT: HOUSING MEDIAN INCOME BY OCEAN PROXIMITY") +
  theme(legend.title=element_blank()) 

##BOXPLOT 3: Median House Value by Ocean Proximity
## USING BASE R
boxplot(housing$median_house_value ~ housing$ocean_proximity,ylab="Median House Value (in dollars)",xlab = "Ocean Proximity", main ="Boxplot: Median House Value by Ocean Proximity")

## USING GGPLOT
ggplot(housing, aes(y=ocean_proximity, x=median_house_value,fill=ocean_proximity, varwidth=TRUE))+
  geom_boxplot() + theme_minimal() + coord_flip()+
  labs(x="MEDIAN HOUSE VALUE (in dollars)", y="OCEAN PROXIMITY",title="BOXPLOT: MEDIAN HOUSE VALUE BY OCEAN PROXIMITY") +
  theme(legend.title=element_blank()) 


## 3. DATA TRANSFORMATION
## a) There are many NA values in the total_bedrooms variable (the only variable with missing values).

## check where missing values, NAs, are
as.data.frame(sapply(housing, function(x) sum(is.na(x)))) 
##207 in total_bedrooms

## Use the “statistical median” for missing total_bedrooms values.
## compute median
median(housing$total_bedrooms, na.rm=TRUE)
## [1] 435

## The median is used instead of mean because it is less influenced by extreme outliers. 
## This may not be the best method, as these missing values could represent actual buildings (e.g. a warehouse) with no bedrooms, 
## but imputation often makes the best of a bad situation. 

## Use the impute() function covered in class, or write code to accomplish the requirement.
housing$total_bedrooms[is.na(housing$total_bedrooms)]<- median(housing$total_bedrooms, na.rm=TRUE)

## check for missing values, NAs, again
as.data.frame(sapply(housing, function(x) sum(is.na(x)))) 
##0 in total_bedrooms

## install packages for creating subset
install.packages("tidyverse")
library(dplyr)

## create subset of all numerical variables for correlation
housingcor <- housing %>%     
  select (c("latitude", "longitude","housing_median_age","total_rooms",
            "total_bedrooms", "population", "households","median_income",
            "median_house_value")) 

## run correlation
cor(housingcor)

## Summary of correlation
##very strong negative correlation: latitude to longitude  
##very strong positive correlation: total_rooms to total bedrooms, population and households  
##very strong positive correlation: total bedrooms to total rooms, population and households  
##very strong  positive correlation: population to total rooms, total bedrooms, households 
##very strong  positive correlation:total_rooms to total bedrooms 
##very strong positive correlation: households to population, total rooms and total bedrooms
##strong positive correlation: median house value to median income
## everything else has a weak correlation

## visual correlation
## install packages

## correlation plot 
install.packages("corrplot")         # Install corrplot package
library(corrplot)                  # Load corrplot package

## correlation plot with some specs (formula from Daniel)
par(xpd=TRUE)  # because corrplot() use of margins is quirky
corrplot(cor(na.omit(housingcor)), 
         method="ellipse", 
         type="upper",
         cl.pos="n",
         tl.pos="diag",
         mar = c(2, 0, 4, 0))
corrplot(cor(na.omit(housingcor)),
         method="number",
         type="lower",
         tl.pos="n",
         cl.pos="n",
         add=TRUE)

## narrow and elongated: strong correlations 
## Rounder, less elongated: weaker associations
## Darker colors: stronger correlations vs lighter colors
## Positive correlation: slope from lower left to upper right
## negative if it slopes from the upper left to the lower right.

## Summary of visual correlation
## Very strong negative correlation: latitude to longitude
## Very strong positive correlation: total rooms to total bedrooms, population and households
## Very strong positive correlation: total bedrooms to total rooms, population and households
## Very strong positive correlation: population to total rooms, total bedrooms and households
## Very strong positive correlation: households to total rooms, total bedrooms and population

## Strong positive correlation: Median income to median house value
## Weak correlation: housing median age to median income and median house value
## Weak correlation: median house value to latitude, longitude, housing median age, total rooms, total bedrooms ( even weaker) , population ( even weaker), households( even weaker) 

## b) Split the ocean_proximity variable into a number of binary categorical variables consisting of 1s and 0s. 
## Although many machine learning algorithms in R can handle categorical data stored in a factor variable, 
## but we will cater to the lowest common denominator and do the splitting ourselves. 
## Once you’re done with the splitting, you can remove the ocean_proximity variable from the data frame.

## install packages
install.packages("sjmisc")
library(sjmisc)

## split ocean proximity into binary categorical variables of 1s and 0s
housing$ocean_proximity <- to_dummy(housing$ocean_proximity, var.name = "name", suffix = c("numeric", "label"))

## c) Use the total_bedrooms and total_rooms variables along with households 
## to create two new variables: mean_bedrooms and mean_rooms 
## as these are likely to be more accurate depictions of the houses in a given group. 

housing$mean_bedrooms <- housing$total_bedrooms/housing$households
housing$mean_rooms <- housing$total_rooms/housing$households

## remove the total_bedrooms and total_rooms variables once you’ve accomplished this 
housing = subset(housing, select = -c(total_bedrooms, total_rooms)) 

## d) Perform feature scaling. 
## Scale each numerical variable 
## except for median_house_value (as this is our response variable), 
## and the binary categorical variables.

## create subset of just numerical variables
df <- subset(housing, select = -c(median_house_value, ocean_proximity)) 

## scale subset
dfNormZ <- as.data.frame(scale(df[1:8]))

## e) The result of your data transformation efforts should yield a new data frame named cleaned_housing with the following variables:
## "NEAR BAY" "<1H OCEAN" "INLAND"
## "NEAR OCEAN" "ISLAND" "longitude"
## "latitude" "housing_median_age" "population"
## "households" "median_income" "mean_bedrooms"
## "mean_rooms" "median_house_value"

## create new data.frame
cleaned_housing <- data.frame(dfNormZ, housing$median_house_value, housing$ocean_proximity)

## rename columns
names(cleaned_housing)[9] <- "median_house_value"
names(cleaned_housing)[10] <- "<1H OCEAN "
names(cleaned_housing)[11] <- "INLAND "
names(cleaned_housing)[12] <- "ISLAND"
names(cleaned_housing)[13] <- "NEAR BAY"
names(cleaned_housing)[14] <- "NEAR OCEAN"

## 4. Create Training and Test Sets
## Get ready for machine learning: create the training and test sets using a random sample index.
## a) Create a random sample index for the cleaned_housing data frame.
## Use default replace=F to not allow duplicate selections 

## Split data set into training set and test set
n <- nrow(cleaned_housing)  # Number of observations = 20640
ntrain <- round(n*0.7)    # 70% for training set
set.seed(314)             # Set seed for reproducible results
sample_index <- sample(n, ntrain) # Create an index

## b) Create a training set named train consisting of 70% of rows of the cleaned_housing df
train <-cleaned_housing[sample_index,]  

## c) Create a test set named test consisting of 30% of the rows of the cleaned_housing data frame.
test <-cleaned_housing[-sample_index,] 

## 5. Supervised Machine Learning - Regression

## install random forest package
install.packages('randomForest')
library(randomForest)
  
##  use the randomForest() algorithm in the randomForest package for training and inference. 
## Our goal is to predict the median house value using regression methods.

## First, separate your training set train into: 
## train_x and train_y where train_x is a data frame that has all variables 
## except the response variable median_house_value 
## and train_y is a numeric vector (not a data frame) with only the response variable values from median_house_value. 

## define predictor and target variables in training set
train_x = data.frame(train[,-9])
train_y = train[,9]

## call the randomForest() algorithm, passing to it both components of the training set created above. 
## Specify arguments ntree=500, and importance=TRUE. Return the resulting model in the object variable rf
## as in: rf = randomForest(x=train_x, y=train_y,ntree=500, importance=TRUE)

## Set seed for reproducible results
set.seed(314)     

#run randomForest
rf = randomForest(x=train_x, y=train_y,
                  ntree=500, importance=TRUE, type=regression)

## Use names(rf) to see all the different metrics computed by the algorithm.
names(rf)
##[1] "call"            "type"            "predicted"       "mse"            
##[5] "rsq"             "oob.times"       "importance"      "importanceSD"   
##[9] "localImportance" "proximity"       "ntree"           "mtry"           
##[13] "forest"          "coefs"           "y"               "test"           
##[17] "inbag"

print(rf)
## Call:
## randomForest(x = train_x, y = train_y, ntree = 500, importance = TRUE,      type = regression) 
## Type of random forest: regression
## Number of trees: 500
## No. of variables tried at each split: 4

## Mean of squared residuals: 2489689423
##% Var explained: 81.51

## 6. Evaluating Model Performance
## Determine the quality of the fit for the statistical model, 
## i.e. evaluating the performance of the random forest algorithm used above.

## a) Calculate the root mean squared error (RMSE) for the trained model. 
## Use the object element rf$mse for this purpose. This element is a vector of calculated mean squared error (MSE) values
## one for each “tree” used in the algorithm. 
## Use the last MSE value, i.e. the last element of the vector. 

##get mse
rf$mse
## 2489689423 ## last element of the vector

## calculate the square root of last element above to get RMSE
## The resulting RMSE is your prediction of median price of a house
## in a given district to within a RMSE delta of the actual median house price. 
##This can serve as your benchmark moving forward as you experiment with other statistical models.

Train_Set_RMSE <- sqrt(2489689423)

## print RMSE
print(Train_Set_RMSE)
## [1] 49896.79

##b) Next, check how well the model makes predictions by using the test set. 
## Split the test set in the same way as the training set above:
## create a new data frame test_x and numeric vector test_y. 

## define predictor and target variables in testing set
test_x = data.frame(test[,-9])
test_y = test[,9]

## Use the predict() function using the trained model rf along with test_x 
## to calculate a vector of predicted median house values.

pred_y <- predict(rf, test_x, type="response") 

## c) Calculate the RMSE for the test set using the vector of predicted median house values 
## and the actual values from the test set, test_y. 
## To calculate RMSE you can use the UDF we described in class
## r write your own code. ##check module 9 for RMSE code

## install package
library(Metrics)

## get RMSE
Test_Set_RMSE <- rmse(test_y,pred_y)
## [1] 48504.14

## print summary of metrics
cat("Train Set RMSE", Train_Set_RMSE, "Test Set RMSE ", Test_Set_RMSE)

## for rf ## Train Set RMSE 49896.79 Test Set RMSE  48504.14

## d) How does the test set RMSE compare with the training set RMSE? 
## Did the model score roughly the same on the training and testing data, 
## suggesting that it is not overfit and that it makes good predictions?
  
## The Train Set RMSE and Test Set RMSE are pretty close to each other, give or take a few hundred dollars.
## The model doesn't overfit and makes good predictions.
  
## get rsq
print(rf$rsq) ## 0.8151249, last element of the vector

## e)  Run the variable importance plot with the varImpPlot() function 
varImpPlot(rf) ## basic plot

## add color and some details
varImpPlot(rf, bg = "red", cex=1, pch=21,main ="RANDOM FOREST REGRESSION VARIABLE IMPORTANCE PLOT")

## Discuss what it indicated about your feature vector 

## Median income is the most important feature, removing it will mean 98-99% increase in mse
## Next is housing median age, removing it will mean about 80% increase in mse
## longitude and latitude are 3rd and 4th most important, respectively, removing either will mean about 62-63% increase in MSE
## mean bedrooms and mean rooms are 5th and 6th most important, removing either will mean about 47-48% increase in MSE
## INLAND and population are 7th and 8th most important, removing either will mean about 40-42% increase in MSE
## householads is 9th most important, removing it will mean about 39% increase in MSE
## the rest are not that important, removing them will mean less than 20% increase in MSE

## you may re-train your model using only the feature variables suggested by this plot
   
## RETRAIN WITH ONLY IMPORTANT VARIABLES
## create subset minus the least 4 important variables based on varImpPlot(rf)
cleaned_housing2 <- subset(cleaned_housing, select = -c(`<1H OCEAN `,`NEAR OCEAN`,`NEAR BAY`,`ISLAND`)) 

## 4. Create Training and Test Sets
## Get ready for machine learning: create the training and test sets using a random sample index.
## a) Create a random sample index for the cleaned_housing data frame.
## Use default replace=F to not allow duplicate selections 
        
# Split data set into training set and test set
n <- nrow(cleaned_housing2)  # Number of observations = 20640
ntrain <- round(n*0.7)    # 70% for training set
set.seed(314)             # Set seed for reproducible results
sample_index <- sample(n, ntrain) # Create an index

## b) Create a training set named train consisting of 70% of rows of the cleaned_housing df
train <-cleaned_housing2[sample_index,]  
        
## c) Create a test set named test consisting of 30% of the rows of the cleaned_housing data frame.
test <-cleaned_housing2[-sample_index,] 
 
## 5. Supervised Machine Learning - Regression

## define predictor and target variables in training set
train_x = data.frame(train[,-9])
train_y = train[,9]

## call the randomForest() algorithm again

## Set seed for reproducible results
set.seed(314)    
rf2 = randomForest(x=train_x, y=train_y,
                  ntree=500, importance=TRUE, type=regression)

print(rf2)
## Call:
## randomForest(x = train_x, y = train_y, ntree = 500, importance = TRUE,      type = regression) 
## Type of random forest: regression
## Number of trees: 500
## No. of variables tried at each split: 3

## Mean of squared residuals: 2527682876
## % Var explained: 81.23

##get mse
rf2$mse
## 2527682876 ## last element of the vector

## calculate RMSE based on last element above
Train_Set_RMSE <- sqrt(2527682876)

## print RMSE
print(Train_Set_RMSE)
## [1] 50276.07

## get rsq
print(rf2$rsq) ## 0.8123037, last element of the vector

## define predictor and target variables in testing set
test_x = data.frame(test[,-9])
test_y = test[,9]

## Use the predict() function using the trained model rf along with test_x 
## to calculate a vector of predicted median house values.

pred_y <- predict(rf2, test_x, type="response") 

## c) Calculate RMSE for the test set using the vector of predicted median house values and actual values from the test set, test_y.

## install package
library(Metrics)

## get RMSE
Test_Set_RMSE <- rmse(test_y,pred_y)
## [1] 48862.85

## print summary of metrics
cat("Train Set RMSE", Train_Set_RMSE, "Test Set RMSE ", Test_Set_RMSE)
## for rf2 ## Train Set RMSE 50276.07 Test Set RMSE  48862.85

## tune random forest
mtry <- tuneRF(train[-1],train$median_house_value, ntreeTry=500,
                       stepFactor=2,improve=0.05, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m) ##6
        
## get optimal number of trees for least error
which.min(rf2$mse) ## 372
        
## run random forest again with optimal number of trees, and best.m
set.seed(314)
rf3 <- randomForest(x=train_x, y=train_y,
                           type=regression, ntree=372, 
                           mtry=best.m, importance=TRUE)
        
print(rf3)
        
## Call:
## randomForest(x = train_x, y = train_y, ntree = 372, mtry = best.m,importance = TRUE, type = regression) 
## Type of random forest: regression
## Number of trees: 372
## No. of variables tried at each split: 6

## Mean of squared residuals: 2463601186
## % Var explained: 81.71
        
##get mse
rf3$mse
## 2463601186 ## last element of the vector
     
## calculate RMSE based on last element above
Train_Set_RMSE <- sqrt(2463601186)
        
## print RMSE
print(Train_Set_RMSE)
## [1] 49634.68
        
## get rsq
print(rf3$rsq) ## 0.8170621, last element of the vector

## b) ## define predictor and target variables in testing set
test_x = data.frame(test[,-9])
test_y = test[,9]

## Use the predict() function using the trained model rf3 along with test_x 
## to calculate a vector of predicted median house values.
pred_y <- predict(rf3, test_x, type="response") 

## c) Calculate the RMSE for the test set 
## install package
library(Metrics)

## get RMSE
Test_Set_RMSE <- rmse(test_y,pred_y)
## [1] 48288.36

## print summary of metrics
cat("Train Set RMSE", Train_Set_RMSE, "Test Set RMSE ", Test_Set_RMSE)
## rf3: Train Set RMSE 49634.68 Test Set RMSE  48288.36
## rf3 RSquared ## 0.8170621  ## close to 1, this rsq is pretty good

## RETRAIN AGAIN WITH ONLY THE MOST IMPORTANT VARIABLES
## create subset with just the 4 most important variables based on varImpPlot(rf)
cleaned_housing3 <- subset(cleaned_housing, select = c(latitude, longitude, housing_median_age, median_income, median_house_value)) 

## 4. Create Training and Test Sets
## Get ready for machine learning: create the training and test sets using a random sample index.
## a) Create a random sample index for the cleaned_housing data frame.
## Use default replace=F to not allow duplicate selections 

# Split data set into training set and test set
n <- nrow(cleaned_housing3)  # Number of observations = 20640
ntrain <- round(n*0.7)    # 70% for training set
set.seed(314)             # Set seed for reproducible results
sample_index <- sample(n, ntrain) # Create an index

## b) Create a training set named train consisting of 70% of rows of the cleaned_housing df
train <-cleaned_housing3[sample_index,]  

## c) Create a test set named test consisting of 30% of the rows of the cleaned_housing data frame.
test <-cleaned_housing3[-sample_index,] 

## 5. Supervised Machine Learning - Regression

## define predictor and target variables in training set
train_x = data.frame(train[,-5])
train_y = train[,5]

## call the randomForest() algorithm again

## Set seed for reproducible results
set.seed(314)    
rf4 = randomForest(x=train_x, y=train_y,
                   ntree=500, importance=TRUE, type=regression)

print(rf4)
## Call:
## randomForest(x = train_x, y = train_y, ntree = 500, importance = TRUE,      type = regression) 
## Type of random forest: regression
## Number of trees: 500
## No. of variables tried at each split: 1

## Mean of squared residuals: 2676204794
## % Var explained: 80.13

##get mse
rf4$mse
## 2676204794 ## last element of the vector

## calculate RMSE based on last element above
Train_Set_RMSE <- sqrt(2676204794)

## print RMSE
print(Train_Set_RMSE)
## [1] 51732.05

## get rsq
print(rf4$rsq) ## 0.8012750, last element of the vector

## define predictor and target variables in testing set
test_x = data.frame(test[,-5])
test_y = test[,5]

## Use the predict() function using the trained model rf along with test_x 
## to calculate a vector of predicted median house values.

pred_y <- predict(rf4, test_x, type="response") 

## c) Calculate RMSE for the test set using the vector of predicted median house values and actual values from the test set, test_y.

## install package
library(Metrics)

## get RMSE
Test_Set_RMSE <- rmse(test_y,pred_y)
## [1] 49735.01

## print summary of metrics
cat("Train Set RMSE", Train_Set_RMSE, "Test Set RMSE ", Test_Set_RMSE)
## for rf4 ## Train Set RMSE 51732.05 Test Set RMSE  49735.01
## for rf4$rsq ## 0.8012750

## TUNE AGAIN AND RUN RANDOM FOREST FOR THE 5TH TIME
mtry <- tuneRF(train[-1],train$median_house_value, ntreeTry=500,
               stepFactor=2,improve=0.05, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m) ##4

## get optimal number of trees for least error
which.min(rf4$mse) ## 289

## run random forest again with optimal number of trees, and best.m
set.seed(314)
rf5 <- randomForest(x=train_x, y=train_y,
                    type=regression, ntree=289, 
                    mtry=best.m, importance=TRUE)

print(rf5)
## Call:
## randomForest(x = train_x, y = train_y, ntree = 289, mtry = best.m,      importance = TRUE, type = regression) 
## Type of random forest: regression
## Number of trees: 289
## No. of variables tried at each split: 4

## Mean of squared residuals: 2435340839
## % Var explained: 81.92

##get mse
rf5$mse
## 2435340839 ## last element of the vector

## calculate RMSE based on last element above
Train_Set_RMSE <- sqrt(2435340839)

## print RMSE
print(Train_Set_RMSE)
## [1] 49349.17

## get rsq
print(rf5$rsq) ## 0.8191606, last element of the vector

## b) ## define predictor and target variables in testing set
test_x = data.frame(test[,-5])
test_y = test[,5]

## Use the predict() function using the trained model rf3 along with test_x 
## to calculate a vector of predicted median house values.
pred_y <- predict(rf5, test_x, type="response") 

## c) Calculate the RMSE for the test set 
## install package
library(Metrics)

## get RMSE
Test_Set_RMSE <- rmse(test_y,pred_y)
## [1] 48063.6

## print summary of metrics
cat("Train Set RMSE", Train_Set_RMSE, "Test Set RMSE ", Test_Set_RMSE)

## rf5: Train Set RMSE 49349.17 Test Set RMSE  48063.6
print(rf5$rsq) ## 0.8191606

# for rf4 ## Train Set RMSE 51732.05 Test Set RMSE  49735.01
## for rf4$rsq ## 0.8012750

## rf3: Train Set RMSE 49634.68 Test Set RMSE  48288.36
## rf3 RSquared ## 0.8170621  ## close to 1, this rsq is pretty good

## rf: Train Set RMSE 49896.79 Test Set RMSE  48504.14
## rf RSquared ##  0.8151249 ## close to 1, this rsq is pretty good

## rf2: ## Train Set RMSE 50276.07 Test Set RMSE  48862.85
## rf2 RSquared ##  0.8123037 ## close to 1, this rsq is pretty good


## Formula for Normalized RMSE = RMSE / (max value – min value)
## get max and min value of target variable
summary(cleaned_housing3$median_house_value)
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 14999  119600  179700  206856  264725  500001 

NormalizedRMSE <- 49349.17/(500001 - 14999)
## 0.1017504

## END OF CODE
