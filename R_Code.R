car_price <- read.csv("CarPrice_Assignment.csv")
summary(car_price)
str(car_price)

library(dplyr)
library(tidyr)
library(MASS)
library(car)
library(stringr)
library(ggplot2)

str(car_price)

#Check for duplicate values
sum(duplicated(car_price$car_ID))

#Check for NA values
sum(is.na(car_price))

#Separating make and model name into two different columns
car_make_model <- car_price %>% separate(CarName, c("make"), " ")

sum(is.na(car_make_model$make))

#Checking for spelling mistake in make and model name

car_make_model$make<-tolower(car_make_model$make)  #All Lower cases
unique(c(as.character(car_make_model$make)))
car_make_model$make <- ifelse(car_make_model$make == "maxda" , "mazda" , car_make_model$make)
car_make_model$make <- ifelse(car_make_model$make == "vw" | car_make_model$make == "vokswagen" , "volkswagen" , car_make_model$make)
car_make_model$make <- ifelse(car_make_model$make == "toyouta" , "toyota" , car_make_model$make)
car_make_model$make <- ifelse(car_make_model$make == "porcshce" , "porsche" , car_make_model$make)
unique(c(as.character(car_make_model$make)))


#changing into factors
car_make_model$make<-as.factor(car_make_model$make)
car_make_model$symboling<-as.factor(car_make_model$symboling)
car_make_model$fueltype<-as.factor(car_make_model$fueltype)
car_make_model$aspiration<-as.factor(car_make_model$aspiration)
car_make_model$doornumber<-as.factor(car_make_model$doornumber)
car_make_model$carbody<-as.factor(car_make_model$carbody)
car_make_model$drivewheel<-as.factor(car_make_model$drivewheel)
car_make_model$enginelocation<-as.factor(car_make_model$enginelocation)
car_make_model$enginetype<-as.factor(car_make_model$enginetype)
car_make_model$cylindernumber<-as.factor(car_make_model$cylindernumber)
car_make_model$fuelsystem<-as.factor(car_make_model$fuelsystem)
str(car_make_model)


#########checking for outliers


##wheelbase column
quantile(car_make_model$wheelbase,seq(0,1,0.01))
#considering jump of 2 units is fine there is a jump on 5 units from 99% to 100% so replacing it
car_make_model$wheelbase[which(car_make_model$wheelbase > 115.544)] <- 115.544

##carlength column 
quantile(car_make_model$carlength,seq(0,1,0.01))
car_make_model$carlength[which(car_make_model$carlength > 202.480)] <- 202.480

##carheight colum
quantile(car_make_model$carheight,seq(0,1,0.01))

##carwith column
quantile(car_make_model$carwidth,seq(0,1,0.01))
car_make_model$carwidth[which(car_make_model$carwidth < 62.536)] <- 62.536

##curbweight column ---doubt
quantile(car_make_model$curbweight,seq(0,1,0.01))
car_make_model$curbweight[which(car_make_model$curbweight < 1819.72)] <- 1819.72

##enginesize column
quantile(car_make_model$enginesize,seq(0,1,0.01))
car_make_model$enginesize[which(car_make_model$enginesize > 256.08)] <- 256.08 
car_make_model$enginesize[which(car_make_model$enginesize < 90.00)] <- 90.00


##borratio
quantile(car_make_model$boreratio,seq(0,1,0.01))

## stroke column
quantile(car_make_model$stroke,seq(0,1,0.01))
car_make_model$stroke[which(car_make_model$stroke < 2.6400)] <- 2.6400

##compression ratio column
quantile(car_make_model$compressionratio,seq(0,1,0.01))
car_make_model$compressionratio[which(car_make_model$compressionratio > 10.9400)] <- 10.9400 

##horsepower column
quantile(car_make_model$horsepower,seq(0,1,0.01))
car_make_model$horsepower[which(car_make_model$horsepower > 184)] <- 184

##peakrpm column
quantile(car_make_model$peakrpm,seq(0,1,0.01))

##citympg column
quantile(car_make_model$citympg,seq(0,1,0.01))
car_make_model$citympg[which(car_make_model$citympg > 38)] <- 38.00


##highway mpg column
quantile(car_make_model$highwaympg,seq(0,1,0.01))
car_make_model$highwaympg[which(car_make_model$highwaympg > 49.88)] <- 49.88

summary(car_make_model)
str(car_make_model)

########## Converting categorical variables to numerics

##Converting fueltype - Gas as 0 and diesel as 1
levels(car_make_model$fueltype)<- c(1,0)
car_make_model$fueltype<- as.numeric(levels(car_make_model$fueltype))[car_make_model$fueltype]

##Converting aspiration - turbo as 0 and std as 1
levels(car_make_model$aspiration)<- c(1,0)
car_make_model$aspiration<- as.numeric(levels(car_make_model$aspiration))[car_make_model$aspiration]

##Converting doornumber - two doors as 0 and four doors as 1
levels(car_make_model$doornumber)<- c(1,0)
car_make_model$doornumber<- as.numeric(levels(car_make_model$doornumber))[car_make_model$doornumber]


##Converting enginelocation - rear as 0 and front as 1
levels(car_make_model$enginelocation)<- c(1,0)
car_make_model$enginelocation<- as.numeric(levels(car_make_model$enginelocation))[car_make_model$enginelocation]


########### Converting categorical variable to dummy variable
str(car_make_model)
## Carbody column
dummy_carbody <- data.frame(model.matrix( ~carbody, data = car_make_model))

#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "fusrnishingstatus". 
dummy_carbody <- dummy_carbody[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "furnishingstatus" column
car_make_model <- cbind(car_make_model[,-7], dummy_carbody)
str(car_make_model)


### drivewheel column
dummy_drivewheel <- data.frame(model.matrix( ~drivewheel, data = car_make_model))

#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "fusrnishingstatus". 
dummy_drivewheel <- dummy_drivewheel[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "furnishingstatus" column
car_make_model <- cbind(car_make_model[,-7], dummy_drivewheel)


## enginetype column
dummy_enginetype <- data.frame(model.matrix( ~enginetype, data = car_make_model))

#This column should be removed from the newly created dummy_enginetype dataframe containing the dummy values for the variable "enginetype". 
dummy_enginetype <- dummy_enginetype[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "enginetype" column
car_make_model <- cbind(car_make_model[,-13], dummy_enginetype)


## cylindernumber column
dummy_cylindernumber <- data.frame(model.matrix( ~cylindernumber, data = car_make_model))

#This column should be removed from the newly created dummy_enginetype dataframe containing the dummy values for the variable "enginetype". 
dummy_cylindernumber <- dummy_cylindernumber[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "enginetype" column
car_make_model <- cbind(car_make_model[,-13], dummy_cylindernumber)
View(car_make_model)


## fuelsystem column
dummy_fuelsystem <- data.frame(model.matrix( ~fuelsystem, data = car_make_model))

#This column should be removed from the newly created dummy_enginetype dataframe containing the dummy values for the variable "fuelsystem". 
dummy_fuelsystem <- dummy_fuelsystem[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "fuelsystem" column
car_make_model <- cbind(car_make_model[,-14], dummy_fuelsystem)


## make column
dummy_make <- data.frame(model.matrix( ~make, data = car_make_model))

#This column should be removed from the newly created dummy_enginetype dataframe containing the dummy values for the variable "fuelsystem". 
dummy_make <- dummy_make[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "fuelsystem" column
car_make_model <- cbind(car_make_model[,-3], dummy_make)



##binning symboling column
levels(car_make_model$symboling)
levels(car_make_model$symboling)[1:2]<-"safe"
levels(car_make_model$symboling)[2:3]<-"average"
levels(car_make_model$symboling)[3:4]<-"Risk-prone"

## symboling column
dummy_symboling <- data.frame(model.matrix( ~symboling, data = car_make_model))

#This column should be removed from the newly created dummy_symboling dataframe containing the dummy values for the variable "fuelsystem". 
dummy_symboling <- dummy_symboling[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "symboling" column
car_make_model <- cbind(car_make_model[,-2], dummy_symboling)




str(car_make_model)
##############

# Divide into training and test data set
#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(car_make_model), 0.7*nrow(car_make_model))
# generate the train data set
train = car_make_model[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = car_make_model[-trainindices,]


##############

#Execute the first model_1 multilinear model in the training set. 
model_1 <-lm(price~.,data=train)

# Check the summary of model. 
summary(model_1)


step<-stepAIC(model_1,direction = "both")

#########################################
### CREATING MODEL THROUGH AIC OUTPUT ###
#########################################



model_2 <- lm(price ~ car_ID + fueltype + aspiration + enginelocation + wheelbase + 
                carwidth + curbweight + enginesize + stroke + compressionratio + 
                peakrpm + highwaympg + drivewheelrwd + enginetypedohcv + 
                enginetypel + enginetypeohcf + enginetyperotor + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + fuelsystem2bbl + 
                fuelsystemmpfi + makeaudi + makebmw + makedodge + makehonda + 
                makeisuzu + makemazda + makemercury + makemitsubishi + makenissan + 
                makeplymouth + makerenault + makesaab + maketoyota + makevolkswagen + 
                makevolvo , data = train)

# Check the summary of model.
summary(model_2)
vif(model_2)
#Multiple R-squared:  0.9785,	Adjusted R-squared:  0.971
# carmake toyota has vif 127 but pvalue *** not removing it , whereas carId has vif 124
#removing carId


####model 3 ---- Removing carID column
model_3 <- lm(price ~ fueltype + aspiration + enginelocation + wheelbase + 
                carwidth + curbweight + enginesize + stroke + compressionratio + 
                peakrpm + highwaympg + drivewheelrwd + enginetypedohcv + 
                enginetypel + enginetypeohcf + enginetyperotor + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + fuelsystem2bbl + 
                fuelsystemmpfi + makeaudi + makebmw + makedodge + makehonda + 
                makeisuzu + makemazda + makemercury + makemitsubishi + makenissan + 
                makeplymouth + makerenault + makesaab + maketoyota + makevolkswagen + 
                makevolvo , data = train)

## Check the summary of model.
summary(model_3)
vif(model_3)
#Multiple R-squared:  0.9765,	Adjusted R-squared:  0.9685
# enginesize 38.79:*** , cylindernumberfour 24.89:* , curbweight 21:* , fueltype 13:0.11 ,
# carwidth 12.44:** , removing fueltype


####Model 4 Removing fueltype - 13 VIF in 3rd model
model_4 <- lm(price ~ aspiration + enginelocation + wheelbase + 
                carwidth + curbweight + enginesize + stroke + compressionratio + 
                peakrpm + highwaympg + drivewheelrwd + enginetypedohcv + 
                enginetypel + enginetypeohcf + enginetyperotor + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + fuelsystem2bbl + 
                fuelsystemmpfi + makeaudi + makebmw + makedodge + makehonda + 
                makeisuzu + makemazda + makemercury + makemitsubishi + makenissan + 
                makeplymouth + makerenault + makesaab + maketoyota + makevolkswagen + 
                makevolvo , data = train)

## Check the summary of model.
summary(model_4)
vif(model_4)
#Multiple R-squared:  0.9759,	Adjusted R-squared:  0.9681
# enginesize 38.6:*** , cylindernumberfour 24.89:* , curbweight 19.3:** ,
# carwidth 12: ** , highwaympg 10.8:0.21
#so removing highwaympg



####Model 5 Removing highwaympg - 10.8 VIF in 4th model
model_5 <- lm(price ~ aspiration + enginelocation + wheelbase + 
                carwidth + curbweight + enginesize + stroke + compressionratio + 
                peakrpm + drivewheelrwd + enginetypedohcv + 
                enginetypel + enginetypeohcf + enginetyperotor + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + fuelsystem2bbl + 
                fuelsystemmpfi + makeaudi + makebmw + makedodge + makehonda + 
                makeisuzu + makemazda + makemercury + makemitsubishi + makenissan + 
                makeplymouth + makerenault + makesaab + maketoyota + makevolkswagen + 
                makevolvo , data = train)
## Check the summary of model.
summary(model_5)
vif(model_5)
#Multiple R-squared:  0.9756,	Adjusted R-squared:  0.9679 
#enginesize 35:***,cylindernumberfour 23.4:** ,curbweight 18:* , carwidth 12:**
#cyylindernumbersix 9.4:*** , cylindernumberfive 8.3:*** , drivewheelrwd 8.2:* 
#so removing curbweight



####Model 6 Removing curbweight 18 VIF in 5th model
model_6 <- lm(price ~ aspiration + enginelocation + wheelbase + 
                carwidth + enginesize + stroke + compressionratio + 
                peakrpm + drivewheelrwd + enginetypedohcv + 
                enginetypel + enginetypeohcf + enginetyperotor + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + fuelsystem2bbl + 
                fuelsystemmpfi + makeaudi + makebmw + makedodge + makehonda + 
                makeisuzu + makemazda + makemercury + makemitsubishi + makenissan + 
                makeplymouth + makerenault + makesaab + maketoyota + makevolkswagen + 
                makevolvo , data = train)
## Check the summary of model.
summary(model_6)
vif(model_6)
#Multiple R-squared:  0.9742,	Adjusted R-squared:  0.9663 
#enginesize 28:*** , cylindernumberfour 21:* , carwidth 12:*** 
#removing cylindernumberfour and checking the value of R square and adjusted R square



####Model 7 Removing cylindernumberfour 21 VIF in 6th model
model_7 <- lm(price ~ aspiration + enginelocation + wheelbase + 
                carwidth + enginesize + stroke + compressionratio + 
                peakrpm + drivewheelrwd + enginetypedohcv + 
                enginetypel + enginetypeohcf + enginetyperotor + cylindernumberfive + 
                cylindernumbersix + fuelsystem2bbl + 
                fuelsystemmpfi + makeaudi + makebmw + makedodge + makehonda + 
                makeisuzu + makemazda + makemercury + makemitsubishi + makenissan + 
                makeplymouth + makerenault + makesaab + maketoyota + makevolkswagen + 
                makevolvo , data = train)

## Check the summary of model.
summary(model_7)
vif(model_7)
#Multiple R-squared:  0.973,	Adjusted R-squared:  0.9651 (not much effect from last model so keeping this model) 
#enginesize 17:*** , carwidth 11.99:*** , fuelsystemmmpfi 7.21:.57  
#so removing fuelsystemmmpfi


####Model 8 Removing fuelsystemmmpfi 7.21 VIF in 7th model
model_8 <- lm(price ~ aspiration + enginelocation + wheelbase + 
                carwidth + enginesize + stroke + compressionratio + 
                peakrpm + drivewheelrwd + enginetypedohcv + 
                enginetypel + enginetypeohcf + enginetyperotor + cylindernumberfive + 
                cylindernumbersix + fuelsystem2bbl + 
                makeaudi + makebmw + makedodge + makehonda + 
                makeisuzu + makemazda + makemercury + makemitsubishi + makenissan + 
                makeplymouth + makerenault + makesaab + maketoyota + makevolkswagen + 
                makevolvo + makevolvo , data = train)

## Check the summary of model.
summary(model_8)
vif(model_8)
#Multiple R-squared:  0.9729,	Adjusted R-squared:  0.9653 (not much difference from last values so continuing with this model)
#enginesize 17:*** , carwidth 11.8:***, makehonda 7:*** ,maketoyota 6.8:*** , drivewheelrwd 6.17:**
#removing drivewheelrwd and checking for values of multiple R squared and adjusted R squared


####Model 9 Removing drivewheelrwd - 6.1 VIF in 8th model
model_9 <- lm(price ~ aspiration + enginelocation + wheelbase + 
                carwidth + enginesize + stroke + compressionratio + 
                peakrpm + enginetypedohcv + 
                enginetypel + enginetypeohcf + enginetyperotor + cylindernumberfive + 
                cylindernumbersix + fuelsystem2bbl + 
                makeaudi + makebmw + makedodge + makehonda + 
                makeisuzu + makemazda + makemercury + makemitsubishi + makenissan + 
                makeplymouth + makerenault + makesaab + maketoyota + makevolkswagen + 
                makevolvo , data = train)

## Check the summary of model.
summary(model_9)
vif(model_9)
#Multiple R-squared:  0.9702,	Adjusted R-squared:  0.9622 (not much effect on R squared values so continuing with the model) 
#enginesize 15:***, carwidth 11:*** , maketoyota 6.7:*** , makehonda 6.44:*** , wheelbase 5.8:** 
#so removing wheelbase and checking for values of multiple R squared and adjusted R squared


####Model 10 Removing wheelbase - 5.8 VIF in 9th model
model_10 <- lm(price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke + compressionratio + 
                 peakrpm + enginetypedohcv + 
                 enginetypel + enginetypeohcf + enginetyperotor + cylindernumberfive + 
                 cylindernumbersix + fuelsystem2bbl + 
                 makeaudi + makebmw + makedodge + makehonda + 
                 makeisuzu + makemazda + makemercury + makemitsubishi + makenissan + 
                 makeplymouth + makerenault + makesaab + maketoyota + makevolkswagen + 
                 makevolvo , data = train)

## Check the summary of model.
summary(model_10)
vif(model_10)
#Multiple R-squared:  0.9679,	Adjusted R-squared:  0.9596 (not much effect on R squared values so continuing with the model) 
#enginesize 15:*** , carwidth 7.99:*** , maketoyota 6.7:*** , makehonda 6.2:*** , makemazda 5.0:*** 
#makenissan 4.4:*** , enginetypeohcf 4.2:***, stroke 4.0:*** , makevolkswagen 4.0:*** , makemitsubishi 3.7:***
#makeplymouth 3.13:*** , peakrpm 3.10:*
#removing peakrpm and checking for values of R squared


####Model 11 Removing peakrpm - 3.10 VIF in 11th model
model_11 <- lm(price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke + compressionratio + 
                 enginetypedohcv + 
                 enginetypel + enginetypeohcf + enginetyperotor + cylindernumberfive + 
                 cylindernumbersix + fuelsystem2bbl + 
                 makeaudi + makebmw + makedodge + makehonda + 
                 makeisuzu + makemazda + makemercury + makemitsubishi + makenissan + 
                 makeplymouth + makerenault + makesaab + maketoyota + makevolkswagen + 
                 makevolvo , data = train)

## Check the summary of model.
summary(model_11)
vif(model_11)
#Multiple R-squared:  0.966,	Adjusted R-squared:  0.9577(not much effect on R squared values so continuing with the model)
#enginesize 14:*** , carwidth 7.99:*** , maketoyota 6.7:*** , makehonda 5.85:*** , makemazda 5.0:*** 
#makenissan 4.3:*** , enginetypeohcf 4.15:***, stroke 3.9:*** , makevolkswagen 4.0:*** ,makemitsubishi 3.7:***
#makeplymouth 3.04:*** , fuelsystem2bbl 2.78:0.1
# removing fuelsystem2bbl



####Model 12 Removing fuelsystem2bbl - 2.78 VIF in 12th model
model_12 <- lm(price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke + compressionratio + 
                 enginetypedohcv + 
                 enginetypel + enginetypeohcf + enginetyperotor + cylindernumberfive + 
                 cylindernumbersix + 
                 makeaudi + makebmw + makedodge + makehonda + 
                 makeisuzu + makemazda + makemercury + makemitsubishi + makenissan + 
                 makeplymouth + makerenault + makesaab + maketoyota + makevolkswagen + 
                 makevolvo , data = train)

## Check the summary of model.
summary(model_12)
vif(model_12)
#Multiple R-squared:  0.9652,	Adjusted R-squared:  0.957 (not much effect on R squared values so continuing with the model)
#enginesize 13:*** , carwidth 7.99:*** , maketoyota 6.6:*** , makehonda 5.4:*** , makemazda 5.0:*** 
#makenissan 4.3:*** , enginetypeohcf 4.15:***, stroke 3.9:*** , makevolkswagen 3.7:*** ,makemitsubishi 3.7:***
#makeplymouth 3.04:*** , makevolvo 2.8:*** , makeaudi 2.59:** 
# removing makeaudi



####Model 13 Removing makeaudi 2.59 VIF in 13th model
model_13 <- lm(price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke + compressionratio + 
                 enginetypedohcv + 
                 enginetypel + enginetypeohcf + enginetyperotor + cylindernumberfive + 
                 cylindernumbersix + 
                 makebmw + makedodge + makehonda + 
                 makeisuzu + makemazda + makemercury + makemitsubishi + makenissan + 
                 makeplymouth + makerenault + makesaab + maketoyota + makevolkswagen + 
                 makevolvo , data = train)

## Check the summary of model.
summary(model_13)
vif(model_13)
#Multiple R-squared:  0.9629,	Adjusted R-squared:  0.9546  (not much effect on R squared values so continuing with the model)
#enginesize 10:*** , carwidth 7.51:*** , maketoyota 5.05:*** , makehonda 3.96:*** , makemazda 3.7:*** 
#enginetypeohcf 3.73:***, stroke 3.6:*** , makenissan 3.3:*** , makevolkswagen 2.8:*** ,makemitsubishi 2.8:***
#makeplymouth 2.3:*** , makevolvo 2.4:***  
# now since all the VIF values having value more than 2 have pvalue ***
# so starting to remove variables having pvalue *** but high pvalues
# not removing enginesize and carwidth for now because in dry run i found out they affect R values
# removing maketoyota


####Model 14 Removing maketoyota
model_14 <- lm(price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke + compressionratio + 
                 enginetypedohcv + 
                 enginetypel + enginetypeohcf + enginetyperotor + cylindernumberfive + 
                 cylindernumbersix + 
                 makebmw + makedodge + makehonda + 
                 makeisuzu + makemazda + makemercury + makemitsubishi + makenissan + 
                 makeplymouth + makerenault + makesaab + makevolkswagen + 
                 makevolvo , data = train)

## Check the summary of model.
summary(model_14)
vif(model_14)
#Multiple R-squared:  0.9434,	Adjusted R-squared:  0.9313
# jump of .2 almost noted so putting maketoyota back and removing makehonda


####Model 15 Removing makehonda and putting maketoyota
model_15 <- lm(price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke + compressionratio + 
                 enginetypedohcv + 
                 enginetypel + enginetypeohcf + enginetyperotor + cylindernumberfive + 
                 cylindernumbersix + 
                 makebmw + makedodge + maketoyota + 
                 makeisuzu + makemazda + makemercury + makemitsubishi + makenissan + 
                 makeplymouth + makerenault + makesaab + makevolkswagen + 
                 makevolvo , data = train)

## Check the summary of model.
summary(model_15)
vif(model_15)
#Multiple R-squared:  0.958,	Adjusted R-squared:  0.949 - not much difference from 13th model so continuing
# removing high pvalues now on



####Model 16 Removing compressionratio
model_16 <- lm(price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke + 
                 enginetypedohcv + 
                 enginetypel + enginetypeohcf + enginetyperotor + cylindernumberfive + 
                 cylindernumbersix + 
                 makebmw + makedodge + maketoyota + 
                 makeisuzu + makemazda + makemercury + makemitsubishi + makenissan + 
                 makeplymouth + makerenault + makesaab + makevolkswagen + 
                 makevolvo , data = train)

## Check the summary of model.
summary(model_16)
vif(model_16)
# Multiple R-squared:  0.9579,	Adjusted R-squared:  0.9494  - not much difference from 15th model so continuing
# removing enginetypedohcv


####Model 17 Removing enginetypedohcv
model_17 <- lm(price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke + 
                 enginetypel + enginetypeohcf + enginetyperotor + cylindernumberfive + 
                 cylindernumbersix + 
                 makebmw + makedodge + maketoyota + 
                 makeisuzu + makemazda + makemercury + makemitsubishi + makenissan + 
                 makeplymouth + makerenault + makesaab + makevolkswagen + 
                 makevolvo , data = train)

## Check the summary of model.
summary(model_17)
vif(model_17)
# Multiple R-squared:  0.9573,	Adjusted R-squared:  0.9491 - not much difference from 16th model so continuing
# removing enginetypeohcf



####Model 18 Removing enginetypeohcf
model_18 <- lm(price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke + 
                 enginetypel + enginetyperotor + cylindernumberfive + 
                 cylindernumbersix + 
                 makebmw + makedodge + maketoyota + 
                 makeisuzu + makemazda + makemercury + makemitsubishi + makenissan + 
                 makeplymouth + makerenault + makesaab + makevolkswagen + 
                 makevolvo , data = train)

## Check the summary of model.
summary(model_18)
vif(model_18)
# Multiple R-squared:  0.9386,	Adjusted R-squared:  0.9273  - not much difference from 17th model so continuing
# removing cylindernumberfive



####Model 19 Removing cylindernumberfive
model_19 <- lm(price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke + 
                 enginetypel + enginetyperotor + 
                 cylindernumbersix + 
                 makebmw + makedodge + maketoyota + 
                 makeisuzu + makemazda + makemercury + makemitsubishi + makenissan + 
                 makeplymouth + makerenault + makesaab + makevolkswagen + 
                 makevolvo , data = train)

## Check the summary of model.
summary(model_19)
vif(model_19)
# Multiple R-squared:  0.9385,	Adjusted R-squared:  0.9279   - not much difference from 18th model so continuing
# removing makesaab


####Model 20 Removing makesaab
model_20 <- lm(price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke + 
                 enginetypel + enginetyperotor + 
                 cylindernumbersix + 
                 makebmw + makedodge + maketoyota + 
                 makeisuzu + makemazda + makemercury + makemitsubishi + makenissan + 
                 makeplymouth + makerenault + makevolkswagen + 
                 makevolvo , data = train)

## Check the summary of model.
summary(model_20)
vif(model_20)
# Multiple R-squared:  0.9383,	Adjusted R-squared:  0.9282 - not much difference from 19th model so continuing
# removing makeisuzu



####Model 21 Removing makeisuzu
model_21 <- lm(price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke + 
                 enginetypel + enginetyperotor + 
                 cylindernumbersix + 
                 makebmw + makedodge + maketoyota + 
                 makemazda + makemercury + makemitsubishi + makenissan + 
                 makeplymouth + makerenault + makevolkswagen + 
                 makevolvo , data = train)

## Check the summary of model.
summary(model_21)
vif(model_21)
# Multiple R-squared:  0.9379,	Adjusted R-squared:  0.9283 - not much difference from 20th model so continuing
# removing makeisuzu


####Model 22 Removing makevolvo
model_22 <- lm(price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke + 
                 enginetypel + enginetyperotor + 
                 cylindernumbersix + 
                 makebmw + makedodge + maketoyota + 
                 makemazda + makemercury + makemitsubishi + makenissan + 
                 makeplymouth + makerenault + makevolkswagen, data = train)

## Check the summary of model.
summary(model_22)
vif(model_22)
# Multiple R-squared:  0.9374,	Adjusted R-squared:  0.9283 - not much difference from 21st model so continuing
# removing makevolkswagen


####Model 23 Removing makevolkswagen
model_23 <- lm(price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke + 
                 enginetypel + enginetyperotor + 
                 cylindernumbersix + 
                 makebmw + makedodge + maketoyota + 
                 makemazda + makemercury + makemitsubishi + makenissan + 
                 makeplymouth + makerenault, data = train)

## Check the summary of model.
summary(model_23)
vif(model_23)
# Multiple R-squared:  0.9367,	Adjusted R-squared:  0.9281  - not much difference from 22nd model so continuing
# removing makemercury


####Model 24 Removing makemercury
model_24 <- lm(price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke + 
                 enginetypel + enginetyperotor + 
                 cylindernumbersix + 
                 makebmw + makedodge + maketoyota + 
                 makemazda + makemitsubishi + makenissan + 
                 makeplymouth + makerenault, data = train)

## Check the summary of model.
summary(model_24)
vif(model_24)
# Multiple R-squared:  0.9356,	Adjusted R-squared:  0.9274 - not much difference from 23 model so continuing
# removing makeplymouth



####Model 25 Removing makeplymouth
model_25 <- lm(price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke + 
                 enginetypel + enginetyperotor + 
                 cylindernumbersix + 
                 makebmw + makedodge + maketoyota + 
                 makemazda + makemitsubishi + makenissan + 
                 makerenault, data = train)

## Check the summary of model.
summary(model_25)
vif(model_25)
# Multiple R-squared:  0.9338,	Adjusted R-squared:  0.926 - not much difference from 25 model so continuing
# removing makenissan



####Model 26 Removing makenissan
model_26 <- lm(price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke + 
                 enginetypel + enginetyperotor + 
                 cylindernumbersix + 
                 makebmw + makedodge + maketoyota + 
                 makemazda + makemitsubishi + 
                 makerenault, data = train)

## Check the summary of model.
summary(model_26)
vif(model_26)
# Multiple R-squared:  0.9326,	Adjusted R-squared:  0.9252 - not much difference from 25 model so continuing
# removing makedodge



####Model 27 Removing makedodge
model_27 <- lm(price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke + 
                 enginetypel + enginetyperotor + 
                 cylindernumbersix + 
                 makebmw + maketoyota + 
                 makemazda + makemitsubishi + 
                 makerenault, data = train)

## Check the summary of model.
summary(model_27)
vif(model_27)
# Multiple R-squared:  0.9311,	Adjusted R-squared:  0.9242 - not much difference from 26 model so continuing
# removing makemazda


####Model 28 Removing makemazda
model_28 <- lm(price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke + 
                 enginetypel + enginetyperotor + 
                 cylindernumbersix + 
                 makebmw + maketoyota + 
                 makemitsubishi + 
                 makerenault, data = train)

## Check the summary of model.
summary(model_28)
vif(model_28)
# Multiple R-squared:  0.9291,	Adjusted R-squared:  0.9225 - not much difference from 27 model so continuing
# removing makerenault


####Model 29 Removing makerenault
model_29 <- lm(price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke + 
                 enginetypel + enginetyperotor + 
                 cylindernumbersix + 
                 makebmw + maketoyota + 
                 makemitsubishi, data = train)

## Check the summary of model.
summary(model_29)
vif(model_29)
# Multiple R-squared:  0.927,	Adjusted R-squared:  0.9208 - not much difference from 28 model so continuing
# removing makemitsubishi



####Model 30 Removing makemitsubishi
model_30 <- lm(price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke + 
                 enginetypel + enginetyperotor + 
                 cylindernumbersix + 
                 makebmw + maketoyota, data = train)

## Check the summary of model.
summary(model_30)
vif(model_30)
# Multiple R-squared:  0.9239,	Adjusted R-squared:  0.9181 - not much difference from 29 model so continuing
# removing maketoyota


####Model 31 Removing maketoyota
model_31 <- lm(price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke + 
                 enginetypel + enginetyperotor + 
                 cylindernumbersix + 
                 makebmw, data = train)

## Check the summary of model.
summary(model_31)
vif(model_31)
# Multiple R-squared:  0.922,	Adjusted R-squared:  0.9167 - not much difference from 30 model so continuing
# removing enginetypel


####Model 32 Removing enginetypel
model_32 <- lm(price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke + 
                 enginetyperotor + 
                 cylindernumbersix + 
                 makebmw, data = train)

## Check the summary of model.
summary(model_32)
vif(model_32)
# Multiple R-squared:  0.9184,	Adjusted R-squared:  0.9135 - not much difference from 31 model so continuing
# removing aspiration



####Model 33 Removing aspiration
model_33 <- lm(price ~ enginelocation + 
                 carwidth + enginesize + stroke + 
                 enginetyperotor + 
                 cylindernumbersix + 
                 makebmw, data = train)

## Check the summary of model.
summary(model_33)
vif(model_33)
# Multiple R-squared:  0.9145,	Adjusted R-squared:  0.9101  - not much difference from 32 model so continuing
# removing enginetyperotor



####Model 34 Removing enginetyperotor
model_34 <- lm(price ~ enginelocation + 
                 carwidth + enginesize + stroke + 
                 cylindernumbersix + 
                 makebmw, data = train)

## Check the summary of model.
summary(model_34)
vif(model_34)
# Multiple R-squared:  0.9081,	Adjusted R-squared:  0.904 - not much difference from 33 model so continuing
# removing cylindernumbersix


####Model 35 Removing cylindernumbersix
model_35 <- lm(price ~ enginelocation + 
                 carwidth + enginesize + stroke + 
                 makebmw, data = train)

## Check the summary of model.
summary(model_35)
vif(model_35)
# Multiple R-squared:  0.9024,	Adjusted R-squared:  0.8988 - not much difference from 34 model so continuing
# removing stroke


####Model 36 Removing stroke
model_36 <- lm(price ~ enginelocation + 
                 carwidth + enginesize + makebmw, data = train)

## Check the summary of model.
summary(model_36)
vif(model_36)
# Multiple R-squared:  0.8903,	Adjusted R-squared:  0.8871 - not much difference from 35 model so continuing
# removing makebmw


##################
## PREDICTION 1 ##
##################

test_trial <- test
View(test_trial[,-19])
#Prefiction on test2[,-20] 20 is the column number for price
Predict <- predict(model_36,test_trial[,-19])
test_trial$test_price<-Predict
#r variable is storing the correlation of actual price to predicted price
r<-cor(test_trial$price,test_trial$test_price)
r^2

#the value for r squared is 0.84



#from domain understanding adding peakrpm back and checking for R values
####Model 37 adding peakrpm
model_37 <- lm(price ~ enginelocation + peakrpm + 
                 carwidth + enginesize + makebmw, data = train)

summary(model_37)
vif(model_37)
#Multiple R-squared:  0.9004,	Adjusted R-squared:  0.8968 - it increased so keeping peakrpm
# trying to remove carwidths


####Model 38 remove carwidths
model_38 <- lm(price ~ enginelocation + peakrpm + 
                 enginesize + makebmw, data = train)

summary(model_38)
vif(model_38)
# Multiple R-squared:  0.8502,	Adjusted R-squared:  0.8458  - decreased significantly so keeping it


####Model 39 adding carwidths
model_39 <- lm(price ~ enginelocation + peakrpm + 
                 carwidth + enginesize + makebmw, data = train)

summary(model_39)
vif(model_39)




##################
## PREDICTION 2 ##
##################

test_trial_1 <- test
View(test_trial_1[,-19])
#Prefiction on test_trial_1[,-19] 19 is the column number for price
Predict <- predict(model_39,test_trial_1[,-19])
test_trial_1$test_price<-Predict
#r variable is storing the correlation of actual price to predicted price
r1<-cor(test_trial_1$price,test_trial_1$test_price)
r1^2

# the new R squared value is .85 which is better than previous model So we will be using this model 
# the final important variables are

##################
# enginelocation #
#     peakrpm    #
#    carwidth    #
#   enginesize   #
#     makebmw    #
##################
