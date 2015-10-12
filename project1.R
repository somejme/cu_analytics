# Predictive Analytics BANA 6660 
# Project 1
# Submission by: Courtney Baughman, Jamie White, Sayooj Das
# On: 15 October 2015
# 
# Objective: 
#   To predict the cost of the insurance policy at a line level per the provided data set using a regression model.
# 


# Dataset available: 
#
data = read.csv("C:/Users/sayooj/SkyDrive/Documents/UC Denver/Fall 2015/Predictive Analytics/train_dat1")
dim(data)
str(data)

# Metadata for given dataset:  
# customer_ID   - A unique identifier for the customer
# shopping_pt   - Unique identifier for the shopping point of a given customer
# record_type   - 0=shopping point, 1=purchase point
# day           - Day of the week (0-6, 0=Monday)
# time          - Time of day (HH:MM)
# state         - State where shopping point occurred
# location      - Location ID where shopping point occurred
# group_size    - How many people will be covered under the policy (1, 2, 3 or 4)
# homeowner     - Whether the customer owns a home or not (0=no, 1=yes)
# car_age       - Age of the customer's car
# car_value     - How valuable was the customer's car when new
# risk_factor   - An ordinal assessment of how risky the customer is (1, 2, 3, 4)
# age_oldest    - Age of the oldest person in customer group
# age_youngest  - Age of the youngest person in customer's group
# married_couple  - Does the customer group contain a married couple (0=no, 1=yes)
# C_previous      - What the customer formerly had or currently has for product option C (0=nothing, 1, 2, 3,4)
# duration_previous -  how long (in years) the customer was covered by their previous issuer
# cost          - cost of the quoted coverage options


cor(data[,c(2,3,4,5,8,9,10,11,13,14,15,16,17,18,19)],use="pairwise",method="spearman")

# The correlation matrix above demonstrates that the correlation between the response variabe, cost, and the predictors is extremely low.
# The interesting correlations in the matrix were: 
#   cost-homeowner (-22%)
#   cost-car_age (-28%)
#   age_oldest_age_youngest (90%)
#   cost-age_oldest (-22%)
#   cost-age_youngest (-26%)
#   cost-duration_previous (-17%)
#   risk_factor-homeowner (-18%)
#   risk_factor-age_oldest (-30%)
#   risk_factor-age_youngest (-29%)
#   risk_factor-C_previous (-13%)
#   risk_factor-duration_previous (-17%)


# Feature adjustment to fit model

# convert the following columns to factors
data$X = as.factor(data$X)
data$customer_ID = as.factor(data$customer_ID)
data$shopping_pt = as.factor(data$shopping_pt)
data$record_type = as.factor(data$record_type)
data$day = as.factor(data$day)
data$location = as.factor(data$location)
data$homeowner = as.factor(data$homeowner)
data$risk_factor = as.factor(data$risk_factor) ##in-doubt
data$married_couple = as.factor(data$married_couple)
data$C_previous = as.factor(data$C_previous)

# Derived predictors

# Weekend
data$weekend = as.factor(ifelse(data$day %in% 0:4, "N", "Y"))


# Timeframe
data$HH = as.integer(sub(":.*", "", data$time)) #get the HOUR from TIME and convert to INTEGER
      ## pic why split
data$timeframe = as.factor(ifelse(data$HH %in% 8:17, "bus_hours","non_bus_hours"))
                                       
# Group features

data$age_diff = data$age_oldest-data$age_youngest

data$single = as.factor(ifelse(data$age_diff==0 & data$group_size==1,
                                     "Y", "N"))

data$family = as.factor(ifelse(data$group_size >= 2 
                                     & data$married_couple==1, "Y", "N"))


data$family_w_kids = as.factor(ifelse(data$group_size > 2 
                                            & data$married_couple==1 
                                            & data$age_youngest<=21 , "Y", "N"))

data$homeowner = ifelse(data$homeowner=="1",1,0)
data$single = ifelse(data$single=="Y",1,0)
data$family = ifelse(data$family=="Y",1,0)
data$family_w_kids = ifelse(data$family_w_kids=="Y",1,0)

data$risk_factor = ifelse(data$risk_factor=="1",1,
                          ifelse(data$risk_factor=="2",2,
                                 ifelse(data$risk_factor=="3",3,
                                        ifelse(data$risk_factor=="4",4,NA))))




# Missing Value Analysis:





# Filling missing NA values:

# C_previous

levels(data$C_previous) <- c("1", "2", "3", "4", "Unknown")
data$C_previous[is.na(data$C_previous)] = "Unknown"

# duration_previous
# We noticed that the duration_previous is always NA when the C_previous is NA, and vice-versa. 

data$duration_previous[is.na(data$duration_previous)] = 0


# risk_factor

risk <- data[!is.na(data$risk_factor), ]
norisk <- data[is.na(data$risk_factor), ]

# Per our correlation matrix, the risk factor variable is not highly correlated with any single predictor variable. Considering the 
# risk factor is an ordinal numerical variable, a multiple linear regression model was used to predict the missing values and ROUNDED to the 
# nearest integer. The predictor variables were determined via Exploratory Data Analysis, the visuals of which are as follows: 

#Categorize State based on classification tree

# pic: risk factor distribution by state
# pic: risk factor distribution by age youngest
# pic: classification tree by state

hx_tree = ctree(risk$risk_factor ~  risk$state)

plot (hx_tree, main="Conditional Inference Tree")  # the ctree

data$state_cat = ifelse(data$state %in% c("ID","NY","FL"),1,
                        ifelse(data$state %in% c("DC","UT","WI","CO","KY","MS","OH","TN"),3,2))

hx_lm = lm(risk_factor ~  #risk$married_couple+
                               single+
                               age_oldest+
                               age_youngest+
                               state_cat+
                               duration_previous+
                               car_age+
                               car_value+
                               homeowner,data=risk)


summary(hx_lm)

# An assessment of the misclassification of risk factors was performed against the dataset with populated risk factors.
# The linear model predicted risk factor with a 34% accuracy rating. We chose to proceed with this approach considering it is
# better than the 25% random assumption. 

pred.response.lm = as.character(hx_lm$fitted.values,data=risk)
input.response.lm = as.character (risk$risk_factor) # actuals
#View(cbind(input.response.lm,substr(pred.response.lm,1,1)))
mean (input.response.lm != substr(pred.response.lm,1,1)) # misclassification %





# Populated the missing risk factor in the original dataset
norisk$risk_factor = round(predict(hx_lm,newdata=norisk))

data_wo_NA = rbind(risk,norisk)

dim(data_wo_NA)




## Using the correlation matrix, we selected variables that had a relaively high correlation with the response variable, Cost. 
# Additionally, we performed EDA for the non-numeric variables in Tableau. Below are the plots: 

# pic: Cost by weekend
# pic: Cost by State
# pic: Cost by Car Value
# pic: Cost by time
# pic: Cost by risk factor (Courtney needs complete dataset)


lm_cost = lm (data_wo_NA$cost ~ data_wo_NA$risk_factor+
                              data_wo_NA$car_age+
                              data_wo_NA$car_value+
                              data_wo_NA$state_cat+
                              data_wo_NA$age_oldest+
                              data_wo_NA$age_youngest+
                              data_wo_NA$family+
                              data_wo_NA$timeframe+
                              data_wo_NA$weekend+
                              data_wo_NA$duration_previous+
                              data_wo_NA$homeowner)


summary(lm_cost)




# Interpretations from the Linear Model: 

# 1. Car Valu has the most signifcant impact on Cost. 
# 2. State category
# 3. Homeowner













#1. Import Libraries
library(ggplot2)
library(tree)
library(party)
library(leaps)



#Data preparation function
#convert the following columns to factors
train_dat1$X = as.factor(train_dat1$X)
train_dat1$customer_ID = as.factor(train_dat1$customer_ID)
train_dat1$shopping_pt = as.factor(train_dat1$shopping_pt)
train_dat1$record_type = as.factor(train_dat1$record_type)
train_dat1$day = as.factor(train_dat1$day)
train_dat1$location = as.factor(train_dat1$location)
train_dat1$homeowner = as.factor(train_dat1$homeowner)
train_dat1$risk_factor = as.factor(train_dat1$risk_factor) ##in-doubt
train_dat1$married_couple = as.factor(train_dat1$married_couple)
train_dat1$C_previous = as.factor(train_dat1$C_previous)

#Derived variables
train_dat1$weekend = as.factor(ifelse(train_dat1$day %in% 0:4, "N", "Y"))

# table(train_dat1$weekend)
# N         Y 
# 327901    4723 

# summary(train_dat1[train_dat1$weekend =="Y",]$cost)
# summary(train_dat1[train_dat1$weekend =="N",]$cost)


train_dat1$HH = as.integer(sub(":.*", "", train_dat1$time)) #get the HOUR from TIME and convert to INTEGER

train_dat1$timeframe = as.factor(ifelse(train_dat1$HH %in% 6:15, "day",
                                        ifelse(train_dat1$HH %in% 16:18, "evening",
                                               "night")))

# table(train_dat1$timeframe)
# day       evening   night 
# 280477    47793     4354 

train_dat1$family = as.factor(ifelse(train_dat1$group_size >= 2 
                                     & train_dat1$married_couple==1, "Y", "N"))

# nrow(train_dat1[train_dat1$family=="Y",])
# 59657

train_dat1$family_w_kids = as.factor(ifelse(train_dat1$group_size > 2 
                                            & train_dat1$married_couple==1 
                                            & train_dat1$age_youngest<=21 , "Y", "N"))

# nrow(train_dat1[train_dat1$family_w_kids=="Y",])
# 2801

train_dat1$age_diff = train_dat1$age_oldest-train_dat1$age_youngest

train_dat1$single = as.factor(ifelse(train_dat1$age_diff==0 & train_dat1$group_size==1,
                                     "Y", "N"))

# nrow(train_dat1[train_dat1$single=="Y",])
# 247638


##########################
#Fill duration_previous and C_previous
##########################

# train_dat1$duration_previous[is.na(train_dat1$duration_previous)] = 0
# train_dat1$duration_previous[is.null(train_dat1$duration_previous)] = 0
# levels(train_dat1$C_previous) <- c("1", "2", "3", "4", "Unknown")
# train_dat1$C_previous[is.na(train_dat1$C_previous)] = "Unknown"
# train_dat1$C_previous[is.null(train_dat1$C_previous)] = "Unknown"
# 
# table(train_dat1[train_dat1$duration_previous==0,]$C_previous) #roughly 20k not covered by previous issuer
# table(train_dat1[train_dat1$duration_previous!=0,]$C_previous)
# summary(train_dat1[train_dat1$duration_previous==0,]$cost)


##########################
#
#Fill incomplete RISK factors
#
##########################


risk <- train_dat1[!is.na(train_dat1$risk_factor), ]
norisk <- train_dat1[is.na(train_dat1$risk_factor), ]

# str(risk)
risk$homeowner = ifelse(risk$homeowner=="1",1,0)
risk$single = ifelse(risk$single=="Y",1,0)
risk$family = ifelse(risk$family=="Y",1,0)
risk$family_w_kids = ifelse(risk$family_w_kids=="Y",1,0)

risk$risk_factor = ifelse(risk$risk_factor=="1",1,
                          ifelse(risk$risk_factor=="2",2,
                                 ifelse(risk$risk_factor=="3",3,
                                        ifelse(risk$risk_factor=="4",4,NA))))

norisk$risk_factor = as.numeric(norisk$risk_factor)
# str(norisk)

norisk$homeowner = ifelse(norisk$homeowner=="1",1,0)
norisk$single = ifelse(norisk$single=="Y",1,0)
norisk$family = ifelse(norisk$family=="Y",1,0)
norisk$family_w_kids = ifelse(norisk$family_w_kids=="Y",1,0)

# View(risk)



# plot(hx)
# text(hx, pretty=0) 
# summary(hx)
# 
# cv_hx = cv.tree(hx,FUN=prune.misclass)
# names(cv_hx)
# 
# prune_mod = prune.tree(hx, best = cv_hx$size[which.min(cv_hx$dev)]) 
# plot(prune_mod)
# text(prune_mod)



risk$car_val_sig = ifelse(risk$car_value %in% c("g","h","i"),1,0)


# 
# RMSE <- sqrt(mean((risk$risk_factor-hx$fitted.values)^2))

hx_tree = ctree(risk$risk_factor ~  risk$state)

plot (hx_tree, main="Conditional Inference Tree")  # the ctree


#############Prediction###############

pred.response.lm = as.character(hx_lm$fitted.values,data=risk)
input.response.lm = as.character (risk$risk_factor) # actuals
#View(cbind(input.response.lm,substr(pred.response.lm,1,1)))
mean (input.response.lm != substr(pred.response.lm,1,1)) # misclassification %

pred.response = as.character(predict(hx_tree),data=risk) # predict on test data
input.response = as.character (risk$risk_factor) # actuals
View(cbind(input.response,substr(pred.response,1,1)))
mean (input.response != substr(pred.response,1,1)) # misclassification %


View(risk)


############################ Independent model on recods with a risk factor#################################

lm_risk = lm(risk$cost ~ risk$car_age+
                          #risk$car_value+
                          #risk$car_age+
                          risk$risk_factor+
                          risk$duration_previous+
                          risk$weekend+
                          risk$timeframe+
                          risk$single+
                          risk$state_cat+
                          risk$family,data=risk)

summary(lm_risk)

lm_cost = lm (data_wo_NA$cost ~ data_wo_NA$risk_factor+
                data_wo_NA$car_age+
                data_wo_NA$car_value+
                data_wo_NA$state_cat+
                data_wo_NA$age_oldest+
                data_wo_NA$age_youngest+
                data_wo_NA$family+
                data_wo_NA$timeframe+
                data_wo_NA$weekend+
                data_wo_NA$duration_previous+
                data_wo_NA$homeowner)

leaps<-regsubsets(data_wo_NA$cost ~ data_wo_NA$risk_factor+
                    data_wo_NA$car_age+
                    data_wo_NA$car_value+
                    data_wo_NA$state_cat+
                    data_wo_NA$age_oldest+
                    data_wo_NA$age_youngest+
                    data_wo_NA$family+
                    data_wo_NA$timeframe+
                    data_wo_NA$weekend+
                    data_wo_NA$duration_previous+
                    data_wo_NA$homeowner,data=data_wo_NA,nbest=10)

summary(leaps)

plot(leaps,scale="r2")

subsets(leaps, statistic="rsq")

write.csv(train_dat1, "C:/Users/sayooj/SkyDrive/Documents/UC Denver/Fall 2015/Predictive Analytics/train_dat1.csv")
