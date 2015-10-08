#1. Import Libraries
library(ggplot2)
library(tree)
library(party)
library(leaps)

#2. Data Extract & Clean

train_dat1 <- read.csv("C:/Users/das.sayooj/Downloads/train_dat1")
# dim(train_dat1)
# View(train_dat1)

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

train_dat1$duration_previous[is.na(train_dat1$duration_previous)] = 0
train_dat1$duration_previous[is.null(train_dat1$duration_previous)] = 0
levels(train_dat1$C_previous) <- c("1", "2", "3", "4", "Unknown")
train_dat1$C_previous[is.na(train_dat1$C_previous)] = "Unknown"
train_dat1$C_previous[is.null(train_dat1$C_previous)] = "Unknown"

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

risk$state_cat = ifelse(risk$state %in% c("ID","NY","FL"),1,
                        ifelse(risk$state %in% c("DC","UT","WI","CO","KY","MS","OH","TN"),3,2))

risk$car_val_sig = ifelse(risk$car_value %in% c("g","h","i"),1,0)

hx_lm = lm(risk$risk_factor ~  risk$married_couple+
                                risk$family+
                                risk$age_oldest+
                                risk$age_youngest+
                                risk$state+
                                risk$duration_previous+
                                risk$car_age+
                                risk$car_value+
                                risk$car_val_sig+
                                risk$homeowner,data=risk)
                                #risk$married_couple)
          
summary(hx_lm)
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

leaps<-regsubsets(risk$cost ~ risk$car_age+
                    #risk$car_value+
                    risk$car_age+
                    risk$risk_factor+
                    risk$duration_previous+
                    risk$weekend+
                    risk$timeframe+
                    risk$single+
                    risk$state_cat+
                    risk$family,data=risk,nbest=10)

summary(leaps)

plot(leaps,scale="r2")

subsets(leaps, statistic="rsq")
