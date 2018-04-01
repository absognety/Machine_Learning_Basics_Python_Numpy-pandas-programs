
#****************************************Automated Logistic Regression code******************************************************#

install.packages("caret")
install.packages("corrplot")
install.packages("dplyr")
install.packages("caTools")
install.packages("gains")
install.packages("tibble")
install.packages("scales")
install.packages("ggplot2")

################ Installing all required packages #######################

library(caret)
library(corrplot)
library(plyr)
library(caTools)
library(gains)
library(tibble)
library(scales)
library(ggplot2)

#########################################################################

ptm <- proc.time()                                      #RECORDING THE START TIME FOR THE CODE

#Get the current working directory

getwd()

#Set the current working Directory

setwd("U:/Documents/veera")

#################Reading SAS file##################

install.packages("haven")

library(haven)

data <- read_sas("dataset.sas7bdat")                     # READING A SAS DATASET INTO R

data <- as.data.frame(data)

#n <- 500                                               # NUMBER OF OBSERVATIONS USER WANTS TO GIVE

#data <- data[1:n, ]                                      # REDUCTION INTO LIMITED NUMBER OF ROWS

length(names(data))

resp <- "CONV"

##################SORTING DATA BY THE TARGET VARIABLE COLUMN##########################  

#data <- data[order(data$depvar), ]

#####################################################################################

event_nonevent <- function(dt,resp = " ")
{
   data <- dt
   i <- which(names(data) == resp)    
   event.rate <- length(which(data[,i] == 1))/nrow(data)      #####EVENT RATE######
   nonevent.rate <- length(which(data[,i] == 0))/nrow(data)   #######NON-EVENT RATE########
   event.per <- event.rate * 100                   #***************PERCENTAGE OF EVENTS*************#
   nonevent.per <- nonevent.rate * 100              #************PERCENTAGE OF NONEVENTS***********#
   eve_nonper <- c(event.per, nonevent.per)
   names(eve_nonper) <- c("Percentage of events", "Percentage of nonevents")
   return(eve_nonper)
}

event_nonevent(data, resp = "CONV")

predictor_predicted <- function(dt, resp = " ")
{
   data <- dt
   data <- as.data.frame(data)
   input <- data[, !names(data) %in% c(resp)]
  depvar <- data[, names(data) %in% c(resp)]
  names(input) <- names(data)[-which(names(data) == resp)]
  vars.depvar <- data.frame(depvar,input)
  names(depvar) <- resp
  return(vars.depvar)
}

#***********************PARTITIONING INTO TRAINING AND VALIDATION SETS****************************#

sample_size <- floor(0.70 * nrow(data))  #*******floor/ceiling based on the circumstances*******#

set.seed(4000)

train.ind <- sample(seq_len(nrow(data)), size = sample_size) 

train.set <- data[+train.ind, ]

test.set <- data[-train.ind, ]

event_nonevent(train.set, resp = "CONV") 

##########******************OVER - SAMPLING IF NONEVENT.PER IS HIGHER*****************###########

train.set_depvar_zero <- train.set[which(train.set$CONV == 0), ]

event_trainset <- train.set[which(train.set$CONV == 1), ]

############## Selecting 1% of non-events ####################

smp_size <- ceiling(0.01*nrow(train.set_depvar_zero))  

ind <- sample(seq_len(nrow(train.set_depvar_zero)), size = smp_size)

nonevent_trainset <- train.set_depvar_zero[+ind, ]
   
############## concatenation of 100% of events and 1% of non-events ###############

train_set <- rbind(nonevent_trainset, event_trainset)

event_nonevent(train_set, resp = "CONV")

train_set <- train_set[order(train_set$CONV), ]

depvar_vars <- predictor_predicted(train_set, resp = "CONV") #SEPARATING PREDICTOR AND TARGET VARIABLES#

depvar_vars <- as_data_frame(depvar_vars)

vars <- depvar_vars[2:ncol(depvar_vars)]

depvar <- depvar_vars[1]

perc_miss <- sum(is.na(vars))/prod(dim(vars)) * 100

vars.char <- vars[, sapply(vars,class) == 'character']      #SEPARATION INTO NUMERIC VARIABLES

vars.num <- vars[, sapply(vars,class) == 'numeric']         #SEPARATION INTO CHARACTER VARIABLES

dim(vars.num)

#if all the entities of a training example are NA's then we just have to remove that from consideration. So, for that

#ind <- apply(vars.num,1, function(x) all(is.na(x)))

#check_test <- vars.num[!ind, ]

#str(mydata1)  #FULL DESCRIPTION WITH NUMBER OF LEVELS FOR CATEGORICAL VARIABLES

#summary(vars.num) #SUMMARY WITH MEAN,MEDIAN,STANDARD DEVIATION AND NUMBER OF MISSING VALUES

#drop_variables <- c(variables with zero standard deviation or having only one level or variables like marital status)

#newdata <- mydata1[c(-column number of a variables)]


#*************************INITIAL VARIABLE SCREENING************************#

mydata1 <- vars.num

df0 <- names(mydata1)

df0 <- as_data_frame(df0)

scores <- c(0.01,0.05,0.25,0.5,0.75,0.95,0.99)

df <- matrix(nrow = ncol(mydata1) ,ncol=length(scores))              #ACCUMULATING THE PERCENTILE SCORES IN A MATRIX

for(i in c(1:ncol(mydata1)))
{
         df[i,] <- quantile(mydata1[[i]], probs = scores, na.rm = TRUE)
}

df <- as_data_frame(df)

df1 <- matrix(nrow = ncol(mydata1),ncol = 1)          #ACCUMULATING THE MEANS OF VARIABLES IN A MATRIX                        

for(i in c(1:ncol(mydata1)))
{
        df1[i,] <- mean(mydata1[[i]], na.rm = TRUE)
}

df1 <- as_data_frame(df1)

df2 <- matrix(nrow = ncol(mydata1),ncol = 1)          #ACCUMULATING THE MEDIANS OF VARIABLES IN A MATRIX

for(i in c(1:ncol(mydata1)))
{
       df2[i,] <- median(mydata1[[i]], na.rm = TRUE)
}

df2 <- as_data_frame(df2)

df3 <- matrix(nrow = ncol(mydata1),ncol = 1)      #ACCUMULATING THE PERCENTAGES OF MISSING VARIABLES OF INDIVIDUAL VARIABLES IN A MATRIX

for(i in c(1:ncol(mydata1)))
{
      df3[i,] <- sum(is.na(mydata1[i]))/prod(dim(mydata1[i]))
}

df3 <- as_data_frame(df3)

df4 <- matrix(nrow = ncol(mydata1), ncol = 1)         #ACCUMULATING THE STANDARD DEVIATIONS IN A MATRIX

for(i in c(1:ncol(mydata1)))
{
  df4[i,] <- sd(mydata1[[i]], na.rm = TRUE)
}

df4 <- as_data_frame(df4)

df5 <- matrix(nrow = ncol(mydata1), ncol = 1)         #ACCUMULATING THE MAXIMUM VALUES IN A MATRIX

for(i in c(1:ncol(mydata1)))
{
       df5[i,] <- max(mydata1[[i]], na.rm = TRUE)
}

df5 <- as_data_frame(df5)

df6 <- matrix(nrow = ncol(mydata1), ncol = 1)        #ACCUMULATING THE MINIMUM VALUES IN A MATRIX

for(i in c(1:ncol(mydata1)))
{
        df6[i,] <- min(mydata1[[i]], na.rm = TRUE)
}

df6 <- as_data_frame(df6)   
   
initial_param <- cbind(df0,df3,df4,df6,df,df5)      #MERGING ALL THE MATRICES USING CBIND COMMAND #SIMILAR TO PROC MEANS#

colnames(initial_param) <- c("Variable","pmiss","StdDev","min","P1","P5","P25","P50","P75","P95","P99","max")

initial_param <- as_data_frame(initial_param)

initial_param


          #***********************VARIABLE EXCLUSION***************************#

exclusion <- c("IP_I", "PRD_D", "HOME_SQR_FT_C", "LOT_SQR_FT_C", "LOR_C", "WRKNG_WMN_F")

vars.afexcluded <- vars.num[, !names(vars.num) %in% c("IP_I")]    #**********Removing IP_I from analysis [NUMERIC VARIABLES]***#

excluded.var <- vars.num[, names(vars.num) %in% c("IP_I")]      #**********Storing IP_I into a separate variable**********#

vars.afexcluded

###################Removing following variables from analysis#################################

###1.PRD_D, 2.HOME_SQR_FT_C, 3.LOT_SQR_FT_C, 4.LOR_C, 5.WRKING_WMN_F [CHARACTER VARIABLES]########################

vars.char_ <- vars.char[, !names(vars.char) %in% c("PRD_D","HOME_SQR_FT_C","LOT_SQR_FT_C","LOR_C","WRKNG_WMN_F")]

char <- vars.char[, names(vars.char) %in% c("PRD_D","HOME_SQR_FT_C","LOT_SQR_FT_C","LOR_C","WRKNG_WMN_F")]

vars.char_

vars.missing <- vars.afexcluded

vars <- cbind(vars.afexcluded,vars.char_)

vars <- as_data_frame(vars)

#######################Dropping Variables with ZERO Variance###########################

initial_param_novariance <- initial_param[which(initial_param$StdDev == 0), ]

vars.novariance <- initial_param_novariance$Variable

vars.afnovariance <- vars[, !names(vars) %in% vars.novariance]

length(vars.afnovariance) 

names(vars.afnovariance)

num_vars <- vars.afnovariance[sapply(vars.afnovariance, is.numeric)]

char_vars <- vars.afnovariance[sapply(vars.afnovariance, is.character)]

#**********************************MISSING VALUE IMPUTATION**********************************#

PercentVarMiss <- function(daf) {

frame <- daf

per.var.miss <- matrix(nrow = ncol(frame), ncol = 1)

for(i in c(1:ncol(frame)))
{
     per.var.miss[i,] <- sum(is.na(frame[i]))/prod(dim(frame[i])) * 100   ##/## summary(input.num)##/##
}

per.var.miss <- as.numeric(per.var.miss)

return(per.var.miss)
}

indiv_var_missing <- PercentVarMiss(num_vars)

all_percent_var_missing <- c(PercentVarMiss(depvar), indiv_var_missing)

missthreshold <- 50            #**********Missing Threshold set as 50%**********#

j <- matrix(nrow = ncol(num_vars), ncol = 1)

for(i in c(1:ncol(num_vars)))
{
    if( indiv_var_missing[i] >= missthreshold)
    {
          j[i,1] <- names(num_vars[i])
    }
    else
          j[i,1] <- NA
}

j

remove_full_miss <- j[!is.na(j)]

class(remove_full_miss)

num_vars_removedthreshold <- num_vars[, !names(num_vars) %in% remove_full_miss]

vars_imputed <- num_vars_removedthreshold

install.packages("Hmisc")

library(Hmisc)

#################variable1, variable2, variable3-------variables to be imputed with 0#######################

impzero <- names(vars_imputed)[c(1:244,264,265,295:395,416,481,482,500,501)]

missingimp_zerovar <- vars_imputed[, names(vars_imputed) %in% impzero]

imp_zero <- missingimp_zerovar

dim(imp_zero)

for(i in c(1:ncol(imp_zero)))
{
    imp_zero[i][is.na(imp_zero[i])] <- 0       #VARIABLES THAT ARE TO BE IMPUTED WITH ZERO # Ex - CREDIT CARD VARIABLES
}

sum(is.na(imp_zero))

################var1, var2, var3------------------variables to be imputed with mean##############################

#miss_method <- data[, names(data) %in% c("var1", "var2", "var3",.........)]

#imp_mean <- miss_method

#dim(imp_mean)

#for(i in c(1:ncol(imp_mean)))
#{
#    imp_mean[i][is.na(imp_mean[i])] <- mean(imp_mean[[i]], na.rm = TRUE)    #VARIABLES THAT ARE TO BE IMPUTED WITH MEAN  
#}                                                                         

######################variab1, variab2, variab3,.........VARIABLES THAT ARE TO BE IMPUTED WITH MEDIAN####################

miss_median <- vars_imputed[, !names(vars_imputed) %in% impzero]

imp_median <- miss_median

dim(imp_median)

for(i in c(1:ncol(imp_median)))
{
      imp_median[i][is.na(imp_median[i])] <- median(imp_median[[i]], na.rm = TRUE)   
}

#imputed_vars <- cbind(imp_zero, imp_mean, imp_median,imp_max/min)

imputed_vars <- cbind(imp_zero, imp_median)

imputed_vars <- as_data_frame(imputed_vars)

sum(is.na(imputed_vars))

######################Removing Higly Correlated Variables#################################

######################Correlation probability set at 0.1##############################


     corr.prob <- c()
     corr.var <- cbind(imputed_vars,depvar)
     for(i in c(1:(ncol(corr.var)-1)))
     {
         corr_spear <- cor.test(corr.var[[i]], corr.var[[which(names(corr.var) == "depvar")]], type = "spearman")
         corr.prob[i] <- corr_spear$p.value
     }
   afrem_corr_var <- imputed_vars[, -which(corr.prob > 0.1)]   #***********Correlation Probability set as 0.1***********#
   
#install.packages("scales")

#library(caret)

#corr_mat <- cor(imputed_vars)

#hc <- findCorrelation(corr_mat, cutoff = 0.5)    #Removing variables with Corr.Coeff greater than 0.5#

#hc <- sort(hc)

#post_corr_vars <- imputed_vars[, -c(hc)]

#dim(post_corr_vars)

#*****************************TREATMENT OF OUTLIERS*********************************#

#IDENTIFICATION OF OUTLIERS

library(scales)

rem_outliers <- function(dt){

afrem_corr_var <- dt

out_data <- afrem_corr_var

for(i in c(1:ncol(out_data)))
{
    out_data[[i]] <- squish(out_data[[i]], quantile(out_data[[i]], c(0.01,0.99)))
}

return(out_data)
}

after_treat_out <- rem_outliers(afrem_corr_var)

      #***********************WEIGHT OF EVIDENCE AND INFORMATION VALUE CALCULATIONS*************************#

install.packages("Information")

library(Information)

names(depvar) <- resp

needed_data <- cbind(after_treat_out, depvar)

IV <- Information::create_infotables(data = needed_data, y = "CONV")

iv_predictors <- IV$Summary

low_iv_var <- iv_predictors[iv_predictors$IV < 0.03, ] #IV threshold set at 0.03 #dropping var with IV less than 0.03

low_iv_names <- low_iv_var$Variable

names_iv_predictors <- iv_predictors$Variable

post_rem_iv <- names_iv_predictors[!names_iv_predictors %in% low_iv_names]

iv_exist_var <- after_treat_out[, names(after_treat_out) %in% post_rem_iv]

#****************************VARIANCE INFLATION FACTOR FOR CONTINUOUS VARIABLES*******************************#

install.packages("usdm")

library(usdm)

in_frame <- as.data.frame(iv_exist_var)

VIF <- vif(in_frame)

VIF

thresh_vif <- 2.5

var_large_vif <- VIF$Variables[VIF$VIF > thresh_vif]

length(var_large_vif)

high_vif_filter <- iv_exist_var[, !names(iv_exist_var) %in% var_large_vif]

dim(high_vif_filter)

                #***************************CLUSTER ANALYSIS****************************#

install.packages("ClustOfVar")

library(ClustOfVar)

if(FALSE)
{

daf <- as.data.frame(high_vif_filter)

clust <- hclustvar(daf)

assign <- cutreevar(clust, k = 4)

assign$cluster

clust_ass <- assign$cluster

cluster_no <- clust_ass[order(names(clust_ass))]

cluster_no <- as.data.frame(cluster_no)

cluster_nam <- rownames(cluster_no)

cluster_var <- iv_predictors[iv_predictors$Variable %in% cluster_nam, ]

cluster_var_sorted <- cluster_var[order(cluster_var$Variable), ]

iv_clusterno <- cbind(cluster_var_sorted, cluster_no)

cluster_one <- iv_clusterno[iv_clusterno$cluster_no == 1, ]

cluster_two <- iv_clusterno[iv_clusterno$cluster_no == 2, ]

cluster_three <- iv_clusterno[iv_clusterno$cluster_no == 3, ]

cluster_four <- iv_clusterno[iv_clusterno$cluster_no == 4, ]

varone <- cluster_one$Variable[cluster_one$IV == max(cluster_one$IV)]

vartwo <- cluster_two$Variable[cluster_two$IV == max(cluster_two$IV)]

varthree <- cluster_three$Variable[cluster_three$IV == max(cluster_three$IV)]

varfour <- cluster_four$Variable[cluster_four$IV == max(cluster_four$IV)]

num_var_model <- high_vif_filter[, names(high_vif_filter) %in% c(varone,vartwo,varthree,varfour)]

dim(num_var_model)

}

    #***************************CATEGORICAL VARIABLE INTO NUMERIC VARIABLE CONVERSION**********************************#

library(Information)

woe_func <- function(dat1, dat2){

char_vars<- dat1

depvar <- dat2

char_depvar <- cbind(char_vars, depvar)

IV_char <- Information::create_infotables(data = char_depvar, y = names(depvar))

for(i in c(1:ncol(char_vars)))
{
   char_vars[[i]] <- as.factor(char_vars[[i]])
}

post_rem_charvar <- char_vars[, names(char_vars) %in% names(IV_char$Tables)]

num_levels <- c()

for(i in c(1:ncol(post_rem_charvar)))
{
   num_levels[i] <- nlevels(post_rem_charvar[[i]])
}

woe_mat <- matrix(nrow = nrow(post_rem_charvar), ncol = ncol(post_rem_charvar))

for(i in c(1:ncol(post_rem_charvar)))
{
      for(j in c(1:nrow(post_rem_charvar)))
      {
           for(k in c(1:num_levels[i]))
           {
                 dat <- as.data.frame(post_rem_charvar[i][j, ])
                 if(dat == IV_char$Tables[[i]][[1]][k])
                 {
                        woe_mat[j,i] <- IV_char$Tables[[i]][[4]][k]
                 }
          }
     }
}

woe_set <- as.data.frame(woe_mat)

names(woe_set) <- names(IV_char$Tables) 

return(woe_set)
}

woe_table_train <- woe_func(char_vars, depvar)

woe_table <- as_data_frame(woe_table_train)

     corr.prob_char <- c()
     woe_depvar <- cbind(woe_table,depvar)
     for(i in c(1:(ncol(woe_depvar)-1)))
     {
         corr_spear <- cor.test(woe_depvar[[i]], woe_depvar[[which(names(woe_depvar) == "CONV")]], type = "spearman")
         corr.prob_char[i] <- corr_spear$p.value
     }
   afrem_corr_char_var <- woe_table[, -which(corr.prob_char > 0.1)]

library(usdm)

in_frame_char <- as.data.frame(afrem_corr_char_var)

VIF_char <- vif(in_frame_char)

VIF_char

thresh_vif <- 2.5

var_char_large_vif <- VIF_char$Variables[VIF_char$VIF > thresh_vif]

length(var_char_large_vif)

high_vif_filter_char <- afrem_corr_char_var[, !names(afrem_corr_char_var) %in% var_char_large_vif]

dim(high_vif_filter_char)

           ***********************************LOGISTIC REGRESSION********************************#

model_var <- cbind(high_vif_filter,high_vif_filter_char,depvar)

fit.lm_log <- glm(CONV~.,family = binomial(link = 'logit'), data = model_var)

plot(fit.lm_log, 1)

plot(fit.lm_log, 2)

plot(fit.lm_log, 3)

plot(fit.lm_log, 4)

nullmod <- glm(CONV~1, family = "binomial", data = model_var)

R2 <- 1-logLik(fit.lm_log)/logLik(nullmod)

summary(fit.lm_log)

#predictedprob <- predict(fit.lm, test.set, type = 'response')

library(broom)

model_frame <- tidy(fit.lm_log)

final_var_selected <- model_frame[model_frame$p.value < 0.05, ]

names_final_var <- final_var_selected$term

final_var_train <- model_var[, names(model_var) %in% names_final_var]

model_var_rev <- cbind(final_var_train,depvar)

fit.lm_log_rev <- glm(CONV~., family = "binomial", data = model_var_rev)

summary(fit.lm_log_rev)

model_frame_rev <- tidy(fit.lm_log_rev)

final_var_selected_rev <- model_frame_rev$term[model_frame_rev$p.value < 0.05]

model_var_rev_two <- model_var_rev[, names(model_var_rev) %in% final_var_selected_rev]

fit.lm_log_rev_two <- glm(CONV~., family = "binomial", data = cbind(model_var_rev_two, depvar))

final_var_test <- test.set[, names(test.set) %in% names(model_var_rev_two)]


#************************************************MODEL VALIDATION********************************************************#


#************************************K-S TEST*****************************************#


#ks.test(x,y)         #x and y must be numeric vectors

ks.test(model_var_rev_two[1:9], model_var_rev[[10]])


              #********************RANK-ORDERING**************************#






                                        
              #*******************AREA UNDER CURVE************************#

install.packages("caTools")

library(caTools)

#********colAUC(X, y,plotROC = TRUE,alg = c("Wilcoxon","ROC"))**********#

colAUC(input.num,input.num[,column number of the response variable])    #Area under curve of all the features are calculated

install.packages("pROC)

library(pROC)

auc(CONV ~ predict(fit.lm_log_rev_two), data = cbind(model_var_rev_two, depvar))


                       #*********************GAIN AND LIFT CHARTS**********************#


install.packages("gains")

library(gains)

gain <- function(predictedprob,depvar){

gain.chart <- gains(depvar,predictedprob, groups = 10, ties.method=c("max","min","first","average","random"),
                                       conf=c("none","normal","t","boot"), boot.reps=1000, conf.level=0.95,
                                                                               optimal=TRUE,percents= TRUE) 

plot(gain.chart) ############### or plot.gains(gain.chart)####################

################################POPULATION STABILITY INDEX AND CHARACTERISTIC ANALYSIS###############################






proc.time()-ptm










#*input : Development Dataset*/

#/*validdata : Validation Dataset*/

#/*sampling : % of non-events selected. If no samling required, put 1 */

#/*depvar : Dependent Variable (Binary)*/

#/*vars : Independent Variables*/

#/*exclusion : Variables to be excluded */

#/*missingvar  : Variables to be imputed with 0*/

#/*miss_method : Missing Imputation Method - Median*/

#/*missthresh  : Missing Threshold. Above the threshold, variables would be dropped*/

#/*topvars  : Top variables based on IV. Variables to be used on Stepwise Regression*/

#/*pcap        : Percentile Capping*/

#/*maxlevels : Maximum levels in a categorical variable to be used in the model*/

#/*outloc : Output location in which final result would be stored. The file name would be "Results.xls" */

#/*out_woe : Data file in which outlier treatment and mising imputation already done*/

#*****************************************************Vikas Chitturi**************************************************************#




































                     
                    






            












