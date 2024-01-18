####################### set the working environment ################

setwd()
getwd()

######################data cleaning#####################
data <- read.csv("ISSP_Environment_2010.csv")
nrow(data)
# total 50437 observations

install.packages("dplyr")
library(dplyr)


#only use the data from our chosen countries
df <- data %>%
  filter(c_alphan %in% c("US", "GB-GBN", "JP", "DE", "FR", "CA", "MX", "PH", "AR", "CL"))
nrow(df)
# 13713 left after filtering

df <- df %>%
  mutate(c_flag = ifelse(c_alphan %in% c("US", "GB-GBN", "JP", "DE", "FR", "CA"), 1, 0))
summary(df$c_flag) # 60.6% observations are from developed countries


variables <- c("c_alphan", "c_flag", "v18", "v19", "v29", "v30", "v31", 
               "v39", "v40", "v41", "v42", "v43", "v44", "v45", "v55", "v56",
               "v57", "v58", "v59", "v60")
df <- na.omit(df[variables])
#since 8,9 = don't want to answer, we also omit this number
df_filtered <- df[!apply(df, 1, function(row) any(row %in% c(8, 9))), , drop = FALSE]
#now 7443 observations left


#In order to reflect that respondents who express higher agreement also have 
#higher levels of environmental concern, a reassignment of values was applied to 
#the "Willingness to Contribute to Environmental Protection", "environment 
#awareness measurement" and "Daily Environmental Actions" indicators in the questionnaire. 
df_filtered$v29r <- 6 - df_filtered$v29
df_filtered$v30r <- 6 - df_filtered$v30
df_filtered$v31r <- 6 - df_filtered$v31
df_filtered$v39r <- 6 - df_filtered$v39
df_filtered$v40r <- 6 - df_filtered$v40
df_filtered$v41r <- 6 - df_filtered$v41
df_filtered$v42r <- 6 - df_filtered$v42
df_filtered$v43r <- 6 - df_filtered$v43
df_filtered$v44r <- 6 - df_filtered$v44
df_filtered$v45r <- 6 - df_filtered$v45
df_filtered$v55r <- 6 - df_filtered$v55
df_filtered$v56r <- 6 - df_filtered$v56
df_filtered$v57r <- 6 - df_filtered$v57
df_filtered$v58r <- 6 - df_filtered$v58
df_filtered$v59r <- 6 - df_filtered$v59
df_filtered$v60r <- 6 - df_filtered$v60

summary(df_filtered)

#center the variable
df_filtered$v18C <- df_filtered$v18 -3
df_filtered$v19C <- df_filtered$v19 -3
df_filtered$v29rC <- df_filtered$v29r -3
df_filtered$v30rC <- df_filtered$v30r -3
df_filtered$v31rC <- df_filtered$v31r -3
df_filtered$v39rC <- df_filtered$v39r -3
df_filtered$v40rC <- df_filtered$v40r -3
df_filtered$v41rC <- df_filtered$v41r -3
df_filtered$v42rC <- df_filtered$v42r -3
df_filtered$v43rC <- df_filtered$v43r -3
df_filtered$v44rC <- df_filtered$v44r -3
df_filtered$v45rC <- df_filtered$v45r -3
df_filtered$v55rC <- df_filtered$v55r -3
df_filtered$v56rC <- df_filtered$v56r -3
df_filtered$v57rC <- df_filtered$v57r -3
df_filtered$v58rC <- df_filtered$v58r -3
df_filtered$v59rC <- df_filtered$v59r -3
df_filtered$v60rC <- df_filtered$v60r -3

summary(df_filtered)

#test the Cronbach's α. Cronbach's alpha is a measure of internal consistency, 
#assessing how well a set of items in a questionnaire or scale reliably measures 
#a single underlying construct. It ranges from 0 to 1, with higher values 
#indicating greater reliability. An ideal alpha value should be greater than 0.7.
install.packages("psych")
library(psych)

awareness <- c("v39rC", "v40rC", "v41rC", "v42rC", "v43rC", "v44rC", "v45rC")
result1 <- alpha(df_filtered[, awareness])
result1 # alpha = 0.81

willingness <- c("v29rC", "v30rC", "v31rC")
result2 <- alpha(df_filtered[, willingness])
result2 # alpha = 0.82

action <- c("v55rC", "v56rC", "v57rC", "v58rC", "v59rC", "v60rC")
result3 <- alpha(df_filtered[, action])
result3 # alpha = 0.79

knowledge <- c("v18C", "v19C")
result4 <- alpha(df_filtered[, knowledge])
result4 # alpha = 0.77 

#create IV & DV
df_filtered$awareness <- rowMeans(df_filtered[,awareness])
df_filtered$willingness <- rowMeans(df_filtered[,willingness])
df_filtered$action <- rowMeans(df_filtered[,action])
df_filtered$knowledge <- rowMeans(df_filtered[,knowledge])

##################analysis start!##########################
#descriptive statistics
describe(df_filtered)
summary(df_filtered)

#correlation matrix
vars <- c("c_flag", "awareness", "willingness", "action", "knowledge")
cor_matrix <- cor(df_filtered[, vars])
cor_matrix

#build the model
library(stargazer)
lm1 <- lm(willingness ~ c_flag + awareness + action + knowledge, data = df_filtered)
stargazer::stargazer(lm1, type="text")

#check assumptions
library(ggplot2)
library(car)
plot(lm1, which = 1) 
plot(lm1, which = 2)

scatterplot(fitted(lm(willingness ~ c_flag, data = df_filtered)),
            resid(lm(willingness ~ c_flag, data = df_filtered)))
scatterplot(fitted(lm(willingness ~ awareness, data = df_filtered)),
            resid(lm(willingness ~ awareness, data = df_filtered)))
scatterplot(fitted(lm(willingness ~ action, data = df_filtered)),
            resid(lm(willingness ~ action, data = df_filtered)))
scatterplot(fitted(lm(willingness ~ knowledge, data = df_filtered)),
            resid(lm(willingness ~ knowledge, data = df_filtered)))

scatterplot(df_filtered$awareness, df_filtered$willingness, jitter = list(x = 1, y = 1))
scatterplot(df_filtered$c_flag, df_filtered$willingness, jitter = list(x = 1, y = 1))
scatterplot(df_filtered$action, df_filtered$willingness, jitter = list(x = 1, y = 1))
scatterplot(df_filtered$knowledge, df_filtered$willingness, jitter = list(x = 1, y = 1))

#test interaction effect
df_filtered$Caware <- df_filtered$awareness * df_filtered$c_flag
df_filtered$Cact <- df_filtered$action * df_filtered$c_flag
df_filtered$Cknow <- df_filtered$knowledge * df_filtered$c_flag

lm2 <- lm(willingness ~ c_flag + awareness + Caware, data = df_filtered) 
stargazer(lm2, type = "text")  

lm3 <- lm(willingness ~ c_flag + action + Cact, data = df_filtered)
stargazer(lm3, type = "text")

lm4 <- lm(willingness ~ c_flag + knowledge + Cknow, data = df_filtered)
stargazer(lm4, type = "text")



                              