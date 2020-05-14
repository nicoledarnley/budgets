library(RODBC)
library(RJDBC)
library(RPostgreSQL)
library(ggplot2)
library(tidyr)
library(GGally)

dbname="dev"  
host='redshift.lampogroup.net'  
port='5439'  
user='nicole_darnley'  
password='uyVs92kgK2MKT2k' 

drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, host=host, 
                 port=port,
                 dbname=dbname, 
                 user=user, 
                 password=password)

df <- dbGetQuery(con, "select date_trunc('day',trial_end),
COUNT(distinct email),
COUNT(DISTINCT (case when purchased_on is not null then email end)) as purchases 
from stripe_rpt.fp_subscription_trials_vw
where trial_start >'2020-03-23'
and trial_end <current_date
group by 1
order by 1"
)

dbDisconnect(con)

###### Linear Regression
# Remove first row
head(df)
df <-tail(df,-1)
colnames(df)[2] <-"trials"


#add columns
#df$cvr <- df$purchases/df$trials
#df$trial_log<- log(df$trials)
#df$id <- seq.int(nrow(df))


ggplot(data = df, aes(x = trials, y = purchases)) +
  geom_point() +
  stat_smooth(method = lm)

#remove rows with more than 2000 trials
#df <- df[!df$trials>2000,]

#linear model
fit <- lm(purchases ~ trials, data = df)
summary(fit)
histogram(fit$residuals)

#Trial Volume Estimates
trial_estimates <- rbind(1269,
                         1985,
                         2058,
                         1474,
                         1561,
                         1436,
                         2963,
                         2022,
                         1260,
                         1658,
                         1426,
                         1419,
                         1513,
                         2368,
                         2455,
                         1758,
                         1862,
                         1713
                         
) 
                  
trial_estimates <-as.data.frame(trial_estimates)
colnames(trial_estimates)<-c('trials')
#trial_estimates$trial_log<- log(trial_estimates$trials) 

#Predict purchases based on trial estimates
predict(fit, data.frame(trial_estimates))


###Polynomial Regression

#ggplot(df, aes(trials, cvr) ) +
#  geom_point() +
#  stat_smooth(method = lm, formula = y ~ poly(x, 4, raw = TRUE))

#fit2 <- lm(cvr ~ trials + I(trials^4), data = df)
#summary(fit2)
#histogram(fit2$residuals)

#predict(fit2, data.frame(trial_estimates))


#rm(list = ls())




