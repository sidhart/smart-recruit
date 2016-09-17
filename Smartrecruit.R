recTrain$Manager_Grade <- as.factor(recTrain$Manager_Grade)
recTrain$Manager_Status <- as.factor(recTrain$Manager_Status)
recTrain$Manager_Gender <- as.factor(recTrain$Manager_Gender)
recTrain$Applicant_BirthDate <- as.Date(recTrain$Applicant_BirthDate, "%m/%d/%Y")
recTrain$Business_Sourced <-  as.factor(recTrain$Business_Sourced)


str(recTrain)
recTrain <- recTrain[-c(24:25)]

#find age
install.packages("lubridate")
library(lubridate)
recTrain$Age_at_appl <- year(recTrain$Application_Receipt_Date)-year(recTrain$Applicant_BirthDate)

recTrain$mgr_yrs_exp <- year(recTrain$Application_Receipt_Date)-year(recTrain$Manager_DOJ)

recTrain$mgr_age_appl <- year(recTrain$Application_Receipt_Date)-year(recTrain$Manager_DoB)
recTrain$mgr_accept_ratio <- (recTrain$Manager_Num_Coded/recTrain$Manager_Num_Application) 
#pincodes
recTrain2 <- sqldf('select recTrain.*,pincodes.State as appl_state from recTrain join pincodes on recTrain.Applicant_City_PIN = pincodes.Pincode')
recTrain2$appl_state <- as.factor(recTrain2$appl_state)

recTrain1$office_state <- 
  
sqldf('select recTrain.appl_city_pin,pin_state.State as appl_state from recTrain left join pin_state on recTrain.appl_city_pin = pin_state.Pincode')

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

recTrain5[is.nan.data.frame(recTrain5)] <- 0


fit1 = glm(Business_Sourced ~ Applicant_Gender + Applicant_Marital_Status
           +Applicant_Qualification+
             mgr_yrs_exp+
             Age_at_appl+
             mgr_age_appl+
             same_zip
             , data=scTrain, family=binomial)
summary(fit1)


