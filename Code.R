# ACTL3141 Assignment

##### Package installation and dataset import #####

install.packages('KMsurv')
install.packages('survival')
install.packages('markovchain')
install.packages('diagram')
install.packages('ggplot2')
install.packages('dplyr')
install.packages('lubridate')

library(KMsurv)
library(survival)
library(markovchain)
library(diagram)
library(ggplot2)
library(dplyr)
library(lubridate)

setwd("/Users/jasonkhu/Desktop/ACTL3141/Assignment")
getwd() #check wd
data <- read.csv('CovidHospDataBrasil.csv', header=TRUE, sep=',')
head(data) #observe top rows

##### 1. Descriptive Analysis #####

# Subset data by categorical variables - to be used in later code
data_death <- data %>% filter(covidDeath == TRUE)
data_nodeath <- data %>% filter(covidDeath == FALSE)
data_icu <- data %>% filter(icu == TRUE)
data_noicu <- data %>% filter(icu == FALSE)
data_vaxxed <- data %>% filter(vaccine == TRUE)
data_unvaxxed <- data %>% filter(vaccine == FALSE)

# age distribution + mean + variance + median
age_dist <- ggplot(data, aes(x=age)) + geom_histogram(binwidth=1)
age_dist + labs(title='Distribution of age', 
                y="Number of patients",
                x="Age (nearest birthday) when admitted")
summary(data$age)
age_var <- var(data$age)
age_sd <- sd(data$age)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(data$age)
  # close-to-symmetrical distribution of age
length(data$age)

# age and icu
age_dist_groupedbyicu <- ggplot(data, aes(x=age)) + 
  geom_histogram(aes(color=icu), binwidth=1, position="identity", alpha=0.5)
age_dist_groupedbyicu + labs(title = "Distribution of age - segmented by ICU admittance status",
                             y = "Number of patients",
                             x="Age (nearest birthday) when admitted")

summary(data_icu$age)
age_var_icu <- var(data_icu$age)
age_sd_icu <- sd(data_icu$age)

summary(data_noicu$age)
age_var_noicu <- var(data_noicu$age)
age_sd_noicu <- sd(data_noicu$age)

# age and death
age_dist_groupedbydeath <- ggplot(data, aes(x=age)) + 
  geom_histogram(aes(color=covidDeath), binwidth=1, position="identity", alpha=0.5)
age_dist_groupedbydeath + labs(title = "Distribution of age - segmented by COVID-19 death status",
                               y = "Number of patients",
                               x="Age (nearest birthday) when admitted")

summary(data_death$age)
age_var_death <- var(data_death$age)
age_sd_death <- sd(data_death$age)

summary(data_nodeath$age)
age_var_nodeath <- var(data_nodeath$age)
age_sd_nodeath <- sd(data_nodeath$age)

# Symptoms

  # Symptoms include: fever, cough, sorethroat, dyspnoea, respdistress,oxygensat,
  # diarrhea, vomit 

symptoms_list <- c('fever', 'cough', 'sorethroat', 'dyspnoea', 'respdistress', 
                   'oxygensat','diarrhea', 'vomit')

symptoms_plot_func <- function(input_data){
  symptoms_prop <- c()
  for (symptom in symptoms_list){
    symptoms_prop <- append(symptoms_prop, 
                            sum(as.integer(input_data[,colnames(input_data)==symptom]))/nrow(input_data))
  }
  symptoms_prop
  symptoms_data <- data.frame(symptoms_list, symptoms_prop)
  
  symptoms_plot <- ggplot(symptoms_data, 
                          aes(x=symptoms_prop, 
                              y=reorder(symptoms_list,symptoms_prop))) +
    geom_bar(stat='Identity') +
    scale_x_continuous(labels = scales::percent)
  symptoms_plot + ggtitle("Most common symptoms shown by COVID-19 patients") +
    xlab("Proportion of patient population") + 
    ylab("Symptom")
  print(symptoms_data)
}

symptoms_plot_func(data)

# Comorbidities

  # Comorbidities include: cardio, hematologic, downsyn, hepatic, asthma, diabetes,
  # neurological, pneumopathy, immuno, renal, obesity

comorb_list <- c('cardio', 'hematologic', 'downsyn', 'hepatic', 'asthma', 'diabetes',
                 'neurological', 'pneumopathy', 'immuno', 'renal', 'obesity')

comorbidity_plot_func <- function(input_data){
  comorb_prop <- c()
  for (comorbidity in comorb_list){
    comorb_prop <- append(comorb_prop, 
                            sum(as.integer(input_data[,colnames(input_data)==comorbidity]))/nrow(input_data))
  }
  comorb_data <- data.frame(comorb_list, comorb_prop)
  comorb_plot <- ggplot(comorb_data, 
                          aes(x=comorb_prop, 
                              y=reorder(comorb_list,comorb_prop))) + 
    geom_bar(stat='Identity') +
    scale_x_continuous(labels = scales::percent) 
  comorb_plot + ggtitle("Most common comorbidities in COVID-19 patients") +
    xlab("Proportion of patient population") + 
    ylab("Comorbidity")
  #print(comorb_data)
}

comorbidity_plot_func(data)

# Symptoms vs vaccination status
symptoms_plot_func(data_vaxxed)
symptoms_plot_func(data_unvaxxed)

# Vaccination vs icu/death
icu_plot <- ggplot(data, aes(y=icu)) + 
  geom_bar(aes(x = (..count..)/sum(..count..))) +
  scale_x_continuous(labels = scales::percent) 
icu_plot
icu_plot + facet_grid(.~vaccine) + ggtitle("Proportion of data population admitted to ICU \n(Unvaccinated vs Vaccinated)") +
  xlab("Percentage of entire population") + 
  ylab("Admitted to ICU")

death_plot <- ggplot(data, aes(y=covidDeath)) + 
  geom_bar(aes(x = (..count..)/sum(..count..))) +
  scale_x_continuous(labels = scales::percent)
death_plot
death_plot + facet_grid(.~vaccine) + ggtitle("Proportion of data population dead \n(Unvaccinated vs Vaccinated)") +
  xlab("Percentage of entire population") + 
  ylab("Death status")

##### 2. Mortality & Survival Analysis #####

data$study_time <- as.Date(as.character(data$dateEndObs), format="%Y-%m-%d") -
  as.Date(as.character(data$dateHosp), format="%Y-%m-%d") + 1
data$delta <- as.integer(data$covidDeath)

data$agegt40 <- ifelse(data$age>=40, TRUE, FALSE)
data$agegt60 <- ifelse(data$age>=60, TRUE, FALSE)
data$agegt80 <- ifelse(data$age>=80, TRUE, FALSE)
months <- month(as.POSIXlt(data$dateHosp, format="%Y-%m-%d"))
data$winter <- ifelse(month(as.POSIXlt(data$dateHosp, format="%Y-%m-%d"))>=6 &
                        month(as.POSIXlt(data$dateHosp, format="%Y-%m-%d"))<=8,
                      TRUE, FALSE)
data$summer <- ifelse(month(as.POSIXlt(data$dateHosp, format="%Y-%m-%d"))==12 |
                        month(as.POSIXlt(data$dateHosp, format="%Y-%m-%d"))<=2,
                      TRUE, FALSE)
data$autumn <- ifelse(month(as.POSIXlt(data$dateHosp, format="%Y-%m-%d"))>=3 &
                        month(as.POSIXlt(data$dateHosp, format="%Y-%m-%d"))<=5,
                      TRUE, FALSE)
data$spring <- ifelse(month(as.POSIXlt(data$dateHosp, format="%Y-%m-%d"))>=9 &
                        month(as.POSIXlt(data$dateHosp, format="%Y-%m-%d"))<=11,
                      TRUE, FALSE)

# KM estimates

cens.covid <- Surv(data$study_time, data$delta)
km.covid <- survfit(cens.covid ~ as.factor(data$obesity), conf.int=0.95, conf.type="log")
summary(km.covid)

#log-rank test 
logrank <- survdiff(cens.covid ~ obesity, rho = 0)
logrank

#peto-peto test
peto <- survdiff(cens.covid ~ obesity, rho = 1)
peto

#KM estimates plot
plot(km.covid, xlab = "time",
     ylab = "S(x)", col=c("black", "red"))
title(expression("KM estimates \n(no obesity  vs" * phantom(" obesity") *")"), col.main = "black")
title(expression(phantom("KM estimates \n(no obesity vs") * " obesity" * phantom(")")), col.main = "red")

# Cox regression

attach(data)

cox.covid <- coxph(cens.covid ~ as.factor(data$vaccine), method = "breslow")
summary(cox.covid)

  # To test the PH assumptions, compare trajectory of log cumulative curves

plot(survfit(cens.covid ~ data$obesity), col=c("black", "red"), fun="cumhaz", log="y")
title(expression("Cumulative Hazard Rates \n(no obesity vs" * phantom(" obesity") *")"), col.main = "black")
title(expression(phantom("Cumulative Hazard Rates \n(no obesity vs") * " obesity" * phantom(")")), col.main = "red")

##### 3.1 Transition Intensity Estimates (Singular ages) #####

# Need the total time spent in each state 
# And number of transitions between each state

# Assumptions of my estimates
  # A person who is in icu gets discharged from the icu back into the hospital
    # if the date of discharge from the icu is not the same as the hospital
  # A person who has an icu discharge date = hospital discharge date is discharged
    # directly from the icu (to cured)
  # So a life where the EndObs date is the same as the DisIcu date is IcuToDeath/Cure

age_vector = seq(0,114)

# Number from hospital to death = 
  # number of rows where covidDeath = TRUE AND
  # icu = FALSE OR
  # dateDisIcu != dateEndObs

nHospToDeath <- function(){
  vector <- c()
  for (ageval in age_vector){
    vector <- append(vector, nrow(data %>% filter(covidDeath == TRUE) %>% 
            filter(icu == FALSE | dateDisIcu != dateEndObs)
            %>% filter(age == ageval))
    )
  }
  vector
}

# Number from icu to death = 
  # number of rows where covidDeath = TRUE AND
  # icu = TRUE AND
  # dateDisIcu == dateEndObs

nIcuToDeath <- function(){
  vector <- c()
  for (ageval in age_vector){
    vector <- append(vector, nrow(data %>% filter(covidDeath == TRUE) %>% 
            filter(icu == TRUE & dateDisIcu == dateEndObs)
            %>% filter(age == ageval))
    )
  }
  vector
}

# Number from hosp to cure = 
  # number of rows where covidDeath = FALSE AND
  # icu = FALSE OR
  # dateDisIcu != dateEndObs

nHospToCure <- function(){
  vector <- c()
  for (ageval in age_vector){
    vector <- append(vector, nrow(data %>% filter(covidDeath == FALSE) %>% 
                       filter(icu == FALSE | dateDisIcu != dateEndObs) %>%
                         filter(age == ageval))
    )
  }
  vector
}

# Number from icu to cure = 
  # number of rows where covidDeath = FALSE AND
  # icu = TRUE AND
  # dateDisIcu == dateEndObs

nIcuToCure <- function(){
  vector <- c()
  for (ageval in age_vector){
    vector <- append(vector, nrow(data %>% filter(covidDeath == FALSE) %>% 
                        filter(icu == TRUE & dateDisIcu == dateEndObs) %>%
                          filter(age == ageval))
    )
  }
  vector
}

# Number from hosp to icu = 
  # number of rows where icu = TRUE AND
  # dateAdmIcu != dateHosp

nHospToIcu <- function(){
  vector <- c()
  for (ageval in age_vector){
    vector <- append(vector, nrow(data %>% filter(icu == TRUE & dateAdmIcu != dateHosp)
                                   %>% filter(age == ageval)))
  }
  vector
}

# Number from icu to hosp =
  # number of rows where icu = TRUE AND
  # dateDisIcu != dateEndObs

nIcuToHosp <- function(){
  vector <- c()
  for (ageval in age_vector){
    vector <- append(vector, nrow(data %>% filter(icu == TRUE & dateDisIcu != dateEndObs)
                     %>% filter(age == ageval)))
  }
  vector
}

# Total time spent in icu =
  # Number of days between discharge and admission + 1

data$timeInIcu <- ifelse(is.na(data$dateAdmIcu), 0, as.Date(as.character(data$dateDisIcu), format="%Y-%m-%d") -
  as.Date(as.character(data$dateAdmIcu), format="%Y-%m-%d") + 1)
totalTimeInIcu <- function(){
  vector <- c()
  for (ageval in age_vector){
    data_agesubset <- data %>% filter(age == ageval)
    vector <- append(vector, sum(data_agesubset$timeInIcu))
  }
  vector
} 

# Total time spent in Hosp = 
  # Study time - total time spent in icu

data$timeInHosp <- data$study_time - data$timeInIcu
totalTimeInHosp <- function(){
  vector <- c()
  for (ageval in age_vector){
    data_agesubset <- data %>% filter(age == ageval)
    vector <- append(vector, sum(as.integer(data_agesubset$timeInHosp)))
  }
  vector
} 

# Transition rates across all ages

rateHospToDeath <- nHospToDeath()/totalTimeInHosp()
rateHospToCure <- nHospToCure()/totalTimeInHosp()
rateHospToIcu <- nHospToIcu()/totalTimeInHosp()
rateIcuToDeath <- nIcuToDeath()/totalTimeInIcu()
rateIcuToCure <- nIcuToCure()/totalTimeInIcu()
rateIcuToHosp <- nIcuToHosp()/totalTimeInIcu()

# Plots of the transition rates across age

ageRates <- data.frame(age_vector, rateHospToDeath, rateHospToCure, rateHospToIcu,
                       rateIcuToDeath, rateIcuToCure, rateIcuToHosp)
ggplot(data=ageRates, aes(x=age_vector, y=rateIcuToCure, group=1)) +
  geom_line() + ggtitle("I to C age-specific transition rates") +
  xlab("Singular age") + ylab("Transition rate estimate")

ageRates

# (Transition numbers and total times disaggregated)

#nHospToDeath
nrow(data %>% filter(covidDeath == TRUE) %>% 
       filter(icu == FALSE | dateDisIcu != dateEndObs))
#nHospToCure
nrow(data %>% filter(covidDeath == FALSE) %>% 
       filter(icu == FALSE | dateDisIcu != dateEndObs))
#nHospToIcu
nrow(data %>% filter(icu == TRUE & dateAdmIcu != dateHosp))
#nIcuToDeath
nrow(data %>% filter(covidDeath == TRUE) %>% 
       filter(icu == TRUE & dateDisIcu == dateEndObs))
#nIcuToCure
nrow(data %>% filter(covidDeath == FALSE) %>% 
       filter(icu == TRUE & dateDisIcu == dateEndObs))
#nIcuToHosp
nrow(data %>% filter(icu == TRUE & dateDisIcu != dateEndObs))
#totalTimeInIcu
sum(data$timeInIcu)
#totalTimeInHosp
sum(data$timeInHosp)

##### 3.2 Transition Intensity Estimates (Interval ages) #####

data$age_int = ifelse(data$age>=0 & data$age<10, "0-9",
                      ifelse(data$age>=10 & data$age<20, "10-19", 
                             ifelse(data$age>=20 & data$age<30, "20-29",
                                    ifelse(data$age>=30 & data$age<40, "30-39", 
                                           ifelse(data$age>=40 & data$age<50, "40-49", 
                                                  ifelse(data$age>=50 & data$age<60, "50-59",
                                                         ifelse(data$age>=60 & data$age<70, "60-69",
                                                                ifelse(data$age>=70 & data$age<80, "70-79",
                                                                       ifelse(data$age>=80 & data$age<90, "80-89",
                                                                              ifelse(data$age>=90 & data$age<100, "90-99",
                                                                                     ifelse(data$age>=100 & data$age<110, "100-109","110-119")))))))))))

data[c('age', 'age_int')] #test

age_int_vector = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69",
               "70-79", "80-89", "90-99", "100-109", "110-119")

# Number from hospital to death = 
# number of rows where covidDeath = TRUE AND
# icu = FALSE OR
# dateDisIcu != dateEndObs

nHospToDeath_int <- function(){
  vector <- c()
  for (ageint in age_int_vector){
    vector <- append(vector, nrow(data %>% filter(covidDeath == TRUE) %>% 
                                    filter(icu == FALSE | dateDisIcu != dateEndObs)
                                  %>% filter(age_int == ageint))
    )
  }
  vector
}

# Number from icu to death = 
# number of rows where covidDeath = TRUE AND
# icu = TRUE AND
# dateDisIcu == dateEndObs

nIcuToDeath_int <- function(){
  vector <- c()
  for (ageint in age_int_vector){
    vector <- append(vector, nrow(data %>% filter(covidDeath == TRUE) %>% 
                                    filter(icu == TRUE & dateDisIcu == dateEndObs)
                                  %>% filter(age_int == ageint))
    )
  }
  vector
}

# Number from hosp to cure = 
# number of rows where covidDeath = FALSE AND
# icu = FALSE OR
# dateDisIcu != dateEndObs

nHospToCure_int <- function(){
  vector <- c()
  for (ageint in age_int_vector){
    vector <- append(vector, nrow(data %>% filter(covidDeath == FALSE) %>% 
                                    filter(icu == FALSE | dateDisIcu != dateEndObs) %>%
                                    filter(age_int == ageint))
    )
  }
  vector
}

# Number from icu to cure = 
# number of rows where covidDeath = FALSE AND
# icu = TRUE AND
# dateDisIcu == dateEndObs

nIcuToCure_int <- function(){
  vector <- c()
  for (ageint in age_int_vector){
    vector <- append(vector, nrow(data %>% filter(covidDeath == FALSE) %>% 
                                    filter(icu == TRUE & dateDisIcu == dateEndObs) %>%
                                    filter(age_int == ageint))
    )
  }
  vector
}

# Number from hosp to icu = 
# number of rows where icu = TRUE AND
# dateAdmIcu != dateHosp

nHospToIcu_int <- function(){
  vector <- c()
  for (ageint in age_int_vector){
    vector <- append(vector, nrow(data %>% filter(icu == TRUE & dateAdmIcu != dateHosp)
                                  %>% filter(age_int == ageint)))
  }
  vector
}

# Number from icu to hosp =
# number of rows where icu = TRUE AND
# dateDisIcu != dateEndObs

nIcuToHosp_int <- function(){
  vector <- c()
  for (ageint in age_int_vector){
    vector <- append(vector, nrow(data %>% filter(icu == TRUE & dateDisIcu != dateEndObs)
                                  %>% filter(age_int == ageint)))
  }
  vector
}

# Total time spent in icu =
# Number of days between discharge and admission + 1

data$timeInIcu <- ifelse(is.na(data$dateAdmIcu), 0, as.Date(as.character(data$dateDisIcu), format="%Y-%m-%d") -
                           as.Date(as.character(data$dateAdmIcu), format="%Y-%m-%d") + 1)
totalTimeInIcu_int <- function(){
  vector <- c()
  for (ageint in age_int_vector){
    data_agesubset <- data %>% filter(age_int == ageint)
    vector <- append(vector, sum(data_agesubset$timeInIcu))
  }
  vector
} 

# Total time spent in Hosp = 
# Study time - total time spent in icu

data$timeInHosp <- data$study_time - data$timeInIcu
totalTimeInHosp_int <- function(){
  vector <- c()
  for (ageint in age_int_vector){
    data_agesubset <- data %>% filter(age_int == ageint)
    vector <- append(vector, sum(as.integer(data_agesubset$timeInHosp)))
  }
  vector
} 

# Transition rates across all ages

rateHospToDeath_int <- nHospToDeath_int()/totalTimeInHosp_int()
rateHospToCure_int <- nHospToCure_int()/totalTimeInHosp_int()
rateHospToIcu_int <- nHospToIcu_int()/totalTimeInHosp_int()
rateIcuToDeath_int <- nIcuToDeath_int()/totalTimeInIcu_int()
rateIcuToCure_int <- nIcuToCure_int()/totalTimeInIcu_int()
rateIcuToHosp_int <- nIcuToHosp_int()/totalTimeInIcu_int()

# Plots of the transition rates across age

order_vector <- c(LETTERS[1:12])

ageRates_int <- data.frame(age_int_vector, order_vector, rateHospToDeath_int, rateHospToCure_int, rateHospToIcu_int,
                           rateIcuToDeath_int, rateIcuToCure_int, rateIcuToHosp_int)
ggplot(data=ageRates_int, aes(x=age_int_vector, y=rateIcuToCure_int, group=1)) +
  geom_line() + ggtitle("I to C age-specific transition rates") + ylab("Transition rate estimate") +
  scale_x_discrete(name ="Age interval", limits=age_int_vector)

##### 3.3 Transition Intensity Estimates (Interval ages, split by vaccination status) #####

age_int_vector = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69",
                   "70-79", "80-89", "90-99", "100-109", "110-119")

# Number from hospital to death = 
# number of rows where covidDeath = TRUE AND
# icu = FALSE OR
# dateDisIcu != dateEndObs

nHospToDeath_int_v <- function(){
  vector <- c()
  for (ageint in age_int_vector){
    vector <- append(vector, nrow(data %>% filter(covidDeath == TRUE) %>% 
                                    filter(icu == FALSE | dateDisIcu != dateEndObs)
                                  %>% filter(age_int == ageint) %>% filter(vaccine == TRUE))
    )
  }
  vector
}

nHospToDeath_int_u <- function(){
  vector <- c()
  for (ageint in age_int_vector){
    vector <- append(vector, nrow(data %>% filter(covidDeath == TRUE) %>% 
                                    filter(icu == FALSE | dateDisIcu != dateEndObs)
                                  %>% filter(age_int == ageint) %>% filter(vaccine == FALSE))
    )
  }
  vector
}

# Number from icu to death = 
# number of rows where covidDeath = TRUE AND
# icu = TRUE AND
# dateDisIcu == dateEndObs

nIcuToDeath_int_v <- function(){
  vector <- c()
  for (ageint in age_int_vector){
    vector <- append(vector, nrow(data %>% filter(covidDeath == TRUE) %>% 
                                    filter(icu == TRUE & dateDisIcu == dateEndObs)
                                  %>% filter(age_int == ageint) %>% filter(vaccine == TRUE))
    )
  }
  vector
}

nIcuToDeath_int_u <- function(){
  vector <- c()
  for (ageint in age_int_vector){
    vector <- append(vector, nrow(data %>% filter(covidDeath == TRUE) %>% 
                                    filter(icu == TRUE & dateDisIcu == dateEndObs)
                                  %>% filter(age_int == ageint) %>% filter(vaccine == FALSE))
    )
  }
  vector
}

# Number from hosp to cure = 
# number of rows where covidDeath = FALSE AND
# icu = FALSE OR
# dateDisIcu != dateEndObs

nHospToCure_int_v <- function(){
  vector <- c()
  for (ageint in age_int_vector){
    vector <- append(vector, nrow(data %>% filter(covidDeath == FALSE) %>% 
                                    filter(icu == FALSE | dateDisIcu != dateEndObs) %>%
                                    filter(age_int == ageint) %>% filter(vaccine == TRUE))
    )
  }
  vector
}

nHospToCure_int_u <- function(){
  vector <- c()
  for (ageint in age_int_vector){
    vector <- append(vector, nrow(data %>% filter(covidDeath == FALSE) %>% 
                                    filter(icu == FALSE | dateDisIcu != dateEndObs) %>%
                                    filter(age_int == ageint) %>% filter(vaccine == FALSE))
    )
  }
  vector
}

# Number from icu to cure = 
# number of rows where covidDeath = FALSE AND
# icu = TRUE AND
# dateDisIcu == dateEndObs

nIcuToCure_int_v <- function(){
  vector <- c()
  for (ageint in age_int_vector){
    vector <- append(vector, nrow(data %>% filter(covidDeath == FALSE) %>% 
                                    filter(icu == TRUE & dateDisIcu == dateEndObs) %>%
                                    filter(age_int == ageint) %>% filter(vaccine == TRUE))
    )
  }
  vector
}

nIcuToCure_int_u <- function(){
  vector <- c()
  for (ageint in age_int_vector){
    vector <- append(vector, nrow(data %>% filter(covidDeath == FALSE) %>% 
                                    filter(icu == TRUE & dateDisIcu == dateEndObs) %>%
                                    filter(age_int == ageint) %>% filter(vaccine == FALSE))
    )
  }
  vector
}

# Number from hosp to icu = 
# number of rows where icu = TRUE AND
# dateAdmIcu != dateHosp

nHospToIcu_int_v <- function(){
  vector <- c()
  for (ageint in age_int_vector){
    vector <- append(vector, nrow(data %>% filter(icu == TRUE & dateAdmIcu != dateHosp)
                                  %>% filter(age_int == ageint) %>% filter(vaccine == TRUE)))
  }
  vector
}

nHospToIcu_int_u <- function(){
  vector <- c()
  for (ageint in age_int_vector){
    vector <- append(vector, nrow(data %>% filter(icu == TRUE & dateAdmIcu != dateHosp)
                                  %>% filter(age_int == ageint) %>% filter(vaccine == FALSE)))
  }
  vector
}

# Number from icu to hosp =
# number of rows where icu = TRUE AND
# dateDisIcu != dateEndObs

nIcuToHosp_int_v <- function(){
  vector <- c()
  for (ageint in age_int_vector){
    vector <- append(vector, nrow(data %>% filter(icu == TRUE & dateDisIcu != dateEndObs)
                                  %>% filter(age_int == ageint) %>% filter(vaccine == TRUE)))
  }
  vector
}

nIcuToHosp_int_u <- function(){
  vector <- c()
  for (ageint in age_int_vector){
    vector <- append(vector, nrow(data %>% filter(icu == TRUE & dateDisIcu != dateEndObs)
                                  %>% filter(age_int == ageint) %>% filter(vaccine == FALSE)))
  }
  vector
}

# Total time spent in icu =
# Number of days between discharge and admission + 1

data$timeInIcu <- ifelse(is.na(data$dateAdmIcu), 0, as.Date(as.character(data$dateDisIcu), format="%Y-%m-%d") -
                           as.Date(as.character(data$dateAdmIcu), format="%Y-%m-%d") + 1)

totalTimeInIcu_int_v <- function(){
  vector <- c()
  for (ageint in age_int_vector){
    data_agesubset <- data %>% filter(age_int == ageint) %>% filter(vaccine == TRUE)
    vector <- append(vector, sum(data_agesubset$timeInIcu))
  }
  vector
} 

totalTimeInIcu_int_u <- function(){
  vector <- c()
  for (ageint in age_int_vector){
    data_agesubset <- data %>% filter(age_int == ageint) %>% filter(vaccine == FALSE)
    vector <- append(vector, sum(data_agesubset$timeInIcu))
  }
  vector
} 

# Total time spent in Hosp = 
# Study time - total time spent in icu

data$timeInHosp <- data$study_time - data$timeInIcu

totalTimeInHosp_int_v <- function(){
  vector <- c()
  for (ageint in age_int_vector){
    data_agesubset <- data %>% filter(age_int == ageint) %>% filter (vaccine == TRUE)
    vector <- append(vector, sum(as.integer(data_agesubset$timeInHosp)))
  }
  vector
} 

totalTimeInHosp_int_u <- function(){
  vector <- c()
  for (ageint in age_int_vector){
    data_agesubset <- data %>% filter(age_int == ageint) %>% filter (vaccine == FALSE)
    vector <- append(vector, sum(as.integer(data_agesubset$timeInHosp)))
  }
  vector
} 

# Transition rates across all ages

rateHospToDeath_int_v <- nHospToDeath_int_v()/totalTimeInHosp_int_v()
rateHospToCure_int_v <- nHospToCure_int_v()/totalTimeInHosp_int_v()
rateHospToIcu_int_v <- nHospToIcu_int_v()/totalTimeInHosp_int_v()
rateIcuToDeath_int_v <- nIcuToDeath_int_v()/totalTimeInIcu_int_v()
rateIcuToCure_int_v <- nIcuToCure_int_v()/totalTimeInIcu_int_v()
rateIcuToHosp_int_v <- nIcuToHosp_int_v()/totalTimeInIcu_int_v()

rateHospToDeath_int_u <- nHospToDeath_int_u()/totalTimeInHosp_int_u()
rateHospToCure_int_u <- nHospToCure_int_u()/totalTimeInHosp_int_u()
rateHospToIcu_int_u <- nHospToIcu_int_u()/totalTimeInHosp_int_u()
rateIcuToDeath_int_u <- nIcuToDeath_int_u()/totalTimeInIcu_int_u()
rateIcuToCure_int_u <- nIcuToCure_int_u()/totalTimeInIcu_int_u()
rateIcuToHosp_int_u <- nIcuToHosp_int_u()/totalTimeInIcu_int_u()

# Plots of the transition rates across age

order_vector <- c(LETTERS[1:12])

ageRates_int_v <- data.frame(age_int_vector, order_vector, 
                           rateHospToDeath_int_v, rateHospToCure_int_v, rateHospToIcu_int_v,
                           rateIcuToDeath_int_v, rateIcuToCure_int_v, rateIcuToHosp_int_v,
                           rateHospToDeath_int_u, rateHospToCure_int_u, rateHospToIcu_int_u,
                           rateIcuToDeath_int_u, rateIcuToCure_int_u, rateIcuToHosp_int_u)
ggplot(data=ageRates_int_v, aes(x=age_int_vector, group=1)) +
  geom_line(aes(y=rateIcuToHosp_int_v, colour="Vaccinated")) +
  geom_line(aes(y=rateIcuToHosp_int_u, colour="Unvaccinated")) +
  ggtitle("I to H age-specific transition rates") + ylab("Transition rate estimate") +
  scale_x_discrete(name ="Age interval", limits=age_int_vector)
