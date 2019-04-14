library(RMySQL)
library(DBI)

mydb = DBI::dbConnect(MySQL(),
                      user='greenwic_nwu_147',
                      password='xv@y@V~gzCO%',
                      dbname='greenwic_edu_nwu',
                      host='67.225.186.169')

#data for Nursing Assistant
NursingAssitant = dbGetQuery(mydb, "select * from greenwich_master_nwu_fall_2018_proj1
                             where vertical = 'Healthcare' and job_id in (select job_id from greenwich_role_nwu_fall_2018_proj1
                             where role = 'Nursing Assistant')")

#data for Nurse Manager
NurseManager = dbGetQuery(mydb, "select * from greenwich_master_nwu_fall_2018_proj1
                          where vertical = 'Healthcare' and job_id in (select job_id from greenwich_role_nwu_fall_2018_proj1
                          where role = 'Nurse Manager')")

#add a new column called statecounty which is just a combination of state and county
NursingAssitant['statecounty'] <- paste(NursingAssitant$state, NursingAssitant$county)
NurseManager['statecounty'] <-  paste(NurseManager$state, NurseManager$county)

#load county population data
county_pop <- read.csv('county_pop2.csv')

#combine two datasets with the county population dataset
NurseM <- merge(NurseManager, county_pop, by.x="statecounty", 
                by.y="statecounty", all.x = TRUE)
NursingA <-  merge(NursingAssitant, county_pop, 
                   by.x="statecounty", by.y="statecounty", all.x = TRUE)

NurseM <- subset(NurseM, select = -c(X))
NursingA <- subset(NursingA, select = -c(X))

#remove all the rows with NA or 0 or 1 as time_to_fill
NurseM <- NurseM[NurseM$time_to_fill > 1,]
NurseM <- NurseM[!is.na(NurseM$time_to_fill),]
NurseM <- NurseM[!is.na(NurseM$ruralurban),]

NursingA <- NursingA[NursingA$time_to_fill > 1,]
NursingA <- NursingA[!is.na(NursingA$time_to_fill),]
NursingA <- NursingA[!is.na(NursingA$ruralurban),]

#find the percentage of rural counties in two datasets 
sum(NurseM$ruralurban, na.rm = TRUE)/(length(NurseM$ruralurban) 
                                      - length(which(is.na(NurseM$ruralurban))))
#0.08324864 for nurse manager

sum(NursingA$ruralurban, na.rm = TRUE)/(length(NursingA$ruralurban) 
                                        - length(which(is.na(NursingA$ruralurban))))
#0.1277767 for nursing assistant

#t-test on the difference of mean of time-to-fill between rural and urban counties (Nurse Manager)
t.test(NurseM$time_to_fill[NurseM$ruralurban == 1],NurseM$time_to_fill[NurseM$ruralurban == 0], 
       var.equal=FALSE, paired=FALSE)
#t-test on the difference of mean of time-to-fill between rural and urban counties (Nursing Assistant)
t.test(NursingA$time_to_fill[NursingA$ruralurban == 1],NursingA$time_to_fill[NursingA$ruralurban == 0], 
       var.equal=FALSE, paired=FALSE)
#t-test on the difference of mean of time-to-fill between nursing assistant and nurse manager in rural area
t.test(NurseM$time_to_fill[NurseM$ruralurban == 1],NursingA$time_to_fill[NursingA$ruralurban == 1], 
       var.equal=FALSE, paired=FALSE)
#t-test on the difference of mean of time-to-fill between nursing assistant and nurse manager in urban area
t.test(NurseM$time_to_fill[NurseM$ruralurban == 0],NursingA$time_to_fill[NursingA$ruralurban == 0], 
       var.equal=FALSE, paired=FALSE)
t.test(NurseM$time_to_fill,NursingA$time_to_fill, var.equal=FALSE, paired=FALSE)

#salary for EDA
t.test(NurseM$salary[NurseM$ruralurban == 1],NurseM$salary[NurseM$ruralurban == 0],
       var.equal=FALSE, paired=FALSE)
t.test(NursingA$salary[NursingA$ruralurban == 1],NursingA$salary[NursingA$ruralurban == 0], 
       var.equal=FALSE, paired=FALSE)
t.test(NurseM$salary[NurseM$ruralurban == 1],NursingA$salary[NursingA$ruralurban == 1],
       var.equal=FALSE, paired=FALSE)
t.test(NurseM$salary[NurseM$ruralurban == 0],NursingA$salary[NursingA$ruralurban == 0], 
       var.equal=FALSE, paired=FALSE)
#nursing assistant from rural to urban -> salary can increase by around 4.6%
#while nurse manager from rural to urban salary can increase by 8.1%

#simple linear regression
model1 <- lm(time_to_fill ~ as.factor(ruralurban), data = NurseM)
summary(model1)
model2 <- lm(time_to_fill ~ as.factor(ruralurban), data = NursingA)
summary(model2)
#the coefficient of ruralurban in model 2 is siginificant --> confirmed that the time_to_fill is about 0.80803 days
#longer for nursing assistant in rural area 

#nurse manager -- rural/urban analysis withnin each state
states <- unique(NurseM$state)
store <- matrix(0,length(states),3)
for (i in 1: length(states)) {
  ruralavg = mean(NurseM$time_to_fill[NurseM$state == states[i] & NurseM$ruralurban == 1])
  urbanavg = mean(NurseM$time_to_fill[NurseM$state == states[i] & NurseM$ruralurban == 0])
  store[i,1] = states[i]
  store[i,2] = ruralavg
  store[i,3] = urbanavg
  
}

store <-as.data.frame(store)
store <- store[store[2] != "NaN",]
store$V2 <- as.numeric(store$V2)
store$V3 <- as.numeric(store$V3)
store$V4 <- store$V2 - store$V3

urban_faster <- store[store[4] > 0, ]
rural_faster <- store[store[4] < 0, ]

#nursing assistant -- rural/urban analysis withnin each state
states <- unique(NursingA$state)
store <- matrix(0,length(states),3)

for (i in 1: length(states)) {
  ruralavg = mean(NursingA$time_to_fill[NursingA$state == states[i] & NursingA$ruralurban == 1])
  urbanavg = mean(NursingA$time_to_fill[NursingA$state == states[i] & NursingA$ruralurban == 0])
  store[i,1] = states[i]
  store[i,2] = ruralavg
  store[i,3] = urbanavg
  
}

store <-as.data.frame(store)
store <- store[store[2] != "NaN",]
store$V2 <- as.numeric(store$V2)
store$V3 <- as.numeric(store$V3)
store$V4 <- store$V2 - store$V3

urban_faster <- store[store[4] > 0, ]
rural_faster <- store[store[4] < 0, ]
