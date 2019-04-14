library(RMySQL)
library(DBI)
library(dplyr)
library(lubridate)
library(mosaic)
library(pROC)
setwd("~/z/My_files/Everything data/teamuber_project1/")
mydb = DBI::dbConnect(MySQL(), 
                      user='greenwic_nwu_147', 
                      password='xv@y@V~gzCO%', 
                      dbname='greenwic_edu_nwu',
                      host='67.225.186.169')

master.df = dbGetQuery(mydb, "select * from greenwich_master_nwu_fall_2018_proj1
                       where vertical = 'Healthcare' and job_id in (select job_id from greenwich_role_nwu_fall_2018_proj1 
                       where role = 'Nurse Manager' or role = 'Nursing Assistant')")
role.df= dbGetQuery(mydb,"select * from greenwich_role_nwu_fall_2018_proj1 where job_id in (select job_id from
                    greenwich_master_nwu_fall_2018_proj1
                    where vertical = 'Healthcare') and role in ('Nurse Manager' ,'Nursing Assistant')")

##Remove duplicates from Roles
role.df=role.df[!(duplicated(role.df$job_id) | duplicated(role.df$job_id,fromLast = TRUE)),]

tag.df=dbGetQuery(mydb,"select * from greenwich_tags_nwu_fall_2018_proj1 where job_id in (select job_id from greenwich_role_nwu_fall_2018_proj1
                  where role in ('Nurse Manager' ,'Nursing Assistant'))")

#Select Role
role_interest="Nursing Assistant"


#Created zip aggregate variable with first 3 digits of zip
master.df$zip_aggregate <- substr(master.df$zip,1,3)
#Master dataframe with no NA variables for county and time to fill
master.noNA <- master.df[!(is.na(master.df[,'county'])),]
master.noNA <- master.noNA[!(is.na(master.noNA[,'time_to_fill'])),]
#merged master and roles by job id
master.noNA <- merge(master.noNA,role.df,by="job_id")

master.noNA=master.noNA[master.noNA$role==role_interest,]
#New variable called statecounty that concatenates state and county variables
master.noNA['statecounty'] <- paste(master.noNA$state, master.noNA$county)
tag_count=left_join(master.noNA[,c(1,2)],tag.df,by="job_id")
tag_count=tag_count%>%group_by(job_id)%>%summarise(tag_count=n())

unique_county <- unique(master.noNA$statecounty)
county_pop <- read.csv('county_pop2.csv')

#withrural df adds urban/rural criterion to dataset
withrural <- merge(master.noNA, county_pop, by.x="statecounty", by.y="statecounty", all.x = TRUE)
withrural <- subset(withrural, select = -c(X))
#Outlier removal, removes highest and lowest 10%
lower_limit = quantile(withrural$time_to_fill, probs = 0.1)
upper_limit = quantile(withrural$time_to_fill, probs = 0.9)
withrural <- withrural %>%filter(time_to_fill > lower_limit & time_to_fill < upper_limit) 

withrural = left_join(withrural,tag_count,by="job_id")
withrural[is.na(withrural$tag_count),'tag_count']=0


withrural<- 
  mutate(withrural,
         time_to_fill=ifelse(time_to_fill<median(time_to_fill),1,0))
#breakdown time by month and year
withrural$post_month = month(withrural$post_date)
withrural$post_year = year(withrural$post_date)

#Creating region chunks
west_coast = c('CA','OR','WA')
southwest = c('AZ','NM','UT','NV')
rockies = c('ID','MT','WY','CO')
texas = c('TX')
plains = c('ND','SD','NE','KS','OK','IA')
great_lakes = c('MN','WI','IL','IN','MI')
rust_belt = c('OH','PA','WV')
middle_south = c('AR','MO','KY','TN','NC')
south = c('LA','MS','AL','SC','GA','FL')
east_coast = c('VA','DC','MD','DE','NJ','NY','CT','RI','MA')
northeast = c('VT','NH','ME')
uniques = c('AK','HI')

# create a dataframe to list the region chunk for each state
region_list <- c(rep('west_coast',length(west_coast)),rep('southwest',length(southwest)),rep('rockies',length(rockies)),rep('texas',length(texas)),
                 rep('plains',length(plains)),rep('great_lakes',length(great_lakes)),rep('rust_belt',length(rust_belt)),rep('middle_south', length(middle_south))
                 ,rep('south',length(south)),rep('east_coast', length(east_coast)),rep('northeast', length(northeast)),rep('uniques', length(uniques)))
region_definitions <- c(west_coast, southwest, rockies, texas, plains, great_lakes, rust_belt, middle_south, south, east_coast, northeast, uniques)
state_names <- c('california', 'oregon', 'washington', 'arizona', 'new mexico', 'utah', 'nevada', 'idaho', 'montana', 'wyoming', 'colorado', 
                 'texas', 'north dakota', 'south dakota','nebraska', 'kansas', 'oklahoma', 'iowa', 'minnesota', 'wisconsin', 'illinois', 'indiana',
                 'michigan', 'ohio', 'pennsylvania', 'west virginia','arkansas', 'missouri', 'kentucky', 'tennessee', 'north carolina', 'louisiana', 
                 'mississippi', 'alabama', 'south carolina', 'georgia', 'florida', 'virginia', 'district of columbia', 'maryland', 'delaware', 
                 'new jersey', 'new york', 'connecticut', 'rhode island', 'massachusetts', 'vermont', 'new hampshire', 'maine', 'alaska', 'hawaii')
regions_df <- data.frame(cbind(region_list,region_definitions,state_names))


# add the region chunks to the master_df
df <- withrural[!(is.na(withrural[,'state'])),]
for (state in regions_df$region_definitions){
  df[df['state'] == state,'chunk'] <- regions_df[regions_df['region_definitions'] == state, 'region_list']
}

m_freq = data.frame(df %>% 
                      group_by(post_year, post_month, state) %>%
                      summarise(freq = n(), time_salary = cor(time_to_fill, salary), mean_time = mean(time_to_fill)))

#merge with the main dataset so there is a count for each year month and state
#NurseAssistant = merge(NurseAssistant, a_freq, by = c("post_year", "post_month", "state"), all.x = TRUE)
df = merge(df, m_freq, by = c("post_year", "post_month", "state"), all.x = TRUE)
df[is.na(df)]=0

if (role_interest=="Nurse Manager"){
  tf=read.csv("NM_Tf2.csv")  
} else {
  tf=read.csv("NA_Tf2.csv")  
}

tf=tf[,c(1,3:ncol(tf))]

df2=left_join(df,tf,by="job_id")
df2[is.na(df2)]=0
df3=df2%>%select(job_id,salary,chunk,post_month,tag_count,ruralurban,time_to_fill,contains("tf_idf"))
df3$post_month=as.factor(df3$post_month)

#Split into train-test
smp_size <- floor(0.75 * nrow(df3))
## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(df3)), size = smp_size)
train <- df3[train_ind, ]
if (nrow(train)>80000){
  ind_new=sample(seq_len(nrow(train)),size=80000)
  train=train[ind_new,]
}
test <- df3[-train_ind, ]

fit_null=glm(time_to_fill~1,data=train,family = "binomial")
fit_full=glm(time_to_fill~.-job_id,data=train,family = "binomial")
mod.log2=step(fit_null,scope = list(lower=fit_null,upper=fit_full),direction="forward")
summary(mod.log2)
mod.log2.copy=mod.log2

flag=0

while(flag==0){
  sign_pred=names(which(summary(mod.log2)$coefficients[,4]<0.05))
  # sign_pred=names(sign_pred)[2:length(sign_pred)]
  sign_pred=sign_pred[grep("tf_idf",sign_pred)]
  var_list=c("chunk","salary","post_month","tag_count",sign_pred)
  formula2=as.formula(paste("time_to_fill", paste(var_list, collapse=" + "), sep=" ~ ")) 
  mod.log2=glm(formula=formula2,data=train,family="binomial")
  
  tf_comp=numeric()
  for (i in sign_pred){
    print(i)
    p_val=summary(mod.log2)$coefficients[,4][[i]]
    print(p_val)
    # paste(i,summary(mod.logppp)$coefficients[,4][[i]])
    
    if (summary(mod.log2)$coefficients[,4][[i]]<0.05){
      tf_comp=c(tf_comp,0)
    }   else {
      tf_comp=c(tf_comp,1)
    }
  }
  if (sum(tf_comp)==0){
    flag=1
  }
}

#Caclulate pstar from train data
CCR <- rep(0,99)
for(i in c(seq(0.01,0.99, by = 0.01))){
  j = 100*i
  tab <- table(mod.log2$y, mod.log2$fitted.values>i)
  CCR[j] <- sum(diag(tab))/sum(tab)}
pstar <- which.max(CCR)/100

#Use pstar on test data and calculate performance statistics
probs <- predict(mod.log2,newdata=test,type="response")
tab <- table(test$time_to_fill, probs>pstar)
Sensitivity <- 1- (tab[2,1]/(tab[2,1]+tab[2,2]))
Specificity <- 1 -(tab[1,2]/(tab[1,1]+tab[1,2]))
Precision <- tab[2,2]/(tab[1,2]+tab[2,2])
Recall <- tab[2,2]/(tab[2,1]+tab[2,2])
F1 <- (2*Precision*Recall)/(Precision+Recall)
auc <- roc(train$time_to_fill, mod.log2$fitted.values)
plot(auc)
print(auc)
