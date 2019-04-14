library(RMySQL)
connection = DBI::dbConnect(MySQL(), dbname='greenwic_edu_nwu', user = 'greenwic_nwu_147', password='xv@y@V~gzCO%',host='67.225.186.169')

#read in overall dataset
master = dbGetQuery(connection, "Select * from greenwich_master_nwu_fall_2018_proj1")
role = dbGetQuery(connection, "Select * from greenwich_role_nwu_fall_2018_proj1")
tag = dbGetQuery(connection, "Select * from greenwich_tags_nwu_fall_2018_proj1")

#the subset of the data that only contins entries about nurses
nurses = dbGetQuery(connection, "Select * from greenwich_master_nwu_fall_2018_proj1 where job_id in (Select job_id from greenwich_role_nwu_fall_2018_proj1 where role = 'Nurse Manager' or role = 'Nursing Assistant')")
nurse_tags = dbGetQuery(connection, "Select * from greenwich_tags_nwu_fall_2018_proj1 where job_id in (Select job_id from greenwich_role_nwu_fall_2018_proj1 where role = 'Nurse Manager' or role = 'Nursing Assistant')")
role = dbGetQuery(connection, "Select * from greenwich_master_nwu_fall_2018_proj1 where job_id in (Select job_id from greenwich_role_nwu_fall_2018_proj1 where role = 'Nurse Manager')")

#### DATA CLEANING ####
#### missing values ####
#time to fill and fill date
length(which(is.na(role$time_to_fill)))

#total 24.75% (66722)
length(which(is.na(role)))

#subset of manager and assistant that does not have missing value in time_to_fill
role_time = role[which(!is.na(role$time_to_fill)),]

#correlation between time to fill and salary
cor(role_time$time_to_fill, role_time$salary)

#extract post year and month
library(lubridate)
role_time$post_year = year(role_time$post_date)
role_time$post_month = month(role_time$post_date)
#correlation subset by year and month
library(plyr)
ddply(role_time,c("post_year", "post_month"),function(x) cor(x$salary,x$time_to_fill))

#ploting with ggplot2
#visualizations, but not very useful
library(ggplot2)
#salary by posting date for manger
ggplot(data = role_time) + geom_point(aes(y = salary, x = post_date), colour = 'orange') + ggtitle("Salary by posting date")
#salary by time to fill for manager
ggplot(data = role_time) + geom_point(aes(y = salary, x = time_to_fill, colour=factor(post_month))) + ggtitle("Salary and Time-to-fill by month for nurse managers")

#### integrate the urban/rural variable ####
role_time['statecounty'] = paste(role_time$state, role_time$county)

#merge with external population dataset
county_pop = read.csv('county_pop2.csv')
role_ru = merge(role_time, county_pop, by.x="statecounty", by.y="statecounty", all.x = TRUE)
role_ru = subset(role_ru, select = -c(X))

#### analysis with salary and region #### 

#only use the time to fill larger than 1 subset and no missing value for state
role_ru = na.omit(role_ru[role_ru$time_to_fill > 1 & !is.na(role_ru$state),])

#by state and month+year has very high correlation!!!!!
role_bystatemonth = ddply(role_bystatemonth, c("post_year", "post_month", "state"), function(x) cor(x$salary,x$time_to_fill)) 
role_bystatemonth$V1 = role_bystatemonth$V1

#by state only does not have high correlation, nor does by rural/urban + month/year
ddply(role_time,c("state"),function(x) cor(x$salary,x$time_to_fill)) #-0.05 to 0.13
ddply(role_time,c("state"),function(x) cor(x$salary,x$time_to_fill)) #-0.18 to 0.28

#heat map for state and correlation
library(plotly)
#for assistant
plot_ly(y = role_bystatemonth$post_month, x=role_bystatemonth$state, z = role_bystatemonth$V1, type = "heatmap", colorscale = "Greys")
#for manager
plot_ly(y = role_bystatemonth$post_month, x=role_bystatemonth$state, z = role_bystatemonth$V1, type = "heatmap", colorscale = "Greys")


#### Influence of Count on salary and region #### 

#frequency count for each month and state for assistant and manager 
#create the frequency count variable 
role_freq = data.frame(role_ru %>% 
                      group_by(post_year, post_month, state) %>%
                      summarise(freq = n(), time_salary = cor(time_to_fill, salary), mean_time = mean(time_to_fill)))

#merge with the main dataset so there is a count for each year month and state
role_freqru = merge(role_ru, role_freq, by = c("post_year", "post_month", "state"), all.x = TRUE)

#correlation between count and average time to fill by state 
cor(role_freqru$freq, role_freqru$time_to_fill)

#freq and correlation between salary and time to fill
treemap(role_freq,
        index=c("state"), 
        vSize="freq",
        vColor="time_salary", 
        type="manual",palette="RdYlBu")

#freq and mean time to fill
treemap(role_freq,
        index=c("state"), 
        vSize="freq",
        vColor="mean_time", 
        type="manual",palette="RdYlBu")

#larger states (west and east coast) also have more postings in general, and takes longer to fill jobs than middle sized states.
#these states with more postings also have stronger, and positive correlation between salary and time to fill. So when there are more postings, the time it takes for high salary job to be filled is also longer. 
#when there is less postings, the time it take for high salary jobs to be filled are shorter. 
#therefore, count might be a confounding/intermediate variable that we can look into when predicting time_to_fill


#### Final Summary of my work ####
#This part of the analysis mainly focused on the relationship between salary and time-to-fill, and how different factors influenced this relationship.
#By subsetting the data into different time, location and role bins, it is clear that there is an obvious geographical difference in the correlation between salary and time-to-fill, and the relationship is greatly influenced by the time of the year as well as the role we are predicting (manager or assistant).
#Therefore, when predicting time-to-fill, it would be helpful to look at different regions, months and roles invdividually, instead of putting them all together.
#Moreover, the number of postings avaliable for each region in a time frame might be a significant variable mediating the relationship between salary and time-to-fill, so it would be helpful to include count of postings in the predictive model, and further investigate its influence as well.







