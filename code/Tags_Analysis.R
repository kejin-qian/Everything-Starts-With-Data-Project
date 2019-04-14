# establish the connection
library(RMySQL)
username <- readline(prompt="Enter the user: ")
connection <- DBI::dbConnect(MySQL(), dbname = "greenwic_edu_nwu", user = username, password = rstudioapi::askForPassword('Password: '), host = "67.225.186.169")

# pull the master_role_df data frame for further analysis without repeated SQL queries
# in order to adjust roles or verticals, check the master.vertical or role.role entries within the query
master_tag_df <- dbGetQuery(connection, "SELECT * FROM greenwich_master_nwu_fall_2018_proj1 AS master JOIN greenwich_role_nwu_fall_2018_proj1 AS role ON master.job_id = role.job_id JOIN greenwich_tags_nwu_fall_2018_proj1 AS tag ON master.job_id = tag.job_id WHERE master.vertical = 'Healthcare' AND (role.role = 'Nurse Manager' OR role.role = 'Nursing Assistant')")

#drop the additional job_id columns
master_tag_df[[18]] <- NULL
master_tag_df[[16]] <- NULL

#check overall NA rate
sum(is.na(master_tag_df$time_to_fill))*(100.0)/nrow(master_tag_df)

#we want to compile various statistics on tags, their prevalence, and their NA rate
tags <- c()
tag_count <- c()
tag_NA_rate <- c()

#check for count and NA rate for each tag, appending to a vector (this takes a little while)
for (tag in unique(master_tag_df$tag)){
  tags <- c(tags,tag)
  NA_rate <- sum(is.na(master_tag_df$time_to_fill[master_tag_df['tag'] == tag]))*100.0/nrow(master_tag_df[master_tag_df['tag'] ==tag, ])
  tag_NA_rate <- c(tag_NA_rate,NA_rate)
  t_count <- sum(master_tag_df$tag == tag)
  tag_count <- c(tag_count,t_count)
}

# we will use dplyr for some of our manipulation
library(tidyverse)

# create a data frame of the tags and their counts and NA rates
tags_df <- data.frame(cbind(tags, tag_count, tag_NA_rate), stringsAsFactors = FALSE)
tags_df$tag_count <- as.integer(tags_df$tag_count)
tags_df$tag_NA_rate <- as.numeric(tags_df$tag_NA_rate)

# we look at the range of values for both NA rate and counts
tags_df %>%
  ggplot() +
  geom_violin(scale = "area", aes(x = 'tags', y = tag_NA_rate)) +
  labs(title= "NA rate distribution", x = "Tag", y= "NA Rate") +
  coord_flip()

tags_df %>%
  ggplot() +
  geom_violin(scale = "area", aes(x = 'tags', y = tag_count)) +
  labs(title= "Count distribution", x = "Tag", y= "Count") +
  coord_flip()

# because we have a few very large counts, we want to replot only those values that are below a specific threshold
tags_df %>%
  filter(tag_count < 1000) %>%
  ggplot() +
  geom_violin(scale = "area", aes(x = 'tags', y = tag_count)) +
  labs(title= "Count distribution", x = "Tag", y= "Count") +
  coord_flip()

# we can also look at a scatter plot of the tags for both count and NA rate
tags_df %>%
  filter(tag_count < 1000) %>%
  ggplot() +
  geom_point(aes(x = tag_NA_rate, y = tag_count)) +
  labs(title= "Tag Counts versus their NA Rate", x = "NA Rate", y= "Count")

# in order to faciliate a certain quality of analysis, we will ignore tags in the lowest 10% in terms of count
count_lower_limit <- quantile(tags_df$tag_count, probs = 0.1)
tags_df <- filter(tags_df, tags_df$tag_count > count_lower_limit)

# we re-examine the NA rates
tags_df %>%
  filter(tag_count < 1000) %>%
  ggplot() +
  geom_point(aes(x = tag_NA_rate, y = tag_count)) +
  labs(title= "Tag Counts versus their NA Rate", x = "NA Rate", y= "Count")

# we see that there are some tags that have large NA rates and low counts (bottom right of the graph),
# so we will eliminate the NAs and see if the counts drop below the acceptable limit

master_tag_df_NA_drop <- master_tag_df[master_tag_df$tag %in% tags_df$tags & !(is.na(master_tag_df$time_to_fill)),]

#check for count and NA rate for each tag, appending to a vector (this takes a little while)
tags <- c()
tag_count <- c()
tag_NA_rate <- c()

for (tag in unique(master_tag_df_NA_drop$tag)){
  tags <- c(tags,tag)
  NA_rate <- sum(is.na(master_tag_df_NA_drop$time_to_fill[master_tag_df_NA_drop['tag'] == tag]))*100.0/nrow(master_tag_df_NA_drop[master_tag_df_NA_drop['tag'] ==tag, ])
  tag_NA_rate <- c(tag_NA_rate,NA_rate)
  t_count <- sum(master_tag_df_NA_drop$tag == tag)
  tag_count <- c(tag_count,t_count)
}

# create a data frame of the tags and their counts and NA rates
tags2_df <- data.frame(cbind(tags, tag_count, tag_NA_rate), stringsAsFactors = FALSE)
tags2_df$tag_count <- as.integer(tags2_df$tag_count)
tags2_df$tag_NA_rate <- as.numeric(tags2_df$tag_NA_rate)

# in order to faciliate a certain quality of analysis, we will ignore tags in the lowest 10% in terms of count
count_lower_limit <- quantile(tags2_df$tag_count, probs = 0.1)
tags2_df <- filter(tags2_df, tags2_df$tag_count > count_lower_limit)

# we re-examine the NA rates
tags2_df %>%
  filter(tag_count < 1000) %>%
  ggplot() +
  geom_violin(scale = "area", aes(x = 'tags', y = tag_count)) +
  labs(title= "Count distribution", x = "Tag", y= "Count") +
  coord_flip()

# now that we've cleaned up our tags a bit, we can update our master_df one more time and begin analyzing the different trends
master_tag_df_updated <- master_tag_df_NA_drop[master_tag_df_NA_drop$tag %in% tags2_df$tags,]
lower_limit = quantile(master_tag_df_updated$time_to_fill, probs = 0.1)
upper_limit = quantile(master_tag_df_updated$time_to_fill, probs = 0.9)
master_tag_df_updated <- filter(master_tag_df_updated,time_to_fill > lower_limit & time_to_fill < upper_limit)

# look at the list of distinct tags
unique(master_tag_df_updated$tag)

# by hand we went through the tags and identified tags that refer to the same subject (shift needed, education, deparment/location, experience, language skills)
education_tags = c("GED","High School Diploma","Trade School Certification","Bachelor Degree","College Degree","Master","Masters Degree","MD","PhD","MBA","Juris Doctorate","CPA")
department_tags = c("Emergency Room","Emergency Department","ENT","ICU","Intensive Care","Hospital","Clinic","Behavorial Health","Nursing Home","NICU","Neonatal Intensive Care","Pediatric","Geriatric","Oncology","Hospice","Cardiology","Radiology","Endocrinology","Physiology","Gynecology","Women's Health","Psychiatry","Occupational Therapy","Dermatology","Ophthalmology","Internal Medicine","Plastic Surgery","Anesthesiology","Osteopathic","Nuclear Medicine","Gerontology","Neonatology","Optometry")
shift_tags = c("Night Shift","2nd Shift","On Call","Day Shift","Evening Shift","Evenings","Weekends Only","3rd Shift","1st Shift","First Shift","12 Hour Shift","Overnight","Third Shift","Rotating Shift","Weekend Only","Second Shift","Shift Premium","Split Shift")
experience_tags = c(" Yr","1 Year","2 Years","3 Years","5 Years","10 Years")
language_tags = c("Russian","German","English","Spanish","Korean","Vietnamese")

# plot a violin plot for the time_to_fill for each part of a tag set
master_tag_df_updated %>%
  filter(tag %in% education_tags) %>%
  add_count(tag) %>%
  group_by(tag) %>%
  ggplot() +
  geom_boxplot(aes(x = reorder(tag,n), y = time_to_fill)) +
  geom_text(aes(x = tag, y = 10, label = n)) +
  labs(title= "Time to Fill By Education Tag", x = "Tag", y= "Time To Fill in Days") +
  coord_flip()

# plot a column plot for the average time_to_fill for each part of a tag set
master_tag_df_updated %>%
  filter(tag %in% education_tags) %>%
  add_count(tag) %>%
  group_by(tag) %>%
  summarize(avg_time_to_fill = mean(time_to_fill), count_tag = mean(n)) %>%
  ggplot() +
  geom_col(aes(x = reorder(tag,avg_time_to_fill), y = avg_time_to_fill)) +
  geom_text(aes(x = reorder(tag,avg_time_to_fill), y = avg_time_to_fill, label = count_tag)) +
  labs(title= "Average Time to Fill By Education Tag", x = "Tag", y= "Average Time To Fill in Days") +
  coord_flip()

education_means <- master_tag_df_updated %>%
  filter(tag %in% education_tags) %>%
  add_count(tag) %>%
  group_by(tag) %>%
  summarize(avg_time_to_fill = mean(time_to_fill), var_time_to_fill = var(time_to_fill), count_tag = mean(n))

edu_stat_sig <- matrix(nrow = nrow(education_means), ncol = nrow(education_means))
rownames(edu_stat_sig) <- education_means$tag
colnames(edu_stat_sig) <- education_means$tag
for (i in 1:nrow(edu_stat_sig)){
  for (j in 1:ncol(edu_stat_sig)){
    mean_diff <- abs(education_means[i,2] - education_means[j,2])[1,1]
    std_dev <- sqrt((education_means[i,3]/education_means[i,4]) + (education_means[j,3]/education_means[j,4]))[1,1]
    t_stat <- mean_diff/std_dev
    deg_free <- education_means[i,4] + education_means[j,4] - 2
    t_value <- pt(t_stat, deg_free[1,1], lower.tail = FALSE)
    if (t_value < 0.05){
      edu_stat_sig[i,j] <- t_value
    }
    else{
      edu_stat_sig[i,j] <- 'NOT SIG'
    }
     
  }
}

# plot a violin plot for the time_to_fill for each part of a tag set
master_tag_df_updated %>%
  filter(tag %in% department_tags) %>%
  add_count(tag) %>%
  group_by(tag) %>%
  ggplot() +
  geom_boxplot(aes(x = reorder(tag,n), y = time_to_fill)) +
  geom_text(aes(x = tag, y = 10, label = n)) +
  labs(title= "Time to Fill By Department Tag", x = "Tag", y= "Time To Fill in Days") +
  coord_flip()

# plot a column plot for the average time_to_fill for each part of a tag set
master_tag_df_updated %>%
  filter(tag %in% department_tags) %>%
  add_count(tag) %>%
  group_by(tag) %>%
  summarize(avg_time_to_fill = mean(time_to_fill), count_tag = mean(n)) %>%
  ggplot() +
  geom_col(aes(x = reorder(tag,avg_time_to_fill), y = avg_time_to_fill)) +
  geom_text(aes(x = reorder(tag,avg_time_to_fill), y = avg_time_to_fill, label = count_tag)) +
  labs(title= "Average Time to Fill By Department Tag", x = "Tag", y= "Average Time To Fill in Days") +
  coord_flip()

department_means <- master_tag_df_updated %>%
  filter(tag %in% department_tags) %>%
  add_count(tag) %>%
  group_by(tag) %>%
  summarize(avg_time_to_fill = mean(time_to_fill), var_time_to_fill = var(time_to_fill), count_tag = mean(n))

dept_stat_sig <- matrix(nrow = nrow(department_means), ncol = nrow(department_means))
rownames(dept_stat_sig) <- department_means$tag
colnames(dept_stat_sig) <- department_means$tag
for (i in 1:nrow(dept_stat_sig)){
  for (j in 1:ncol(dept_stat_sig)){
    mean_diff <- abs(department_means[i,2] - department_means[j,2])[1,1]
    std_dev <- sqrt((department_means[i,3]/department_means[i,4]) + (department_means[j,3]/department_means[j,4]))[1,1]
    t_stat <- mean_diff/std_dev
    deg_free <- department_means[i,4] + department_means[j,4] - 2
    t_value <- pt(t_stat, deg_free[1,1], lower.tail = FALSE)
    if (t_value < 0.05){
      dept_stat_sig[i,j] <- t_value
    }
    else{
      dept_stat_sig[i,j] <- 'NOT SIG'
    }
    
  }
}


# plot a violin plot for the time_to_fill for each part of a tag set
master_tag_df_updated %>%
  filter(tag %in% shift_tags) %>%
  add_count(tag) %>%
  group_by(tag) %>%
  ggplot() +
  geom_boxplot(aes(x = reorder(tag,n), y = time_to_fill)) +
  geom_text(aes(x = tag, y = 10, label = n)) +
  labs(title= "Time to Fill By Shift Tag", x = "Tag", y= "Time To Fill in Days") +
  coord_flip()

# plot a column plot for the average time_to_fill for each part of a tag set
master_tag_df_updated %>%
  filter(tag %in% shift_tags) %>%
  add_count(tag) %>%
  group_by(tag) %>%
  summarize(avg_time_to_fill = mean(time_to_fill), count_tag = mean(n)) %>%
  ggplot() +
  geom_col(aes(x = reorder(tag,avg_time_to_fill), y = avg_time_to_fill)) +
  geom_text(aes(x = reorder(tag,avg_time_to_fill), y = avg_time_to_fill, label = count_tag)) +
  labs(title= "Average Time to Fill By Shift Tag", x = "Tag", y= "Average Time To Fill in Days") +
  coord_flip()

shift_means <- master_tag_df_updated %>%
  filter(tag %in% shift_tags) %>%
  add_count(tag) %>%
  group_by(tag) %>%
  summarize(avg_time_to_fill = mean(time_to_fill), var_time_to_fill = var(time_to_fill), count_tag = mean(n))

shift_stat_sig <- matrix(nrow = nrow(shift_means), ncol = nrow(shift_means))
rownames(shift_stat_sig) <- shift_means$tag
colnames(shift_stat_sig) <- shift_means$tag
for (i in 1:nrow(shift_stat_sig)){
  for (j in 1:ncol(shift_stat_sig)){
    mean_diff <- abs(shift_means[i,2] - shift_means[j,2])[1,1]
    std_dev <- sqrt((shift_means[i,3]/shift_means[i,4]) + (shift_means[j,3]/shift_means[j,4]))[1,1]
    t_stat <- mean_diff/std_dev
    deg_free <- shift_means[i,4] + shift_means[j,4] - 2
    t_value <- pt(t_stat, deg_free[1,1], lower.tail = FALSE)
    if (t_value < 0.05){
      shift_stat_sig[i,j] <- t_value
    }
    else{
      shift_stat_sig[i,j] <- 'NOT SIG'
    }
    
  }
}

# plot a violin plot for the time_to_fill for each part of a tag set
master_tag_df_updated %>%
  filter(tag %in% experience_tags) %>%
  add_count(tag) %>%
  group_by(tag) %>%
  ggplot() +
  geom_boxplot(aes(x = reorder(tag,n), y = time_to_fill)) +
  geom_text(aes(x = tag, y = 10, label = n)) +
  labs(title= "Time to Fill By Experience Tag", x = "Tag", y= "Time To Fill in Days") +
  coord_flip()

# plot a column plot for the average time_to_fill for each part of a tag set
master_tag_df_updated %>%
  filter(tag %in% experience_tags) %>%
  add_count(tag) %>%
  group_by(tag) %>%
  summarize(avg_time_to_fill = mean(time_to_fill), count_tag = mean(n)) %>%
  ggplot() +
  geom_col(aes(x = reorder(tag,avg_time_to_fill), y = avg_time_to_fill)) +
  geom_text(aes(x = reorder(tag,avg_time_to_fill), y = avg_time_to_fill, label = count_tag)) +
  labs(title= "Average Time to Fill By Experience Tag", x = "Tag", y= "Average Time To Fill in Days") +
  coord_flip()

exp_means <- master_tag_df_updated %>%
  filter(tag %in% experience_tags) %>%
  add_count(tag) %>%
  group_by(tag) %>%
  summarize(avg_time_to_fill = mean(time_to_fill), var_time_to_fill = var(time_to_fill), count_tag = mean(n))

exp_stat_sig <- matrix(nrow = nrow(exp_means), ncol = nrow(exp_means))
rownames(exp_stat_sig) <- exp_means$tag
colnames(exp_stat_sig) <- exp_means$tag
for (i in 1:nrow(exp_stat_sig)){
  for (j in 1:ncol(exp_stat_sig)){
    mean_diff <- abs(exp_means[i,2] - exp_means[j,2])[1,1]
    std_dev <- sqrt((exp_means[i,3]/exp_means[i,4]) + (exp_means[j,3]/exp_means[j,4]))[1,1]
    t_stat <- mean_diff/std_dev
    deg_free <- exp_means[i,4] + exp_means[j,4] - 2
    t_value <- pt(t_stat, deg_free[1,1], lower.tail = FALSE)
    if (t_value < 0.05){
      exp_stat_sig[i,j] <- t_value
    }
    else{
      exp_stat_sig[i,j] <- 'NOT SIG'
    }
    
  }
}

# plot a violin plot for the time_to_fill for each part of a tag set
master_tag_df_updated %>%
  filter(tag %in% language_tags) %>%
  add_count(tag) %>%
  group_by(tag) %>%
  ggplot() +
  geom_boxplot(aes(x = reorder(tag,n), y = time_to_fill)) +
  geom_text(aes(x = tag, y = 10, label = n)) +
  labs(title= "Time to Fill By Language Tag", x = "Tag", y= "Time To Fill in Days") +
  coord_flip()

# plot a column plot for the average time_to_fill for each part of a tag set
master_tag_df_updated %>%
  filter(tag %in% language_tags) %>%
  add_count(tag) %>%
  group_by(tag) %>%
  summarize(avg_time_to_fill = mean(time_to_fill), count_tag = mean(n)) %>%
  ggplot() +
  geom_col(aes(x = reorder(tag,avg_time_to_fill), y = avg_time_to_fill)) +
  geom_text(aes(x = reorder(tag,avg_time_to_fill), y = avg_time_to_fill, label = count_tag)) +
  labs(title= "Average Time to Fill By Language Tag", x = "Tag", y= "Average Time To Fill in Days") +
  coord_flip()

lang_means <- master_tag_df_updated %>%
  filter(tag %in% language_tags) %>%
  add_count(tag) %>%
  group_by(tag) %>%
  summarize(avg_time_to_fill = mean(time_to_fill), var_time_to_fill = var(time_to_fill), count_tag = mean(n))

lang_stat_sig <- matrix(nrow = nrow(lang_means), ncol = nrow(lang_means))
rownames(lang_stat_sig) <- lang_means$tag
colnames(lang_stat_sig) <- lang_means$tag
for (i in 1:nrow(lang_stat_sig)){
  for (j in 1:ncol(lang_stat_sig)){
    mean_diff <- abs(lang_means[i,2] - lang_means[j,2])[1,1]
    std_dev <- sqrt((lang_means[i,3]/lang_means[i,4]) + (lang_means[j,3]/lang_means[j,4]))[1,1]
    t_stat <- mean_diff/std_dev
    deg_free <- lang_means[i,4] + lang_means[j,4] - 2
    t_value <- pt(t_stat, deg_free[1,1], lower.tail = FALSE)
    if (t_value < 0.05){
      lang_stat_sig[i,j] <- t_value
    }
    else{
      lang_stat_sig[i,j] <- 'NOT SIG'
    }
    
  }
}
