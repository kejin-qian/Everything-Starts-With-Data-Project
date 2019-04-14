# establish the connection
library(RMySQL)
username <- readline(prompt="Enter the user: ")
connection <- DBI::dbConnect(MySQL(), dbname = "greenwic_edu_nwu", user = username, password = rstudioapi::askForPassword('Password: '), host = "67.225.186.169")

# pull the master_role_df data frame for further analysis without repeated SQL queries
# in order to adjust roles or verticals, check the master.vertical or role.role entries within the query
master_role_df <- dbGetQuery(connection, "SELECT * FROM greenwich_master_nwu_fall_2018_proj1 AS master JOIN greenwich_role_nwu_fall_2018_proj1 AS role ON master.job_id = role.job_id WHERE master.vertical = 'Healthcare' AND (role.role = 'Nurse Manager' OR role.role = 'Nursing Assistant')")

# define region user_defined_regions to examine
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

# create a dataframe to list the region user_defined_region for each state

# this operation repeats the name of each region as many times as there are states within the region (i.e. northeast three times), making a long vector
region_list <- c(rep('west_coast',length(west_coast)),rep('southwest',length(southwest)),rep('rockies',length(rockies)),rep('texas',length(texas)),
                 rep('plains',length(plains)),rep('great_lakes',length(great_lakes)),rep('rust_belt',length(rust_belt)),rep('middle_south', length(middle_south))
                 ,rep('south',length(south)),rep('east_coast', length(east_coast)),rep('northeast', length(northeast)),rep('uniques', length(uniques)))

# this operation makes one long vector of all the states (as abbreviations) in all the regions
region_definitions <- c(west_coast, southwest, rockies, texas, plains, great_lakes, rust_belt, middle_south, south, east_coast, northeast, uniques)

# this operation provides the full names of each state, in case that is needed for a value lookup
state_names <- c('california', 'oregon', 'washington', 'arizona', 'new mexico', 'utah', 'nevada', 'idaho', 'montana', 'wyoming', 'colorado', 
                 'texas', 'north dakota', 'south dakota','nebraska', 'kansas', 'oklahoma', 'iowa', 'minnesota', 'wisconsin', 'illinois', 'indiana',
                 'michigan', 'ohio', 'pennsylvania', 'west virginia','arkansas', 'missouri', 'kentucky', 'tennessee', 'north carolina', 'louisiana', 
                 'mississippi', 'alabama', 'south carolina', 'georgia', 'florida', 'virginia', 'district of columbia', 'maryland', 'delaware', 
                 'new jersey', 'new york', 'connecticut', 'rhode island', 'massachusetts', 'vermont', 'new hampshire', 'maine', 'alaska', 'hawaii')

# finally we column bind all three vectors to make a dataframe that has each state abbreviation, state name, and what region it is defined as being in
regions_df <- data.frame(cbind(region_list,region_definitions,state_names))

# using our defined regions dataframe, we add the regions we have defined to the master_df as master_role_df_w_states
master_role_df_w_states <- master_role_df[!(is.na(master_role_df[,'state'])),]
for (state in regions_df$region_definitions){
master_role_df_w_states[master_role_df_w_states['state'] == state,'user_defined_region'] <- regions_df[regions_df['region_definitions'] == state, 'region_list']
}

# check portion of rows with time_to_fill as NA for each region
for (region in unique(regions_df$region_list)){
  print(region)
  print(sum(is.na(master_role_df_w_states$time_to_fill[master_role_df_w_states['user_defined_region'] == region]))*100.0/nrow(master_role_df_w_states[master_role_df_w_states['user_defined_region'] == region, ]))
}

# in our case, all NA rates are below 5%, so we drop the data to examine only entries with completed time_to_fill
master_role_df_w_states <- master_role_df_w_states[!(is.na(master_role_df_w_states$time_to_fill)),]

# we can now examine the average time to fill for the various user_defined_regions, using deplyr and ggplot2
library(dplyr)
library(ggplot2)

# as part of our analysis, we decided to ignore the upper and lower 10% of values, as they were likely to skew our results
# and jobs that filled in those time ranges might have not been correlated directly to an actual position being filled

# compute the mean and the SD for time_to_fill that is limited to the 10%-90% range of data
lower_limit = quantile(master_role_df_w_states$time_to_fill, probs = 0.1)
upper_limit = quantile(master_role_df_w_states$time_to_fill, probs = 0.9)
user_defined_region_10_90_avgs <- master_role_df_w_states %>%
  filter(time_to_fill > lower_limit & time_to_fill < upper_limit) %>%
  group_by(user_defined_region) %>%
  summarise(avg_time_to_fill = mean(time_to_fill), sd_time_to_fill = sd(time_to_fill))

# plot of the different mean times to fill for the data
master_role_df_w_states %>%
  filter(time_to_fill > lower_limit & time_to_fill < upper_limit) %>%
  group_by(user_defined_region) %>%
  summarise(avg_time_to_fill = mean(time_to_fill)) %>%
  ggplot() +
  geom_col(aes(x = reorder(user_defined_region,-avg_time_to_fill), y = avg_time_to_fill)) +
  labs(title= "Average Time to Fill By Region", x = "Region", y= "Time To Fill in Days") +
  coord_flip()

# plot a box plot for the time_to_fill for each region
master_role_df_w_states %>%
  filter(time_to_fill > lower_limit & time_to_fill < upper_limit) %>%
  group_by(user_defined_region) %>%
  ggplot() +
  geom_boxplot(aes(x = user_defined_region, y = time_to_fill)) +
  labs(title= "Average Time to Fill By Region", x = "Region", y= "Time To Fill in Days") +
  coord_flip()

# plot a violin plot for the time_to_fill for each region
master_role_df_w_states %>%
  filter(time_to_fill > lower_limit & time_to_fill < upper_limit) %>%
  group_by(user_defined_region) %>%
  ggplot() +
  geom_violin(scale = "area", aes(x = user_defined_region, y = time_to_fill)) +
  labs(title= "Average Time to Fill By Region", x = "Region", y= "Time To Fill in Days") +
  coord_flip()

# showing the data collected on a map of the US

# load the map data for states
map <- map_data("state")

# for each state in the map data, add the respective region from the region_df, as well as the abbreviation
for (state in regions_df$state_names){
  map[map['region'] == state,'id'] <- regions_df[regions_df['state_names'] == state, 'region_list']
  map[map['region'] == state,'abbrev'] <- regions_df[regions_df['state_names'] == state, 'region_definitions']
}

# rename the columns of the map so that region is used as the id, rather than the state
colnames(map) <- c('long', 'lat', 'group', 'order', 'reg', 'subregion', 'id', 'abbrev')

# plot the average time_to_fill for each region on a map by the separate regions
master_role_df_w_states %>%
  filter(time_to_fill > lower_limit & time_to_fill < upper_limit)  %>%
  group_by(user_defined_region, role) %>%
  summarize(avg_time_to_fill = mean(time_to_fill)) %>%
  ggplot(aes(fill = avg_time_to_fill)) +
  geom_map(aes(map_id = user_defined_region), map = map) +
  expand_limits(x = map$long, y = map$lat) +
  scale_fill_gradient(high = "red", low = "green") +
  labs(title= "Average Time to Fill By Region and Role", x = "Longitude", y= "Latitude") +
  geom_polygon(data = map, aes(x = long, y = lat, group = group), fill = NA, color = "black") +
  facet_grid(role ~ .)

# plot the average time_to_fill for each region on a map for nurse manager only
master_role_df_w_states %>%
  filter(role == 'Nurse Manager') %>%
  filter(time_to_fill > lower_limit & time_to_fill < upper_limit)  %>%
  group_by(user_defined_region) %>%
  summarize(avg_time_to_fill = mean(time_to_fill)) %>%
  ggplot(aes(fill = avg_time_to_fill)) +
  geom_map(aes(map_id = user_defined_region), map = map) +
  expand_limits(x = map$long, y = map$lat) +
  scale_fill_gradient(high = "red", low = "green") +
  labs(title= "Average Time to Fill By Region for Nurse Manager", x = "Longitude", y= "Latitude") +
  geom_polygon(data = map, aes(x = long, y = lat, group = group), fill = NA, color = "black")

# plot the average time_to_fill for each region on a map for nurse manager only
master_role_df_w_states %>%
  filter(role == 'Nursing Assistant') %>%
  filter(time_to_fill > lower_limit & time_to_fill < upper_limit)  %>%
  group_by(user_defined_region) %>%
  summarize(avg_time_to_fill = mean(time_to_fill)) %>%
  ggplot(aes(fill = avg_time_to_fill)) +
  geom_map(aes(map_id = user_defined_region), map = map) +
  expand_limits(x = map$long, y = map$lat) +
  scale_fill_gradient(high = "red", low = "green") +
  labs(title= "Average Time to Fill By Region for Nursing Assistant", x = "Longitude", y= "Latitude") +
  geom_polygon(data = map, aes(x = long, y = lat, group = group), fill = NA, color = "black")

#Moran's Index Examination:

# to check whether there are indeed regional/spatial correlations for our data, we examine Moran's Index, which
# is a measure of the spatial auto-correlation of data

# to measure Moran's Index, we use the "ape" and "geosphere" packages
install.packages("ape")
library(ape)
install.packages("geosphere")
library(geosphere)

# we define a function for calculating centroids for a given matrix
cntrd <- function(x) {
  data.frame(centroid(as.matrix(x[,c("long", "lat")])))
}

# we compute the centroid for each state
by(map, map$reg, cntrd) %>% head()

# we group the map by states, with each's centroid listed in the data frame
states <- group_by(map, abbrev) %>%
  do(cntrd(.))

# we compute the distance of each state from each other, saving it in the state.dists matrix
state.dists <- as.matrix(dist(cbind(states$lon, states$lat)))

# for computing Moran's Index, we need the inverse of the distances, with the diagonal of the matrix set to 0
state.dists.inv <- 1/state.dists
diag(state.dists.inv) <- 0

# we now compute the average time_to_fill for each state, ignoring Alaska and Hawaii (they aren't in the map data)
state.avg_fill <-master_role_df_w_states %>%
  filter(user_defined_region != "uniques") %>%
  group_by(state) %>%
  summarize(avg_time_to_fill = mean(time_to_fill))

# we compute Moran's Index using the average time_to_fill as our values and the inverse matrix of distances
Moran.I(state.avg_fill$avg_time_to_fill, state.dists.inv)

# we now wish to compute the Moran's Index for the regions we have defined, using being in the same region as being "close"

# we generate a binary matrix for each state pair
state.dists.bin <- matrix(rep(1,49^2), ncol = 49, nrow = 49)

# for each state, we define what region it is in 
for (st in regions_df$region_definitions){
  states[states['abbrev'] == st,'region'] <- regions_df[regions_df['region_definitions'] == st, 'region_list']
}

# for all pairs of states, if the two states are in the same region, their distance is 1, otherwise it is 0
for (i in 1:49){
  for (j in 1:49){
    if (states[i,'region'] == states[j,'region']){
      state.dists.bin[i,j] <- 1
    }
    else {state.dists.bin[i,j] <- 0}
  }
}

# we set the diagonal of the binary matrix to 0
diag(state.dists.bin) <- 0

# we compute Moran's Index and compare it to that of the individual states
Moran.I(state.avg_fill$avg_time_to_fill, state.dists.bin)

# ultimately, we are looking at whether our defined regions have greater spatial autocorrelation 
# than the spatial autocorrelation based on distance between state centroids. So long as both Indexes
# are significant (p < 0.05), we can compare the observed Moran's Index and, if it is greater than 0,
# we can compare the degree of spatial autocorrelation for the two methods of computing "closeness."
