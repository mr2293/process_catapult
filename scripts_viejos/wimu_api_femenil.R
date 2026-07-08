#Libraries
library(mice) #needed for Power BI
library(httr) #request (GET) to the API
library(vctrs) #join dataframes by rows
library(lubridate) #transform date column (character) to yyyy-MM-dd (date)
library(tidyr) #expand lists from dataframe
library(anytime) #transform data format to UNIX
library(dplyr) #selecting columns and manipulating dataframes
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
options(scipen = 100)

#Token (user and password for the WIMU API, plus start date)
username <- Sys.getenv("WIMU_USERNAME")
password <- Sys.getenv("WIMU_PASSWORD")
date <- "2025/12/17"
# date_end <- "2026/03/18"
team <- "5d55d7172ab79c0012d744f7"
get_token <- GET(paste("https://femexfut.wimucloud.com/apis/rest/login?username=",username,"&password=",password,sep=""))
if (http_error(get_token)) stop("Login failed — check WIMU_USERNAME and WIMU_PASSWORD env vars (HTTP ", status_code(get_token), ")")
get_token <- as.data.frame(jsonlite::fromJSON(content(get_token, as = "text", encoding = "UTF-8")))

#Players Endpoint to Dataframe
players <- data.frame()
for(i in 1:10) {
  pla <- tryCatch(GET(paste("https://femexfut.wimucloud.com/apis/rest/players?page=", i, sep=""),
             add_headers(Authorization = paste(get_token[[1]]))), error = function(e) NULL)
  if (is.null(pla) || http_error(pla)) break
  pla <- jsonlite::fromJSON(content(pla, as = "text", encoding = "UTF-8"), flatten = TRUE)  # flatten nested fields at parse time
  pla <- as.data.frame(pla)
  pla <- mutate(pla, across(everything(), as.character))
  players <- vec_rbind(players, pla)
}
rm(pla)

# players_flat <- players
#
# for (col in names(players_flat)) {
#   if (is.list(players_flat[[col]])) {
#     players_flat[[col]] <- sapply(players_flat[[col]], function(x) {
#       tryCatch(
#         paste(unlist(x), collapse = ", "),
#         error = function(e) NA_character_
#       )
#     })
#   }
# }
#
# write.csv(players_flat, "/Users/mateorodriguez/Desktop/players.csv", row.names = FALSE)

#Teams Endpoint to Dataframe
teams <- GET("https://femexfut.wimucloud.com/apis/rest/teams",
             add_headers(Authorization=paste(get_token[[1]])))
if (http_error(teams)) stop("Teams request failed (HTTP ", status_code(teams), ")")
teams <- as.data.frame(jsonlite::fromJSON(content(teams, as = "text", encoding = "UTF-8")))

# Team IDs
# America Primer Equipo : 5d6812762ab79c0013380530
# America Femenil : 5d55d7172ab79c0012d744f7
# America Sub-23 : 5af93feef5acc8125ec33c36

#Attributes Endpoint to Dataframe
attributes <- GET("https://femexfut.wimucloud.com/apis/rest/attributes",
                  add_headers(Authorization=paste(get_token[[1]])))
if (http_error(attributes)) stop("Attributes request failed (HTTP ", status_code(attributes), ")")
attributes <- as.data.frame(jsonlite::fromJSON(content(attributes, as = "text", encoding = "UTF-8")))

#Loop to extract sesions dataframes based on the page parameter (10 pages => last 2000 sessions)
sessions <- data.frame()
for(i in 1:10) {
  ses <- tryCatch(GET(paste("https://femexfut.wimucloud.com/apis/rest/sessions?sort=start,desc&page=",
                   i,sep=""),
             add_headers(Authorization=paste(get_token[[1]]))), error = function(e) NULL)
  if (is.null(ses) || http_error(ses)) break
  ses <- as.data.frame(jsonlite::fromJSON(content(ses, as = "text", encoding = "UTF-8")))
  sessions <- vec_rbind(sessions,ses)
}
rm(ses)
##Adding columns for date, time and date_time using "start" (UNIX format)
# sessions$date_time <- as_datetime((sessions$start)/1000) #/1000 to convert milliseconds
# sessions$date <- substr(sessions$date_time, start = 1, stop = 10)
# sessions$time <- substr(sessions$date_time, start = 12, stop = 20)

#Loop to extract informs dataframes filtering by the start date
##If there is no date, we download the last 7 days
date1 <- ifelse(nchar(date) < 10,today()-7,date)
date1 <- anydate(date1)
start <- as.numeric(as.POSIXct(date1))*1000
# end <- as.numeric(as.POSIXct(date_end))*1000
team <- team
##We need to extract the ID of each session from the start date
sessions_informs <- data.frame()
for (i in 1:10) {
  sessions_inf <- tryCatch(GET(paste("https://femexfut.wimucloud.com/apis/rest/sessions?sort=start,desc&page=",
                            i,"&start=",start,
                            # "&end=",end,
                            "&team=",team,sep=""),add_headers(Authorization=paste(get_token[[1]]))), error = function(e) NULL)
  if (is.null(sessions_inf) || http_error(sessions_inf)) break
  sessions_inf <- as.data.frame(jsonlite::fromJSON(content(sessions_inf, as = "text", encoding = "UTF-8")))
  sessions_informs <- vec_rbind(sessions_informs,sessions_inf)
}
# sessions_informs <- as.data.frame(sessions_informs)
# sessions_informs$date_time <- as_datetime((sessions_informs$start)/1000)
# sessions_informs$date <- substr(sessions_informs$date_time, start = 1, stop = 10)
# sessions_informs$time <- substr(sessions_informs$date_time, start = 12, stop = 20)
##Adding the session ID to the URL that we will use to get the informs data for each session
urls <- c()
for (i in 1:nrow(sessions_informs)) {
  urls[i] <- paste("https://femexfut.wimucloud.com/apis/rest/informs?session=",
                   sessions_informs$id[[i]],sep = "")
}
##Creating a dataframe for each session to finally merge all of them under the same dashboard
informs <- data.frame() #empty dataframe
for(i in 1:length(urls)) {
  inf <- tryCatch(GET(urls[i], add_headers(Authorization=paste(get_token[[1]]))), error = function(e) NULL) #API request
  if (is.null(inf) || http_error(inf)) next
  inf <- as.data.frame(jsonlite::fromJSON(content(inf, as = "text", encoding = "UTF-8"))) #convert from JSON to dataframe
  informs <- vec_rbind(informs,inf) #join dataframe extracted with the previous
}
rm(sessions_inf)
rm(urls)
rm(inf)
##Date & Time columns
# informs$date_time <- as_datetime((informs$start)/1000)
# informs$date <- substr(informs$date_time, start = 1, stop = 10)
# informs$time <- substr(informs$date_time, start = 12, stop = 20)
sessions_informs$date_time <- as_datetime((sessions_informs$start)/1000)
# sessions_informs$date <- substr(sessions_informs$date_time, start = 1, stop = 10)
# sessions_informs$time <- substr(sessions_informs$date_time, start = 12, stop = 20)

#Expand columns
players <- jsonlite::flatten(players)
sessions <- jsonlite::flatten(sessions)
informs <- jsonlite::flatten(informs)

#Removing "list" columns from players and sessions (not needed)
players <- purrr::discard(players, is.list)
sessions <- purrr::discard(sessions, is.list)

#Columns needed for the output
##Player: Max Speed, Max Acc, Max Dec
players_selection <- players[,c("id","wimuName","position","maxSpeed","maxAcc","minAcc")]
##Teams: Name
teams_selection <- teams[,c("id","name")]
##Session: Group, Team, Date, Name, Type
sessions_selection <- sessions_informs[,c("id","date_time","name","type","center","matchDay",
                                          "group","weekCalendar")]
##Attributes: Weather (Session), Contact Level (Drill)
attributes_selection <- attributes[,c("id","name","tags")]
##Informs: Core Metrics
informs_selection <- informs[,c(
  "team",
  "username",
  "position",
  "created",
  "matchDay",
  "start",
  "end",
  "session",
  "task",
  "player",
  "duration",
  "drillsDuration",
  "distance.distance",
  "distance.distanceMin",
  "distance.HSRRelCount",
  "distance.HSRRelDistance",
  "distance.HSRAbsCount",
  "distance.HSRAbsDistance",
  "distance.percentageHSRAbs",
  "distance.ranges",
  # "accelerations.accelerations",
  "accelerations.highIntensityAccAbsCounter",
  # "accelerations.highIntensityAccAbsDistance",
  # "accelerations.decelerations",
  "accelerations.highIntensityDecAbsCounter",
  # "accelerations.highIntensityDecAbsDistance",
  # "accelerations.maxAcceleration",
  # "accelerations.maxDeceleration",
  "accelerations.ranges",
  "sprint.abs",
  "sprint.nSprints",
  "sprint.distance",
  "sprint.distanceRelative",
  "sprint.maxSpeed",
  "sprint.relRepetitions",
  "load.hmld",
  "load.hmldMin",
  "load.Player_Load",
  "load.Player_LoadMin",
  "load.highIntWorkAccDist",
  "steps.stepBalance"
  # "rpe.RPECentral",
  # "rpe.wellnessSleep",
  # "rpe.wellnessDoms",
  # "rpe.wellnessStress",
  # "rpe.wellnessMood",
  # "rpe.wellnessFatigue"
)]

# informs_selection <- informs_selection |>
#   filter(task == "Drills")

#Merge dataframes to get the final output
##Players
players_selection <- players_selection %>%
  rename_with(~ paste0("player.", .), .cols = -id)
informs_players <- merge(informs_selection, players_selection,
                         by.x = "player", by.y = "id", all.x = TRUE)
names(informs_players)
##Sessions
sessions_selection <- sessions_selection %>%
  rename_with(~ paste0("session.", .), .cols = -id)
informs_sessions <- merge(informs_players, sessions_selection,
                          by.x = "session", by.y = "id", all.x = TRUE)
##Teams
teams_selection <- teams_selection %>%
  rename_with(~ paste0("team.", .), .cols = -id)
informs_teams <- merge(informs_sessions, teams_selection,
                       by.x = "session.center", by.y = "id", all.x = TRUE)
informs_sessions <- informs_teams
names(informs_sessions)


#Final changes
##Filter only for Drills - we could also maintain it with all the tasks
informs_sessions <- informs_sessions[informs_sessions$task == "Drills",]

########## -------------- ############## ----------------- ###############
########## -------------- ############## ----------------- ###############
########## -------------- ############## ----------------- ###############

##Extract Speed Zones
my_id_columns <- names(informs_sessions) #original columns except "distance.ranges"
my_id_columns <- my_id_columns[my_id_columns != "distance.ranges"]
informs_sessions_unested <- informs_sessions %>%
  unnest(col = distance.ranges, names_sep = "_")
informs_sessions_final <- informs_sessions_unested %>%
  mutate(column_name = paste0("speed_zones_", distance.ranges_min, "_", distance.ranges_max)) %>%
  pivot_wider(id_cols = all_of(my_id_columns), names_from = column_name,
              values_from = distance.ranges_value, values_fill = NA, values_fn = mean) %>%
  select(-starts_with("distance.ranges_"))

##Extract Acceleration Zones
my_id_columns <- names(informs_sessions_final) #original columns - "accelerations.ranges"
my_id_columns <- my_id_columns[my_id_columns != "accelerations.ranges"]
informs_sessions_unested_v2 <- informs_sessions_final %>%
  unnest(col = accelerations.ranges, names_sep = "_")
informs_sessions_final_v2 <- informs_sessions_unested_v2 %>%
  mutate(column_name = paste0("accelerations_zones_", accelerations.ranges_min, "_",
                              accelerations.ranges_max)) %>%
  pivot_wider(id_cols = all_of(my_id_columns), names_from = column_name,
              values_from = accelerations.ranges_value, values_fill = NA, values_fn = mean) %>%
  select(-starts_with("accelerations.ranges_"))

##Convert duration to minutes
final_dataframe <- informs_sessions_final_v2
final_dataframe$duration_min <- final_dataframe$duration / 60000

final_dataframe <- final_dataframe %>%
  mutate(across(everything(), ~ suppressWarnings(type.convert(.x, as.is = TRUE))))

##Percentage of Max Speed
final_dataframe$percentage_maxSpeed <- (as.numeric(final_dataframe$sprint.maxSpeed) /
                                          as.numeric(final_dataframe$player.maxSpeed))*100

##Relative metrics (HSR Abs & Rel, Sprint Abs & Rel, Acc & HIA, Dec & HID)
final_dataframe$distance.HSRRelCountMin <- final_dataframe$distance.HSRRelCount / final_dataframe$duration_min
final_dataframe$distance.HSRAbsCountMin <- final_dataframe$distance.HSRAbsCount / final_dataframe$duration_min
final_dataframe$distance.HSRRelDistanceMin <- final_dataframe$distance.HSRRelDistance / final_dataframe$duration_min
final_dataframe$distance.HSRAbsDistanceMin <- final_dataframe$distance.HSRAbsDistance / final_dataframe$duration_min
final_dataframe$sprint.absMin <- final_dataframe$sprint.abs / final_dataframe$duration_min
final_dataframe$sprint.nSprintsMin <- final_dataframe$sprint.nSprints / final_dataframe$duration_min
final_dataframe$sprint.distanceMin <- final_dataframe$sprint.distance / final_dataframe$duration_min
final_dataframe$sprint.distanceRelativeMin <- final_dataframe$sprint.distanceRelative / final_dataframe$duration_min
# final_dataframe$accelerations.accelerationsMin <- final_dataframe$accelerations.accelerations / final_dataframe$duration_min
# final_dataframe$accelerations.decelerationsMin <- final_dataframe$accelerations.decelerations / final_dataframe$duration_min
# final_dataframe$accelerations.highIntensityAccAbsCounterMin <- final_dataframe$accelerations.highIntensityAccAbsCounter / final_dataframe$duration_min
# final_dataframe$accelerations.highIntensityDecAbsCounterMin <- final_dataframe$accelerations.highIntensityDecAbsCounter / final_dataframe$duration_min
# final_dataframe$accelerations.highIntensityAccAbsDistanceMin <- final_dataframe$accelerations.highIntensityAccAbsDistance / final_dataframe$duration_min
# final_dataframe$accelerations.highIntensityDecAbsDistanceMin <- final_dataframe$accelerations.highIntensityDecAbsDistance / final_dataframe$duration_min

##Reorder and Remove Columns
###Add: week calendar, match day
names(final_dataframe)
final_dataframe <- final_dataframe %>%
  relocate(session, session.name, session.center, team.name, session.date_time, session.weekCalendar,
           session.type, session.matchDay, session.group, task, player, player.wimuName,
           player.position, player.maxSpeed, player.maxAcc, player.minAcc, duration, duration_min)
names(final_dataframe)

final_dataframe <- final_dataframe %>%
  arrange(session.date_time)

final_dataframe$session.date_time <- with_tz(final_dataframe$session.date_time, tzone = "America/Mexico_City")
