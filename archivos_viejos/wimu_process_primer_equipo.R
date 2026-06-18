### SCRIPT PARA PROCESAR DATOS WIMU PRIMER EQUIPO (API) -----
library(pacman)
p_load(tidyverse, ggplot2, readxl,
       skimr, scales, lubridate, readr, writexl, zoo, openxlsx, tidylog)
options(scipen = 999)
# Fetch data from the API (produces final_dataframe)
source("wimu_api.R")
# Rename and transform API columns to match downstream format
data_micro <- final_dataframe %>%
  select(-player) %>%  # drop player ID — player.wimuName becomes the player column
  rename(
    # date              = session.date_time,
    player            = username,
    task_name         = task,
    match_day         = session.matchDay,
    session_duration  = duration_min,
    distance_m        = distance.distance,
    dist_over_time    = distance.distanceMin,
    HSR_abs_dist      = distance.HSRAbsDistance,
    perc_HSR_abs      = distance.percentageHSRAbs,
    max_speed         = sprint.maxSpeed,
    distance_abs      = sprint.distance,
    sprints_abs_count = sprint.abs,
    sprints_rel_count = distance.HSRRelCount,
    player_load       = load.Player_Load,
    HMLD_m            = load.hmld,
    step_balance      = steps.stepBalance
  ) %>%
  mutate(
    date            = as.Date(session.date_time, tz = "America/Mexico_City"),
    drill_duration  = drillsDuration / 60000,  # milliseconds to minutes
    HSR_over_time   = HSR_abs_dist / session_duration,
    perc_HSR_abs    = perc_HSR_abs / 100,
    acc             = accelerations.highIntensityAccAbsCounter,
    decc            = accelerations.highIntensityDecAbsCounter,
    acc_plus_decc   = acc + decc,
    tiempo_efectivo = drill_duration / session_duration,
    RPE             = 3.0,
    TL              = RPE * player_load,
    match_day       = ifelse(match_day != "MD", gsub(" MD", "", match_day), match_day)
  ) %>%
  filter(team.name == "America Primer Equipo") |>
  filter(!player %in% c("www", "Guillermo Ochoa")) %>%
  distinct(player, date, match_day, .keep_all = TRUE)
# Select output columns
data_micro <- data_micro %>%
  select(player, task_name, match_day, date, session_duration, drill_duration, distance_m,
         dist_over_time, HSR_abs_dist, HSR_over_time, perc_HSR_abs,
         HMLD_m, distance_abs, sprints_abs_count, max_speed, acc, decc, acc_plus_decc,
         player_load, sprints_rel_count, step_balance, tiempo_efectivo, RPE, TL) %>%
  arrange(date)
# Write outputs — paths come from env vars in CI, fall back to local paths
path_csv  <- Sys.getenv("DASHBOARD_CARGAS_CSV",
                        unset = "/Users/mateorodriguez/Desktop/analisis_CA/dashboard_cargas/micros/micros_shiny_comb.csv")
path_xlsx <- Sys.getenv("CARGAS7_XLSX",
                        unset = "/Users/mateorodriguez/Desktop/analisis_CA/cargas_fisicas_7/data/Sessions_micro01.xlsx")
path_xlsx_clausura26 <- "/Users/mateorodriguez/Desktop/analisis_CA/Temporadas/clausura_26/micros_procesados/Sessions_micro01.xlsx"
write_csv(data_micro, path_csv)
if (dir.exists(dirname(path_xlsx))) {
  write_xlsx(data_micro, path = path_xlsx)
}
if (dir.exists(dirname(path_xlsx_clausura26))) {
  write_xlsx(data_micro, path = path_xlsx_clausura26)
}