### SCRIPT PARA PROCESAR DATOS CATAPULT PRIMER EQUIPO (API) -----
library(pacman)
p_load(tidyverse, scales, lubridate, readr, writexl, openxlsx, tidylog)
options(scipen = 999)

# =============================================================================
# 1) Fetch data from the Catapult Connect API
#    Produces: stats_df, activities_df, activity_tags_df, etc.
# =============================================================================
source("catapult_api.R")

# =============================================================================
# 2) Build lookup tables from activity metadata
# =============================================================================

# Match Day from activity tags (DayCode)
match_day_lookup <- activity_tags_df %>%
  filter(tag_type_name == "DayCode") %>%
  select(activity_id, match_day_raw = tag_name) %>%
  distinct(activity_id, .keep_all = TRUE)

# Activity date + name
activity_date_lookup <- activities_df %>%
  mutate(date = as.Date(start_time, tz = "America/Mexico_City")) %>%
  select(activity_id, date, session_name = name)

# =============================================================================
# 3) Helper: safely reference a column (returns 0 if missing)
# =============================================================================
safe_col <- function(df, col_name) {
  if (col_name %in% names(df)) df[[col_name]] else rep(0, nrow(df))
}

# =============================================================================
# 4) Join metadata onto stats_df
# =============================================================================
stats_enriched <- stats_df %>%
  select(-any_of("date")) %>%
  left_join(activity_date_lookup, by = "activity_id") %>%
  left_join(match_day_lookup,     by = "activity_id")

# =============================================================================
# 5) Aggregate periods → one row per player per activity
#    - SUM:  distances, durations, efforts, load, counts
#    - MAX:  max velocity, max acc, max dec
#    - FIRST: profile values (same across periods)
# =============================================================================
data_agg <- stats_enriched %>%
  group_by(date, athlete_id, athlete_name) %>%
  summarise(
    # Track which sessions were combined
    session_name          = paste(unique(session_name), collapse = " | "),
    match_day_raw         = first(na.omit(match_day_raw)),
    # ---- Summed metrics ----
    total_distance_raw    = sum(total_distance,       na.rm = TRUE),
    total_duration_s      = sum(total_duration,        na.rm = TRUE),
    total_player_load     = sum(total_player_load,     na.rm = TRUE),
    hmld_gen2             = sum(safe_col(cur_data(), "hmld_gen2"),           na.rm = TRUE),
    hsr_abs_raw           = sum(safe_col(cur_data(), "high_speed_distance"), na.rm = TRUE),
    hsr_rel_raw           = sum(safe_col(cur_data(), "high_speed_distance_>75%_(total)"), na.rm = TRUE),
    hsr_rel_efforts       = sum(safe_col(cur_data(), "high_speed_efforts_>75%_(total)"), na.rm = TRUE),
    sprint_dist_raw       = sum(safe_col(cur_data(), "sprint_distance_>_30_km/hr"),       na.rm = TRUE),
    sprint_efforts        = sum(safe_col(cur_data(), "sprint_efforts_>_30km/hr"),          na.rm = TRUE),
    hr_exertion           = sum(safe_col(cur_data(), "heart_rate_exertion"),  na.rm = TRUE),
    energy                = sum(safe_col(cur_data(), "energy"),               na.rm = TRUE),
    red_zone              = sum(safe_col(cur_data(), "red_zone"),             na.rm = TRUE),
    
    # Acc / Dec counts (summed across periods)
    acc_gt2               = sum(safe_col(cur_data(), "gen2_acceleration_band6plus_total_effort_count"), na.rm = TRUE),
    acc_gt3               = sum(safe_col(cur_data(), "gen2_acceleration_band7plus_total_effort_count"), na.rm = TRUE),
    decc_gt2              = sum(safe_col(cur_data(), "gen2_acceleration_band3plus_total_effort_count"), na.rm = TRUE),
    decc_gt3              = sum(safe_col(cur_data(), "gen2_acceleration_band2plus_total_effort_count"), na.rm = TRUE),
    hia                   = sum(safe_col(cur_data(), "explosive_efforts"),     na.rm = TRUE),
    
    # ---- Max metrics (take max across periods) ----
    max_velocity          = max(safe_col(cur_data(), "max_vel"),                na.rm = TRUE),
    max_acc               = max(safe_col(cur_data(), "max_effort_acceleration"), na.rm = TRUE),
    max_dec               = max(safe_col(cur_data(), "max_effort_deceleration"), na.rm = TRUE),
    
    # ---- Profile values (same across periods, take first) ----
    profile_max_vel       = first(safe_col(cur_data(), "athlete_max_velocity")),
    
    .groups = "drop"
  )

# =============================================================================
# 6) Apply multipliers & compute derived metrics
# =============================================================================
data_micro <- data_agg %>%
  mutate(
    # ---- Multipliers (km → m for distance fields) ----
    distance_m       = total_distance_raw * 1000,
    HSR_abs_dist     = hsr_abs_raw * 1000,
    HSR_rel_dist     = hsr_rel_raw * 1000,
    sprint_dist      = sprint_dist_raw * 1000,
    
    # ---- Duration ----
    session_duration = total_duration_s / 60,   # seconds → minutes
    
    # ---- Rates (recomputed after aggregation) ----
    dist_over_time   = ifelse(total_duration_s > 0,
                              distance_m / (total_duration_s / 60), 0),
    HSR_over_time    = ifelse(session_duration > 0,
                              HSR_abs_dist / session_duration, 0),
    pl_per_min       = ifelse(session_duration > 0,
                              total_player_load / session_duration, 0),
    
    # ---- Velocity ----
    pct_max_velocity = ifelse(profile_max_vel > 0,
                              (max_velocity / profile_max_vel) * 100, NA_real_),
    
    # ---- Acc / Dec matching WIMU high-intensity counters (>3 m/s²) ----
    acc             = acc_gt3,
    decc            = decc_gt3,
    acc_plus_decc   = acc_gt3 + decc_gt3,
    
    # ---- Match day cleanup ----
    match_day        = case_when(
      is.na(match_day_raw)  ~ NA_character_,
      match_day_raw == "MD" ~ "MD",
      TRUE                  ~ gsub(" MD", "", match_day_raw)
    ),
    
    # ---- RPE & Training Load (update RPE manually after sessions) ----
    RPE              = 3.0,
    TL               = RPE * total_player_load
  ) %>%
  
  # ---- Rename to match downstream WIMU format ----
rename(
  player         = athlete_name,
  player_load    = total_player_load,
  hmld_m         = hmld_gen2,
  max_speed      = max_velocity,
  sprint_efforts_count = sprint_efforts
) %>%
  
  # ---- Filter ----
filter(!player %in% c("www", "Guillermo Ochoa")) %>%
  distinct(player, date, match_day, .keep_all = TRUE) %>%
  
  # ---- Select & order output columns ----
select(
  player, date, match_day, session_name,
  session_duration,
  distance_m, dist_over_time,
  HSR_abs_dist, HSR_rel_dist, hsr_rel_efforts, HSR_over_time,
  sprint_dist, sprint_efforts_count,
  max_speed, pct_max_velocity, profile_max_vel,
  player_load, pl_per_min,
  hmld_m,
  acc, decc, acc_plus_decc,
  acc_gt2, acc_gt3, decc_gt2, decc_gt3,
  hia,
  max_acc, max_dec,
  hr_exertion, energy, red_zone,
  RPE, TL
) %>%
  arrange(date, player)

# =============================================================================
# 7) Summary
# =============================================================================
cat(sprintf("\nProcessed %d rows for %d players across %d sessions.\n",
            nrow(data_micro),
            n_distinct(data_micro$player),
            n_distinct(data_micro$date)))

# =============================================================================
# 8) Write outputs
# =============================================================================
path_csv  <- Sys.getenv("DASHBOARD_CARGAS_CSV",
                        unset = "/Users/mateorodriguez/Desktop/analisis_CA/dashboard_cargas/micros/micros_shiny_comb.csv")
path_xlsx <- Sys.getenv("CARGAS7_XLSX",
                        unset = "/Users/mateorodriguez/Desktop/analisis_CA/cargas_fisicas_7/data/Sessions_micro01.xlsx")
path_xlsx_apertura26 <- "/Users/mateorodriguez/Desktop/analisis_CA/Temporadas/apertura_26/micros_procesados/Sessions_micro01.xlsx"

write_csv(data_micro, path_csv)

if (dir.exists(dirname(path_xlsx))) {
  write_xlsx(data_micro, path = path_xlsx)
}

if (dir.exists(dirname(path_xlsx_apertura26))) {
  write_xlsx(data_micro, path = path_xlsx_apertura26)
}