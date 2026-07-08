# ============================================================================
# Catapult Connect API v6 — Full Data Pull + Stats (Femenil)
#
# Endpoints used:
#   1. GET  /activities                          (list all activity IDs)
#   2. GET  /activities/{id}?include=all         (deep activity details)
#   3. GET  /activities/{id}/periods             (detailed period info)
#   4. GET  /periods/{id}?include=period_athletes (period athletes — optional)
#   5. GET  /parameters                          (all parameters)
#   6. GET  /parameters/{id}                     (single parameter — optional)
#   7. POST /stats                               (performance metrics)
# ============================================================================

library(httr)
library(jsonlite)
library(dplyr)
library(purrr)

# --- Configuration -----------------------------------------------------------
base_url   <- "https://connect-us.catapultsports.com/api/v6"
token_env  <- Sys.getenv("CATAPULT_TOKEN_FEMENIL")
if (!nzchar(token_env)) stop("CATAPULT_TOKEN_FEMENIL env var is not set.")
token      <- token_env
delay_sec  <- 3    # seconds between requests; bump to 60 if you hit 429s
start_date <- "2025-06-01"  # only fetch activities on or after this date (YYYY-MM-DD)

# --- Helper: authenticated GET -----------------------------------------------
catapult_get <- function(endpoint) {
  response <- GET(
    url = paste0(base_url, endpoint),
    add_headers(Authorization = paste("Bearer", token)),
    accept_json()
  )
  if (http_error(response)) {
    stop(sprintf("GET [%s] %s: %s",
                 status_code(response), endpoint,
                 content(response, "text", encoding = "UTF-8")))
  }
  fromJSON(content(response, "text", encoding = "UTF-8"),
           simplifyVector = FALSE)
}

# --- Helper: authenticated POST ----------------------------------------------
catapult_post <- function(endpoint, body) {
  response <- POST(
    url = paste0(base_url, endpoint),
    add_headers(Authorization = paste("Bearer", token)),
    content_type_json(),
    accept_json(),
    body = toJSON(body, auto_unbox = TRUE)
  )
  if (http_error(response)) {
    stop(sprintf("POST [%s] %s: %s",
                 status_code(response), endpoint,
                 content(response, "text", encoding = "UTF-8")))
  }
  fromJSON(content(response, "text", encoding = "UTF-8"),
           simplifyVector = TRUE, flatten = TRUE)
}

# --- Helper: safe tag extraction ---------------------------------------------
extract_tag <- function(tag) {
  if (is.atomic(tag)) tag <- as.list(tag)
  tibble(
    tag_id        = tag[["id"]]            %||% NA_character_,
    tag_name      = tag[["tag_name"]]      %||% tag[["name"]] %||% NA_character_,
    tag_type_id   = tag[["tag_type_id"]]   %||% NA_character_,
    tag_type_name = tag[["tag_type_name"]] %||% NA_character_
  )
}


# #############################################################################
# PART A: ACTIVITIES + DEEP DETAILS
# #############################################################################

# ---- A1: Get list of all activity IDs ---------------------------------------
activities_list <- catapult_get("/activities")
activity_ids    <- map_chr(activities_list, "id")
n_acts          <- length(activity_ids)

cat(sprintf("Found %d total activities.\n", n_acts))

# ---- Filter by start_date ---------------------------------------------------
if (!is.null(start_date) && start_date != "") {
  start_epoch <- as.numeric(as.POSIXct(start_date, tz = "UTC"))
  activity_starts <- map_dbl(activities_list, "start_time")
  keep <- activity_starts >= start_epoch
  activities_list <- activities_list[keep]
  activity_ids    <- activity_ids[keep]
  n_acts          <- length(activity_ids)
  cat(sprintf("  -> After filtering (>= %s): %d activities.\n", start_date, n_acts))
}

cat("\n")

# ---- A2: Deep activity details  /activities/{id}?include=all ----------------
cat(sprintf("--- Fetching deep activity details (delay = %ds) ---\n", delay_sec))

details_list <- list()

for (i in seq_along(activity_ids)) {
  aid <- activity_ids[i]
  cat(sprintf("  [%d/%d] %s\n", i, n_acts, aid))

  details_list[[aid]] <- tryCatch({
    catapult_get(paste0("/activities/", aid, "?include=all"))[[1]]
  }, error = function(e) {
    warning(sprintf("Failed for %s: %s", aid, e$message))
    return(NULL)
  })

  if (i < n_acts) Sys.sleep(delay_sec)
}

details_list <- compact(details_list)
cat(sprintf("  -> Retrieved deep details for %d activities.\n\n", length(details_list)))

# ---- A3: Parse core activity info -------------------------------------------
activities_df <- map_dfr(details_list, function(act) {
  tibble(
    activity_id    = act$id,
    game_id        = act$game_id        %||% NA_character_,
    name           = act$name,
    start_time     = as.POSIXct(act$start_time, origin = "1970-01-01"),
    end_time       = as.POSIXct(act$end_time,   origin = "1970-01-01"),
    modified_at    = act$modified_at    %||% NA_character_,
    is_injected    = act$is_injected    %||% NA_integer_,
    owner_id       = act$owner_id       %||% act$owner$id %||% NA_character_,
    owner_name     = act$owner$full_name %||% act$owner$name %||% NA_character_,
    owner_email    = act$owner$email    %||% NA_character_,
    venue_name     = act$venue$name     %||% NA_character_,
    venue_lat      = act$venue$lat      %||% NA_real_,
    venue_lng      = act$venue$lng      %||% NA_real_,
    venue_width    = act$venue$width    %||% NA_integer_,
    venue_length   = act$venue$length   %||% NA_integer_,
    venue_rotation = act$venue$rotation %||% NA_integer_
  )
})

# ---- A4: Parse activity tags ------------------------------------------------
activity_tags_df <- map_dfr(details_list, function(act) {
  tags <- act$activity_tags %||% act$tags
  if (is.null(tags) || length(tags) == 0) return(NULL)
  map_dfr(tags, extract_tag) %>% mutate(activity_id = act$id, .before = 1)
})

# ---- A5: Parse periods from deep details ------------------------------------
deep_periods_df <- map_dfr(details_list, function(act) {
  if (is.null(act$periods) || length(act$periods) == 0) return(NULL)
  map_dfr(act$periods, function(p) {
    tibble(
      activity_id  = act$id,
      period_id    = p$id,
      period_name  = p$name,
      period_start = as.POSIXct(p$start_time, origin = "1970-01-01"),
      period_end   = as.POSIXct(p$end_time,   origin = "1970-01-01")
    )
  })
})

# ---- A6: Parse period tags --------------------------------------------------
period_tags_df <- map_dfr(details_list, function(act) {
  if (is.null(act$periods) || length(act$periods) == 0) return(NULL)
  map_dfr(act$periods, function(p) {
    tags <- p$period_tags %||% p$tags
    if (is.null(tags) || length(tags) == 0) return(NULL)
    map_dfr(tags, extract_tag) %>%
      mutate(activity_id = act$id, period_id = p$id, .before = 1)
  })
})

# ---- A7: Parse athletes from deep details -----------------------------------
athletes_df <- map_dfr(details_list, function(act) {
  if (is.null(act$athletes) || length(act$athletes) == 0) return(NULL)
  map_dfr(act$athletes, function(a) {
    tibble(
      activity_id = act$id,
      athlete_id  = a$id,
      athlete_name = a$name %||% NA_character_,
      activity_participation = paste(
        unlist(a$activity_participation$participation_tag_list), collapse = ", "
      )
    )
  })
})

# ---- A8: Parse athlete tags ------------------------------------------------
athlete_tags_df <- map_dfr(details_list, function(act) {
  if (is.null(act$athletes) || length(act$athletes) == 0) return(NULL)
  map_dfr(act$athletes, function(a) {
    tags <- a$athlete_tags
    if (is.null(tags) || length(tags) == 0) return(NULL)
    map_dfr(tags, extract_tag) %>%
      mutate(activity_id = act$id, athlete_id = a$id, .before = 1)
  })
})

# ---- A9: Parse athlete-period participation ---------------------------------
athlete_participation_df <- map_dfr(details_list, function(act) {
  if (is.null(act$athletes) || length(act$athletes) == 0) return(NULL)
  map_dfr(act$athletes, function(a) {
    if (is.null(a$participation) || length(a$participation) == 0) return(NULL)
    map_dfr(a$participation, function(part) {
      tibble(
        activity_id       = act$id,
        athlete_id        = a$id,
        athlete_name      = a$name %||% NA_character_,
        period_id         = part$period_id,
        participation_tag = paste(
          unlist(part$participation_tag_list), collapse = ", "
        )
      )
    })
  })
})

# ---- A10: Parse flagged regions ---------------------------------------------
flagged_regions_df <- map_dfr(details_list, function(act) {
  if (is.null(act$athletes) || length(act$athletes) == 0) return(NULL)
  map_dfr(act$athletes, function(a) {
    if (is.null(a$flagged_region) || length(a$flagged_region) == 0) return(NULL)
    map_dfr(a$flagged_region, function(fr) {
      fr_tags <- unlist(fr$flagged_region_tag_list)
      if (is.null(fr_tags) || length(fr_tags) == 0) return(NULL)
      tibble(
        activity_id  = act$id,
        athlete_id   = a$id,
        athlete_name = a$name %||% NA_character_,
        period_id    = fr$period_id,
        flagged_tag  = fr_tags
      )
    })
  })
})

# ---- A11: Parse teams -------------------------------------------------------
teams_df <- map_dfr(details_list, function(act) {
  if (is.null(act$teams) || length(act$teams) == 0) return(NULL)
  map_dfr(act$teams, function(t) {
    tibble(
      activity_id      = act$id,
      team_id          = t$id,
      team_name        = t$name         %||% NA_character_,
      team_slug        = t$slug         %||% NA_character_,
      sport_name       = t$sport_name   %||% NA_character_,
      primary_colour   = t$primary_colour  %||% NA_character_,
      secondary_colour = t$secondary_colour %||% NA_character_
    )
  })
})


# #############################################################################
# PART B: DETAILED PERIOD INFO
# #############################################################################

cat(sprintf("--- Fetching period details per activity (delay = %ds) ---\n", delay_sec))

period_details_list <- list()

for (i in seq_along(activity_ids)) {
  aid <- activity_ids[i]
  cat(sprintf("  [%d/%d] %s\n", i, n_acts, aid))

  period_details_list[[aid]] <- tryCatch(
    catapult_get(paste0("/activities/", aid, "/periods")),
    error = function(e) {
      warning(sprintf("Periods failed for %s: %s", aid, e$message))
      return(list())
    }
  )

  if (i < n_acts) Sys.sleep(delay_sec)
}

periods_detail_df <- map_dfr(names(period_details_list), function(aid) {
  periods <- period_details_list[[aid]]
  if (length(periods) == 0) return(NULL)
  map_dfr(periods, function(p) {
    tibble(
      activity_id     = aid,
      period_id       = p$id,
      period_name     = p$name,
      period_depth_id = p$period_depth_id    %||% NA_character_,
      period_start    = as.POSIXct(p$start_time, origin = "1970-01-01"),
      period_start_cs = p$start_centiseconds %||% NA_real_,
      period_end      = as.POSIXct(p$end_time, origin = "1970-01-01"),
      period_end_cs   = p$end_centiseconds   %||% NA_real_,
      lft             = p$lft                %||% NA_integer_,
      rgt             = p$rgt                %||% NA_integer_,
      is_synced       = p$is_synced          %||% NA_integer_,
      is_deleted      = p$is_deleted         %||% NA_integer_,
      is_injected     = p$is_injected        %||% NA_integer_,
      created_at      = p$created_at         %||% NA_character_,
      modified_at     = p$modified_at        %||% NA_character_
    )
  })
})


# #############################################################################
# PART C: PARAMETERS
# #############################################################################

cat("\n--- Fetching parameters ---\n")
Sys.sleep(delay_sec)

params_list <- catapult_get("/parameters")

parameters_df <- map_dfr(params_list, function(p) {
  tibble(
    parameter_id      = p$id,
    parameter_type_id = p$parameter_type_id %||% NA_character_,
    name              = p$name,
    original_name     = p$original_name     %||% NA_character_,
    base_name         = p$base_name         %||% NA_character_,
    slug              = p$slug              %||% NA_character_,
    band              = p$band              %||% NA_real_,
    aggregation       = p$aggregation       %||% NA_character_,
    group_by          = p$group_by          %||% NA_character_,
    unit_type         = p$unit_type         %||% NA_character_,
    calculation       = p$calculation       %||% NA_character_,
    ctr_order         = p$ctr_order         %||% NA_real_,
    created_at        = p$created_at        %||% NA_character_,
    modified_at       = p$modified_at       %||% NA_character_
  )
})

cat(sprintf("  -> Retrieved %d parameters.\n", nrow(parameters_df)))


# #############################################################################
# PART D: STATS  (POST /stats — performance metrics per athlete per period)
# #############################################################################

# ---- D1: Define parameter slugs to request ----------------------------------
#   Using Gen2 acceleration bands to match OpenField Cloud config.
#   Verify velocity band thresholds match your OpenField settings.
#   Note: max_vel returns m/s — multiply by 3.6 for km/h.

stat_slugs <- c(
  # ---- Distance ----
  "total_distance",                                # Distance (km from API → *1000 for m)
  "meterage_per_minute",                           # Distance/time (m/min)
  "total_duration",                                # Duration (s)

  # ---- High Speed Running ----
  "high_speed_distance",                           # Abs HSR (km → *1000 for m)
  "high_speed_distance_>75%_(total)",              # Rel HSR / Explosive Dist (km → *1000)
  "high_speed_distance_%",                         # Abs HSR (% of distance)
  "high_speed_efforts",                            # Abs HSR (count)
  "high_speed_distance_per_minute",                # HSR dist per min
  "high_speed_efforts_>75%_(total)",               # Rel HSR efforts (>75% max vel)

  # ---- Sprints ----
  "max_vel",                                       # Max Speed (already km/h, mult=1)
  "percentage_max_velocity",                       # % of max velocity
  "athlete_max_velocity",                          # Profile max velocity (km/h)
  "sprint_distance_>15mph",                        # Sprint Distance >25 km/h (km → *1000)
  "sprint_distance_>_30_km/hr",                    # Sprint Distance >30 km/h (km → *1000)
  "sprint_efforts",                                # Sprint Count >25 km/h
  "sprint_efforts_>_30km/hr",                      # Sprint Count >30 km/h
  "sprint_distance_per_minute",                    # Sprint dist per min

  # ---- Gen2 Acceleration Bands (matching OpenField config) ----
  #   Band 1: [-10, -4]  |  Band 2: [-4, -3]  |  Band 3: [-3, -2]
  #   Band 4: [-2, 0]    |  Band 5: [0, 2]    |  (4 & 5 = neutral, skip)
  #   Band 6: [2, 3]     |  Band 7: [3, 4]    |  Band 8: [4, 10]
  "gen2_acceleration_band8_total_effort_count",    # Acc  [4, 10]
  "gen2_acceleration_band7_total_effort_count",    # Acc  [3, 4]
  "gen2_acceleration_band6_total_effort_count",    # Acc  [2, 3]
  "gen2_acceleration_band3_total_effort_count",    # Dec  [-3, -2]
  "gen2_acceleration_band2_total_effort_count",    # Dec  [-4, -3]
  "gen2_acceleration_band1_total_effort_count",    # Dec  [-10, -4]

  # ---- Max Acc / Dec ----
  "max_effort_acceleration",                       # Max Acceleration (m/s²)
  "max_effort_deceleration",                       # Max Deceleration (m/s²)

  # ---- Combined Acc & Dec ----
  "gen2_acceleration_band6plus_total_effort_count", # Acc >2 m/s²
  "gen2_acceleration_band7plus_total_effort_count", # Acc >3 m/s²
  "gen2_acceleration_band3plus_total_effort_count", # Dec >2 m/s²
  "gen2_acceleration_band2plus_total_effort_count", # Dec >3 m/s²
  "accel&decel_efforts",                            # Acc&Dec total
  "accel&decel_efforts_per_minute",                 # Acc&Dec per min

  # ---- Load ----
  "total_player_load",                             # Player Load (a.u.)
  "player_load_per_minute",                        # PL per min
  "high_metabolic_load_distance",                  # HMLD (m)
  "hmld_gen2",                                     # HMLD Gen 2 (m)

  # ---- Explosive ----
  "explosive_yardage_(total)",                     # Explosive Distance (m)
  "explosive_efforts",                             # HIA / Explosive Efforts

  # ---- Heart Rate ----
  "max_heart_rate",                                # Max HR
  "mean_heart_rate",                               # Mean HR
  "percentage_max_heart_rate",                     # % Max HR
  "heart_rate_exertion",                           # HR Exertion

  # ---- Energy ----
  "energy",                                        # Energy
  "red_zone"                                       # Red Zone
)

# Remove duplicates
stat_slugs <- unique(stat_slugs)

# Validate slugs against parameters_df
valid_slugs   <- stat_slugs[stat_slugs %in% parameters_df$slug]
invalid_slugs <- stat_slugs[!stat_slugs %in% parameters_df$slug]

cat(sprintf("\n--- Slug validation ---\n"))
cat(sprintf("  Requested: %d  |  Valid: %d  |  Invalid: %d\n",
            length(stat_slugs), length(valid_slugs), length(invalid_slugs)))

if (length(invalid_slugs) > 0) {
  cat("  Invalid slugs (not in your account):\n")
  cat(paste("   -", invalid_slugs, collapse = "\n"), "\n")
}

# Only send valid slugs to the API
stat_slugs <- valid_slugs
cat(sprintf("  Sending %d slugs to /stats.\n\n", length(stat_slugs)))

# ---- D2: Pull stats for each activity (grouped by athlete + period) ---------
cat(sprintf("--- Fetching stats per activity (delay = %ds) ---\n", delay_sec))

stats_all <- list()

for (i in seq_along(activity_ids)) {
  aid <- activity_ids[i]
  cat(sprintf("  [%d/%d] %s\n", i, n_acts, aid))

  stats_all[[aid]] <- tryCatch({
    catapult_post("/stats", body = list(
      filters = list(
        list(
          name       = "activity_id",
          comparison = "=",
          values     = list(aid)
        )
      ),
      group_by   = list("athlete", "period"),
      parameters = as.list(stat_slugs),
      source     = "cached_stats"
    ))
  }, error = function(e) {
    warning(sprintf("Stats failed for %s: %s", aid, e$message))
    return(NULL)
  })

  if (i < n_acts) Sys.sleep(delay_sec)
}

# ---- D3: Combine all stats into one data frame -----------------------------
stats_df <- bind_rows(
  compact(stats_all),
  .id = "activity_id"
)

cat(sprintf("  -> Retrieved %d stat rows across all activities.\n", nrow(stats_df)))

# ---- D4: Clean up & join activity names -------------------------------------
if (nrow(stats_df) > 0) {
  stats_df <- stats_df %>%
    left_join(
      activities_df %>% select(activity_id, activity_name = name),
      by = "activity_id"
    ) %>%
    relocate(activity_id, activity_name, .before = 1)
}


# #############################################################################
# SUMMARY
# #############################################################################
cat("\n============================\n")
cat("        SUMMARY\n")
cat("============================\n")
cat(sprintf("Activities:             %d rows\n", nrow(activities_df)))
cat(sprintf("Activity Tags:          %d rows\n", nrow(activity_tags_df)))
cat(sprintf("Teams:                  %d rows\n", nrow(teams_df)))
cat(sprintf("Deep Periods:           %d rows\n", nrow(deep_periods_df)))
cat(sprintf("Period Tags:            %d rows\n", nrow(period_tags_df)))
cat(sprintf("Periods (detailed):     %d rows\n", nrow(periods_detail_df)))
cat(sprintf("Athletes:               %d rows\n", nrow(athletes_df)))
cat(sprintf("Athlete Tags:           %d rows\n", nrow(athlete_tags_df)))
cat(sprintf("Athlete Participation:  %d rows\n", nrow(athlete_participation_df)))
cat(sprintf("Flagged Regions:        %d rows\n", nrow(flagged_regions_df)))
cat(sprintf("Parameters:             %d rows\n", nrow(parameters_df)))
cat(sprintf("Stats:                  %d rows\n", nrow(stats_df)))
cat("============================\n")

names(stats_df)
