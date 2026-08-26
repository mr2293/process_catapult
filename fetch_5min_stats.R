# ==============================================================================
# fetch_catapult_5min_stats.R
# Produce 5-minute velocity-band distances whose totals match POST /stats
#
# Strategy:
#   1. POST /stats → ground-truth HSR / Sprint totals per athlete per period
#   2. GET efforts → timestamped individual velocity efforts
#   3. Bin efforts into 5-minute segments within each half
#   4. Scale each athlete's segment distances so they sum to the stats totals
#
# Catapult Connect API v6
# ==============================================================================

library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)

# --- Config -------------------------------------------------------------------
BASE_URL <- "https://connect-us.catapultsports.com/api/v6"
TOKEN    <- Sys.getenv("CATAPULT_TOKEN")

headers <- add_headers(
  Authorization = paste("Bearer", TOKEN),
  Accept        = "application/json"
)

ACTIVITY_ID <- "c1bbbc84-b00a-4519-a69e-cbbf03c16769"
SEGMENT_SECONDS <- 300   # 5 minutes

# Half period IDs
PERIOD_IDS <- c(
  "6296e089-35fa-4974-bc90-439c7b3ce937",   # Primer Tiempo
  "af2bf54e-332d-4cba-9fba-35c14775fd28"    # Segundo Tiempo
)

# Velocity band mapping (matching your Catapult config)
#   Band 4 = 21–25 km/h (HSR)
#   Band 5 = 25–30 km/h (VHSR)
#   Band 6 = 30–40 km/h (Sprint)
HSR_BANDS    <- c(4, 5, 6)   # 21+ km/h
SPRINT_BANDS <- c(6)         # 30+ km/h

# --- Helpers ------------------------------------------------------------------
catapult_get <- function(endpoint, query_params = list()) {
  url  <- paste0(BASE_URL, endpoint)
  resp <- GET(url, headers, query = query_params)
  if (status_code(resp) != 200) {
    warning(sprintf("GET %s -> HTTP %s: %s",
                    endpoint, status_code(resp),
                    content(resp, "text", encoding = "UTF-8")))
    return(NULL)
  }
  content(resp, "parsed", simplifyVector = TRUE)
}

catapult_post <- function(endpoint, body_list) {
  url  <- paste0(BASE_URL, endpoint)
  resp <- POST(url, headers,
               body = toJSON(body_list, auto_unbox = TRUE),
               content_type_json())
  if (!(status_code(resp) %in% c(200, 201))) {
    warning(sprintf("POST %s -> HTTP %s: %s",
                    endpoint, status_code(resp),
                    content(resp, "text", encoding = "UTF-8")))
    return(NULL)
  }
  content(resp, "parsed", simplifyVector = TRUE, flatten = TRUE)
}

# ==============================================================================
# STEP 1 — Fetch athletes and period metadata
# ==============================================================================
cat("=== Step 1: Fetching athletes and periods ===\n")

athletes_raw <- catapult_get(paste0("/activities/", ACTIVITY_ID, "/athletes"))
athletes_df  <- tibble(
  athlete_id   = athletes_raw$id,
  first_name   = athletes_raw$first_name,
  last_name    = athletes_raw$last_name,
  display_name = paste(athletes_raw$first_name, athletes_raw$last_name)
)

periods_raw <- catapult_get(paste0("/activities/", ACTIVITY_ID, "/periods"))
half_periods <- periods_raw |>
  filter(id %in% PERIOD_IDS) |>
  arrange(start_time) |>
  mutate(
    half_label       = c("1H", "2H"),
    start_epoch      = as.numeric(start_time),
    end_epoch        = as.numeric(end_time),
    duration_sec     = end_epoch - start_epoch,
    match_min_offset = c(0, 45)
  )

cat(sprintf("Found %d athletes, %d half periods\n",
            nrow(athletes_df), nrow(half_periods)))

# ==============================================================================
# STEP 2 — Pull ground-truth stats via POST /stats
# ==============================================================================
cat("\n=== Step 2: Fetching stats via POST /stats ===\n")

# Slugs for the metrics we need — these come from your parameters list
STAT_SLUGS <- c(
  "total_distance",
  "high_speed_distance",
  "sprint_distance_>_30_km/hr"
)

# You can also add velocity-band-specific slugs if they exist in your account.
# Run this to find them:
#   parameters_df %>% filter(grepl("velocity_band|vel_band", slug, ignore.case = TRUE))

stats_raw <- catapult_post("/stats", body_list = list(
  filters = list(
    list(
      name       = "activity_id",
      comparison = "=",
      values     = list(ACTIVITY_ID)
    )
  ),
  group_by   = list("athlete", "period"),
  parameters = as.list(STAT_SLUGS),
  source     = "cached_stats"
))

if (is.null(stats_raw) || (is.data.frame(stats_raw) && nrow(stats_raw) == 0)) {
  stop("POST /stats returned no data. Check your slugs and activity ID.")
}

cat(sprintf("  Stats response: %d rows, %d columns\n",
            nrow(stats_raw), ncol(stats_raw)))
cat("  Columns: ", paste(names(stats_raw), collapse = ", "), "\n")

# --- Parse the stats response ------------------------------------------------
# The POST /stats response is a data.frame with columns like:
#   athlete_id, period_id, total_distance, high_speed_distance, etc.
# Distance values may be in km — multiply by 1000 for meters.

# Identify the column names (they may use slug or cleaned names)
find_col <- function(df, patterns) {
  for (p in patterns) {
    matches <- grep(p, names(df), ignore.case = TRUE, value = TRUE)
    if (length(matches) > 0) return(matches[1])
  }
  NA_character_
}

col_athlete  <- find_col(stats_raw, c("athlete_id", "athlete\\.id"))
col_period   <- find_col(stats_raw, c("period_id", "period\\.id"))
col_name     <- find_col(stats_raw, c("athlete_name", "athlete\\.name", "name"))
col_totdist  <- find_col(stats_raw, c("total_distance"))
col_hsr      <- find_col(stats_raw, c("high_speed_distance"))
col_sprint   <- find_col(stats_raw, c("sprint_distance.*30", "sprint_distance"))

cat(sprintf("\n  Column mapping:\n"))
cat(sprintf("    athlete_id: %s\n", col_athlete))
cat(sprintf("    period_id:  %s\n", col_period))
cat(sprintf("    name:       %s\n", col_name))
cat(sprintf("    total_dist: %s\n", col_totdist))
cat(sprintf("    hsr:        %s\n", col_hsr))
cat(sprintf("    sprint:     %s\n", col_sprint))

# Build a clean stats table
stats_clean <- stats_raw |>
  transmute(
    athlete_id = .data[[col_athlete]],
    period_id  = .data[[col_period]],
    athlete_name = if (!is.na(col_name)) .data[[col_name]] else NA_character_,
    total_dist_km = as.numeric(.data[[col_totdist]]),
    hsr_dist_km   = as.numeric(.data[[col_hsr]]),
    sprint_dist_km = as.numeric(.data[[col_sprint]])
  ) |>
  # Filter to only the two halves we care about
  filter(period_id %in% PERIOD_IDS) |>
  mutate(
    # Convert km → m (Catapult /stats typically returns km)
    total_dist_m  = total_dist_km * 1000,
    hsr_dist_m    = hsr_dist_km   * 1000,
    sprint_dist_m = sprint_dist_km * 1000
  )

# Aggregate across both halves for full-game totals
stats_full_game <- stats_clean |>
  group_by(athlete_id) |>
  summarise(
    total_dist_m  = sum(total_dist_m,  na.rm = TRUE),
    hsr_dist_m    = sum(hsr_dist_m,    na.rm = TRUE),
    sprint_dist_m = sum(sprint_dist_m, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(select(athletes_df, athlete_id, display_name), by = "athlete_id")

cat("\n  Full-game stats (meters):\n")
print(
  stats_full_game |>
    select(display_name, total_dist_m, hsr_dist_m, sprint_dist_m) |>
    arrange(desc(total_dist_m)),
  n = 25
)

# ==============================================================================
# STEP 3 — Pull velocity EFFORTS (for temporal distribution)
# ==============================================================================
cat("\n=== Step 3: Fetching velocity efforts ===\n")

all_efforts <- list()
idx <- 1

for (pid in PERIOD_IDS) {
  half_info <- half_periods |> filter(id == pid)
  cat(sprintf("\n  --- %s ---\n", half_info$name))
  
  for (a in seq_len(nrow(athletes_df))) {
    aid   <- athletes_df$athlete_id[a]
    aname <- athletes_df$display_name[a]
    cat(sprintf("    [%d/%d] %s ... ", a, nrow(athletes_df), aname))
    
    raw <- catapult_get(
      sprintf("/periods/%s/athletes/%s/efforts", pid, aid),
      query_params = list(effort_types = "velocity")
    )
    
    if (!is.null(raw) && length(raw) > 0) {
      rec <- if (is.data.frame(raw)) raw[1, ] else raw[[1]]
      vel_raw <- rec$data$velocity_efforts
      
      efforts_df <- tryCatch({
        if (!is.null(vel_raw) && length(vel_raw) > 0) {
          df <- if (is.data.frame(vel_raw)) vel_raw else bind_rows(vel_raw)
          df |>
            mutate(
              athlete_id   = aid,
              athlete_name = aname,
              period_id    = pid,
              half_label   = half_info$half_label,
              half_start   = half_info$start_epoch,
              half_offset  = half_info$match_min_offset,
              band         = as.integer(band)
            )
        } else {
          tibble()
        }
      }, error = function(e) { message("parse error: ", e$message); tibble() })
      
      if (nrow(efforts_df) > 0) {
        all_efforts[[idx]] <- efforts_df
        idx <- idx + 1
      }
      cat(sprintf("%d efforts\n", nrow(efforts_df)))
    } else {
      cat("0 efforts\n")
    }
    Sys.sleep(0.3)
  }
}

efforts <- bind_rows(all_efforts)
cat(sprintf("\nTotal velocity efforts: %d\n", nrow(efforts)))

# --- Quick check: what columns does the efforts response have? ----------------
cat("\nEfforts columns: ", paste(names(efforts), collapse = ", "), "\n")

# ==============================================================================
# STEP 4 — Bin efforts into 5-minute segments
# ==============================================================================
cat("\n=== Step 4: Binning efforts into 5-min segments ===\n")

# Identify the distance column — might be "distance", "total_distance", etc.
dist_col <- intersect(names(efforts), c("distance", "total_distance", "effort_distance"))[1]
if (is.na(dist_col)) {
  cat("  WARNING: No distance column found in efforts. Available columns:\n")
  cat("  ", paste(names(efforts), collapse = ", "), "\n")
  stop("Cannot proceed without a distance column in efforts data.")
}
cat(sprintf("  Using distance column: '%s'\n", dist_col))

efforts_binned <- efforts |>
  rename(effort_dist = all_of(dist_col)) |>
  mutate(
    secs_into_half = as.numeric(start_time) - half_start,
    bin_index      = floor(secs_into_half / SEGMENT_SECONDS),
    min_from       = half_offset + bin_index * (SEGMENT_SECONDS / 60),
    min_to         = min_from + (SEGMENT_SECONDS / 60),
    is_hsr         = band %in% HSR_BANDS,
    is_sprint      = band %in% SPRINT_BANDS
  )

# Aggregate per athlete × segment
segments_raw <- efforts_binned |>
  group_by(athlete_id, athlete_name, half_label, period_id, min_from, min_to) |>
  summarise(
    effort_hsr_dist    = sum(effort_dist[is_hsr],    na.rm = TRUE),
    effort_sprint_dist = sum(effort_dist[is_sprint], na.rm = TRUE),
    effort_total_dist  = sum(effort_dist,            na.rm = TRUE),
    n_efforts          = n(),
    .groups = "drop"
  ) |>
  arrange(athlete_name, min_from)

cat(sprintf("  %d segment rows\n", nrow(segments_raw)))

# ==============================================================================
# STEP 5 — Scale segment distances to match POST /stats totals
# ==============================================================================
cat("\n=== Step 5: Scaling segments to match stats ===\n")

# Effort-level totals per athlete
effort_totals <- efforts_binned |>
  group_by(athlete_id) |>
  summarise(
    effort_hsr_sum    = sum(effort_dist[is_hsr],    na.rm = TRUE),
    effort_sprint_sum = sum(effort_dist[is_sprint], na.rm = TRUE),
    .groups = "drop"
  )

# Compute scaling factors
scaling <- stats_full_game |>
  select(athlete_id, hsr_dist_m, sprint_dist_m) |>
  left_join(effort_totals, by = "athlete_id") |>
  mutate(
    # Efforts distance might be in m already, or km — check the magnitude
    # If effort sums are ~0.5 and stats are ~500, efforts are in km
    scale_hsr    = if_else(effort_hsr_sum > 0,    hsr_dist_m / effort_hsr_sum,       1),
    scale_sprint = if_else(effort_sprint_sum > 0, sprint_dist_m / effort_sprint_sum, 1)
  )

cat("\nScaling factors (first 10 athletes):\n")
print(
  scaling |>
    left_join(select(athletes_df, athlete_id, display_name), by = "athlete_id") |>
    select(display_name, hsr_dist_m, effort_hsr_sum, scale_hsr,
           sprint_dist_m, effort_sprint_sum, scale_sprint) |>
    head(10),
  width = 120
)

# If scale factors are ~1000, efforts are in km and stats in m — auto-detect
median_scale <- median(scaling$scale_hsr, na.rm = TRUE)
if (median_scale > 500) {
  cat(sprintf(
    "\n  Auto-detected: efforts appear to be in km (median scale = %.0f)\n",
    median_scale
  ))
  cat("  Converting efforts to meters before scaling...\n")
  effort_totals <- effort_totals |>
    mutate(across(starts_with("effort_"), ~ .x * 1000))
  segments_raw <- segments_raw |>
    mutate(across(starts_with("effort_"), ~ .x * 1000))
  # Recompute scaling
  scaling <- stats_full_game |>
    select(athlete_id, hsr_dist_m, sprint_dist_m) |>
    left_join(effort_totals, by = "athlete_id") |>
    mutate(
      scale_hsr    = if_else(effort_hsr_sum > 0,    hsr_dist_m / effort_hsr_sum,       1),
      scale_sprint = if_else(effort_sprint_sum > 0, sprint_dist_m / effort_sprint_sum, 1)
    )
}

# Apply scaling
segments_scaled <- segments_raw |>
  left_join(select(scaling, athlete_id, scale_hsr, scale_sprint), by = "athlete_id") |>
  mutate(
    hsr_dist      = effort_hsr_dist    * scale_hsr,      # meters, scaled to match stats
    sprint_dist   = effort_sprint_dist * scale_sprint,    # meters, scaled to match stats
    hsr_only_dist = hsr_dist - sprint_dist,               # HSR excluding sprint (for stacking)
    match_minute  = (min_from + min_to) / 2               # x-axis position
  ) |>
  select(-scale_hsr, -scale_sprint)

# ==============================================================================
# STEP 6 — Validation
# ==============================================================================
cat("\n=== Step 6: Validation ===\n")

validation <- segments_scaled |>
  group_by(athlete_id, athlete_name) |>
  summarise(
    seg_hsr_sum    = round(sum(hsr_dist,    na.rm = TRUE), 1),
    seg_sprint_sum = round(sum(sprint_dist, na.rm = TRUE), 1),
    .groups = "drop"
  ) |>
  left_join(
    select(stats_full_game, athlete_id, hsr_dist_m, sprint_dist_m),
    by = "athlete_id"
  ) |>
  mutate(
    hsr_diff    = abs(seg_hsr_sum    - hsr_dist_m),
    sprint_diff = abs(seg_sprint_sum - sprint_dist_m),
    hsr_ok      = hsr_diff < 1,
    sprint_ok   = sprint_diff < 1
  )

cat("\nValidation (meters):\n")
print(
  validation |> select(athlete_name, seg_hsr_sum, hsr_dist_m, hsr_ok,
                       seg_sprint_sum, sprint_dist_m, sprint_ok),
  n = 25
)

mismatches <- validation |> filter(!hsr_ok | !sprint_ok)
if (nrow(mismatches) == 0) {
  cat("\n  ALL athletes match within 1m tolerance.\n")
} else {
  cat(sprintf("\n  WARNING: %d athletes have mismatches > 1m.\n", nrow(mismatches)))
}

# ==============================================================================
# STEP 7 — Export
# ==============================================================================
output_dir  <- "/Users/mateorodriguez/Desktop/analisis_CA/fichas_post_partido/data"

out_segments <- file.path(output_dir, paste0("segments_5min_", ACTIVITY_ID, ".csv"))
write.csv(segments_scaled, out_segments, row.names = FALSE)
cat(sprintf("\nExported %d segment rows to %s\n", nrow(segments_scaled), out_segments))

out_stats <- file.path(output_dir, paste0("stats_fullgame_", ACTIVITY_ID, ".csv"))
write.csv(stats_full_game, out_stats, row.names = FALSE)
cat(sprintf("Exported %d athlete stats to %s\n", nrow(stats_full_game), out_stats))

# ==============================================================================
# COLUMN REFERENCE FOR YOUR VISUALIZATION
# ==============================================================================
# segments_scaled columns:
#   athlete_id, athlete_name, half_label, period_id
#   min_from, min_to, match_minute    ← x-axis (e.g. 0–5, 5–10, ...)
#   hsr_only_dist   ← orange bar (21–30 km/h, HSR minus sprint)
#   sprint_dist     ← yellow bar (30+ km/h)
#   hsr_dist        ← total HSR (21+ km/h) = hsr_only + sprint
#   effort_total_dist, n_efforts     ← raw effort counts
#
# Sum of hsr_dist across all segments == POST /stats high_speed_distance * 1000
# Sum of sprint_dist across segments == POST /stats sprint_distance_>_30_km/hr * 1000