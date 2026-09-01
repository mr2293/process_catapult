# ==============================================================================
# fetch_catapult_efforts.R
# Fetch velocity & acceleration efforts for all athletes in a Catapult activity
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

# --- Helper: GET with error handling ------------------------------------------
catapult_get <- function(endpoint, query_params = list()) {
  url <- paste0(BASE_URL, endpoint)
  
  resp <- GET(url, headers, query = query_params)
  
  if (status_code(resp) != 200) {
    warning(
      sprintf("HTTP %s on %s — %s",
              status_code(resp), endpoint,
              content(resp, "text", encoding = "UTF-8"))
    )
    return(NULL)
  }
  
  content(resp, "parsed", simplifyVector = TRUE)
}

# --- 1. List activities -------------------------------------------------------
get_activities <- function() {
  catapult_get("/activities")
}

# --- 2. List periods for a given activity -------------------------------------
get_periods <- function(activity_id) {
  catapult_get(paste0("/activities/", activity_id, "/periods"))
}

# --- 3. List athletes in an activity -----------------------------------------
#     Uses the global athlete `id`, which is what the efforts endpoint expects.
get_athletes_in_activity <- function(activity_id) {
  data <- catapult_get(paste0("/activities/", activity_id, "/athletes"))
  
  if (is.null(data)) return(tibble())
  
  tibble(
    athlete_id         = data$id,
    athlete_first_name = data$first_name,
    athlete_last_name  = data$last_name
  )
}

# --- 4. Fetch efforts for ONE athlete in a period -----------------------------
#     Also works at the activity level — just swap the endpoint if needed.
get_efforts_for_athlete <- function(period_id, athlete_id,
                                    effort_types = "acceleration,velocity") {
  endpoint <- sprintf("/periods/%s/athletes/%s/efforts", period_id, athlete_id)
  
  catapult_get(endpoint, query_params = list(effort_types = effort_types))
}

# --- 5. Parse the efforts response into tidy tibbles --------------------------
parse_efforts <- function(raw) {
  if (is.null(raw) || length(raw) == 0) {
    return(list(velocity = tibble(), acceleration = tibble()))
  }
  
  rec <- if (is.data.frame(raw)) raw[1, ] else raw[[1]]
  
  meta <- tibble(
    athlete_id         = rec$athlete_id,
    athlete_first_name = rec$athlete_first_name,
    athlete_last_name  = rec$athlete_last_name,
    device_id          = rec$device_id,
    team_name          = rec$team_name
  )
  
  # --- Velocity efforts ---
  vel_raw <- rec$data$velocity_efforts
  vel_df <- tryCatch({
    if (!is.null(vel_raw) && length(vel_raw) > 0) {
      df <- if (is.data.frame(vel_raw)) vel_raw else bind_rows(vel_raw)
      df |> mutate(effort_type = "velocity") |> bind_cols(meta)
    } else {
      tibble()
    }
  }, error = function(e) { message("  Velocity parse error: ", e$message); tibble() })
  
  # --- Acceleration efforts ---
  acc_raw <- rec$data$acceleration_efforts
  acc_df <- tryCatch({
    if (!is.null(acc_raw) && length(acc_raw) > 0) {
      df <- if (is.data.frame(acc_raw)) acc_raw else bind_rows(acc_raw)
      df |> mutate(effort_type = "acceleration") |> bind_cols(meta)
    } else {
      tibble()
    }
  }, error = function(e) { message("  Accel parse error: ", e$message); tibble() })
  
  list(velocity = vel_df, acceleration = acc_df)
}

# ==============================================================================
# MAIN
# ==============================================================================

# Step A: Browse activities to find the one you want ---------------------------
activities <- get_activities()
# View(activities)

# Step B: Pick an activity and get its periods ---------------------------------
ACTIVITY_ID <- "c1f449cb-e7eb-4a9c-b949-f036b57a8f49"
periods <- get_periods(ACTIVITY_ID)
# View(periods)

# Step C: Pick the periods you want (vector of IDs) ---------------------------
#     You can use one or many. Inspect `periods` to grab the correct IDs.
PERIOD_IDS <- c(
  "8ce0c312-e5ed-4b21-8eb1-455d54f2e9c8",   # Primer Tiempo
  "a64f2c25-2428-4866-bb92-07e6e3738c85"    # Segundo Tiempo
)

# Step D: Discover which athletes participated ---------------------------------
athletes <- get_athletes_in_activity(ACTIVITY_ID)
cat(sprintf("Found %d athletes\n", nrow(athletes)))

# Step E: Loop over each period × athlete and fetch efforts --------------------
all_velocity     <- list()
all_acceleration <- list()
idx <- 1

for (pid in PERIOD_IDS) {
  cat(sprintf("\n--- Period: %s ---\n", pid))
  
  for (i in seq_len(nrow(athletes))) {
    aid  <- athletes$athlete_id[i]
    name <- paste(athletes$athlete_first_name[i], athletes$athlete_last_name[i])
    cat(sprintf("[%d/%d] Fetching efforts for %s ...\n", i, nrow(athletes), name))
    
    raw    <- get_efforts_for_athlete(pid, aid)
    parsed <- parse_efforts(raw)
    
    # Tag each row with the period it came from
    if (nrow(parsed$velocity) > 0)
      parsed$velocity <- mutate(parsed$velocity, period_id = pid)
    if (nrow(parsed$acceleration) > 0)
      parsed$acceleration <- mutate(parsed$acceleration, period_id = pid)
    
    all_velocity[[idx]]     <- parsed$velocity
    all_acceleration[[idx]] <- parsed$acceleration
    idx <- idx + 1
    
    Sys.sleep(0.3)
  }
}

# Step F: Bind into master data frames, sorted chronologically -----------------
velocity_efforts <- bind_rows(all_velocity)
if (nrow(velocity_efforts) > 0) velocity_efforts <- arrange(velocity_efforts, start_time)

acceleration_efforts <- bind_rows(all_acceleration)
if (nrow(acceleration_efforts) > 0) acceleration_efforts <- arrange(acceleration_efforts, start_time)

cat(sprintf(
  "\nDone — %d velocity efforts, %d acceleration efforts\n",
  nrow(velocity_efforts), nrow(acceleration_efforts)
))

# Optional: combine both into one long-format table ---------------------------
#   Columns that don't overlap (max_velocity vs acceleration) become NA
all_efforts <- bind_rows(velocity_efforts, acceleration_efforts) |>
  arrange(start_time)

# Quick peek at the structure
glimpse(velocity_efforts)
glimpse(acceleration_efforts)

# Per-player effort counts
velocity_efforts |>
  count(athlete_first_name, athlete_last_name, period_id, sort = TRUE)

# High-speed efforts only (e.g., band 5+)
velocity_efforts |>
  filter(as.integer(band) >= 5) |>
  count(athlete_last_name, sort = TRUE)

# Export to CSV ----------------------------------------------------------------
output_dir <- "/Users/mateorodriguez/Desktop/analisis_CA/fichas_post_partido/data"
output_path <- file.path(output_dir, paste0("efforts_", ACTIVITY_ID, ".csv"))

write.csv(all_efforts, output_path, row.names = FALSE)
cat(sprintf("Exported %d rows to %s\n", nrow(all_efforts), output_path))