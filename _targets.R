# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c(
    "dplyr",
    "forcats",
    "ggbeeswarm",
    "ggplot2",
    "ggrain",
    "ggrepel",
    "Hmisc",
    "patchwork",
    "purrr",
    "questionr",
    "ragg",
    "reactable",
    "readr",
    "rlang",
    "rogme",
    "skimr",
    "tidyr",
    "tidyselect"
  )
)

# Load functions
tar_source()

# Load data
data("INCLUSION")
data("VISIT_6M")
data("VISIT_12M")
data("IPAQ")
data("EMAPS")
data("BARRIERS")

# Define pipeline
list(
  tar_target(
    name = INCLUSION_cleaned,
    command =  INCLUSION |>
      dplyr::mutate(dplyr::across(
        c(patient, sex, angioplasty, bypass), as.factor
      ),
      BMI = weight / ((height / 100) ^ 2))
  ),
  tar_target(
    name = VISIT_6M_cleaned,
    command =   VISIT_6M |>
      dplyr::mutate(patient = as.factor(patient))
  ),
  tar_target(
    name = VISIT_12M_cleaned,
    command =   VISIT_12M |>
      dplyr::mutate(patient = as.factor(patient))
  ),
  tar_target(
    name = DB_6MWT,
    command = INCLUSION_cleaned |>
      dplyr::left_join(VISIT_6M_cleaned, by = "patient") |>
      dplyr::left_join(VISIT_12M_cleaned, by = "patient") |>
      dplyr::select(patient, DIST_6WT_M0, DIST_6WT_M6, DIST_6WT_M12) |>
      dplyr::rename(
        MONTH_0 = DIST_6WT_M0,
        MONTH_6 = DIST_6WT_M6,
        MONTH_12 = DIST_6WT_M12
      ) |>
      tidyr::pivot_longer(
        cols = c(MONTH_0, MONTH_6, MONTH_12),
        names_to = "MONTH",
        values_to = "DIST_M"
      ) |>
      dplyr::mutate(
        MONTH = forcats::fct_relevel(MONTH, "MONTH_0", "MONTH_6", "MONTH_12"),
        MONTH = forcats::fct_recode(
          MONTH,
          "0" = "MONTH_0",
          "6" = "MONTH_6",
          "12" = "MONTH_12"
        )
      )
  ),
  tar_target(
    name = DB_IPAQ,
    command = IPAQ |>
      dplyr::mutate(
        patient = as.factor(patient),
        total_hours_heavy = ifelse(
          is.na(total_hours_heavy) |
            total_hours_heavy == 999,
          0,
          total_hours_heavy
        ),
        total_minutes_heavy = ifelse(
          is.na(total_minutes_heavy) |
            total_minutes_heavy == 999,
          0,
          total_minutes_heavy
        ),
        total_hours_moderate = ifelse(
          is.na(total_hours_moderate) |
            total_hours_moderate == 999,
          0,
          total_hours_moderate
        ),
        total_minutes_moderate = ifelse(
          is.na(total_minutes_moderate) |
            total_minutes_moderate == 999,
          0,
          total_minutes_moderate
        ),
        bouts_walk_7days = ifelse(
          is.na(bouts_walk_7days) |
            bouts_walk_7days == 9999,
          0,
          bouts_walk_7days
        ),

        MINUTES_VPA_WK = total_hours_heavy * 60 + total_minutes_heavy,
        MINUTES_MPA_WK = total_hours_moderate * 60 + total_minutes_moderate,
        MINUTES_WALK_WK = bouts_walk_7days * 10,
        MINUTES_TOT_WK = MINUTES_VPA_WK + MINUTES_MPA_WK + MINUTES_WALK_WK,
        MET_MIN_WK = MINUTES_VPA_WK * 8 + MINUTES_MPA_WK * 4 + MINUTES_WALK_WK * 3.3,

        MONTH = forcats::fct_recode(
          as.factor(num_visit),
          "0" = "1",
          "6" = "2",
          "12" = "3"
        )
      )
  ),
  tar_target(
    name = DB_EMAPS,
    command =  EMAPS |>
      dplyr::rename(MONTH = num_visit) |>
      dplyr::mutate(
        patient = as.factor(patient),
        MONTH = forcats::fct_recode(
          as.factor(MONTH),
          "0" = "1",
          "6" = "2",
          "12" = "3"
        )
      ) |>
      dplyr::group_by(MONTH) |>
      dplyr::mutate(
        "Intrinsic motivation"    = (AP_q1  %+% AP_q6  %+%  AP_q11) / 3,
        "Integrated regulation"   = (AP_q7  %+% AP_q10 %+%  AP_q13) / 3,
        "Identified regulation"   = (AP_q4  %+% AP_q12 %+%  AP_q16) / 3,
        "Introjected regulation"  = (AP_q3  %+% AP_14  %+%  AP_q18) / 3,
        "External regulation"     = (AP_q9  %+% AP_q15 %+%  AP_q17) / 3,
        "Amotivation"             = (AP_q2  %+% AP_q5  %+%  AP_q8)  / 3
      ) |>
      dplyr::select(patient, MONTH, "Intrinsic motivation":"Amotivation")
  ),
  tar_target(name = BARRIERS_cleaned, command = BARRIERS |> dplyr::mutate(dplyr::across( patient:autres, as.factor))),
  tar_target(name = analyse_INNCLUSION, command = INCLUSION_cleaned |> skimr::skim()),
  tar_target(name = analyse_sex, command = questionr::freq(INCLUSION_cleaned$sex, digits = 2)),
  tar_target(name = analyse_age, command = analyse_distribution(data = INCLUSION_cleaned, var = "age")),
  tar_target(name = analyse_height, command = analyse_distribution(data = INCLUSION_cleaned, var = "height")),
  tar_target(name = analyse_weight, command = analyse_distribution(data = INCLUSION_cleaned, var = "weight")),
  tar_target(name = analyse_bmi, command = analyse_distribution(data = INCLUSION_cleaned, var = "BMI")),
  tar_target(name = analyse_angioplasty, command = questionr::freq(INCLUSION_cleaned$angioplasty, digits = 2)),
  tar_target(name = analyse_bypass, command = questionr::freq(INCLUSION_cleaned$bypass, digits = 2)),
  tar_target(
    name = DB_6MWT_0_12,
    command =  DB_6MWT |> dplyr::filter(MONTH != "6") |>  dplyr::mutate(MONTH = factor(MONTH, levels = c("0", "12"))) |>
      tidyr::drop_na() |>
      dplyr::group_by(patient) |>
      tidyr::nest() |>
      dplyr::mutate(n_visits = purrr::map_dbl(data, ~ nrow(.x))) |>
      dplyr::filter(n_visits == 2) |>
      dplyr::ungroup() |>
      tidyr::unnest(data)
  )
)


