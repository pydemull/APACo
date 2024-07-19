
# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(packages = c("APACo", "dplyr", "ggplot2", "Hmisc", "tidyr", "patchwork"))

# Define pipeline
list(
  # Get data
  tar_target(INCLUSION, read_data("INCLUSION", "APACo")),
  tar_target(VISIT_6M, read_data("VISIT_6M", "APACo")),
  tar_target(VISIT_12M, read_data("VISIT_12M", "APACo")),
  tar_target(IPAQ, read_data("IPAQ", "APACo")),
  tar_target(EMAPS, read_data("EMAPS", "APACo")),
  tar_target(BARRIERS, read_data("BARRIERS", "APACo")),

  # Prepare general datasets
  tar_target(INCLUSION_cleaned, INCLUSION |> dplyr::mutate(dplyr::across(c(patient, sex, angioplasty, bypass), as.factor), BMI = weight / ((height / 100) ^ 2))),
  tar_target(VISIT_6M_cleaned, VISIT_6M |> dplyr::mutate(patient = as.factor(patient))),
  tar_target(VISIT_12M_cleaned, VISIT_12M |> dplyr::mutate(patient = as.factor(patient))),
  tar_target(DB_6MWT, get_DB_6MWT(INCLUSION_cleaned, VISIT_6M_cleaned, VISIT_12M_cleaned)),
  tar_target(DB_IPAQ, get_DB_IPAQ(IPAQ)),
  tar_target(DB_EMAPS, get_DB_EMAPS(EMAPS)),
  tar_target(BARRIERS_cleaned,
         {
             # Replace erroneous value 9 by 0 for patient 32
             BARRIERS[BARRIERS$patient == "32", "meteo_defavorable"] <- 0

             # Convert questionnaire variables to factors
             BARRIERS <- BARRIERS |> dplyr::mutate(dplyr::across(patient:autres, as.factor))
         }
  )
  ,

  # Perform descriptive analysis of the participants
  tar_target(analysis_INCLUSION, INCLUSION_cleaned |> skimr::skim()),
  tar_target(analysis_sex, questionr::freq(INCLUSION_cleaned$sex, digits = 2)),
  tar_target(analysis_age, analyse_distribution(data = INCLUSION_cleaned, var = "age")),
  tar_target(analysis_height, analyse_distribution(data = INCLUSION_cleaned, var = "height")),
  tar_target(analysis_weight, analyse_distribution(data = INCLUSION_cleaned, var = "weight")),
  tar_target(analysis_bmi, analyse_distribution(data = INCLUSION_cleaned, var = "BMI")),
  tar_target(analysis_angioplasty, questionr::freq(INCLUSION_cleaned$angioplasty, digits = 2)),
  tar_target(analysis_bypass, questionr::freq(INCLUSION_cleaned$bypass, digits = 2)),

  # Analyse change in 6MWT distance between 0 and 12 months
  tar_target(DB_6MWT_0_12, get_DB_6MWT_0_12(DB_6MWT)),
  tar_target(t_test_results_6MWT, t_test_6MWT_0_12(DB_6MWT_0_12)),
  tar_target(change_6MWT, analyse_change_6MWT(DB_6MWT_0_12)),

  # Analyse change in IPAQ MET-min/week between 60 and 12 months
  tar_target(DB_IPAQ_0_12, get_DB_IPAQ_0_12(DB_IPAQ)),
  tar_target(change_IPAQ_0_12, analyse_change_IPAQ(DB_IPAQ_0_12)),

  # Analyse change in IPAQ MET-min/week between 6 and 12 months
  tar_target(DB_IPAQ_6_12, get_DB_IPAQ_6_12(DB_IPAQ)),
  tar_target(change_IPAQ_6_12, analyse_change_IPAQ(DB_IPAQ_6_12)),

  # Analyse change in EMAPS scores between 0 and 12 months
  tar_target(DB_EMAPS_0_12, get_DB_EMAPS_0_12(DB_EMAPS)),
  tar_target(change_EMAPS, analyse_change_EMAPS(DB_EMAPS_0_12)),

  # Analyse barriers to physical activities at 12 months
  tar_target(analysis_BARRIERS, BARRIERS_cleaned |> skimr::skim()),
  tar_target(p_BARRIERS, get_plot_BARRIERS(BARRIERS_cleaned)),
  tar_target(p_BARRIERS_BY_6MWT_DECILE, {

    # Get a data frame with the changes in 6MWT distance
    DB_6MWT_0_12_wide <-
      DB_6MWT_0_12 |>
      pivot_wider(names_from = MONTH, values_from = DIST_M) |>
      mutate(change_6MWT = `12` - `0`)

    # Get the deciles of the changes in 6MWT distance
    deciles_change_6MWT <- quantile(
      x = DB_6MWT_0_12_wide$change_6MWT,
      probs = seq(0, 1, 0.1)
    )

    # Add decile categories to the initial dataset
    DB_6MWT_0_12_wide$decile_change <-
      cut(
        DB_6MWT_0_12_wide$change_6MWT,
        deciles_change_6MWT,
        include.lowest = T,
        labels = F
      )

    # Plot positive response rate to PA barriers questionnaire by decile of 6MWT change
    DB_6MWT_0_12_wide |>
      left_join(BARRIERS_cleaned) |>
      select(-c(autres, autre_precision_obstacle)) |>
      pivot_longer(
        cols = c(trop_vieux:isolement_faible_RS),
        names_to = "barrier",
        values_to = "response"
      ) |>
      mutate(
        barrier = as.factor(barrier),
        barrier = forcats::fct_recode(
          barrier,
          "Unfavourable weather" = "meteo_defavorable",
          "Lack of time" = "manque_temps",
          "Heavy effort / too tired" = "effort_import_fatig",
          "Fear of injury / pain" = "crainte_blessures_douleurs",
          "Lack of interest" = "manque_interet",
          "Difficulty to move" = "deplacements_diff",
          "Too old" = "trop_vieux",
          "Social isolation / weak social network" = "isolement_faible_RS",
          "Too costly" = "cout_trop_eleve"
        )
      ) |>
      count(decile_change, barrier, response, .drop = FALSE) |>
      group_by(decile_change, barrier) |>
      mutate(prop = n / sum(n) * 100) |>
      filter(response == 1) |>
      ggplot(aes(x = decile_change, y = prop)) +
      geom_bar(stat = "identity") +
      labs(x = "Decile of the change in 6MWT distance", y = "% of patients evocating the barrier to physical activity") +
      scale_y_continuous(limits = c(0, 100)) +
      scale_x_continuous(breaks = seq(1, 10, 1)) +
      facet_wrap( ~ barrier)
  }
  ),
tar_target(p_BARRIERS_BY_IPAQ_DECILE, {

  # Get a data frame with the changes in IPAQ MET-min
  DB_IPAQ_6_12_wide <-
    DB_IPAQ_6_12 |>
    pivot_wider(names_from = MONTH, values_from = MET_MIN_WK) |>
    mutate(change_IPAQ_6_12 = `12` - `6`)

  # Get the deciles of the changes in MET_MIN_WK
  deciles_change_IPAQ_6_12 <- quantile(
    x = DB_IPAQ_6_12_wide$change_IPAQ_6_12,
    probs = seq(0, 1, 0.1)
  )

  # Add decile categories to the initial dataset
  DB_IPAQ_6_12_wide$decile_change <-
    cut(
      DB_IPAQ_6_12_wide$change_IPAQ_6_12,
      deciles_change_IPAQ_6_12,
      include.lowest = T,
      labels = F
    )

  # Plot response rate to PA barriers questionnaire by decile of MET_MIN_WK
  DB_IPAQ_6_12_wide |>
    left_join(BARRIERS_cleaned) |>
    select(-c(autres, autre_precision_obstacle)) |>
    pivot_longer(
      cols = c(trop_vieux:isolement_faible_RS),
      names_to = "barrier",
      values_to = "response"
    ) |>
    mutate(
      barrier = as.factor(barrier),
      barrier = forcats::fct_recode(
        barrier,
        "Unfavourable weather" = "meteo_defavorable",
        "Lack of time" = "manque_temps",
        "Heavy effort / too tired" = "effort_import_fatig",
        "Fear of injury / pain" = "crainte_blessures_douleurs",
        "Lack of interest" = "manque_interet",
        "Difficulty to move" = "deplacements_diff",
        "Too old" = "trop_vieux",
        "Social isolation / weak social network" = "isolement_faible_RS",
        "Too costly" = "cout_trop_eleve"
      )
    ) |>
    count(decile_change, barrier, response, .drop = FALSE) |>
    group_by(decile_change, barrier) |>
    mutate(prop = n / sum(n) * 100) |>
    filter(response == 1) |>
    ggplot(aes(x = decile_change, y = prop)) +
    geom_bar(stat = "identity") +
    labs(x = "Decile of the change in IPAQ MET-min/week", y = "% of patients evocating the barrier to physical activity") +
    scale_y_continuous(limits = c(0, 100)) +
    scale_x_continuous(breaks = seq(1, 10, 1)) +
    facet_wrap( ~ barrier)
}
),

  # Export figures 1, 2, and 3
  tar_target(fig1, save_figure("pipeline_output/fig1.tiff", change_6MWT$p, width = 21), format = "file"),
  tar_target(fig2, save_figure("pipeline_output/fig2.tiff", change_IPAQ_6_12$p, scaling = 0.40, width = 21), format = "file"),
  tar_target(fig3, save_figure("pipeline_output/fig3.tiff", p_BARRIERS, scaling = 0.3, height = 5, width = 10), format = "file"),

  # Build report including main results
  tar_render(main, "main.Rmd", output_dir = "pipeline_output/"),

  # Build Supplemental Materials 3
  tar_target(p_6MWT_all, get_plot_6MWT_all(DB_6MWT)),
  tar_target(p_IPAQ_all, get_plot_IPAQ_all(DB_IPAQ)),
  tar_target(p_EMAPS_all, get_plot_EMAPS_all(DB_EMAPS)),
  tar_target(table_all_desc_stat, get_table_all_measurements(DB_6MWT, DB_IPAQ, DB_EMAPS)),
  tar_render(SM3, "SM3.Rmd", output_dir = "pipeline_output/"),

  # Build Supplemental Materials 4
  tar_render(SM4, "SM4.Rmd", output_dir = "pipeline_output/"),

  # Build Supplemental Materials 5
  tar_render(SM5, "SM5.Rmd", output_dir = "pipeline_output/"),

  # Build Supplemental Materials 6
  tar_render(SM6, "SM6.Rmd", output_dir = "pipeline_output/"),

  # Build Supplemental Materials 7
  tar_render(SM7, "SM7.Rmd", output_dir = "pipeline_output/")

)


