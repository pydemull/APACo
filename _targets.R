
# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(packages = c("APACo", "Hmisc"))

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
  tar_target(INCLUSION_cleaned, INCLUSION |> dplyr::mutate(dplyr::across( c(patient, sex, angioplasty, bypass), as.factor), BMI = weight / ((height / 100) ^ 2))),
  tar_target(VISIT_6M_cleaned, VISIT_6M |> dplyr::mutate(patient = as.factor(patient))),
  tar_target(VISIT_12M_cleaned, VISIT_12M |> dplyr::mutate(patient = as.factor(patient))),
  tar_target(DB_6MWT, get_DB_6MWT(INCLUSION_cleaned, VISIT_6M_cleaned, VISIT_12M_cleaned)),
  tar_target(DB_IPAQ, get_DB_IPAQ(IPAQ)),
  tar_target(DB_EMAPS, get_DB_EMAPS(EMAPS)),
  tar_target(BARRIERS_cleaned, BARRIERS |> dplyr::mutate(dplyr::across( patient:autres, as.factor))),

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

  # Analyse change in IPAQ MET-min/week between 6 and 12 months
  tar_target(DB_IPAQ_6_12, get_DB_IPAQ_6_12(DB_IPAQ)),
  tar_target(change_IPAQ, analyse_change_IPAQ(DB_IPAQ_6_12)),

  # Analyse change in EMAPS scores between 0 and 12 months
  tar_target(DB_EMAPS_0_12, get_DB_EMAPS_0_12(DB_EMAPS)),
  tar_target(change_EMAPS, analyse_change_EMAPS(DB_EMAPS_0_12)),

  # Analyse barriers to physical activities at 12 months
  tar_target(analysis_BARRIERS, BARRIERS_cleaned |> skimr::skim()),
  tar_target(p_BARRIERS, get_plot_BARRIERS(BARRIERS_cleaned)),

  # Export figures 1, 2, and 3
  tar_target(fig1, save_figure("out/fig1.tiff", change_6MWT$p), format = "file"),
  tar_target(fig2, save_figure("out/fig2.tiff", change_IPAQ$p, scaling = 0.41, width = 20), format = "file"),
  tar_target(fig3, save_figure("out/fig3.tiff", p_BARRIERS, scaling = 0.3, height = 5, width = 10), format = "file"),

  # Build report including main results
  tar_render(main, "out/main.Rmd"),

  # Build Supplemental Materials 3
  tar_target(p_6MWT_all, get_plot_6MWT_all(DB_6MWT)),
  tar_target(p_IPAQ_all, get_plot_IPAQ_all(DB_IPAQ)),
  tar_target(p_EMAPS_all, get_plot_EMAPS_all(DB_EMAPS)),
  tar_target(table_all_desc_stat, get_table_all_measurements(DB_6MWT, DB_IPAQ, DB_EMAPS)),
  tar_render(SM3, "out/SM3.Rmd"),

  # Build Supplemental Materials 4
  tar_render(SM4, "out/SM4.Rmd"),

  # Build Supplemental Materials 5
  tar_render(SM5, "out/SM5.Rmd")

)


