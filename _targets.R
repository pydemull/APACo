
# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

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
  tar_target(INCLUSION_cleaned, INCLUSION |> dplyr::mutate(dplyr::across( c(patient, sex, angioplasty, bypass), as.factor), BMI = weight / ((height / 100) ^ 2))),
  tar_target(VISIT_6M_cleaned, VISIT_6M |> dplyr::mutate(patient = as.factor(patient))),
  tar_target(VISIT_12M_cleaned, VISIT_12M |> dplyr::mutate(patient = as.factor(patient))),
  tar_target(DB_6MWT, get_DB_6MWT(INCLUSION_cleaned, VISIT_6M_cleaned, VISIT_12M_cleaned)),
  tar_target(DB_IPAQ, get_DB_IPAQ(IPAQ)),
  tar_target(DB_EMAPS, get_DB_EMAPS(EMAPS)),
  tar_target(BARRIERS_cleaned, BARRIERS |> dplyr::mutate(dplyr::across( patient:autres, as.factor))),
  tar_target(analysis_INCLUSION, INCLUSION_cleaned |> skimr::skim()),
  tar_target(analysis_sex, questionr::freq(INCLUSION_cleaned$sex, digits = 2)),
  tar_target(analysis_age, analyse_distribution(data = INCLUSION_cleaned, var = "age")),
  tar_target(analysis_height, analyse_distribution(data = INCLUSION_cleaned, var = "height")),
  tar_target(analysis_weight, analyse_distribution(data = INCLUSION_cleaned, var = "weight")),
  tar_target(analysis_bmi, analyse_distribution(data = INCLUSION_cleaned, var = "BMI")),
  tar_target(analysis_angioplasty, questionr::freq(INCLUSION_cleaned$angioplasty, digits = 2)),
  tar_target(analysis_bypass, questionr::freq(INCLUSION_cleaned$bypass, digits = 2)),
  tar_target(DB_6MWT_0_12, get_DB_6MWT_0_12(DB_6MWT)),
  tar_target(t_test_results_6MWT, t_test_6MWT_0_12(DB_6MWT_0_12)),
  tar_target(change_6MWT, analyse_change_6MWT(DB_6MWT_0_12)),
  tar_target(fig1, save_figure("out/fig1.tiff", change_6MWT$p), format = "file"),
  tar_target(DB_IPAQ_6_12, get_DB_IPAQ_6_12(DB_IPAQ)),
  tar_target(change_IPAQ, analyse_change_IPAQ(DB_IPAQ_6_12)),
  tar_target(fig2, save_figure("out/fig2.tiff", change_IPAQ$p, scaling = 0.41, width = 20), format = "file"),
  tar_target(DB_EMAPS_0_12, get_DB_EMAPS_0_12(DB_EMAPS))


)


