# Welcome to the script allowing to reproduce analyses performed in the paper entitled
# 'Change in exercise capacity, physical activity, and motivation for physical activity
# at 12 months after a cardiac rehabilitation program in coronary heart disease patients:
# a prospective, monocentric, and observational study'

# ----------
# Load data ----
# ----------

data("INCLUSION")
data("VISIT_6M")
data("VISIT_12M")
data("IPAQ")
data("EMAPS")
data("BARRIERS")

# Please use ?`DATASET_NAME` in the Console for information regarding the content of the dataset.


# ------------------------
# Configure the dataset(s) ----
# ------------------------

# Clean INCLUSION dataset
INCLUSION_cleaned <-
  INCLUSION |>
  dplyr::mutate(dplyr::across(c(sex, angioplasty, bypass), as.factor),
                BMI = weight / ((height / 100) ^ 2))

# Create a dataset with all six-minute walking tests
DB_6MWT <-
  INCLUSION_cleaned |>
  dplyr::left_join(VISIT_6M, by = "patient") |>
  dplyr::left_join(VISIT_12M, by = "patient") |>
  dplyr::select(patient, DIST_6WT_M0, DIST_6WT_M6, DIST_6WT_M12) |>
  dplyr::rename(MONTH_0 = DIST_6WT_M0,
                MONTH_6 = DIST_6WT_M6,
                MONTH_12 = DIST_6WT_M12) |>
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

# Clean the IPAQ dataset and add variables of interest
DB_IPAQ <-
  IPAQ |>
  dplyr::mutate(
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

# Clean the EMAPS dataset and add variables of interest
DB_EMAPS <-
  EMAPS |>
  dplyr::rename(MONTH = num_visit) |>
  dplyr::mutate(MONTH = forcats::fct_recode(
    as.factor(MONTH),
    "0" = "1",
    "6" = "2",
    "12" = "3"
  )) |>
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

# Clean BARRIERS dataset
BARRIERS_cleaned <-
  BARRIERS |>
  dplyr::mutate(across(patient:autres, as.factor))


# ------------------------------------------------------------
# Describe participants characteristics at the inclusion stage ----
# ------------------------------------------------------------

# Get an overview of the variables
INCLUSION_cleaned |> skimr::skim()

# Analyse sex
questionr::freq(INCLUSION_cleaned$sex, digits = 2)

# Analyse height
analyse_distribution(data = INCLUSION_cleaned, var = "height")
## => Comment: Height approximately follows a gaussian distribution.

# Analyse weight
analyse_distribution(data = INCLUSION_cleaned, var = "weight")
## => Comment: Weight approximately follows a gaussian distribution.

# Analyse BMI
analyse_distribution(data = INCLUSION_cleaned, var = "BMI")
## => Comment: BMI approximately follows a gaussian distribution.

# Analyse % Angioplasty
questionr::freq(INCLUSION_cleaned$angioplasty, digits = 2)

# Analyse % Bypass
questionr::freq(INCLUSION_cleaned$bypass, digits = 2)


# ------------------------------------------------------------------------------
# Analyse the change in six-minute walking test distance between 0 and 12 months ----
# ------------------------------------------------------------------------------

# Keep the rows for months 0 and 12, and keep the participants with data at both 0 and 12 months
DB_6MWT_0_12 <-
  DB_6MWT |>
  dplyr::filter(MONTH != "6") |>
  dplyr::mutate(MONTH = factor(MONTH, levels = c("0", "12"))) |>
  tidyr::drop_na() |>
  dplyr::group_by(patient) |>
  tidyr::nest() |>
  dplyr::mutate(n_visits = purrr::map_dbl(data, ~ nrow(.x))) |>
  dplyr::filter(n_visits == 2) |>
  dplyr::ungroup() |>
  tidyr::unnest(data)

# Test the change in central tendency as initially planned in the project
t.test(
  formula = DIST_M ~ MONTH,
  data = DB_6MWT_0_12,
  mu = 0,
  paired = TRUE,
  var.equal = FALSE
)

# Analyse the change in the distribution
change_6MWT <-
  analyse_change(
    data = DB_6MWT_0_12,
    id = "patient",
    x = "MONTH",
    y = "DIST_M",
    rain_side = "f1x1",
    nudge_y = 6,
    color_fill = "#0089C6",
    color_stat = "black",
    labs_1x = "Months post-program",
    labs_1y = "6MWT distance (m)",
    labs_2x = "",
    labs_2y = "6MWT distance [Month 12 - Month 0] (m)",
    labs_3x = "6MWT distance at Month 0 (m)",
    labs_3y = "6MWT distance at Month 12 (m)",
    labs_4x = "Months post-program",
    labs_4y = "6MWT distance (m)",
    labs_5x = "Deciles of 6MWT distance at Month 0 (m)",
    labs_5y = "Decile Month 12 - Decile Month 0 (m)",
    labs_6x = "Quantiles",
    labs_6y = "Quantile sum = q + 1−q"
  )

## View the results of the change analysis
### The `sf` object is a dataframe containing the information relating to
### the shift function.
### The `daf` object is a dataframe containing the information relating to
### the difference asymmetry function.
### You may want to export the figure to have a proper view.
change_6MWT

# Export the figure
# Please modify the path to the output using the first argument of the function
# below so that it fits your needs.
ragg::agg_tiff(
  "../../fig1.tiff",
  scaling = 0.4,
  height = 16,
  width = 16,
  unit = "cm",
  res = 400
)
change_6MWT$p
dev.off()


# -------------------------------------------------------------------------------
# Analyse the change in PA dose in MET-min/week (IPAQ-SF) between 6 and 12 months ----
# -------------------------------------------------------------------------------

# Keep the rows for months 6 and 12, and keep the participants with data at both 6 and 12 months
DB_IPAQ_6_12 <-
  DB_IPAQ |>
  dplyr::filter(MONTH != "0") |>
  dplyr::mutate(MONTH = factor(MONTH, levels = c("6", "12"))) |>
  dplyr::select(patient, MONTH, MET_MIN_WK) |>
  tidyr::drop_na() |>
  dplyr::group_by(patient) |>
  tidyr::nest() |>
  dplyr::mutate(n_visits = purrr::map_dbl(data, ~ nrow(.x))) |>
  dplyr::filter(n_visits == 2) |>
  dplyr::ungroup() |>
  tidyr::unnest(data)

# Analyse the change in the distribution
change_IPAQ <-
  analyse_change(
    data = DB_IPAQ_6_12,
    id = "patient",
    x = "MONTH",
    y = "MET_MIN_WK",
    rain_side = "f1x1",
    nudge_y = 6,
    color_fill = "#BFD61F",
    color_stat = "black",
    labs_1x = "Months post-program",
    labs_1y = "MET-min/week",
    labs_2x = "",
    labs_2y = "MET-min/week [Month 12 - Month 0]",
    labs_3x = "MET-min/week at Month 0",
    labs_3y = "MET-min/week at Month 12",
    labs_4x = "Months post-program",
    labs_4y = "MET-min/week",
    labs_5x = "Deciles of MET-min/week at Month 0",
    labs_5y = "Decile Month 12 - Decile Month 0 (MET-min/week)",
    labs_6x = "Quantiles",
    labs_6y = "Quantile sum = q + 1−q"
  )

## View the results of the change analysis
### The `sf` object is a dataframe containing the information relating to
### the shift function.
### The `daf` object is a dataframe containing the information relating to
### the difference asymmetry function.
### You may want to export the figure to have a proper view.
change_IPAQ

# Export the figure
# Please modify the path to the output using the first argument of the function
# below so that it fits your needs.
ragg::agg_tiff(
  "../../fig2.tiff",
  scaling = 0.41,
  height = 16,
  width = 16,
  unit = "cm",
  res = 400
)
change_IPAQ$p
dev.off()


# ----------------------------------------------------------
# Analyse the change in EMAPS scores between 0 and 12 months ----
# ----------------------------------------------------------

# Keep the rows for months 0 and 12, and keep the participants with data at both 0 and 12 months
DB_EMAPS_0_12 <-
  DB_EMAPS |>
  dplyr::filter(MONTH != "6") |>
  dplyr::mutate(MONTH = factor(MONTH, levels = c("0", "12"))) |>
  tidyr::drop_na() |>
  dplyr::group_by(patient) |>
  tidyr::nest() |>
  dplyr::mutate(n_visits = purrr::map_dbl(data, ~ nrow(.x))) |>
  dplyr::filter(n_visits == 2) |>
  dplyr::ungroup() |>
  tidyr::unnest(data)

# Analyse the change in the distributions
DB_EMAPS_0_12_renamed <- DB_EMAPS_0_12 |>
  dplyr::rename( # variables are renamed because previous wording was problematic for the code below
    INTRINSIC = "Intrinsic motivation",
    INTEGRATED = "Integrated regulation",
    IDENTIFIED = "Identified regulation",
    INTROJECTED = "Introjected regulation",
    EXTERNAL = "External regulation",
    AMOTIVATION = "Amotivation"
  )
change_emaps <-
  purrr::map(names(DB_EMAPS_0_12_renamed)[3:8], function(x) {
  if (x == "INTRINSIC") {
    x_lab = "Intrinsic motivation"
    color_fill <- "chartreuse4"
  }

  if (x == "INTEGRATED")
  {
    x_lab = "Integrated motivation"
    color_fill <- "chartreuse4"
  }

  if (x == "IDENTIFIED")
  {
    x_lab = "Identified motivation"
    color_fill <- "chartreuse4"
  }
  if (x == "INTROJECTED")
  {
    x_lab = "Introjected motivation"
    color_fill <- "grey50"
  }

  if (x == "EXTERNAL")
  {
    x_lab = "External regulation"
    color_fill <- "#FAC090"
  }

  if (x == "AMOTIVATION")
  {
    x_lab = "Amotivation"
    color_fill <- "#E46C0A"
  }


  list_emaps <-
    analyse_change(
    data = DB_EMAPS_0_12_renamed |> dplyr::select(patient, MONTH, x),
    id = "patient",
    x = "MONTH",
    y = x,
    rain_side = "f1x1",
    nudge_y = 0.1,
    color_fill = color_fill,
    color_stat = "black",
    labs_1x = "Months post-program",
    labs_1y = paste0(x_lab, " score"),
    labs_2x = "",
    labs_2y = paste0(x_lab, " score [Month 12 - Month 0]"),
    labs_3x = paste0(x_lab, " score at Month 0"),
    labs_3y = paste0(x_lab, " score at Month 12"),
    labs_4x = "Months post-program",
    labs_4y = paste0(x_lab, " score"),
    labs_5x = paste0("Deciles of ", x_lab, " score at Month 0"),
    labs_5y = "Decile Month 12 - Decile Month 0",
    labs_6x = "Quantiles",
    labs_6y = "Quantile sum = q + 1−q"
  )

  return(list_emaps)

})

## View the results of the change analysis
### The `sf` object is a dataframe containing the information relating to
### the shift function.
### The `daf` object is a dataframe containing the information relating to
### the difference asymmetry function.
### You may want to export the figure to have a proper view.
change_emaps


# --------------
# Barriers to PA ----
# --------------

# Analyse answers
skimr::skim(BARRIERS_cleaned)


# Make the figure
p_bar <-
  BARRIERS_cleaned |>
  dplyr::select(patient:isolement_faible_RS) |>
  tidyr::pivot_longer(cols = c(-patient),
                      names_to = "var",
                      values_to = "rep") |>
  dplyr::mutate(
    rep  = as.factor(rep),
    var = forcats::fct_recode(
      var,
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
  dplyr::count(var, rep) |>
  dplyr::filter(rep == 1) |>
  dplyr::mutate(
    n_tot = 77,
    prop_estimate = purrr::map2_dbl(n, n_tot, function(n, n_tot) {
      ((
        binom.test(n, n_tot, 0.5, alternative = "two.sided", conf.level = 0.95)
      )$estimate[[1]]) * 100
    }),
    prop_ci_low = purrr::map2_dbl(n, n_tot, function(n, n_tot) {
      ((
        binom.test(n, n_tot, 0.5, alternative = "two.sided", conf.level = 0.95)
      )$conf.int[[1]]) * 100
    }),
    prop_ci_up = purrr::map2_dbl(n, n_tot, function(n, n_tot) {
      ((
        binom.test(n, n_tot, 0.5, alternative = "two.sided", conf.level = 0.95)
      )$conf.int[[2]]) * 100
    }),
    magnitude = ifelse(prop_estimate > 15, "high", "low")
  ) |>
  ggplot(aes(
    x = forcats::fct_reorder(var, n),
    y = n,
    color = magnitude
  )) +
  ggrepel::geom_text_repel(
    aes(
      label = paste0(
        round(prop_estimate, 1),
        "% [",
        round(prop_ci_low, 1),
        "%; ",
        round(prop_ci_up, 1),
        "%]"
      )
    ),
    hjust = 0,
    nudge_x = 0.3,
    nudge_y = 1.5,
    size = 7,
    direction = "y",
    force = 0.2
  ) +
  geom_segment(
    aes(
      x = forcats::fct_reorder(var, n),
      y = 0,
      xend = forcats::fct_reorder(var, n),
      yend = 60
    ),
    linewidth  = 0.5,
    color = "grey80"
  ) +
  geom_errorbar(
    aes(
      x = forcats::fct_reorder(var, n),
      ymin = prop_ci_low,
      ymax = prop_ci_up
    ),
    width = 0.2,
    linewidth = 1
  ) +
  geom_point(
    shape = 21,
    stroke = 2,
    fill = "grey90",
    size = 4
  ) +
  labs(y = NULL, x = NULL) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values = c("grey30", "grey60")) +
  coord_flip(ylim = c(0, 60)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.position = "none",
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
  )

# View the figure
p_bar

# Export the figure
# Please modify the path to the output using the first argument of the function
# below so that it fits your needs.
ragg::agg_tiff(
  "../../fig3.tiff",
  scaling = 0.3,
  height = 5,
  width = 10,
  unit = "cm",
  res = 400
)
p_bar
dev.off()


# -------------------------------------------------------------------------
# Build supplemental materials SM3. Data relating to all the 6MWT distances, ----
# IPAQ-SF scores, and EMAPS scores measured during the study
# -------------------------------------------------------------------------

# Figures

## 6MWT distance
p_6MWT_all <-
  view_rainclouds(
    data = DB_6MWT,
    id = "patient",
    x = "MONTH",
    y = "DIST_M",
    color_fill = "#0089C6",
    color_stat = "black",
    labs_x = "Months post-program",
    labs_y = "6-min walking test distance (m)"
  )

## IPAQ MET-min/week
p_IPAQ_all <-
  view_rainclouds(
    data = DB_IPAQ,
    id = "patient",
    x = "MONTH",
    y = "MET_MIN_WK",
    color_fill = "#BFD61F",
    color_stat = "black",
    labs_x = "Months post-program",
    labs_y = "MET-min/week"
  )

##  EMAPS
p_emaps_all <-
  DB_EMAPS |>
  tidyr::pivot_longer(
    cols = -c(patient, MONTH),
    names_to = "type_motiv",
    values_to = "val"
  ) |>
  dplyr::mutate(across(type_motiv, \(x) factor(
    x,
    levels = c(
      "Intrinsic motivation",
      "Integrated regulation",
      "Identified regulation",
      "Introjected regulation",
      "External regulation",
      "Amotivation"
    )
  ))) |>
  ggplot(aes(x = MONTH, y = val, fill = type_motiv)) +
  ggrain::geom_rain(
    id.long.var = "patient",
    cov = "type_motiv",
    point.args = rlang::list2(alpha = 0.3, size = 3),
    line.args = rlang::list2(alpha = 0, linewidth = 0),
    point.args.pos = rlang::list2(position = position_jitter(
      width = .04,
      height = 0,
      seed = 42
    ))
  ) +
  geom_line(
    aes(group = patient, color = type_motiv),
    alpha = 0.2,
    position = position_jitter(
      width = .04,
      height = 0,
      seed = 42
    )
  ) +
  stat_summary(
    aes(group = 1),
    fun = "mean",
    geom = "line",
    size = 0.5,
    color = "black"
  ) +
  stat_summary(
    aes(group = MONTH),
    fun = "mean",
    geom = "point",
    size = 2,
    color = "black"
  ) +
  stat_summary(
    aes(group = MONTH),
    fun.data = "mean_sdl",
    geom = "errorbar",
    fun.args = list(mult = 1),
    width = 0.05,
    linewidth = 0.5,
    color = "black"
  ) +
  scale_y_continuous(breaks = seq(1, 7, 1)) +
  scale_fill_manual(values = c(
    "chartreuse4",
    "chartreuse4",
    "chartreuse4",
    "grey50",
    "#FAC090",
    "#E46C0A"
  )) +
  scale_color_manual(values = c(
    "chartreuse4",
    "chartreuse4",
    "chartreuse4",
    "grey50",
    "#FAC090",
    "#E46C0A"
  )) +
  labs(x = "Months post-program",
       y = "Score") +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    strip.text.x = element_text(size = 12)
  ) +
  facet_wrap(. ~ type_motiv)

# Table

## Build table
all_desc_stat <-
DB_6MWT |>
  dplyr::left_join(DB_IPAQ |> dplyr::select(c(patient, MONTH, MET_MIN_WK)), by = c("patient", "MONTH")) |>
  dplyr::left_join(DB_EMAPS, by = c("patient", "MONTH")) |>
  dplyr::group_by(MONTH) |>
  dplyr::summarise(dplyr::across(
    DIST_M:Amotivation,
    list(
      PARAM_n = ~ sum(!is.na(.x)),
      PARAM_mean = ~ mean(.x, na.rm = TRUE),
      PARAM_sd = ~ sd(.x, na.rm = TRUE),
      PARAM_median = ~ median(.x, na.rm = TRUE),
      PARAM_Q1 =  ~ quantile(., probs = 0.25, na.rm = TRUE),
      PARAM_Q3 =  ~ quantile(., probs = 0.75, na.rm = TRUE)
    )
  )) |>

  tidyr::pivot_longer(
    cols = c(-MONTH),
    names_to = c("Variable", ".value"),
    names_sep = "_PARAM_",
  ) |>
  tidyr::pivot_longer(cols = n:Q3,
                      names_to = "Parameter",
                      values_to = "stat") |>
  tidyr::pivot_wider(names_from = MONTH, values_from = stat) |>
  dplyr::rename("MONTH 0" = "0",
                "MONTH 6" = "6",
                "MONTH 12" = "12") |>
  dplyr::mutate(
    Variable = forcats::fct_recode(Variable, "6MWT distance (m)" = "DIST_M", "IPAQ-SF (MET-min) / week" = "MET_MIN_WK"),
    dplyr::across(`MONTH 0`:`MONTH 12`, ~ round(.x, 2)))



# Formating the table for .html format
all_desc_stat_react <-
  reactable::reactable(
  all_desc_stat,
  columns = list(Variable = reactable::colDef(
    style = reactable::JS(
      "function(rowInfo, colInfo, state) {
        var firstSorted = state.sorted[0]
        // Merge cells if unsorted or sorting by Variable
        if (!firstSorted || firstSorted.id === 'Variable') {
          var prevRow = state.pageRows[rowInfo.viewIndex - 1]
          if (prevRow && rowInfo.row['Variable'] === prevRow['Variable']) {
            return { visibility: 'hidden' }
          }
        }
      }"
    )
  )),
  outlined = TRUE,
  striped = TRUE,
  defaultPageSize = 12
)

# Generate materials
# Please modify the path to the output using the 'output_file' argument so that
# it fits your needs.
rmarkdown::render(
  input = "./inst/templates/SM3.Rmd",
  output_file = "../../../../SM3.html",
  params = list(
    p_6MWT_all = p_6MWT_all,
    p_IPAQ_all = p_IPAQ_all,
    p_emaps_all = p_emaps_all,
    all_desc_stat_react = all_desc_stat_react
  )
)


# --------------------------------------------------------------------------------
# Build supplemental materials SM4. Figures relating to the change in EMAPS scores ----
# between 0 and 12 months (N = 76)
# --------------------------------------------------------------------------------

# Generating the file will take a little bit of time
# Please modify the path to the output using the 'output_file' argument so that
# it fits your needs.
rmarkdown::render(
  input = "./inst/templates/SM4.Rmd",
  output_file = "../../../../SM4.html",
  params = list(
    change_emaps = change_emaps
  )
)


# ----------------------------------------------------------------------------
# Build supplemental materials SM5. Data relating to the shift and difference  ----
# asymmetry functions to describe the change in 6MWT, IPAQ-SF and EMAPS scores
# ----------------------------------------------------------------------------

# Please modify the path to the output using the 'output_file' argument so that
# it fits your needs.
rmarkdown::render(
  input = "./inst/templates/SM5.Rmd",
  output_file = "../../../../SM5.html",
  params = list(
    change_6MWT = change_6MWT,
    change_IPAQ = change_IPAQ,
    change_emaps = change_emaps
  )
)


