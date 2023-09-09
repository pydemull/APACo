

# -----------
# Import data ----
# -----------
INCLUSION <- readr::read_csv2("data/INCLUSION.csv")
VISIT_6M <- readr::read_csv2("data/VISIT_6M.csv")
VISIT_12M <- readr::read_csv2("data/VISIT_12M.csv")
IPAQ <- readr::read_csv2("data/IPAQ.csv")
EMAPS <- readr::read_csv2("data/EMAPS.csv")
BARRIERS <- readr::read_csv2("data/BARRIERS.csv")


# ------------------------
# Configure the dataset(s) ----
# ------------------------
INCLUSION_cleaned <-
  INCLUSION |>
  dplyr::mutate(dplyr::across(c(sex, angioplasty, bypass), as.factor),
                BMI = weight / ((height / 100) ^ 2))


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


# ------------------------------------------------------
# Analyse the change in six-minute walking test distance ----
# ------------------------------------------------------

# Combine the datasets and recode the variables
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

# Visualize data from all the participants at the three times of measurement

## Make the plot
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

## View the plot
p_6MWT_all

## Export the plot
ragg::agg_tiff(
  "out/p_6MWT_all.tiff",
  scaling = 0.5,
  height = 10,
  width = 10,
  unit = "cm",
  res = 400
)
p_6MWT_all
dev.off()

# Perform analyses relating to the change between 0 and 12 months

## Keep the rows for months 0 and 12, and keep the participants with data at both 0 and 12 months
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

## Test the change in central tendency
t.test(
  formula = DIST_M ~ MONTH,
  data = DB_6MWT_0_12,
  mu = 0,
  paired = TRUE,
  var.equal = FALSE
)

## Analyse the change in the distribution
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
    labs_1y = "6WT distance (m)",
    labs_2x = "",
    labs_2y = "6WT distance [Month 12 - Month 0] (m)",
    labs_3x = "6WT distance at Month 0 (m)",
    labs_3y = "6WT distance at Month 12 (m)",
    labs_4x = "Months post-program",
    labs_4y = "6WT distance (m)",
    labs_5x = "Deciles of 6WT distance at Month 0 (m)",
    labs_5y = "Decile Month 12 - Decile Month 0 (m)",
    labs_6x = "Quantiles",
    labs_6y = "Quantile sum = q + 1−q"
  )

## View the results of the change analysis
### The `sf` object is a dataframe containing the information relating to
### the shift function.
### The `daf` object is a dataframe containing the information relating to
### the difference asymetry function.
### You may want to export the figure to have a proper view.
change_6MWT

## Export the figure
ragg::agg_tiff(
  "out/p_6MWT_change.tiff",
  scaling = 0.4,
  height = 16,
  width = 16,
  unit = "cm",
  res = 400
)
change_6MWT$p
dev.off()


# -------------------------------------------------------
# Analyse the change in PA dose in MET-min/week (IPAQ-SF) ----
# -------------------------------------------------------

# Create and recode the variables of interest
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

# Visualize data from all the participants at the three times of measurement

## Make the plot
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

## View the plot
p_IPAQ_all

## Export the plot
ragg::agg_tiff(
  "out/p_IPAQ_all",
  scaling = 0.5,
  height = 10,
  width = 10,
  unit = "cm",
  res = 400
)
p_IPAQ_all
dev.off()

# Perform analyses relating to the change between 6 and 12 months

## Keep the rows for months 6 and 12, and keep the participants with data at both 6 and 12 months
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

## Analyse the change in the distribution
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
### the difference asymetry function.
### You may want to export the figure to have a proper view.
change_IPAQ

## Export the figure
ragg::agg_tiff(
  "out/p_IPAQ_change.tiff",
  scaling = 0.4,
  height = 16,
  width = 20,
  unit = "cm",
  res = 400
)
change_IPAQ$p
dev.off()


# ----------------------------------
# Analyse the change in EMAPS scores ----
# ----------------------------------

# Define a custom + operator allowing to handle NAs
`%+%` <- function(x, y)  mapply(sum, x, y, MoreArgs = list(na.rm = TRUE))

# Compute summary scores and recode variables
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

# Make a figure with all the participants and measurements

## Make the plot
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
    "#C3D69B",
    "#C3D69B",
    "#C3D69B",
    "#D9D9D9",
    "#FAC090",
    "#E46C0A"
  )) +
  scale_color_manual(values = c(
    "#C3D69B",
    "#C3D69B",
    "#C3D69B",
    "#D9D9D9",
    "#FAC090",
    "#E46C0A"
  )) +
  labs(x = "Months post-program",
       y = "Score") +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15),
    strip.text.x = element_text(size = 15)
  ) +
  facet_wrap(. ~ type_motiv)

## View the plot
p_emaps_all

## Export the plot
ragg::agg_tiff(
  "out/p_emaps_all.tiff",
  scaling = 0.7,
  height = 15,
  width = 25,
  unit = "cm",
  res = 400
)
p_emaps_all
dev.off()

# Perform analyses relating to the change  between 0 and 12 months

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

## Analyse the change in the distributions
DB_EMAPS_0_12_renamed <- DB_EMAPS_0_12 |>
  dplyr::rename(
    INTRINSIC = "Intrinsic motivation",
    INTEGRATED = "Integrated regulation",
    IDENTIFIED = "Identified regulation",
    INTROJECTED = "Introjected regulation",
    EXTERNAL = "External regulation",
    AMOTIVATION = "Amotivation"
  )
purrr::map(names(DB_EMAPS_0_12_renamed)[3:8], function(x) {
  if (x == "INTRINSIC") {
    x_lab = "Intrinsic motivation"
    color_fill <- "#C3D69B"
  }

  if (x == "INTEGRATED")
  {
    x_lab = "Integrated motivation"
    color_fill <- "#C3D69B"
  }

  if (x == "IDENTIFIED")
  {
    x_lab = "Identified motivation"
    color_fill <- "#C3D69B"
  }
  if (x == "INTROJECTED")
  {
    x_lab = "Introjected motivation"
    color_fill <- "#D9D9D9"
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


  analyse_change(
    data = DB_EMAPS_0_12_renamed,
    id = "patient",
    x = "MONTH",
    y = x,
    rain_side = "f1x1",
    nudge_y = 2,
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
})




















# Make a figure with the participants having data at both 0 and 12 months

## Make the plot
p_emaps_0_12 <-
  DB_EMAPS_0_12 |>
  select(-n_visits) |>
  mutate(patient = as.factor(patient)) |>
  pivot_longer(
    cols = -c(patient, MONTH),
    names_to = "type_motiv",
    values_to = "val"
  ) |>
  mutate(across(type_motiv, \(x) factor(
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
  geom_rain(
    rain.side = "f1x1",
    id.long.var = "patient",
    cov = "type_motiv",
    point.args = rlang::list2(alpha = 0.3, size = 3),
    line.args = rlang::list2(alpha = 0, linewidth = 0),
    line.args.pos = rlang::list2(position = position_jitter(
      width = .04,
      height = 0,
      seed = 42
    )),
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
    color = stat_color
  ) +
  stat_summary(
    aes(group = MONTH),
    fun = "mean",
    geom = "point",
    size = 2,
    color = stat_color
  ) +
  stat_summary(
    aes(group = MONTH),
    fun.data = "mean_sdl",
    geom = "errorbar",
    fun.args = list(mult = 1),
    width = 0.05,
    linewidth = 0.5,
    color = stat_color
  ) +
  scale_y_continuous(breaks = seq(1, 7, 1)) +
  scale_fill_manual(values = c(
    "#C3D69B",
    "#C3D69B",
    "#C3D69B",
    "#D9D9D9",
    "#FAC090",
    "#E46C0A"
  )) +
  scale_color_manual(values = c(
    "#C3D69B",
    "#C3D69B",
    "#C3D69B",
    "#D9D9D9",
    "#FAC090",
    "#E46C0A"
  )) +
  labs(title = "A. Distributions of data",
       x = "Months post-program",
       y = "Score") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(panel.grid = element_blank(),
        strip.text.x = element_text(size = 17)) +
  facet_wrap(. ~ type_motiv, ncol = 2)

## Adjust the positions of the clouds in the plot
p_emaps_0_12$layers[[2]]$position <-
  position_nudge(x = c(rep(c(-0.22, 0.22), each = 1024 * 3)))
p_emaps_0_12$layers[[3]]$position <-
  position_nudge(x = c(-0.15, 0.15))

## View the plot
p_emaps_0_12

# Perform the TOSTs - INTRINSIC motivation

## Get the median of the EMAPS scores (INTRINSIC motivation) at Month 0
median_EMAPS_0_INTRINSIC <-
  median(DB_EMAPS_0_12 |> filter(MONTH == "0") |>  pull(`Intrinsic motivation`))

## Define a function to perform the TOSTs for several equivalence bounds
do_tost_EMAPS_INTRINSIC <- function(perc) {
  equiv_bound <- median_EMAPS_0_INTRINSIC * perc / 100

  res_tost_EMAPS_INTRINSIC <-
    wilcox_TOST(
      formula = `Intrinsic motivation` ~ MONTH,
      data = DB_EMAPS_0_12 |> mutate(MONTH = fct_relevel(MONTH, "12", "0")),
      hypothesis = "EQU",
      paired = TRUE,
      var.equal = FALSE,
      eqb = equiv_bound,
      alpha = 0.05,
      ses = "rb",
      mu = 0
    )

  tab <-
    data.frame(
      lower_ci = res_tost_EMAPS_INTRINSIC$effsize$lower.ci[1],
      estimate = res_tost_EMAPS_INTRINSIC$effsize$estimate[1],
      upper_ci = res_tost_EMAPS_INTRINSIC$effsize$upper.ci[1],
      low_eq = res_tost_EMAPS_INTRINSIC$eqb[1],
      high_eq = res_tost_EMAPS_INTRINSIC$eqb[2],
      decision_tost = res_tost_EMAPS_INTRINSIC$decision$TOST[1],
      decision_test = res_tost_EMAPS_INTRINSIC$decision$test[1]
    ) |>
    mutate(perc = perc)

  return(tab)

}

## Get and combine the results of the TOSTs
res_all_tost_EMAPS_INTRINSIC <-
  map(c(1, 5, 10, 15, 20), do_tost_EMAPS_INTRINSIC) |>
  bind_rows() |>
  mutate(equiv_zone = paste0("±", perc, "%"))

## Make a figure for the TOSTs
p_EMAPS_0_12_INTRINSIC_tost <-
  ggplot() +
  geom_rect(data = res_all_tost_EMAPS_INTRINSIC[5,],
            aes(
              xmin = -Inf,
              ymin = low_eq,
              xmax = Inf,
              ymax = high_eq,
              fill = "20%"
            )) +
  geom_rect(data = res_all_tost_EMAPS_INTRINSIC[4,],
            aes(
              xmin = -Inf,
              ymin = low_eq,
              xmax = Inf,
              ymax = high_eq,
              fill = "15%"
            )) +
  geom_rect(data = res_all_tost_EMAPS_INTRINSIC[3,],
            aes(
              xmin = -Inf,
              ymin = low_eq,
              xmax = Inf,
              ymax = high_eq,
              fill = "10%"
            )) +
  geom_rect(data = res_all_tost_EMAPS_INTRINSIC[2,],
            aes(
              xmin = -Inf,
              ymin = low_eq,
              xmax = Inf,
              ymax = high_eq,
              fill = "5%"
            )) +
  geom_rect(data = res_all_tost_EMAPS_INTRINSIC[1,],
            aes(
              xmin = -Inf,
              ymin = low_eq,
              xmax = Inf,
              ymax = high_eq,
              fill = "1%"
            )) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(data = res_all_tost_EMAPS_INTRINSIC[1,], aes(x = 1, y = estimate), size = 3) +
  geom_segment(data = res_all_tost_EMAPS_INTRINSIC[1,], aes(
    x = 1,
    xend = 1,
    y = lower_ci,
    yend = upper_ci
  )) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(breaks = seq(-3, 3, 0.5)) +
  scale_fill_manual(
    values = c("#E2F0D9", "#C5E0B4", "#A9D18E", "#548235", "#385723"),
    breaks = c("1%", "5%", "10%", "15%", "20%")
  ) +
  labs(title = "B. Median change",
       x = "",
       y = "",
       fill = "Equivalence zone") +
  coord_cartesian(
    xlim = c(0, 2),
    ylim = c(
      res_all_tost_EMAPS_INTRINSIC$low_eq[5],
      res_all_tost_EMAPS_INTRINSIC$high_eq[5]
    ),
    expand = FALSE
  ) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = c(0.2, 0.83),
    legend.background = element_rect(color = "black", fill = "white")
  ) +
  annotate(
    geom = "text",
    label = "Intrinsic motivation",
    x = Inf,
    y = Inf,
    size = 8,
    fontface = "bold",
    color = "white",
    hjust = 1.05,
    vjust = 1.8
  ) +
  annotate(
    geom = "text",
    x = 1.15,
    y = 0.26,
    label = paste0(
      round(res_all_tost_EMAPS_INTRINSIC[1, 2], 1),
      " [",
      format(round(res_all_tost_EMAPS_INTRINSIC[1, 1], 1), nsmall = 1),
      "; ",
      format(round(res_all_tost_EMAPS_INTRINSIC[1, 3], 1), nsmall = 1),
      "]"
    ),
    hjust = 0,
    vjust = 1,
    size = 7
  ) +
  annotate(
    geom = "curve",
    x = 1.15,
    y = 0.14,
    xend = 1.03,
    yend = res_all_tost_EMAPS_INTRINSIC[1, 2],
    curvature = -.35,
    arrow = arrow(length = unit(2, "mm"))
  )

# View the figure
p_EMAPS_0_12_INTRINSIC_tost

# Perform the TOSTs - INTEGRATED regulation

## Get the median of the EMAPS scores (INTEGRATED regulation) at Month 0
median_EMAPS_0_INTEGRATED <-
  median(DB_EMAPS_0_12 |> filter(MONTH == "0") |>  pull(`Integrated regulation`))

## Define a function to perform the TOSTs for several equivalence bounds
do_tost_EMAPS_INTEGRATED <- function(perc) {
  equiv_bound <- median_EMAPS_0_INTEGRATED * perc / 100

  res_tost_EMAPS_INTEGRATED <-
    wilcox_TOST(
      formula = `Integrated regulation` ~ MONTH,
      data = DB_EMAPS_0_12 |> mutate(MONTH = fct_relevel(MONTH, "12", "0")),
      hypothesis = "EQU",
      paired = TRUE,
      var.equal = FALSE,
      eqb = equiv_bound,
      alpha = 0.05,
      ses = "rb",
      mu = 0
    )

  tab <-
    data.frame(
      lower_ci = res_tost_EMAPS_INTEGRATED$effsize$lower.ci[1],
      estimate = res_tost_EMAPS_INTEGRATED$effsize$estimate[1],
      upper_ci = res_tost_EMAPS_INTEGRATED$effsize$upper.ci[1],
      low_eq = res_tost_EMAPS_INTEGRATED$eqb[1],
      high_eq = res_tost_EMAPS_INTEGRATED$eqb[2],
      decision_tost = res_tost_EMAPS_INTEGRATED$decision$TOST[1],
      decision_test = res_tost_EMAPS_INTEGRATED$decision$test[1]
    ) |>
    mutate(perc = perc)

  return(tab)

}

## Get and combine the results of the TOSTs
res_all_tost_EMAPS_INTEGRATED <-
  map(c(1, 5, 10, 15, 20), do_tost_EMAPS_INTEGRATED) |>
  bind_rows() |>
  mutate(equiv_zone = paste0("±", perc, "%"))

## Make the figure for the TOSTs
p_EMAPS_0_12_INTEGRATED_tost <-
  ggplot() +
  geom_rect(data = res_all_tost_EMAPS_INTEGRATED[5,],
            aes(
              xmin = -Inf,
              ymin = low_eq,
              xmax = Inf,
              ymax = high_eq,
              fill = "20%"
            )) +
  geom_rect(data = res_all_tost_EMAPS_INTEGRATED[4,],
            aes(
              xmin = -Inf,
              ymin = low_eq,
              xmax = Inf,
              ymax = high_eq,
              fill = "15%"
            )) +
  geom_rect(data = res_all_tost_EMAPS_INTEGRATED[3,],
            aes(
              xmin = -Inf,
              ymin = low_eq,
              xmax = Inf,
              ymax = high_eq,
              fill = "10%"
            )) +
  geom_rect(data = res_all_tost_EMAPS_INTEGRATED[2,],
            aes(
              xmin = -Inf,
              ymin = low_eq,
              xmax = Inf,
              ymax = high_eq,
              fill = "5%"
            )) +
  geom_rect(data = res_all_tost_EMAPS_INTEGRATED[1,],
            aes(
              xmin = -Inf,
              ymin = low_eq,
              xmax = Inf,
              ymax = high_eq,
              fill = "1%"
            )) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(data = res_all_tost_EMAPS_INTEGRATED[1,], aes(x = 1, y = estimate), size = 3) +
  geom_segment(data = res_all_tost_EMAPS_INTEGRATED[1,], aes(
    x = 1,
    xend = 1,
    y = lower_ci,
    yend = upper_ci
  )) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(breaks = seq(-3, 3, 0.5)) +
  scale_fill_manual(
    values = c("#E2F0D9", "#C5E0B4", "#A9D18E", "#548235", "#385723"),
    breaks = c("1%", "5%", "10%", "15%", "20%")
  ) +
  labs(x = "", y = "", fill = "Equivalence zone") +
  coord_cartesian(
    xlim = c(0, 2),
    ylim = c(
      res_all_tost_EMAPS_INTEGRATED$low_eq[5],
      res_all_tost_EMAPS_INTEGRATED$high_eq[5]
    ),
    expand = FALSE
  ) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = c(0.2, 0.83),
    legend.background = element_rect(color = "black", fill = "white")
  ) +
  annotate(
    geom = "text",
    label = "Integrated regulation",
    x = Inf,
    y = Inf,
    size = 8,
    fontface = "bold",
    color = "white",
    hjust = 1.05,
    vjust = 1.8
  ) +
  annotate(
    geom = "text",
    x = 1.15,
    y = 0.2,
    label = paste0(
      round(res_all_tost_EMAPS_INTEGRATED[1, 2], 1),
      " [",
      format(round(res_all_tost_EMAPS_INTEGRATED[1, 1], 1), nsmall = 1),
      "; ",
      format(round(res_all_tost_EMAPS_INTEGRATED[1, 3], 1), nsmall = 1),
      "]"
    ),
    hjust = 0,
    vjust = 1,
    size = 7
  ) +
  annotate(
    geom = "curve",
    x = 1.17,
    y = 0.22,
    xend = 1.03,
    yend = res_all_tost_EMAPS_INTEGRATED[1, 2],
    curvature = .35,
    arrow = arrow(length = unit(2, "mm"))
  )

# View the figure
p_EMAPS_0_12_INTEGRATED_tost


# Perform the TOSTs - IDENTIFIED regulation

## Get the the median of EMAPS scores (IDENTIFIED regulation) at Month 0
median_EMAPS_0_IDENTIFIED <-
  median(DB_EMAPS_0_12 |> filter(MONTH == "0") |>  pull(`Identified regulation`))

## Define a function to perform the TOSTs for several equivalence bounds
do_tost_EMAPS_IDENTIFIED <- function(perc) {
  equiv_bound <- median_EMAPS_0_IDENTIFIED * perc / 100

  res_tost_EMAPS_IDENTIFIED <-
    wilcox_TOST(
      formula = `Identified regulation` ~ MONTH,
      data = DB_EMAPS_0_12 |> mutate(MONTH = fct_relevel(MONTH, "12", "0")),
      hypothesis = "EQU",
      paired = TRUE,
      var.equal = FALSE,
      eqb = equiv_bound,
      alpha = 0.05,
      ses = "rb",
      mu = 0
    )

  tab <-
    data.frame(
      lower_ci = res_tost_EMAPS_IDENTIFIED$effsize$lower.ci[1],
      estimate = res_tost_EMAPS_IDENTIFIED$effsize$estimate[1],
      upper_ci = res_tost_EMAPS_IDENTIFIED$effsize$upper.ci[1],
      low_eq = res_tost_EMAPS_IDENTIFIED$eqb[1],
      high_eq = res_tost_EMAPS_IDENTIFIED$eqb[2],
      decision_tost = res_tost_EMAPS_IDENTIFIED$decision$TOST[1],
      decision_test = res_tost_EMAPS_IDENTIFIED$decision$test[1]
    ) |>
    mutate(perc = perc)

  return(tab)
}

## Get and combine the results of the TOSTs
res_all_tost_EMAPS_IDENTIFIED <-
  map(c(1, 5, 10, 15, 20), do_tost_EMAPS_IDENTIFIED) |>
  bind_rows() |>
  mutate(equiv_zone = paste0("±", perc, "%"))

## Make a figure for the TOSTs
p_EMAPS_0_12_IDENTIFIED_tost <-
  ggplot() +
  geom_rect(data = res_all_tost_EMAPS_IDENTIFIED[5,],
            aes(
              xmin = -Inf,
              ymin = low_eq,
              xmax = Inf,
              ymax = high_eq,
              fill = "20%"
            )) +
  geom_rect(data = res_all_tost_EMAPS_IDENTIFIED[4,],
            aes(
              xmin = -Inf,
              ymin = low_eq,
              xmax = Inf,
              ymax = high_eq,
              fill = "15%"
            )) +
  geom_rect(data = res_all_tost_EMAPS_IDENTIFIED[3,],
            aes(
              xmin = -Inf,
              ymin = low_eq,
              xmax = Inf,
              ymax = high_eq,
              fill = "10%"
            )) +
  geom_rect(data = res_all_tost_EMAPS_IDENTIFIED[2,],
            aes(
              xmin = -Inf,
              ymin = low_eq,
              xmax = Inf,
              ymax = high_eq,
              fill = "5%"
            )) +
  geom_rect(data = res_all_tost_EMAPS_IDENTIFIED[1,],
            aes(
              xmin = -Inf,
              ymin = low_eq,
              xmax = Inf,
              ymax = high_eq,
              fill = "1%"
            )) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(data = res_all_tost_EMAPS_IDENTIFIED[1,], aes(x = 1, y = estimate), size = 3) +
  geom_segment(data = res_all_tost_EMAPS_IDENTIFIED[1,], aes(
    x = 1,
    xend = 1,
    y = lower_ci,
    yend = upper_ci
  )) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(breaks = seq(-3, 3, 0.5)) +
  scale_fill_manual(
    values = c("#E2F0D9", "#C5E0B4", "#A9D18E", "#548235", "#385723"),
    breaks = c("1%", "5%", "10%", "15%", "20%")
  ) +
  labs(x = "", y = "", fill = "Equivalence zone") +
  coord_cartesian(
    xlim = c(0, 2),
    ylim = c(
      res_all_tost_EMAPS_IDENTIFIED$low_eq[5],
      res_all_tost_EMAPS_IDENTIFIED$high_eq[5]
    ),
    expand = FALSE
  ) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = c(0.2, 0.83),
    legend.background = element_rect(color = "black", fill = "white")
  ) +
  annotate(
    geom = "text",
    label = "Identified regulation",
    x = Inf,
    y = Inf,
    size = 8,
    fontface = "bold",
    color = "white",
    hjust = 1.05,
    vjust = 1.8
  ) +
  annotate(
    geom = "text",
    x = 1.15,
    y = 0.43,
    label = paste0(
      round(res_all_tost_EMAPS_IDENTIFIED[1, 2], 1),
      " [",
      format(round(res_all_tost_EMAPS_IDENTIFIED[1, 1], 1), nsmall = 1),
      "; ",
      format(round(res_all_tost_EMAPS_IDENTIFIED[1, 3], 1), nsmall = 1),
      "]"
    ),
    hjust = 0,
    vjust = 1,
    size = 7
  ) +
  annotate(
    geom = "curve",
    x = 1.17,
    y = 0.31,
    xend = 1.03,
    yend = res_all_tost_EMAPS_IDENTIFIED[1, 2],
    curvature = -.35,
    arrow = arrow(length = unit(2, "mm"))
  )

# View the figure
p_EMAPS_0_12_IDENTIFIED_tost


# Perform the TOSTs - INTROJECTED regulation

## Get the median of the EMAPS scores (INTROJECTED regulation) at Month 0
median_EMAPS_0_INTROJECTED <-
  median(DB_EMAPS_0_12 |> filter(MONTH == "0") |>  pull(`Introjected regulation`))

## Define a function to perform the TOSTs for several equivalence bounds
do_tost_EMAPS_INTROJECTED <- function(perc) {
  equiv_bound <- median_EMAPS_0_INTROJECTED * perc / 100

  res_tost_EMAPS_INTROJECTED <-
    wilcox_TOST(
      formula = `Introjected regulation` ~ MONTH,
      data = DB_EMAPS_0_12 |> mutate(MONTH = fct_relevel(MONTH, "12", "0")),
      hypothesis = "EQU",
      paired = TRUE,
      var.equal = FALSE,
      eqb = equiv_bound,
      alpha = 0.05,
      ses = "rb",
      mu = 0
    )

  tab <-
    data.frame(
      lower_ci = res_tost_EMAPS_INTROJECTED$effsize$lower.ci[1],
      estimate = res_tost_EMAPS_INTROJECTED$effsize$estimate[1],
      upper_ci = res_tost_EMAPS_INTROJECTED$effsize$upper.ci[1],
      low_eq = res_tost_EMAPS_INTROJECTED$eqb[1],
      high_eq = res_tost_EMAPS_INTROJECTED$eqb[2],
      decision_tost = res_tost_EMAPS_INTROJECTED$decision$TOST[1],
      decision_test = res_tost_EMAPS_INTROJECTED$decision$test[1]
    ) |>
    mutate(perc = perc)

  return(tab)
}

## Get and combine the results of the TOSTs
res_all_tost_EMAPS_INTROJECTED <-
  map(c(1, 5, 10, 15, 20), do_tost_EMAPS_INTROJECTED) |>
  bind_rows() |>
  mutate(equiv_zone = paste0("±", perc, "%"))

## Make the figure for the TOSTs
p_EMAPS_0_12_INTROJECTED_tost <-
  ggplot() +
  geom_rect(data = res_all_tost_EMAPS_INTROJECTED[5,],
            aes(
              xmin = -Inf,
              ymin = low_eq,
              xmax = Inf,
              ymax = high_eq,
              fill = "20%"
            )) +
  geom_rect(data = res_all_tost_EMAPS_INTROJECTED[4,],
            aes(
              xmin = -Inf,
              ymin = low_eq,
              xmax = Inf,
              ymax = high_eq,
              fill = "15%"
            )) +
  geom_rect(data = res_all_tost_EMAPS_INTROJECTED[3,],
            aes(
              xmin = -Inf,
              ymin = low_eq,
              xmax = Inf,
              ymax = high_eq,
              fill = "10%"
            )) +
  geom_rect(data = res_all_tost_EMAPS_INTROJECTED[2,],
            aes(
              xmin = -Inf,
              ymin = low_eq,
              xmax = Inf,
              ymax = high_eq,
              fill = "5%"
            )) +
  geom_rect(data = res_all_tost_EMAPS_INTROJECTED[1,],
            aes(
              xmin = -Inf,
              ymin = low_eq,
              xmax = Inf,
              ymax = high_eq,
              fill = "1%"
            )) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(data = res_all_tost_EMAPS_INTROJECTED[1,], aes(x = 1, y = estimate), size = 3) +
  geom_segment(data = res_all_tost_EMAPS_INTROJECTED[1,], aes(
    x = 1,
    xend = 1,
    y = lower_ci,
    yend = upper_ci
  )) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(breaks = seq(-3, 3, 0.5)) +
  scale_fill_manual(
    values = c("#F2F2F2", "#D9D9D9", "#BFBFBF", "#A6A6A6", "#7F7F7F"),
    breaks = c("1%", "5%", "10%", "15%", "20%")
  ) +
  labs(x = "", y = "", fill = "Equivalence zone") +
  coord_cartesian(
    xlim = c(0, 2),
    ylim = c(
      res_all_tost_EMAPS_INTROJECTED$low_eq[5],
      res_all_tost_EMAPS_INTROJECTED$high_eq[5]
    ),
    expand = FALSE
  ) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = c(0.2, 0.83),
    legend.background = element_rect(color = "black", fill = "white")
  ) +
  annotate(
    geom = "text",
    label = "Introjected regulation",
    x = Inf,
    y = Inf,
    size = 8,
    fontface = "bold",
    color = "white",
    hjust = 1.05,
    vjust = 1.8
  ) +
  annotate(
    geom = "text",
    x = 1.15,
    y = 0.15,
    label = paste0(
      round(res_all_tost_EMAPS_INTROJECTED[1, 2], 1),
      " [",
      format(round(res_all_tost_EMAPS_INTROJECTED[1, 1], 1), nsmall = 1),
      "; ",
      format(round(res_all_tost_EMAPS_INTROJECTED[1, 3], 1), nsmall = 1),
      "]"
    ),
    hjust = 0,
    vjust = 1,
    size = 7
  ) +
  annotate(
    geom = "curve",
    x = 1.17,
    y = 0.17,
    xend = 1.03,
    yend = res_all_tost_EMAPS_INTROJECTED[1, 2],
    curvature = .35,
    arrow = arrow(length = unit(2, "mm"))
  )

# View the figure
p_EMAPS_0_12_INTROJECTED_tost


# Perform the TOSTs - EXTERNAL regulation

## Get the median of the EMAPS scores (EXTERNAL regulation) at Month 0
median_EMAPS_0_EXTERNAL <-
  median(DB_EMAPS_0_12 |> filter(MONTH == "0") |>  pull(`External regulation`))

## Define a function to perform the TOSTs for several equivalence bounds
do_tost_EMAPS_EXTERNAL <- function(perc) {
  equiv_bound <- median_EMAPS_0_EXTERNAL * perc / 100

  res_tost_EMAPS_EXTERNAL <-
    wilcox_TOST(
      formula = `External regulation` ~ MONTH,
      data = DB_EMAPS_0_12 |> mutate(MONTH = fct_relevel(MONTH, "12", "0")),
      hypothesis = "EQU",
      paired = TRUE,
      var.equal = FALSE,
      eqb = equiv_bound,
      alpha = 0.05,
      ses = "rb",
      mu = 0
    )

  tab <-
    data.frame(
      lower_ci = res_tost_EMAPS_EXTERNAL$effsize$lower.ci[1],
      estimate = res_tost_EMAPS_EXTERNAL$effsize$estimate[1],
      upper_ci = res_tost_EMAPS_EXTERNAL$effsize$upper.ci[1],
      low_eq = res_tost_EMAPS_EXTERNAL$eqb[1],
      high_eq = res_tost_EMAPS_EXTERNAL$eqb[2],
      decision_tost = res_tost_EMAPS_EXTERNAL$decision$TOST[1],
      decision_test = res_tost_EMAPS_EXTERNAL$decision$test[1]
    ) |>
    mutate(perc = perc)

  return(tab)
}

## Get and combine the results of the TOSTs
res_all_tost_EMAPS_EXTERNAL <-
  map(c(1, 5, 10, 15, 20), do_tost_EMAPS_EXTERNAL) |>
  bind_rows() |>
  mutate(equiv_zone = paste0("±", perc, "%"))

## Make a figure for the TOSTs
p_EMAPS_0_12_EXTERNAL_tost <-
  ggplot() +
  geom_rect(data = res_all_tost_EMAPS_EXTERNAL[5,],
            aes(
              xmin = -Inf,
              ymin = low_eq,
              xmax = Inf,
              ymax = high_eq,
              fill = "20%"
            )) +
  geom_rect(data = res_all_tost_EMAPS_EXTERNAL[4,],
            aes(
              xmin = -Inf,
              ymin = low_eq,
              xmax = Inf,
              ymax = high_eq,
              fill = "15%"
            )) +
  geom_rect(data = res_all_tost_EMAPS_EXTERNAL[3,],
            aes(
              xmin = -Inf,
              ymin = low_eq,
              xmax = Inf,
              ymax = high_eq,
              fill = "10%"
            )) +
  geom_rect(data = res_all_tost_EMAPS_EXTERNAL[2,],
            aes(
              xmin = -Inf,
              ymin = low_eq,
              xmax = Inf,
              ymax = high_eq,
              fill = "5%"
            )) +
  geom_rect(data = res_all_tost_EMAPS_EXTERNAL[1,],
            aes(
              xmin = -Inf,
              ymin = low_eq,
              xmax = Inf,
              ymax = high_eq,
              fill = "1%"
            )) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(data = res_all_tost_EMAPS_EXTERNAL[1,], aes(x = 1, y = estimate), size = 3) +
  geom_segment(data = res_all_tost_EMAPS_EXTERNAL[1,], aes(
    x = 1,
    xend = 1,
    y = lower_ci,
    yend = upper_ci
  )) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(breaks = seq(-3, 3, 0.5)) +
  scale_fill_manual(
    values = c("#FBE5D6", "#F8CBAD", "#F4B183", "#C55A11", "#843C0C"),
    breaks = c("1%", "5%", "10%", "15%", "20%")
  ) +
  labs(x = "", y = "", fill = "Equivalence zone") +
  coord_cartesian(
    xlim = c(0, 2),
    ylim = c(
      res_all_tost_EMAPS_EXTERNAL$low_eq[5] * 4,
      res_all_tost_EMAPS_EXTERNAL$high_eq[5] * 4
    ),
    expand = FALSE
  ) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = c(0.2, 0.83),
    legend.background = element_rect(color = "black", fill = "white"),
    plot.subtitle = element_text(size = 17)
  ) +
  annotate(
    geom = "text",
    label = "External regulation",
    x = Inf,
    y = Inf,
    size = 8,
    fontface = "bold",
    color = "black",
    hjust = 1.05,
    vjust = 1.8
  ) +
  annotate(
    geom = "text",
    x = 1.15,
    y = -0.6,
    label = paste0(
      round(res_all_tost_EMAPS_EXTERNAL[1, 2], 1),
      " [",
      format(round(res_all_tost_EMAPS_EXTERNAL[1, 1], 1), nsmall = 1),
      "; ",
      format(round(res_all_tost_EMAPS_EXTERNAL[1, 3], 1), nsmall = 1),
      "]"
    ),
    hjust = 0,
    vjust = 1,
    size = 7
  ) +
  annotate(
    geom = "curve",
    x = 1.23,
    y = -0.58,
    xend = 1.03,
    yend = res_all_tost_EMAPS_EXTERNAL[1, 2],
    curvature = .35,
    arrow = arrow(length = unit(2, "mm"))
  )

# View the figure
p_EMAPS_0_12_EXTERNAL_tost


# Perform the TOSTs - AMOTIVATION

## Get the median of the EMAPS scores (AMOTIVATION) at Month 0
median_EMAPS_0_AMOTIVATION <-
  median(DB_EMAPS_0_12 |> filter(MONTH == "0") |>  pull(`Amotivation`))

## Define a function to perform the TOSTs for several equivalence bounds
do_tost_EMAPS_AMOTIVATION <- function(perc) {
  equiv_bound <- median_EMAPS_0_AMOTIVATION * perc / 100

  res_tost_EMAPS_AMOTIVATION <-
    wilcox_TOST(
      formula = `Amotivation` ~ MONTH,
      data = DB_EMAPS_0_12 |> mutate(MONTH = fct_relevel(MONTH, "12", "0")),
      hypothesis = "EQU",
      paired = TRUE,
      var.equal = FALSE,
      eqb = equiv_bound,
      alpha = 0.05,
      ses = "rb",
      mu = 0
    )

  tab <-
    data.frame(
      lower_ci = res_tost_EMAPS_AMOTIVATION$effsize$lower.ci[1],
      estimate = res_tost_EMAPS_AMOTIVATION$effsize$estimate[1],
      upper_ci = res_tost_EMAPS_AMOTIVATION$effsize$upper.ci[1],
      low_eq = res_tost_EMAPS_AMOTIVATION$eqb[1],
      high_eq = res_tost_EMAPS_AMOTIVATION$eqb[2],
      decision_tost = res_tost_EMAPS_AMOTIVATION$decision$TOST[1],
      decision_test = res_tost_EMAPS_AMOTIVATION$decision$test[1]
    ) |>
    mutate(perc = perc)

  return(tab)

}

## Get and combine the results of the TOSTs
res_all_tost_EMAPS_AMOTIVATION <-
  map(c(1, 5, 10, 15, 20), do_tost_EMAPS_AMOTIVATION) |>
  bind_rows() |>
  mutate(equiv_zone = paste0("±", perc, "%"))

## Make a figure for the TOSTs
p_EMAPS_0_12_AMOTIVATION_tost <-
  ggplot() +
  geom_rect(data = res_all_tost_EMAPS_AMOTIVATION[5,],
            aes(
              xmin = -Inf,
              ymin = low_eq,
              xmax = Inf,
              ymax = high_eq,
              fill = "20%"
            )) +
  geom_rect(data = res_all_tost_EMAPS_AMOTIVATION[4,],
            aes(
              xmin = -Inf,
              ymin = low_eq,
              xmax = Inf,
              ymax = high_eq,
              fill = "15%"
            )) +
  geom_rect(data = res_all_tost_EMAPS_AMOTIVATION[3,],
            aes(
              xmin = -Inf,
              ymin = low_eq,
              xmax = Inf,
              ymax = high_eq,
              fill = "10%"
            )) +
  geom_rect(data = res_all_tost_EMAPS_AMOTIVATION[2,],
            aes(
              xmin = -Inf,
              ymin = low_eq,
              xmax = Inf,
              ymax = high_eq,
              fill = "5%"
            )) +
  geom_rect(data = res_all_tost_EMAPS_AMOTIVATION[1,],
            aes(
              xmin = -Inf,
              ymin = low_eq,
              xmax = Inf,
              ymax = high_eq,
              fill = "1%"
            )) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(data = res_all_tost_EMAPS_AMOTIVATION[1,], aes(x = 1, y = estimate), size = 3) +
  geom_segment(data = res_all_tost_EMAPS_AMOTIVATION[1,], aes(
    x = 1,
    xend = 1,
    y = lower_ci,
    yend = upper_ci
  )) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(breaks = seq(-3, 3, 0.5)) +
  scale_fill_manual(
    values = c("#FBE5D6", "#F8CBAD", "#F4B183", "#C55A11", "#843C0C"),
    breaks = c("1%", "5%", "10%", "15%", "20%")
  ) +
  labs(x = "", y = "", fill = "Equivalence zone") +
  coord_cartesian(
    xlim = c(0, 2),
    ylim = c(
      res_all_tost_EMAPS_AMOTIVATION$low_eq[5] * 7,
      res_all_tost_EMAPS_AMOTIVATION$high_eq[5] * 7
    ),
    expand = FALSE
  ) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = c(0.2, 0.83),
    legend.background = element_rect(color = "black", fill = "white"),
    plot.subtitle = element_text(size = 17)
  ) +
  annotate(
    geom = "text",
    label = "Amotivation",
    x = Inf,
    y = Inf,
    size = 8,
    fontface = "bold",
    color = "black",
    hjust = 1.05,
    vjust = 1.8
  ) +
  annotate(
    geom = "text",
    x = 1.15,
    y = -0.3,
    label = paste0(
      round(res_all_tost_EMAPS_AMOTIVATION[1, 2], 1),
      " [",
      format(round(res_all_tost_EMAPS_AMOTIVATION[1, 1], 1), nsmall = 1),
      "; ",
      format(round(res_all_tost_EMAPS_AMOTIVATION[1, 3], 1), nsmall = 1),
      "]"
    ),
    hjust = 0,
    vjust = 1,
    size = 7
  ) +
  annotate(
    geom = "curve",
    x = 1.23,
    y = -0.45,
    xend = 1.03,
    yend = res_all_tost_EMAPS_AMOTIVATION[1, 2],
    curvature = -.35,
    arrow = arrow(length = unit(2, "mm"))
  )

# View the figure
p_EMAPS_0_12_AMOTIVATION_tost


# Make the final figure
p_EMAPS_0_12_final <-
  (p_emaps_0_12 |
     (
       (p_EMAPS_0_12_INTRINSIC_tost   | p_EMAPS_0_12_INTEGRATED_tost) /
         (p_EMAPS_0_12_IDENTIFIED_tost  |
            p_EMAPS_0_12_INTROJECTED_tost) /
         (p_EMAPS_0_12_EXTERNAL_tost   |
            p_EMAPS_0_12_AMOTIVATION_tost)
     )) +
  plot_layout(widths = c(2, 2)) & theme(
    plot.title = element_text(size = 25),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 15),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 15),
    strip.text.x = element_text(size = 25)
  )
agg_tiff(
  "out/p_EMAPS.tiff",
  scaling = 0.4,
  height = 20,
  width = 27,
  unit = "cm",
  res = 400
)
p_EMAPS_0_12_final
dev.off()

# --------------
# Barriers to PA ----
# --------------

# Make the figure
p_bar <-
  BARRIERS |>
  select(patient:isolement_faible_RS) |>
  pivot_longer(cols = c(-patient),
               names_to = "var",
               values_to = "rep") |>
  mutate(
    rep  = as.factor(rep),
    var = fct_recode(
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
  count(var, rep) |>
  group_by(var) |>
  mutate(perc = round(n / sum(n) * 100, 1),
         magnitude = ifelse(perc > 15, "high", "low"),) |>
  filter(rep == 1) |>
  ggplot(aes(
    x = fct_reorder(var, n),
    y = n,
    color = magnitude
  )) +
  geom_segment(aes(
    x = fct_reorder(var, n),
    y = 0,
    xend = fct_reorder(var, n),
    yend = n
  ),
  linewidth  = 1) +
  geom_point(
    shape = 21,
    stroke = 2,
    fill = "grey90",
    size = 4
  ) +
  geom_text(
    aes(label = paste0(n, " (", perc, "%)")),
    hjust = 0,
    nudge_y = 0.75,
    fontface = "bold",
    size = 7
  ) +
  labs(y = NULL, x = NULL) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values = c("grey30", "grey60")) +
  coord_flip(ylim = c(0, 36)) +
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
agg_tiff(
  "out/p_barriers.tiff",
  scaling = 0.3,
  height = 5,
  width = 8,
  unit = "cm",
  res = 400
)
p_bar
dev.off()


# ------------------------------------------------------------------------------------------------
# Mean (SD), median (IQR), and dz for the comparisons of the time points (supplementary materials) ----
# ------------------------------------------------------------------------------------------------

# 6MWT distance

## 6 vs 0 months

### Keep the rows for the months 0 and 6, and keep the participants with data at both 0 and 6 months
DB_6MWT_6_0 <-
  DB_6MWT |>
  filter(MONTH != "12") |>
  drop_na() |>
  group_by(patient) |>
  nest() |>
  mutate(n_visits = map_dbl(data, ~ nrow(.x))) |>
  filter(n_visits == 2) |>
  ungroup() |>
  unnest(data) |>
  mutate(MONTH = fct_relevel(MONTH, "6", "0"))

### Get descriptive statistics
describeBy(DB_6MWT_6_0$DIST_M, DB_6MWT_6_0$MONTH, quant = c(0.25, 0.75))

### Get dz
cohens_d(DIST_M ~ MONTH,
         data = DB_6MWT_6_0,
         paired = TRUE,
         pooled_sd = TRUE)

## 12 vs 6 months

### Keep the rows for the months 6 and 12, and keep the participants with data at both 6 and 12 months
DB_6MWT_12_6 <-
  DB_6MWT |>
  filter(MONTH != "0") |>
  drop_na() |>
  group_by(patient) |>
  nest() |>
  mutate(n_visits = map_dbl(data, ~ nrow(.x))) |>
  filter(n_visits == 2) |>
  ungroup() |>
  unnest(data) |>
  mutate(MONTH = fct_relevel(MONTH, "12", "6"))

### Get descriptive statistics
describeBy(DB_6MWT_12_6$DIST_M, DB_6MWT_12_6$MONTH, quant = c(0.25, 0.75))

### Get dz
cohens_d(DIST_M ~ MONTH,
         data = DB_6MWT_12_6,
         paired = TRUE,
         pooled_sd = TRUE)

## 12 vs 0 months

### Keep the rows for the months 0 and 12, and keep the participants with data at both 0 and 12 months
DB_6MWT_12_0 <-
  DB_6MWT |>
  filter(MONTH != "6") |>
  drop_na() |>
  group_by(patient) |>
  nest() |>
  mutate(n_visits = map_dbl(data, ~ nrow(.x))) |>
  filter(n_visits == 2) |>
  ungroup() |>
  unnest(data) |>
  mutate(MONTH = fct_relevel(MONTH, "12", "0"))

### Get descriptive statistics
describeBy(DB_6MWT_12_0$DIST_M, DB_6MWT_12_0$MONTH, quant = c(0.25, 0.75))

### Get dz
cohens_d(DIST_M ~ MONTH,
         data = DB_6MWT_12_0,
         paired = TRUE,
         pooled_sd = TRUE)

# IPAQ MET-min/week

## 6 vs 0 months

### Keep the rows for the months 0 and 6, and keep the participants with data at both 0 and 6 months
DB_IPAQ_6_0 <-
  DB_IPAQ |>
  filter(MONTH != "12") |>
  select(patient, MONTH, MET_MIN_WK) |>
  drop_na() |>
  group_by(patient) |>
  nest() |>
  mutate(n_visits = map_dbl(data, ~ nrow(.x))) |>
  filter(n_visits == 2) |>
  ungroup() |>
  unnest(data) |>
  mutate(MONTH = fct_relevel(MONTH, "6", "0"))

### Get descriptive statistics
describeBy(DB_IPAQ_6_0$MET_MIN_WK, DB_IPAQ_6_0$MONTH, quant = c(0.25, 0.75))

### Get dz
cohens_d(
  MET_MIN_WK ~ MONTH,
  data = DB_IPAQ_6_0,
  paired = TRUE,
  pooled_sd = TRUE
)

## 12 vs 6 months

### Keep the rows for the months 6 and 12, and keep the participants with data at both 6 and 12 months
DB_IPAQ_12_6 <-
  DB_IPAQ |>
  filter(MONTH != "0") |>
  select(patient, MONTH, MET_MIN_WK) |>
  drop_na() |>
  group_by(patient) |>
  nest() |>
  mutate(n_visits = map_dbl(data, ~ nrow(.x))) |>
  filter(n_visits == 2) |>
  ungroup() |>
  unnest(data) |>
  mutate(MONTH = fct_relevel(MONTH, "12", "6"))

### Get descriptive statistics
describeBy(DB_IPAQ_12_6$MET_MIN_WK,
           DB_IPAQ_12_6$MONTH,
           quant = c(0.25, 0.75))

### Get dz
cohens_d(
  MET_MIN_WK ~ MONTH,
  data = DB_IPAQ_12_6,
  paired = TRUE,
  pooled_sd = TRUE
)

## 12 vs 0 months

### Keep the rows for the months 0 and 12, and keep the participants with data at both 0 and 12 months
DB_IPAQ_12_0 <-
  DB_IPAQ |>
  filter(MONTH != "6") |>
  select(patient, MONTH, MET_MIN_WK) |>
  drop_na() |>
  group_by(patient) |>
  nest() |>
  mutate(n_visits = map_dbl(data, ~ nrow(.x))) |>
  filter(n_visits == 2) |>
  ungroup() |>
  unnest(data) |>
  mutate(MONTH = fct_relevel(MONTH, "12", "0"))

### Get descriptive statistics
describeBy(DB_IPAQ_12_0$MET_MIN_WK,
           DB_IPAQ_12_0$MONTH,
           quant = c(0.25, 0.75))

### Get dz
cohens_d(
  MET_MIN_WK ~ MONTH,
  data = DB_IPAQ_12_0,
  paired = TRUE,
  pooled_sd = TRUE
)


# EMAPS - Intrinsic motivation

## 6 vs 0 months

### Keep the rows for the months 0 and 6, and keep the participants with data at both 0 and 6 months
DB_EMAPS_6_0_IM <-
  DB_EMAPS |>
  filter(MONTH != "12") |>
  select(patient, MONTH, `Intrinsic motivation`) |>
  drop_na() |>
  group_by(patient) |>
  nest() |>
  mutate(n_visits = map_dbl(data, ~ nrow(.x))) |>
  filter(n_visits == 2) |>
  ungroup() |>
  unnest(data) |>
  mutate(MONTH = fct_relevel(MONTH, "6", "0"))

### Get descriptive statistics
describeBy(DB_EMAPS_6_0_IM$`Intrinsic motivation`,
           DB_EMAPS_6_0_IM$MONTH,
           quant = c(0.25, 0.75))

### Get dz
cohens_d(
  `Intrinsic motivation` ~ MONTH,
  data = DB_EMAPS_6_0_IM,
  paired = TRUE,
  pooled_sd = TRUE
)

## 12 vs 6 months

### Keep the rows for months 6 and 12, and keep the participants with data at both 6 and 12 months
DB_EMAPS_12_6_IM <-
  DB_EMAPS |>
  filter(MONTH != "0") |>
  select(patient, MONTH, `Intrinsic motivation`) |>
  drop_na() |>
  group_by(patient) |>
  nest() |>
  mutate(n_visits = map_dbl(data, ~ nrow(.x))) |>
  filter(n_visits == 2) |>
  ungroup() |>
  unnest(data) |>
  mutate(MONTH = fct_relevel(MONTH, "12", "6"))

### Get descriptive statistics
describeBy(
  DB_EMAPS_12_6_IM$`Intrinsic motivation`,
  DB_EMAPS_12_6_IM$MONTH,
  quant = c(0.25, 0.75)
)

### Get dz
cohens_d(
  `Intrinsic motivation` ~ MONTH,
  data = DB_EMAPS_12_6_IM,
  paired = TRUE,
  pooled_sd = TRUE
)

## 12 vs 0 months

### Keep the rows for the months 0 and 12, and keep the participants with data at both 0 and 12 months
DB_EMAPS_12_0_IM <-
  DB_EMAPS |>
  filter(MONTH != "6") |>
  select(patient, MONTH, `Intrinsic motivation`) |>
  drop_na() |>
  group_by(patient) |>
  nest() |>
  mutate(n_visits = map_dbl(data, ~ nrow(.x))) |>
  filter(n_visits == 2) |>
  ungroup() |>
  unnest(data) |>
  mutate(MONTH = fct_relevel(MONTH, "12", "0"))

### Get descriptive statistics
describeBy(
  DB_EMAPS_12_0_IM$`Intrinsic motivation`,
  DB_EMAPS_12_0_IM$MONTH,
  quant = c(0.25, 0.75)
)

### Get dz
cohens_d(
  `Intrinsic motivation` ~ MONTH,
  data = DB_EMAPS_12_0_IM,
  paired = TRUE,
  pooled_sd = TRUE
)

# EMAPS - Integrated regulation

## 6 vs 0 months

### Keep the rows for the months 0 and 6, and keep the participants with data at both 0 and 6 months
DB_EMAPS_6_0_INTEG <-
  DB_EMAPS |>
  filter(MONTH != "12") |>
  select(patient, MONTH, `Integrated regulation`) |>
  drop_na() |>
  group_by(patient) |>
  nest() |>
  mutate(n_visits = map_dbl(data, ~ nrow(.x))) |>
  filter(n_visits == 2) |>
  ungroup() |>
  unnest(data) |>
  mutate(MONTH = fct_relevel(MONTH, "6", "0"))

### Get descriptive statistics
describeBy(
  DB_EMAPS_6_0_INTEG$`Integrated regulation`,
  DB_EMAPS_6_0_INTEG$MONTH,
  quant = c(0.25, 0.75)
)

### Get dz
cohens_d(
  `Integrated regulation` ~ MONTH,
  data = DB_EMAPS_6_0_INTEG,
  paired = TRUE,
  pooled_sd = TRUE
)

## 12 vs 6 months

### Keep the rows for the months 6 and 12, and keep the participants with data at both 6 and 12 months
DB_EMAPS_12_6_INTEG <-
  DB_EMAPS |>
  filter(MONTH != "0") |>
  select(patient, MONTH, `Integrated regulation`) |>
  drop_na() |>
  group_by(patient) |>
  nest() |>
  mutate(n_visits = map_dbl(data, ~ nrow(.x))) |>
  filter(n_visits == 2) |>
  ungroup() |>
  unnest(data) |>
  mutate(MONTH = fct_relevel(MONTH, "12", "6"))

### Get descriptive statistics
describeBy(
  DB_EMAPS_12_6_INTEG$`Integrated regulation`,
  DB_EMAPS_12_6_INTEG$MONTH,
  quant = c(0.25, 0.75)
)

### Get dz
cohens_d(
  `Integrated regulation` ~ MONTH,
  data = DB_EMAPS_12_6_INTEG,
  paired = TRUE,
  pooled_sd = TRUE
)

## 12 vs 0 months

### Keep the rows for the months 0 and 12, and keep the participants with data at both 0 and 12 months
DB_EMAPS_12_0_INTEG <-
  DB_EMAPS |>
  filter(MONTH != "6") |>
  select(patient, MONTH, `Integrated regulation`) |>
  drop_na() |>
  group_by(patient) |>
  nest() |>
  mutate(n_visits = map_dbl(data, ~ nrow(.x))) |>
  filter(n_visits == 2) |>
  ungroup() |>
  unnest(data) |>
  mutate(MONTH = fct_relevel(MONTH, "12", "0"))

### Get descriptive statistics
describeBy(
  DB_EMAPS_12_0_INTEG$`Integrated regulation`,
  DB_EMAPS_12_0_INTEG$MONTH,
  quant = c(0.25, 0.75)
)

### Get dz
cohens_d(
  `Integrated regulation` ~ MONTH,
  data = DB_EMAPS_12_0_INTEG,
  paired = TRUE,
  pooled_sd = TRUE
)

# EMAPS - Identified regulation

## 6 vs 0 months

### Keep the rows for the months 0 and 6, and keep the participants with data at both 0 and 6 months
DB_EMAPS_6_0_IDEN <-
  DB_EMAPS |>
  filter(MONTH != "12") |>
  select(patient, MONTH, `Identified regulation`) |>
  drop_na() |>
  group_by(patient) |>
  nest() |>
  mutate(n_visits = map_dbl(data, ~ nrow(.x))) |>
  filter(n_visits == 2) |>
  ungroup() |>
  unnest(data) |>
  mutate(MONTH = fct_relevel(MONTH, "6", "0"))

### Get descriptive statistics
describeBy(
  DB_EMAPS_6_0_IDEN$`Identified regulation`,
  DB_EMAPS_6_0_IDEN$MONTH,
  quant = c(0.25, 0.75)
)

### Get dz
cohens_d(
  `Identified regulation` ~ MONTH,
  data = DB_EMAPS_6_0_IDEN,
  paired = TRUE,
  pooled_sd = TRUE
)

## 12 vs 6 months

### Keep the rows for the months 6 and 12, and keep the participants with data at both 6 and 12 months
DB_EMAPS_12_6_IDEN <-
  DB_EMAPS |>
  filter(MONTH != "0") |>
  select(patient, MONTH, `Identified regulation`) |>
  drop_na() |>
  group_by(patient) |>
  nest() |>
  mutate(n_visits = map_dbl(data, ~ nrow(.x))) |>
  filter(n_visits == 2) |>
  ungroup() |>
  unnest(data) |>
  mutate(MONTH = fct_relevel(MONTH, "12", "6"))

### Get descriptive statistics
describeBy(
  DB_EMAPS_12_6_IDEN$`Identified regulation`,
  DB_EMAPS_12_6_IDEN$MONTH,
  quant = c(0.25, 0.75)
)

### Get dz
cohens_d(
  `Identified regulation` ~ MONTH,
  data = DB_EMAPS_12_6_IDEN,
  paired = TRUE,
  pooled_sd = TRUE
)

## 12 vs 0 months

### Keep the rows for the months 0 and 12, and keep the participants with data at both 0 and 12 months
DB_EMAPS_12_0_IDEN <-
  DB_EMAPS |>
  filter(MONTH != "6") |>
  select(patient, MONTH, `Identified regulation`) |>
  drop_na() |>
  group_by(patient) |>
  nest() |>
  mutate(n_visits = map_dbl(data, ~ nrow(.x))) |>
  filter(n_visits == 2) |>
  ungroup() |>
  unnest(data) |>
  mutate(MONTH = fct_relevel(MONTH, "12", "0"))

### Get descriptive statistics
describeBy(
  DB_EMAPS_12_0_IDEN$`Identified regulation`,
  DB_EMAPS_12_0_IDEN$MONTH,
  quant = c(0.25, 0.75)
)

### Get dz
cohens_d(
  `Identified regulation` ~ MONTH,
  data = DB_EMAPS_12_0_IDEN,
  paired = TRUE,
  pooled_sd = TRUE
)

# EMAPS - Introjected regulation

## 6 vs 0 months

### Keep the rows for the months 0 and 6, and keep the participants with data at both 0 and 6 months
DB_EMAPS_6_0_INTRO <-
  DB_EMAPS |>
  filter(MONTH != "12") |>
  select(patient, MONTH, `Introjected regulation`) |>
  drop_na() |>
  group_by(patient) |>
  nest() |>
  mutate(n_visits = map_dbl(data, ~ nrow(.x))) |>
  filter(n_visits == 2) |>
  ungroup() |>
  unnest(data) |>
  mutate(MONTH = fct_relevel(MONTH, "6", "0"))

### Get descriptive statistics
describeBy(
  DB_EMAPS_6_0_INTRO$`Introjected regulation`,
  DB_EMAPS_6_0_INTRO$MONTH,
  quant = c(0.25, 0.75)
)

### Get dz
cohens_d(
  `Introjected regulation` ~ MONTH,
  data = DB_EMAPS_6_0_INTRO,
  paired = TRUE,
  pooled_sd = TRUE
)

## 12 vs 6 months

### Keep the rows for the months 6 and 12, and keep the participants with data at both 6 and 12 months
DB_EMAPS_12_6_INTRO <-
  DB_EMAPS |>
  filter(MONTH != "0") |>
  select(patient, MONTH, `Introjected regulation`) |>
  drop_na() |>
  group_by(patient) |>
  nest() |>
  mutate(n_visits = map_dbl(data, ~ nrow(.x))) |>
  filter(n_visits == 2) |>
  ungroup() |>
  unnest(data) |>
  mutate(MONTH = fct_relevel(MONTH, "12", "6"))

### Get descriptive statistics
describeBy(
  DB_EMAPS_12_6_INTRO$`Introjected regulation`,
  DB_EMAPS_12_6_INTRO$MONTH,
  quant = c(0.25, 0.75)
)

### Get dz
cohens_d(
  `Introjected regulation` ~ MONTH,
  data = DB_EMAPS_12_6_INTRO,
  paired = TRUE,
  pooled_sd = TRUE
)

## 12 vs 0 months

### Keep the rows for the months 0 and 12, and keep the participants with data at both 0 and 12 months
DB_EMAPS_12_0_INTRO <-
  DB_EMAPS |>
  filter(MONTH != "6") |>
  select(patient, MONTH, `Introjected regulation`) |>
  drop_na() |>
  group_by(patient) |>
  nest() |>
  mutate(n_visits = map_dbl(data, ~ nrow(.x))) |>
  filter(n_visits == 2) |>
  ungroup() |>
  unnest(data) |>
  mutate(MONTH = fct_relevel(MONTH, "12", "0"))

### Get descriptive statistics
describeBy(
  DB_EMAPS_12_0_INTRO$`Introjected regulation`,
  DB_EMAPS_12_0_INTRO$MONTH,
  quant = c(0.25, 0.75)
)

### Get dz
cohens_d(
  `Introjected regulation` ~ MONTH,
  data = DB_EMAPS_12_0_INTRO,
  paired = TRUE,
  pooled_sd = TRUE
)

# EMAPS - External regulation

## 6 vs 0 months

### Keep the rows for the months 0 and 6, and keep the participants with data at both 0 and 6 months
DB_EMAPS_6_0_EXT <-
  DB_EMAPS |>
  filter(MONTH != "12") |>
  select(patient, MONTH, `External regulation`) |>
  drop_na() |>
  group_by(patient) |>
  nest() |>
  mutate(n_visits = map_dbl(data, ~ nrow(.x))) |>
  filter(n_visits == 2) |>
  ungroup() |>
  unnest(data) |>
  mutate(MONTH = fct_relevel(MONTH, "6", "0"))

### Get descriptive statistics
describeBy(DB_EMAPS_6_0_EXT$`External regulation`,
           DB_EMAPS_6_0_EXT$MONTH,
           quant = c(0.25, 0.75))

### Get dz
cohens_d(
  `External regulation` ~ MONTH,
  data = DB_EMAPS_6_0_EXT,
  paired = TRUE,
  pooled_sd = TRUE
)

## 12 vs 6 months

### Keep the rows for the months 6 and 12, and keep the participants with data at both 6 and 12 months
DB_EMAPS_12_6_EXT <-
  DB_EMAPS |>
  filter(MONTH != "0") |>
  select(patient, MONTH, `External regulation`) |>
  drop_na() |>
  group_by(patient) |>
  nest() |>
  mutate(n_visits = map_dbl(data, ~ nrow(.x))) |>
  filter(n_visits == 2) |>
  ungroup() |>
  unnest(data) |>
  mutate(MONTH = fct_relevel(MONTH, "12", "6"))

### Get descriptive statistics
describeBy(
  DB_EMAPS_12_6_EXT$`External regulation`,
  DB_EMAPS_12_6_EXT$MONTH,
  quant = c(0.25, 0.75)
)

### Get dz
cohens_d(
  `External regulation` ~ MONTH,
  data = DB_EMAPS_12_6_EXT,
  paired = TRUE,
  pooled_sd = TRUE
)

## 12 vs 0 months

### Keep the rows for the months 0 and 12, and keep the participants with data at both 0 and 12 months
DB_EMAPS_12_0_EXT <-
  DB_EMAPS |>
  filter(MONTH != "6") |>
  select(patient, MONTH, `External regulation`) |>
  drop_na() |>
  group_by(patient) |>
  nest() |>
  mutate(n_visits = map_dbl(data, ~ nrow(.x))) |>
  filter(n_visits == 2) |>
  ungroup() |>
  unnest(data) |>
  mutate(MONTH = fct_relevel(MONTH, "12", "0"))

### Get descriptive statistics
describeBy(
  DB_EMAPS_12_0_EXT$`External regulation`,
  DB_EMAPS_12_0_EXT$MONTH,
  quant = c(0.25, 0.75)
)

### Get dz
cohens_d(
  `External regulation` ~ MONTH,
  data = DB_EMAPS_12_0_EXT,
  paired = TRUE,
  pooled_sd = TRUE
)

# EMAPS - Amotivation

## 6 vs 0 months

### Keep the rows for months 0 and 6, and keep the participants with data at both 0 and 6 months
DB_EMAPS_6_0_AM <-
  DB_EMAPS |>
  filter(MONTH != "12") |>
  select(patient, MONTH, `Amotivation`) |>
  drop_na() |>
  group_by(patient) |>
  nest() |>
  mutate(n_visits = map_dbl(data, ~ nrow(.x))) |>
  filter(n_visits == 2) |>
  ungroup() |>
  unnest(data) |>
  mutate(MONTH = fct_relevel(MONTH, "6", "0"))

### Get descriptive statistics
describeBy(DB_EMAPS_6_0_AM$`Amotivation`,
           DB_EMAPS_6_0_AM$MONTH,
           quant = c(0.25, 0.75))

### Get dz
cohens_d(
  `Amotivation` ~ MONTH,
  data = DB_EMAPS_6_0_AM,
  paired = TRUE,
  pooled_sd = TRUE
)

## 12 vs 6 months

### Keep the rows for the months 6 and 12, and keep the participants with data at both 6 and 12 months
DB_EMAPS_12_6_AM <-
  DB_EMAPS |>
  filter(MONTH != "0") |>
  select(patient, MONTH, `Amotivation`) |>
  drop_na() |>
  group_by(patient) |>
  nest() |>
  mutate(n_visits = map_dbl(data, ~ nrow(.x))) |>
  filter(n_visits == 2) |>
  ungroup() |>
  unnest(data) |>
  mutate(MONTH = fct_relevel(MONTH, "12", "6"))

### Get descriptive statistics
describeBy(DB_EMAPS_12_6_AM$`Amotivation`,
           DB_EMAPS_12_6_AM$MONTH,
           quant = c(0.25, 0.75))

### Get dz
cohens_d(
  `Amotivation` ~ MONTH,
  data = DB_EMAPS_12_6_AM,
  paired = TRUE,
  pooled_sd = TRUE
)

## 12 vs 0 months

### Keep the rows for the months 0 and 12, and keep the participants with data at both 0 and 12 months
DB_EMAPS_12_0_AM <-
  DB_EMAPS |>
  filter(MONTH != "6") |>
  select(patient, MONTH, `Amotivation`) |>
  drop_na() |>
  group_by(patient) |>
  nest() |>
  mutate(n_visits = map_dbl(data, ~ nrow(.x))) |>
  filter(n_visits == 2) |>
  ungroup() |>
  unnest(data) |>
  mutate(MONTH = fct_relevel(MONTH, "12", "0"))

### Get descriptive statistics
describeBy(DB_EMAPS_12_0_AM$`Amotivation`,
           DB_EMAPS_12_0_AM$MONTH,
           quant = c(0.25, 0.75))

### Get dz
cohens_d(
  `Amotivation` ~ MONTH,
  data = DB_EMAPS_12_0_AM,
  paired = TRUE,
  pooled_sd = TRUE
)
