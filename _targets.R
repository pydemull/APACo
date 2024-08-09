
# Load packages required to define the pipeline ----
library(targets)
library(tarchetypes)

# Set target options ----
tar_option_set(
  packages = c(
    "APACo",
    "cluster",
    "dplyr",
    "ggalluvial",
    "ggplot2",
    "factoextra",
    "FactoMineR",
    "Hmisc",
    "npmv",
    "tidyr",
    "patchwork",
    "scales"
  )
)

# Define pipeline ----
list(
  # Get data ----
  tar_target(INCLUSION, read_data("INCLUSION", "APACo")),
  tar_target(VISIT_6M, read_data("VISIT_6M", "APACo")),
  tar_target(VISIT_12M, read_data("VISIT_12M", "APACo")),
  tar_target(IPAQ, read_data("IPAQ", "APACo")),
  tar_target(EMAPS, read_data("EMAPS", "APACo")),
  tar_target(BARRIERS, read_data("BARRIERS", "APACo")),

  # Prepare general datasets ----
  tar_target(INCLUSION_cleaned, INCLUSION |> mutate(across(c(patient, sex, angioplasty, bypass), as.factor), BMI = weight / ((height / 100) ^ 2))),
  tar_target(VISIT_6M_cleaned, VISIT_6M |> mutate(patient = as.factor(patient))),
  tar_target(VISIT_12M_cleaned, VISIT_12M |> mutate(patient = as.factor(patient))),
  tar_target(DB_6MWT, get_DB_6MWT(INCLUSION_cleaned, VISIT_6M_cleaned, VISIT_12M_cleaned)),
  tar_target(DB_IPAQ, get_DB_IPAQ(IPAQ)),
  tar_target(DB_EMAPS, get_DB_EMAPS(EMAPS)),
  tar_target(BARRIERS_cleaned, {

    # Replace erroneous value 9 by 0 for patient 32
    BARRIERS[BARRIERS$patient == "32", "meteo_defavorable"] <- 0

    # Convert questionnaire variables to factors
    BARRIERS <- BARRIERS |> mutate(across(patient:autres, as.factor))
  }),

  # Perform descriptive analysis of the participants ----
  tar_target(analysis_INCLUSION, INCLUSION_cleaned |> skimr::skim()),
  tar_target(analysis_sex, questionr::freq(INCLUSION_cleaned$sex, digits = 2)),
  tar_target(analysis_age, analyse_distribution(data = INCLUSION_cleaned, var = "age")),
  tar_target(analysis_height, analyse_distribution(data = INCLUSION_cleaned, var = "height")),
  tar_target(analysis_weight, analyse_distribution(data = INCLUSION_cleaned, var = "weight")),
  tar_target(analysis_bmi, analyse_distribution(data = INCLUSION_cleaned, var = "BMI")),
  tar_target(analysis_angioplasty, questionr::freq(INCLUSION_cleaned$angioplasty, digits = 2)),
  tar_target(analysis_bypass, questionr::freq(INCLUSION_cleaned$bypass, digits = 2)),

  # Analyse change in 6MWT distance between 0 and 12 months ----
  tar_target(DB_6MWT_0_12, get_DB_6MWT_0_12(DB_6MWT)),
  tar_target(t_test_results_6MWT, t_test_6MWT_0_12(DB_6MWT_0_12)),
  tar_target(change_6MWT, analyse_change_6MWT(DB_6MWT_0_12)),

  # Analyse change in IPAQ MET-min/week between 0 and 12 months ----
  tar_target(DB_IPAQ_0_12, get_DB_IPAQ_0_12(DB_IPAQ)),
  tar_target(change_IPAQ_0_12, analyse_change_IPAQ(DB_IPAQ_0_12)),

  # Analyse change in IPAQ MET-min/week between 6 and 12 months ----
  tar_target(DB_IPAQ_6_12, get_DB_IPAQ_6_12(DB_IPAQ)),
  tar_target(change_IPAQ_6_12, analyse_change_IPAQ(DB_IPAQ_6_12)),

  # Analyse change in EMAPS scores between 0 and 12 months ----
  tar_target(DB_EMAPS_0_12, get_DB_EMAPS_0_12(DB_EMAPS)),
  tar_target(change_EMAPS, analyse_change_EMAPS(DB_EMAPS_0_12)),

  # Analyse change in motivation profile between 0 and 12 months ----
  # (clustering approach) ----

        ## Subset EMAPS data by time point ----
        tar_target(DB_EMAPS_0_12_M0, DB_EMAPS_0_12 |>  filter(MONTH == "0")),
        tar_target(DB_EMAPS_0_12_M12, DB_EMAPS_0_12 |>  filter(MONTH == "12")),

        ## Scale the variables of the dataset at month 0 ----
        tar_target(
          DB_EMAPS_0_12_M0_scaled,
          DB_EMAPS_0_12_M0 |>
            select(INTRINSIC:AMOTIVATION) |>
            scale() |>
            as.data.frame()
        ),
        ## Scale the variables of the dataset at month 12 ----
        tar_target(
          DB_EMAPS_0_12_M12_scaled,
          DB_EMAPS_0_12_M12 |>
            select(INTRINSIC:AMOTIVATION) |>
            scale() |>
            as.data.frame()
        ),

        ## Build PCA graph for both month 0 and month 12 ----
        tar_target(p_pca, {

              ### Build PCA graph for month 12
              p_pca_m0 <-
                fviz_pca(
                  prcomp(DB_EMAPS_0_12_M0_scaled),
                  title = "PCA - Month 0",
                  geom = "point",
                  ggtheme = theme_classic(),
                  legend = "bottom"
                ) + coord_cartesian(xlim = c(-4, 6), y = c(-5, 3))

              ### Build PCA graph for month 12
              p_pca_m12 <-
                fviz_pca(
                  prcomp(DB_EMAPS_0_12_M12_scaled),
                  title = "PCA - Month 12",
                  geom = "point",
                  ggtheme = theme_classic(),
                  legend = "bottom"
                ) + coord_cartesian(xlim = c(-4, 6), y = c(-5, 3))

              ### Get final figure
              (p_pca_m0 | p_pca_m12)
            }),

        ## Confirm cluser tendency using graphical method ----
        tar_target(cluster_tendency, {

              ### Make visualization for month 0
              p_clus_tend_month0 <-
                fviz_dist(dist(DB_EMAPS_0_12_M0_scaled), show_labels = FALSE) + labs(title = "Month 0")

              ### Make visualization for month 12
              p_clus_tend_month12 <-
                fviz_dist(dist(DB_EMAPS_0_12_M12_scaled), show_labels = FALSE) + labs(title = "Month 12")

              ### Get final figure
              (p_clus_tend_month0 | p_clus_tend_month12) & theme(legend.position = "bottom")
            }),

        ## Determine the optimal number of clusters for K-Medoid approach using ----
        ## the silhouette method ----
        tar_target(optim_n_clusters, {

              ### Build the plot to determine the optimal number of clusters for month 0
              p_silhouette_emaps_m0 <-
                fviz_nbclust(DB_EMAPS_0_12_M0_scaled, pam, method = "silhouette") +
                theme_classic()

              ### Build the plot to determine the optimal number of clusters for month 12
              p_silhouette_emaps_m12 <-
                fviz_nbclust(DB_EMAPS_0_12_M12_scaled, pam, method = "silhouette") +
                theme_classic()

              ### Build figure
              p_silhouette_emaps_m0 | p_silhouette_emaps_m12
            }),

        ## Determine the final clusters using K-Medoid approach ----

            ### Final clusters at Month 0
            tar_target(final_clusters_emaps_m0, {
              set.seed(123); pam(x = DB_EMAPS_0_12_M0_scaled, k = 2, metric = "euclidean")
              }),

            ### Final clusters at Month 12
            tar_target(final_clusters_emaps_m12, {
              set.seed(123); pam(x = DB_EMAPS_0_12_M12_scaled, k = 2,  metric = "euclidean")
              }),

            ### Build figure
            tar_target(final_clusters_emaps_viz, {

                #### Clusters viz at month 0
                p_final_clusters_emaps_m0 <-
                  fviz_cluster(
                    final_clusters_emaps_m0,
                    main = "Month 0",
                    palette = c("#E7298A", "#7570B3"),
                    ellipse.type = "convex",
                    repel = TRUE,
                    ggtheme = theme_minimal(),
                    trans = "reverse"
                  )

                ##### Clusters viz at month 12
                p_final_clusters_emaps_m12 <-
                  fviz_cluster(
                    final_clusters_emaps_m12,
                    main = "Month 12",
                    palette = c("#7570B3", "#E7298A"),
                    ellipse.type = "convex",
                    repel = TRUE,
                    ggtheme = theme_minimal()
                  )

                #### Whole figure
                (p_final_clusters_emaps_m0 | p_final_clusters_emaps_m12) +
                  plot_layout(guides = "collect") & theme(legend.position = "none")
            }),

        ## Attribute clusters IDs to participants ----
        tar_target(DB_EMAPS_0_12_clust, {

          ### Create the vector of clusters IDs for both month 0 and 12
          clusters_emaps <- c(final_clusters_emaps_m0$cluster, final_clusters_emaps_m12$cluster)

          ### Add a column with clusters IDs to the DB_EMAPS_0_12 data frame
          DB_EMAPS_0_12_clust <-
            cbind(DB_EMAPS_0_12 |> arrange(MONTH), cluster = clusters_emaps) |>
            mutate(
              cluster = case_when(
                MONTH == "0" & cluster == 1    ~  "High AU-Mod C",
                MONTH == "0" & cluster == 2    ~  "Very High AU-High C",
                MONTH == "12" & cluster ==  1  ~  "Very High AU-High C",
                MONTH == "12" & cluster ==  2  ~  "High AU-Mod C"
              )
            )

          return(DB_EMAPS_0_12_clust)
        }),

        ## Pivot DB_EMAPS_0_12_clust for subsequent analyses ----
        tar_target(
          DB_EMAPS_0_12_clust_piv,
          DB_EMAPS_0_12_clust |>
            pivot_longer(
              cols = c(INTRINSIC:AMOTIVATION),
              names_to = "Motivation",
              values_to = "Score"
            ) |>
            mutate(Motivation = factor(
              Motivation,
              levels = c(
                "INTRINSIC",
                "INTEGRATED",
                "IDENTIFIED",
                "INTROJECTED",
                "EXTERNAL",
                "AMOTIVATION"
              ),
              labels = c("I-M", "INTEG-R", "IDENT-R", "INTRO-R", "EXTER-R", "AMOTI")
            ))
        ),

          ## Visualize the EMAPS descriptive statistics related the motivation profiles (clusters) ----
          tar_target(
            p_emaps_clust,
            DB_EMAPS_0_12_clust_piv |>
              mutate(
                cluster = factor(cluster, levels = c("Very High AU-High C", "High AU-Mod C")),
                MONTH = factor(MONTH, labels = c("Month 0", "Month 12"))
              ) |>
              ggplot(
                aes(
                  x = Motivation,
                  y = Score,
                  fill = cluster,
                  color = cluster,
                  shape = cluster
                )
              ) +
              ggrain::geom_rain(
                id.long.var = "patient",
                rain.side = "l",
                boxplot.args = list(
                  color = "black",
                  width = 0.17,
                  outlier.shape = NA
                ),
                point.args = list(alpha = 0.5),
                violin.args = list(color = "black"),
                boxplot.args.pos = list(position = position_nudge(x = 0)),
                point.args.pos = list(position = position_nudge(x = 0.2)),
                violin.args.pos = list(position = position_nudge(x = 0.2)),
                line.args.pos = list(position = position_nudge(x = 0.2))
              ) +
              stat_summary(
                aes(group = cluster),
                fun = "median",
                geom = "line",
                color = "black"
              ) +
              scale_y_continuous(breaks = seq(1, 7, 1), expand = expansion(0)) +
              scale_fill_manual(values = c("#7570B3", "#E7298A")) +
              scale_color_manual(values = c("#7570B3", "#E7298A")) +
              coord_cartesian(ylim = c(0.5, 7.5)) +
              labs(
                x = NULL,
                fill = "Cluster",
                color = "Cluster",
                shape = "Cluster"
              ) +
              theme(legend.position = "bottom") +
              facet_grid(MONTH ~ cluster, switch = "y")
          ),

        ## Get a table with the descriptive statistics of the clusters ----
        tar_target(table_stats_clusters_emaps, {

            ### Get n and % of participants per cluster
            n_clusters_emaps <-
              DB_EMAPS_0_12_clust_piv |>
              arrange(Motivation, patient, MONTH) |>
              group_by(patient) |>
              slice(1:2) |>
              group_by(MONTH, cluster) |>
              summarise(N = n()) |>
              ungroup() |>
              mutate(perc = janitor::round_half_up(N / sum(N) * 100, 1),
                     N_perc = paste0(N, " (", perc, ")")) |>
              select(-c(N, perc)) |>
              pivot_wider(names_from = cluster, values_from = N_perc) |>
              mutate(skim_variable = "N (%)") |>
              select(MONTH, skim_variable, `Very High AU-High C`, `High AU-Mod C`)  |>
              rename(Variable = skim_variable)

            ### Get descriptive stats per cluster
            stats_clusters_emaps <-
              DB_EMAPS_0_12_clust |>
              group_by(MONTH, cluster) |>
              skimr::skim() |>
              tibble::tibble() |>
              filter(
                skim_variable %in% c(
                  "INTRINSIC",
                  "INTEGRATED",
                  "IDENTIFIED",
                  "INTROJECTED",
                  "EXTERNAL",
                  "AMOTIVATION"
                )
              ) |>
              select(MONTH,
                     cluster,
                     skim_variable,
                     numeric.p50,
                     numeric.p25,
                     numeric.p75) |>
              mutate(
                numeric.p50 = format(janitor::round_half_up(numeric.p50, 2), nsmall = 2),
                numeric.p25 = format(janitor::round_half_up(numeric.p25, 2), nsmall = 2),
                numeric.p75 = format(janitor::round_half_up(numeric.p75, 2), nsmall = 2),
                median_iqr = paste0(numeric.p50, " (", numeric.p25, " - ", numeric.p75, ")")
              ) |>
              select(-c(numeric.p50:numeric.p75)) |>
              pivot_wider(names_from = cluster, values_from = median_iqr) |>
              select(MONTH, skim_variable, `Very High AU-High C`, `High AU-Mod C`)  |>
              arrange(MONTH) |>
              ungroup()  |>
              rename(Variable = skim_variable)

            ### Get final table
            table_stats_clusters_emaps <-
              bind_rows(n_clusters_emaps, stats_clusters_emaps) |>
              arrange(MONTH) |>
              mutate(Variable = ifelse(Variable == "N (%)", Variable, paste0(Variable, " [Med (IQR)]"))) |>
              rename(Month = MONTH)

            return(table_stats_clusters_emaps)
          }
                     ),

          ## Compare clusters using multivariate analysis at month 0 and then at month 12 ----

                  ### Group comparison at Month 0: detection of a global difference
                  tar_target(nonpartest_global_month0,
                  nonpartest(
                    IDENTIFIED |
                      INTROJECTED |
                      EXTERNAL |
                      AMOTIVATION ~ cluster,
                    data = DB_EMAPS_0_12_clust |> filter(MONTH == "0"),
                    permreps = 1000,
                    plots = FALSE
                  )
                ),
                ### Group comparison at Month 0: detection of the localisation of the difference(s)
                  tar_target(
                    nonpartest_local_month0,
                    ssnonpartest(
                      IDENTIFIED |
                        INTROJECTED |
                        EXTERNAL |
                        AMOTIVATION ~ cluster,
                      data = DB_EMAPS_0_12_clust |> filter(MONTH == "0"),
                      test = c(1, 0, 0, 0),
                      alpha = 0.05,
                      factors.and.variables = TRUE
                    )
                  ),
                  ### Group comparison at Month 12: detection of a global difference
                  tar_target(nonpartest_global_month12,
                             nonpartest(
                               IDENTIFIED |
                                 INTROJECTED |
                                 EXTERNAL |
                                 AMOTIVATION ~ cluster,
                               data = DB_EMAPS_0_12_clust |> filter(MONTH == "12"),
                               permreps = 1000,
                               plots = FALSE
                             )
                  ),
                  ### Group comparison at Month 12: detection of the localisation of the difference(s)
                  tar_target(
                    nonpartest_local_month12,
                    ssnonpartest(
                      IDENTIFIED |
                        INTROJECTED |
                        EXTERNAL |
                        AMOTIVATION ~ cluster,
                      data = DB_EMAPS_0_12_clust |> filter(MONTH == "12"),
                      test = c(1, 0, 0, 0),
                      alpha = 0.05,
                      factors.and.variables = TRUE
                    )
                  ),

      ## Explore the change in motivational profile ----

          ### Build an alluvial plot to see the proportion of profile changes
          tar_target(change_emaps_profile_alluvial, {
            DB_EMAPS_0_12_clust_piv |>
              group_by(MONTH, patient) |>
              slice(1) |>
              mutate(
                cluster = factor(cluster, levels = c(
                  "Very High AU-High C",
                  "High AU-Mod C"
                )),
                Freq = 1
              ) |>
              ggplot(aes(
                x = MONTH,
                stratum = cluster,
                alluvium = patient,
                y = Freq,
                fill = cluster,
                color = cluster
              )) +
              geom_flow() +
              geom_stratum(alpha = .5, linewidth = 0.6, fill = "grey90") +
              geom_text(stat = "stratum", size = 3,
                        aes(label = percent(after_stat(prop), accuracy = .1))) +
              scale_fill_manual(
                values = c("#7570B3", "#E7298A")
              ) +
              scale_color_manual(
                values = c("#7570B3", "#E7298A")
              ) +
              coord_cartesian(xlim = c(1.43, 1.57), ylim = c(0, 75)) +
              theme_bw() +
              theme(
                panel.grid = element_blank(), axis.ticks = element_blank(),
                axis.text.y = element_blank(),
                panel.background = element_rect(color = NA)
              ) +
              labs(x = "Months post-rehabilitation", y = "Number of participants", fill = "Cluster", color = "Cluster") +
              scale_y_continuous(breaks = seq(1, 7, 1), expand = expansion(0))
          }),

          ### Build a data frame with the EMAPS scores deltas associated to the different scenarios ----
          ### (same/different cluster between month 0 and month 12) ----
          tar_target(DB_EMAPS_0_12_diffs,
                       DB_EMAPS_0_12_clust |>
                       select(-n_visits) |>
                       pivot_wider(names_from = MONTH, values_from = c(INTRINSIC:cluster)) |>
                       mutate(
                         diff_INTRINSIC   = INTRINSIC_12 - INTRINSIC_0,
                         diff_INTEGRATED  = INTEGRATED_12 - INTEGRATED_0,
                         diff_IDENTIFIED  = IDENTIFIED_12 - IDENTIFIED_0,
                         diff_INTROJECTED  = INTROJECTED_12 - INTROJECTED_0,
                         diff_EXTERNAL    = EXTERNAL_12 - EXTERNAL_0,
                         diff_AMOTIVATION = AMOTIVATION_12 - AMOTIVATION_0,
                         clus_trans = case_when(
                           cluster_0 == "Very High AU-High C" & cluster_12 == "High AU-Mod C" ~ "From 'Very High AU-High C' to 'High AU-Mod C'",
                           cluster_0 == "High AU-Mod C" & cluster_12 == "Very High AU-High C" ~ "From 'High AU-Mod C' to 'Very High AU-High C'",
                           cluster_0 == cluster_12 ~ "Same cluster"
                         )
                       )
          ),

            ### Build a plot with the EMAPS scores deltas associated to the different scenarios ----
            ### (same/different cluster between month 0 and month 12) ----
            tar_target(p_DB_EMAPS_0_12_diffs,

                       DB_EMAPS_0_12_diffs |>
                         select(c(patient, clus_trans, diff_INTRINSIC:diff_AMOTIVATION)) |>
                         pivot_longer(cols = c(diff_INTRINSIC:diff_AMOTIVATION), names_to = "Motivation", values_to = "Change in score") |>
                         mutate(Motivation = factor(Motivation,
                                                    levels = c("diff_INTRINSIC",
                                                               "diff_INTEGRATED",
                                                               "diff_IDENTIFIED",
                                                               "diff_INTROJECTED",
                                                               "diff_EXTERNAL",
                                                               "diff_AMOTIVATION"),
                                                    labels = c("INTRINSIC",
                                                               "INTEGRATED",
                                                               "IDENTIFIED",
                                                               "INTROJECTED",
                                                               "EXTERNAL",
                                                               "AMOTIVATION"))
                                ) |>
                         ggplot(aes(x = clus_trans, y = `Change in score`)) +
                         ggrain::geom_rain() +
                         geom_jitter(width = 0.05, alpha = 0.5) +
                         coord_flip() +
                         labs(x = "Cluster transition") +
                         facet_wrap(~ Motivation)
                       ),

            ### Compare the EMAPS scores between month 0 and month 12 for participants who stayed ----
            ### in the 'Very High AU-High C' cluster ----
            tar_target(compa_intra_clust_VHAUHC,{

              #### Get participants IDs with in 'Very High AU-High C' cluster both at month 0 and month 12
              id_parts_VHAUHC_0_12 <-
                DB_EMAPS_0_12_diffs |>
                filter(cluster_0 == "Very High AU-High C" & cluster_12 == "Very High AU-High C") |>
                pull(patient)

              #### Intra-cluster comparison for Very High AU-High C
              set.seed(123)
              nonpartest(
                IDENTIFIED |
                  INTROJECTED |
                  EXTERNAL |
                  AMOTIVATION ~ MONTH,
                data = DB_EMAPS_0_12[DB_EMAPS_0_12$patient %in% id_parts_VHAUHC_0_12, ],
                permreps = 1000,
                plots = FALSE
              )
            }),

            ### Compare the EMAPS scores between month 0 and month 12 for participants who stayed ----
            ### in the 'High AU-Mod C' cluster ----
            tar_target(compa_intra_clust_HAUMODC, {

              #### Get participants IDs with in 'High AU-Mod C' cluster both at month 0 and month 12
              id_parts_HAUMODC_0_12 <-
                DB_EMAPS_0_12_diffs |>
                filter(cluster_0 == "High AU-Mod C" & cluster_12 == "High AU-Mod C") |>
                pull(patient)

              ##### Intra-cluster comparison for High AU-Mod C
              set.seed(123)
              nonpartest(
                IDENTIFIED |
                  INTROJECTED |
                  EXTERNAL |
                  AMOTIVATION ~ MONTH,
                data = DB_EMAPS_0_12[DB_EMAPS_0_12$patient %in% id_parts_HAUMODC_0_12, ],
                permreps = 1000,
                plots = FALSE
              )
            }

      ),

  ### Compare the EMAPS scores between month 0 and month 12 for participants who ----
  ### transited from the 'Very High AU-High C' to the 'High AU-Mod C' cluster ----
  tar_target(compa_intra_clust_VHAUHC_to_HAUMODC, {

    #### Get participants IDs with in 'Very High AU-High C' cluster at month 0 and
    #### 'High AU-Mod  C' cluster at month 12
    id_parts_less_motivated_0_12 <-
      DB_EMAPS_0_12_diffs |>
      filter(cluster_0 == "Very High AU-High C" & cluster_12 == "High AU-Mod C") |>
      pull(patient)

    ##### Time points comparison
    set.seed(123)
    nonpartest(
      IDENTIFIED |
        INTROJECTED |
        EXTERNAL |
        AMOTIVATION ~ MONTH,
      data = DB_EMAPS_0_12[DB_EMAPS_0_12$patient %in% id_parts_less_motivated_0_12, ],
      permreps = 1000,
      plots = FALSE
    )
  }

  ),

  ### Compare the EMAPS scores between month 0 and month 12 for participants who ----
  ### transited from the 'High AU-Mod C' to the 'Very High AU-High C' cluster ----
  tar_target(compa_intra_clust_HAUMODCto_VHAUHC, {

    #### Get participants IDs with in 'High AU-Mod C' cluster at month 0 and
    #### 'Very High AU-High  C' cluster at month 12
    id_parts_more_motivated_0_12 <-
      DB_EMAPS_0_12_diffs |>
      filter(cluster_0 == "High AU-Mod C" & cluster_12 == "Very High AU-High C") |>
      pull(patient)

    ##### Time point comparison
    set.seed(123)
    nonpartest(
      IDENTIFIED |
        INTROJECTED |
        EXTERNAL |
        AMOTIVATION ~ MONTH,
      data = DB_EMAPS_0_12[DB_EMAPS_0_12$patient %in% id_parts_more_motivated_0_12, ],
      permreps = 1000,
      plots = FALSE
    )
  }

  ),


  # Get a data frame with the changes in 6MWT distance in wide format ----
  tar_target(DB_6MWT_0_12_wide,
    DB_6MWT_0_12 |>
      pivot_wider(names_from = MONTH, values_from = DIST_M) |>
      mutate(change_6MWT_0_12 = `12` - `0`)
  ),

  # Get a data frame with the changes in IPAQ MET-min in wide format ----
  tar_target(DB_IPAQ_6_12_wide,
    DB_IPAQ_6_12 |>
    pivot_wider(names_from = MONTH, values_from = MET_MIN_WK) |>
    mutate(change_IPAQ_6_12 = `12` - `6`)
  ),

  # Get a data frame with the changes in EMAPS scores in wide format ----
  tar_target(DB_EMAPS_0_12_wide,
    DB_EMAPS_0_12 |>
      pivot_wider(names_from = MONTH, values_from = c(INTRINSIC:AMOTIVATION)) |>
      mutate(
        change_EMAPS_0_12_INTRINSIC = INTRINSIC_12 - INTRINSIC_0,
        change_EMAPS_0_12_INTEGRATED = INTEGRATED_12 - INTEGRATED_0,
        change_EMAPS_0_12_IDENTIFIED = IDENTIFIED_12 - IDENTIFIED_0,
        change_EMAPS_0_12_INTROJECTED = INTROJECTED_12 - INTROJECTED_0,
        change_EMAPS_0_12_EXTERNAL = EXTERNAL_12 - EXTERNAL_0,
        change_EMAPS_0_12_AMOTIVATION = AMOTIVATION_12 - AMOTIVATION_0
        )
  ),




  # Analyse barriers to physical activities at 12 months ----
  tar_target(analysis_BARRIERS, BARRIERS_cleaned |> skimr::skim()),
  tar_target(p_BARRIERS, get_plot_BARRIERS(BARRIERS_cleaned)),

  # Analyse barriers to physical activities at 12 months ----
  # by 6MWT change (0-12 months) decile ----
  tar_target(p_BARRIERS_BY_6MWT_DECILE, {

    ## Get data frame with the changes in 6MWT distance
    df <- DB_6MWT_0_12_wide

    ## Get the deciles of the changes in 6MWT distance
    deciles_change_6MWT <- quantile(
      x = df$change_6MWT_0_12,
      probs = seq(0, 1, 0.1)
    )

    ## Add decile categories to the initial dataset
    df$decile_change <-
      cut(
        df$change_6MWT_0_12,
        deciles_change_6MWT,
        include.lowest = T,
        labels = F
      )

    ## Plot positive response rate to PA barriers questionnaire
    ## by decile of 6MWT change
    df |>
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
      labs(x = "Decile of the change in 6MWT distance",
           y = "% of patients evocating the barrier to physical activity") +
      scale_y_continuous(limits = c(0, 100)) +
      scale_x_continuous(breaks = seq(1, 10, 1)) +
      facet_wrap( ~ barrier)
  }
  ),

  # Analyse barriers to physical activities at 12 months ----
  # by IPAQ change (6-12 months) decile ----
  tar_target(p_BARRIERS_BY_IPAQ_DECILE, {

    ## Get data frame
    df <- DB_IPAQ_6_12_wide

    ## Get the deciles of the changes in MET_MIN_WK
    deciles_change_IPAQ_6_12 <-
      quantile(x = df$change_IPAQ_6_12, probs = seq(0, 1, 0.1))

    ## Add decile categories to the initial dataset
    df$decile_change <-
      cut(
        df$change_IPAQ_6_12,
        deciles_change_IPAQ_6_12,
        include.lowest = T,
        labels = F
      )

    ## Plot response rate to PA barriers questionnaire by decile of MET_MIN_WK
    df |>
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
      facet_wrap(~ barrier)
  }
  ),

  # Get an appropriate data frame  for analysing predictors of the changes in ----
  # exercise capacity and physical activity ----
  tar_target(DB_ALL_VARS,
    INCLUSION_cleaned |>
      left_join(
        DB_IPAQ |>
          filter(MONTH == 6) |>
          rename(MET_MIN_WK_M6 = MET_MIN_WK) |>
          select(patient, MET_MIN_WK_M6, -MONTH)
      ) |>
      left_join(
        DB_EMAPS |>
          ungroup() |>
          filter(MONTH == 0) |>
          select(patient, INTRINSIC:AMOTIVATION, -MONTH) |>
          rename_at(
            vars(
              "INTRINSIC",
              "INTEGRATED",
              "IDENTIFIED",
              "INTROJECTED",
              "EXTERNAL",
              "AMOTIVATION"
            ),
            ~ paste0(
              c(
                "INTRINSIC",
                "INTEGRATED",
                "IDENTIFIED",
                "INTROJECTED",
                "EXTERNAL",
                "AMOTIVATION"
              ),
              "_M0"
            )
          )
      ) |>
      left_join(
        DB_EMAPS |>
          ungroup() |>
          filter(MONTH == 12) |>
          select(patient, INTRINSIC:AMOTIVATION, -MONTH) |>
          rename_at(
            vars(
              "INTRINSIC",
              "INTEGRATED",
              "IDENTIFIED",
              "INTROJECTED",
              "EXTERNAL",
              "AMOTIVATION"
            ),
            ~ paste0(
              c(
                "INTRINSIC",
                "INTEGRATED",
                "IDENTIFIED",
                "INTROJECTED",
                "EXTERNAL",
                "AMOTIVATION"
              ),
              "_M12"
            )
          )
      ) |>
      left_join(DB_6MWT_0_12_wide |> select(patient, change_6MWT_0_12)) |>
      left_join(DB_IPAQ_6_12_wide |> select(patient, change_IPAQ_6_12)) |>
      left_join(DB_EMAPS_0_12_wide |> select(patient, change_EMAPS_0_12_INTRINSIC:change_EMAPS_0_12_AMOTIVATION)
      ) |>
      left_join(BARRIERS_cleaned |> select(patient:isolement_faible_RS)) |>
      select(patient:bypass, BMI, everything()) |>
      rename(
        "Unfavourable weather" = meteo_defavorable,
        "Lack of time" = manque_temps,
        "Heavy effort / too tired" = effort_import_fatig,
        "Fear of injury / pain" = crainte_blessures_douleurs,
        "Lack of interest" = manque_interet,
        "Difficulty to move" = deplacements_diff,
        "Too old" = trop_vieux,
        "Social isolation / weak social network" = isolement_faible_RS,
        "Too costly" = cout_trop_eleve
      )
    ),

  # Export figures 1, 2, and 3 ----
  tar_target(fig1, save_figure("pipeline_output/fig1.tiff", change_6MWT$p, width = 21), format = "file"),
  tar_target(fig2, save_figure("pipeline_output/fig2.tiff", change_IPAQ_6_12$p, scaling = 0.40, width = 21), format = "file"),
  tar_target(fig3, save_figure("pipeline_output/fig3.tiff", p_BARRIERS, scaling = 0.3, height = 5, width = 10), format = "file"),

  # Build report including main results ----
  tar_render(main, "main.Rmd", output_dir = "pipeline_output/"),

  # Build Supplemental Material 3 ----
  tar_target(p_6MWT_all, get_plot_6MWT_all(DB_6MWT)),
  tar_target(p_IPAQ_all, get_plot_IPAQ_all(DB_IPAQ)),
  tar_target(p_EMAPS_all, get_plot_EMAPS_all(DB_EMAPS)),
  tar_target(table_all_desc_stat, get_table_all_measurements(DB_6MWT, DB_IPAQ, DB_EMAPS)),
  tar_render(SM3, "SM3.Rmd", output_dir = "pipeline_output/"),

  # Build Supplemental Material 4 ----
  tar_render(SM4, "SM4.Rmd", output_dir = "pipeline_output/"),

  # Build Supplemental Material 5 ----
  tar_render(SM5, "SM5.Rmd", output_dir = "pipeline_output/"),

  # Build Supplemental Material 6 ----
  tar_render(SM6, "SM6.Rmd", output_dir = "pipeline_output/"),

  # Build Supplemental Material 7 ----
  tar_render(SM7, "SM7.Rmd", output_dir = "pipeline_output/")

)


