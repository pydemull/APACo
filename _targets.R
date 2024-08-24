
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
    "Hmisc",
    "lcmm",
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
  tar_target(BARRIERS_cleaned, BARRIERS |> mutate(across(patient:autres, as.factor))),

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

  # Analyse change in motivational profile for physical activity ----
  # (clustering approach) between 0 and 12 months ----

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
                fviz_dist(dist(DB_EMAPS_0_12_M0_scaled), show_labels = FALSE) +
                labs(title = "Month 0")

              ### Make visualization for month 12
              p_clus_tend_month12 <-
                fviz_dist(dist(DB_EMAPS_0_12_M12_scaled), show_labels = FALSE) +
                labs(title = "Month 12")

              ### Get final figure
              (p_clus_tend_month0 | p_clus_tend_month12) &
                theme(legend.position = "bottom")
            }),

        ## Determine the optimal number of clusters for K-Medoid approach using ----
        ## the silhouette method ----
        tar_target(optim_n_clusters, {

              ### Build the plot to determine the optimal number of clusters
              ### for month 0
              p_silhouette_emaps_m0 <-
                fviz_nbclust(DB_EMAPS_0_12_M0_scaled, pam,
                             method = "silhouette") +
                theme_classic()

              ### Build the plot to determine the optimal number of clusters
              ### for month 12
              p_silhouette_emaps_m12 <-
                fviz_nbclust(DB_EMAPS_0_12_M12_scaled, pam,
                             method = "silhouette") +
                theme_classic()

              ### Build figure
              p_silhouette_emaps_m0 | p_silhouette_emaps_m12
            }),

        ## Determine the final clusters using K-Medoid approach ----

            ### Final clusters at Month 0
            tar_target(final_clusters_emaps_m0, {
              set.seed(123); pam(x = DB_EMAPS_0_12_M0_scaled, k = 2,
                                 metric = "euclidean")
              }),

            ### Final clusters at Month 12
            tar_target(final_clusters_emaps_m12, {
              set.seed(123); pam(x = DB_EMAPS_0_12_M12_scaled, k = 2,
                                 metric = "euclidean")
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
                  plot_layout(guides = "collect") &
                  theme(legend.position = "none")
            }),

        ## Assign clusters IDs to participants ----
        tar_target(DB_EMAPS_0_12_clust, {

          ### Create the vector of clusters IDs for both month 0 and 12
          clusters_emaps <-
            c(
              final_clusters_emaps_m0$cluster,
              final_clusters_emaps_m12$cluster
              )

          ### Add a column with clusters IDs to the DB_EMAPS_0_12 data frame
          DB_EMAPS_0_12_clust <-
            cbind(
              DB_EMAPS_0_12 |> arrange(MONTH),
              cluster = clusters_emaps
              ) |>
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

          ## Visualize the EMAPS descriptive statistics related the motivational ----
          ## profiles ----
          tar_target(
            p_emaps_clust,
            DB_EMAPS_0_12_clust_piv |>
              mutate(
                cluster = factor(cluster,
                                 levels = c("Very High AU-High C", "High AU-Mod C")),
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
              theme(
                legend.position = "right",
                axis.text.x = element_text(angle = 45, hjust = 1)
                ) +
              facet_grid(cluster ~ MONTH, switch = "y")
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
              mutate(
                perc = janitor::round_half_up(N / sum(N) * 100, 1),
                N_perc = paste0(N, " (", perc, ")")
                ) |>
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

          ## Compare clusters using multivariate analysis at month 0 ----
          ## and then at month 12 ----

                  ### Group comparison at Month 0: detecting a global difference ----
                  tar_target(nonpartest_global_month0,
                  nonpartest(
                      INTRINSIC   |
                      INTEGRATED  |
                      IDENTIFIED  |
                      INTROJECTED |
                      EXTERNAL    |
                      AMOTIVATION ~ cluster,
                    data = DB_EMAPS_0_12_clust |> filter(MONTH == "0"),
                    permreps = 1000,
                    plots = FALSE
                  )
                ),
                ### Group comparison at Month 0: detecting the localisation ----
                ### of the difference(s) ----
                  tar_target(
                    nonpartest_local_month0,
                    ssnonpartest(
                        INTRINSIC   |
                        INTEGRATED  |
                        IDENTIFIED  |
                        INTROJECTED |
                        EXTERNAL    |
                        AMOTIVATION ~ cluster,
                      data = DB_EMAPS_0_12_clust |> filter(MONTH == "0"),
                      test = c(1, 0, 0, 0),
                      alpha = 0.05,
                      factors.and.variables = TRUE
                    )
                  ),
                  ### Group comparison at Month 12: detecting a global difference ----
                  tar_target(nonpartest_global_month12,
                             nonpartest(
                                 INTRINSIC   |
                                 INTEGRATED  |
                                 IDENTIFIED  |
                                 INTROJECTED |
                                 EXTERNAL    |
                                 AMOTIVATION ~ cluster,
                               data = DB_EMAPS_0_12_clust |> filter(MONTH == "12"),
                               permreps = 1000,
                               plots = FALSE
                             )
                  ),
                  ### Group comparison at Month 12: detecting the localisation ----
                  ### of the difference(s) ----
                  tar_target(
                    nonpartest_local_month12,
                    ssnonpartest(
                        INTRINSIC   |
                        INTEGRATED  |
                        IDENTIFIED  |
                        INTROJECTED |
                        EXTERNAL    |
                        AMOTIVATION ~ cluster,
                      data = DB_EMAPS_0_12_clust |> filter(MONTH == "12"),
                      test = c(1, 0, 0, 0),
                      alpha = 0.05,
                      factors.and.variables = TRUE
                    )
                  ),

      ## Explore the change in motivational profile ----

          ### Build an alluvial plot to show the proportion of profile changes
          tar_target(p_change_emaps_profile_alluvial, {
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
              geom_text(stat = "stratum", size = 4,
                        aes(label = percent(after_stat(prop), accuracy = .1))) +
              scale_fill_manual(
                values = c("#7570B3", "#E7298A")
              ) +
              scale_color_manual(
                values = c("#7570B3", "#E7298A")
              ) +
              scale_y_continuous(breaks = seq(0, 75, 25), expand = expansion(0)) +
              coord_cartesian(xlim = c(1.43, 1.57), ylim = c(0, 76)) +
              theme_bw() +
              theme(
                panel.grid = element_blank(),
                panel.background = element_rect(color = NA),
                panel.border = element_blank()
              ) +
              labs(
                x = "Months post-rehabilitation",
                y = "Number of participants",
                fill = "Cluster",
                color = "Cluster"
                )
          }),

          ### Build a data frame with the EMAPS scores deltas associated to the ----
          ### different scenarios (same/different clusters between month 0 and ----
          ### month 12) ----
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

            ### Compute the proportions of patients per profile transition scenario ----
            tar_target(prop_trans_prof_motiv,
                       questionr::freq(DB_EMAPS_0_12_diffs$clus_trans) |>
                         tibble::rownames_to_column("clust_trans") |>
                         mutate(n_tot = sum(n)) |>
                         group_by(n) |>
                         mutate(
                           prop_estimate = purrr::map2_dbl(n, n_tot, function(n, n_tot) {
                             ((
                               binom.test(
                                 n,
                                 n_tot,
                                 0.5,
                                 alternative = "two.sided",
                                 conf.level = 0.95
                               )
                             )$estimate[[1]]) * 100
                           }),
                           prop_ci_low = purrr::map2_dbl(n, n_tot, function(n, n_tot) {
                             ((
                               binom.test(
                                 n,
                                 n_tot,
                                 0.5,
                                 alternative = "two.sided",
                                 conf.level = 0.95
                               )
                             )$conf.int[[1]]) * 100
                           }),
                           prop_ci_up = purrr::map2_dbl(n, n_tot, function(n, n_tot) {
                             ((
                               binom.test(
                                 n,
                                 n_tot,
                                 0.5,
                                 alternative = "two.sided",
                                 conf.level = 0.95
                               )
                             )$conf.int[[2]]) * 100
                           })
                         )
                       ),

            ### Build a plot with the EMAPS scores deltas associated to the ----
            ### different scenari (same/different cluster between month 0 and ----
            ### month 12) ----
            tar_target(p_DB_EMAPS_0_12_diffs,

                       DB_EMAPS_0_12_diffs |>
                         select(c(patient, clus_trans, diff_INTRINSIC:diff_AMOTIVATION)) |>
                         pivot_longer(
                           cols = c(diff_INTRINSIC:diff_AMOTIVATION),
                           names_to = "Motivation",
                           values_to = "Change in score (Month 12 - Month 0)"
                           ) |>
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
                         ggplot(aes(x = clus_trans, y = `Change in score (Month 12 - Month 0)`)) +
                         geom_hline(aes(yintercept = 0), linetype = "dashed") +
                         ggrain::geom_rain() +
                         geom_jitter(width = 0.05, alpha = 0.5) +
                         coord_flip() +
                         labs(x = "Profile transition") +
                         facet_wrap(~ Motivation)
                       ),

            ### Compare the EMAPS scores between month 0 and month 12 for ----
            ### participants who stayed in the 'Very High AU-High C' cluster ----
            ### Of note, this analysis violates the assumption of multivariate
            ### observation vectors independance
            tar_target(compa_intra_clust_VHAUHC,{

              #### Get participants IDs in the 'Very High AU-High C' cluster
              #### both at month 0 and month 12
              id_parts_VHAUHC_0_12 <-
                DB_EMAPS_0_12_diffs |>
                filter(cluster_0 == "Very High AU-High C" & cluster_12 == "Very High AU-High C") |>
                pull(patient)

              #### Intra-cluster comparison for Very High AU-High C
              set.seed(123)
              nonpartest(
                  INTRINSIC   |
                  INTEGRATED  |
                  IDENTIFIED  |
                  INTROJECTED |
                  EXTERNAL    |
                  AMOTIVATION ~ MONTH,
                data = DB_EMAPS_0_12[DB_EMAPS_0_12$patient %in% id_parts_VHAUHC_0_12, ],
                permreps = 1000,
                plots = FALSE
              )
            }),

            ### Compare the EMAPS scores between month 0 and month 12 for ----
            ### participants who stayed in the 'High AU-Mod C' cluster ----
            ### Of note, this analysis violates the assumption of multivariate
            ### observation vectors independance
            tar_target(compa_intra_clust_HAUMODC, {

              #### Get participants IDs in the 'High AU-Mod C' cluster both at
              #### month 0 and month 12
              id_parts_HAUMODC_0_12 <-
                DB_EMAPS_0_12_diffs |>
                filter(cluster_0 == "High AU-Mod C" & cluster_12 == "High AU-Mod C") |>
                pull(patient)

              ##### Intra-cluster comparison for High AU-Mod C
              set.seed(123)
              nonpartest(
                  INTRINSIC   |
                  INTEGRATED  |
                  IDENTIFIED  |
                  INTROJECTED |
                  EXTERNAL    |
                  AMOTIVATION ~ MONTH,
                data = DB_EMAPS_0_12[DB_EMAPS_0_12$patient %in% id_parts_HAUMODC_0_12, ],
                permreps = 1000,
                plots = FALSE
              )
            }

      ),

  ### Compare the EMAPS scores between month 0 and month 12 for participants who ----
  ### transited from the 'Very High AU-High C' to the 'High AU-Mod C' cluster ----
  ### Of note, this analysis violates the assumption of multivariate
  ### observation vectors independance
  tar_target(compa_intra_clust_VHAUHC_to_HAUMODC, {

    #### Get participants IDs in the 'Very High AU-High C' cluster at month 0 and
    #### 'High AU-Mod  C' cluster at month 12
    id_parts_less_motivated_0_12 <-
      DB_EMAPS_0_12_diffs |>
      filter(cluster_0 == "Very High AU-High C" & cluster_12 == "High AU-Mod C") |>
      pull(patient)

    ##### Time points comparison
    set.seed(123)
    nonpartest(
        INTRINSIC   |
        INTEGRATED  |
        IDENTIFIED  |
        INTROJECTED |
        EXTERNAL    |
        AMOTIVATION ~ MONTH,
      data = DB_EMAPS_0_12[DB_EMAPS_0_12$patient %in% id_parts_less_motivated_0_12, ],
      permreps = 1000,
      plots = FALSE
    )
  }

  ),

  ### Compare the EMAPS scores between month 0 and month 12 for participants who ----
  ### transited from the 'High AU-Mod C' to the 'Very High AU-High C' cluster ----
  ### Of note, this analysis violates the assumption of multivariate
  ### observation vectors independance
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
        INTRINSIC   |
        INTEGRATED  |
        IDENTIFIED  |
        INTROJECTED |
        EXTERNAL    |
        AMOTIVATION ~ MONTH,
      data = DB_EMAPS_0_12[DB_EMAPS_0_12$patient %in% id_parts_more_motivated_0_12, ],
      permreps = 1000,
      plots = FALSE
    )
  }

  ),

  # Analyse barriers to physical activities at 12 months ----
  tar_target(analysis_BARRIERS, BARRIERS_cleaned |> skimr::skim()),
  tar_target(p_BARRIERS, get_plot_BARRIERS(BARRIERS_cleaned)),


  # Latent class mixed modeling to analyse the type of change in 6MWT distance ----

  ## Get an appropriate data frame for analysing the predictors of the changes in ----
  ## exercise capacity ----
  tar_target(
    DB_PRED_6MWT_0_12,
    DB_6MWT_0_12 |>
      rename(DIST_6MWT = DIST_M) |>
      left_join(
        DB_IPAQ_0_12 |>
          filter(MONTH == "0") |>
          rename(MET_MIN_WK_M0 = MET_MIN_WK) |>
          select(patient, MET_MIN_WK_M0)
      ) |>
      left_join(
        DB_EMAPS_0_12_clust |>
          filter(MONTH == "0") |>
          rename(MOTIVATION_CLUSTER_M0 = cluster) |>
          select(patient, MOTIVATION_CLUSTER_M0)
      ) |>
      left_join(INCLUSION_cleaned |> rename(DIST_6MWT_M0 = DIST_6WT_M0)) |>
      left_join(BARRIERS_cleaned |> select(patient:isolement_faible_RS)) |>
      select(
        patient,
        MONTH,
        sex,
        age,
        angioplasty,
        bypass,
        BMI,
        DIST_6MWT_M0,
        MET_MIN_WK_M0,
        MOTIVATION_CLUSTER_M0,
        c(trop_vieux:isolement_faible_RS),
        DIST_6MWT
      ) |>
      mutate(
        patient = as.integer(patient),
        MONTH = as.numeric(as.character(MONTH)),
        MOTIVATION_CLUSTER_M0 = as.factor(MOTIVATION_CLUSTER_M0)
      ) |>
      as.data.frame()
  ),

  ## Estimate the initial model ----
  tar_target(
    model_6MWT_n1,
    hlme(
      fixed = DIST_6MWT ~ MONTH,
      random = ~ 1,
      subject = "patient",
      var.time = "MONTH",
      data = DB_PRED_6MWT_0_12,
      ng = 1
    )
  ),

  ## Estimate the model with 2 classes ----
  tar_target(model_6MWT_n2, {
    set.seed(123)
    gridsearch(
      hlme(
        fixed = DIST_6MWT ~ MONTH,
        random = ~ 1,
        subject = "patient",
        var.time = "MONTH",
        data = DB_PRED_6MWT_0_12,
        ng = 2,
        mixture = ~ MONTH
      ),
      rep = 100,
      maxiter = 30,
      minit = model_6MWT_n1
    )
  }),

  ## Estimate the model with 3 classes ----
  tar_target(model_6MWT_n3, {
    set.seed(123)
    gridsearch(
      hlme(
        fixed = DIST_6MWT ~ MONTH,
        random = ~ 1,
        subject = "patient",
        var.time = "MONTH",
        data = DB_PRED_6MWT_0_12,
        ng = 3,
        mixture = ~ MONTH
      ),
      rep = 100,
      maxiter = 30,
      minit = model_6MWT_n1
    )
  }),

  ## Estimate the model with 4 classes ----
  tar_target(model_6MWT_n4, {
    set.seed(123)
    gridsearch(
      hlme(
        fixed = DIST_6MWT ~ MONTH,
        random = ~ 1,
        subject = "patient",
        var.time = "MONTH",
        data = DB_PRED_6MWT_0_12,
        ng = 4,
        mixture = ~ MONTH
      ),
      rep = 100,
      maxiter = 30,
      minit = model_6MWT_n1
    )
  }),

  ## Estimate the model with 5 classes ----
  tar_target(model_6MWT_n5, {
    set.seed(123)
    gridsearch(
      hlme(
        fixed = DIST_6MWT ~ MONTH,
        random = ~ 1,
        subject = "patient",
        var.time = "MONTH",
        data = DB_PRED_6MWT_0_12,
        ng = 5,
        mixture = ~ MONTH
      ),
      rep = 100,
      maxiter = 30,
      minit = model_6MWT_n1
    )
  }),

  ## Compare latent class mixed models ----
  tar_target(
    compa_latent_mixed_models_table_6MWT,
    summarytable(
      model_6MWT_n1,
      model_6MWT_n2,
      model_6MWT_n3,
      model_6MWT_n4,
      model_6MWT_n5,
      which = c("AIC", "BIC", "entropy", "%class")
    )
  ),

  ## Reorder the latent classes of the chosen model (3-class model) ----
  tar_target(model_6MWT_n3_permut, {

    ### For unknown reasons, we had to explicitly assign the DB_PRED_6MWT_0_12
    ### object to the parent environment of the function so that the permut() function works
    assign("DB_PRED_6MWT_0_12", DB_PRED_6MWT_0_12, env = parent.frame())
    permut(model_6MWT_n3, order = c(3,1,2))
  }),

  ## Make a plot with the predicted trajectories for 6MWT distance ----
  tar_target(plot_preds_6MWT, {

    # Compute predictions for MONTH effect per latent class
    newdat_6MWT <-
      (predictY(
        model_6MWT_n3_permut,
        data.frame(MONTH = c(0, 12)),
        var.time = "MONTH",
        draws = TRUE
      )[1]) |>
      as.data.frame() |>
      mutate(MONTH = c(0, 12)) |>
      pivot_longer(
        cols = c(everything(), -MONTH),
        names_to = c("lines", "class"),
        names_pattern = "(.*)_(.*)",
        values_to = "pred"
      ) |>
      pivot_wider(names_from = lines, values_from = pred)

     # Rename classes
     newdat_6MWT$class <- factor(newdat_6MWT$class,
                                 labels = c("Class 1", "Class 2", "Class 3")
                                 )


      # Make plot
       ggplot() +
         geom_line(data = DB_PRED_6MWT_0_12 |>
                     inner_join(model_6MWT_n3_permut$pprob) |>
                     mutate(
                       class = as.factor(paste0("class", class)),
                       class = forcats::fct_recode(class,
                                                   "Class 1" = "class1",
                                                   "Class 2" = "class2",
                                                   "Class 3" = "class3"
                       )
                     ),
                   aes(
                     x = MONTH,
                     y = DIST_6MWT,
                     group = patient,
                     linetype = "Participant trajectories"
                     ),
                   color = "grey", alpha = 0.5) +
      geom_ribbon(
           data = newdat_6MWT,
           aes(
             x = MONTH,
             ymin = pred.lower.Ypred,
             ymax = pred.upper.Ypred,
             fill = class,
             group = class
           ),
           alpha = 0.3
         ) +
         geom_line(data = newdat_6MWT, aes(
           x = MONTH,
           y = pred.Ypred,
           color = class,
           group = class
         )) +
         scale_x_continuous(breaks = seq(0, 12, 12)) +
         facet_wrap( ~ class) +
         labs(
           x = "Months post-program",
           y = "Six-min walking test distance (m)",
           color = "Predictions [95% CI]",
           fill = "Predictions [95% CI]",
           linetype = "Posterior classification"
         ) +
         theme_bw()
  }),

  ## Assess potential predictors of the latent classes ----
  tar_target(
    predictors_6MWT_classes,
    externVar(
      model = model_6MWT_n3_permut,
      classmb = ~  DIST_6MWT_M0 + MET_MIN_WK_M0 + MOTIVATION_CLUSTER_M0 +
        meteo_defavorable + manque_temps,
      subject = "patient",
      data = DB_PRED_6MWT_0_12,
      method = "twoStageJoint"
    )
  ),

  # Latent class mixed modeling to analyse the type of change in IPAQ MET-min/wk ----

  # Get an appropriate data frame for analysing the predictors of the changes in ----
  # physical activity ----
  tar_target(
    DB_PRED_IPAQ_6_12,
    DB_IPAQ_6_12 |>
      left_join(
        DB_IPAQ_0_12 |>
          filter(MONTH == "0") |>
          rename(MET_MIN_WK_M0 = MET_MIN_WK) |>
          select(patient, MET_MIN_WK_M0)
      ) |>
      left_join(
        DB_EMAPS_0_12_clust |>
          filter(MONTH == "0") |>
          rename(MOTIVATION_CLUSTER_M0 = cluster) |>
          select(patient, MOTIVATION_CLUSTER_M0)
      ) |>
      left_join(INCLUSION_cleaned |> rename(DIST_6MWT_M0 = DIST_6WT_M0)) |>
      left_join(BARRIERS_cleaned |> select(patient:isolement_faible_RS)) |>
      select(
        patient,
        MONTH,
        sex,
        age,
        angioplasty,
        bypass,
        BMI,
        DIST_6MWT_M0,
        MET_MIN_WK_M0,
        MOTIVATION_CLUSTER_M0,
        c(trop_vieux:isolement_faible_RS),
        MET_MIN_WK
      ) |>
      mutate(
        patient = as.integer(patient),
        MONTH = as.numeric(as.character(MONTH)),
        MOTIVATION_CLUSTER_M0 = as.factor(MOTIVATION_CLUSTER_M0)
      ) |>
      as.data.frame()
  ),

  ## Estimate the initial model ----
  tar_target(
    model_IPAQ_n1,
    hlme(
      fixed = MET_MIN_WK ~ MONTH,
      random = ~ 1,
      subject = "patient",
      var.time = "MONTH",
      data = DB_PRED_IPAQ_6_12,
      ng = 1
    )
  ),

  ## Estimate the model with 2 classes ----
  tar_target(model_IPAQ_n2, {
    set.seed(123)
    gridsearch(
      hlme(
        fixed = MET_MIN_WK ~ MONTH,
        random = ~ 1,
        subject = "patient",
        var.time = "MONTH",
        data = DB_PRED_IPAQ_6_12,
        ng = 2,
        mixture = ~ MONTH
      ),
      rep = 100,
      maxiter = 30,
      minit = model_IPAQ_n1
    )
  }),

  ## Estimate the model with 3 classes ----
  tar_target(model_IPAQ_n3, {
    set.seed(123)
    gridsearch(
      hlme(
        fixed = MET_MIN_WK ~ MONTH,
        random = ~ 1,
        subject = "patient",
        var.time = "MONTH",
        data = DB_PRED_IPAQ_6_12,
        ng = 3,
        mixture = ~ MONTH
      ),
      rep = 100,
      maxiter = 30,
      minit = model_IPAQ_n1
    )
  }),

  ## Estimate the model with 4 classes ----
  tar_target(model_IPAQ_n4, {
    set.seed(123)
    gridsearch(
      hlme(
        fixed = MET_MIN_WK ~ MONTH,
        random = ~ 1,
        subject = "patient",
        var.time = "MONTH",
        data = DB_PRED_IPAQ_6_12,
        ng = 4,
        mixture = ~ MONTH
      ),
      rep = 100,
      maxiter = 30,
      minit = model_IPAQ_n1
    )
  }),

  ## Estimate the model with classes ----
  tar_target(model_IPAQ_n5, {
    set.seed(123)
    gridsearch(
      hlme(
        fixed = MET_MIN_WK ~ MONTH,
        random = ~ 1,
        subject = "patient",
        var.time = "MONTH",
        data = DB_PRED_IPAQ_6_12,
        ng = 5,
        mixture = ~ MONTH
      ),
      rep = 100,
      maxiter = 30,
      minit = model_IPAQ_n1
    )
  }),

  ## Compare latent class mixed models ----
  tar_target(
    compa_latent_mixed_models_table_IPAQ,
    summarytable(
      model_IPAQ_n1,
      model_IPAQ_n2,
      model_IPAQ_n3,
      model_IPAQ_n4,
      model_IPAQ_n5,
      which = c("AIC", "BIC", "entropy", "%class")
    )
  ),


  ## Make a plot with the predicted trajectories for IPAQ MET-min/wk ----
  tar_target(plot_preds_IPAQ, {

    # Compute predictions for MONTH effect per latent class
    newdat_IPAQ <-
      (predictY(
        model_IPAQ_n2,
        data.frame(MONTH = c(0, 12)),
        var.time = "MONTH",
        draws = TRUE
      )[1]) |>
      as.data.frame() |>
      mutate(MONTH = c(6, 12)) |>
      pivot_longer(
        cols = c(everything(), -MONTH),
        names_to = c("lines", "class"),
        names_pattern = "(.*)_(.*)",
        values_to = "pred"
      ) |>
      pivot_wider(names_from = lines, values_from = pred)

    # Rename classes
    newdat_IPAQ$class <- factor(newdat_IPAQ$class,
                                labels = c("Class 1", "Class 2")
    )

    # Make plot
    ggplot() +
      geom_line(data = DB_PRED_IPAQ_6_12 |>
                  inner_join(model_IPAQ_n2$pprob) |>
                  mutate(
                    class = as.factor(paste0("class", class)),
                    class = forcats::fct_recode(class,
                                                "Class 1" = "class1",
                                                "Class 2" = "class2"
                    )
                  ),
                aes(
                  x = MONTH,
                  y = MET_MIN_WK,
                  group = patient,

                  linetype = "Participant trajectories"),
                color = "grey",
                alpha = 0.5
                ) +
      geom_ribbon(data = newdat_IPAQ,
                  aes(x = MONTH, ymin = pred.lower.Ypred, ymax = pred.upper.Ypred,
                      fill = class, group = class), alpha = 0.3) +
      geom_line(data = newdat_IPAQ,
                aes(x = MONTH, y = pred.Ypred, color = class, group = class)) +
      scale_x_continuous(breaks = seq(6, 12, 6)) +
      facet_wrap(~ class) +
      labs(x = "Months post-program",
           y = "IPAQ-SF MET-min/week",
           color = "Predictions [95% CI]",
           fill = "Predictions [95% CI]",
           linetype = "Posterior classification"
           ) +
    theme_bw()

  }),

  ## Asses potential predictors of the latent classes ----
  tar_target(
    predictors_IPAQ_classes,
    externVar(
      model = model_IPAQ_n2,
      classmb = ~  DIST_6MWT_M0 + MET_MIN_WK_M0 + MOTIVATION_CLUSTER_M0 +
        meteo_defavorable + manque_temps,
      subject = "patient",
      data = DB_PRED_IPAQ_6_12,
      method = "twoStageJoint"
    )
  ),

  # Export figures ----
  tar_target(fig1, save_figure("pipeline_output/fig1.tiff", change_6MWT$p, width = 21), format = "file"),
  tar_target(fig2, save_figure("pipeline_output/fig2.tiff", change_IPAQ_6_12$p, scaling = 0.40, width = 21), format = "file"),
  tar_target(fig3, save_figure("pipeline_output/fig3.tiff",
                               p_emaps_clust / p_change_emaps_profile_alluvial +
                                 plot_layout(heights = c(3, 2)) +
                                 plot_annotation(tag_levels = 'A'),
                               scaling = 1, height = 20, width = 20), format = "file"
  ),tar_target(fig4, save_figure("pipeline_output/fig4.tiff", p_BARRIERS, scaling = 0.3, height = 5, width = 10), format = "file"),

  tar_target(fig5, save_figure("pipeline_output/fig5.tiff", (plot_preds_6MWT / plot_preds_IPAQ) +
                                 plot_layout(heights = c(1, 1)) +
                                 plot_annotation(tag_levels = 'A'),
               scaling = 0.6, height = 10, width = 10), format = "file"
),

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
  tar_render(SM7, "SM7.Rmd", output_dir = "pipeline_output/"),

  # Build Supplemental Material 8 ----
  tar_render(SM8, "SM8.Rmd", output_dir = "pipeline_output/")

)


