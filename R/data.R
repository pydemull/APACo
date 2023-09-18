
#' Physical, clinical, and functional characteristics at the inclusion visit
#'
#' Participants data relating to the physical, clinical, and functional profiles
#'     at the end of the cardiac rehabilitation program.
#'
#' @format ## `INCLUSION`
#' A data frame with 83 rows and 8 columns:
#' \describe{
#'   \item{patient}{Participant's ID.}
#'   \item{age}{Age in years.}
#'   \item{sex}{1 = male; 2 = female.}
#'   \item{height}{Height in cm.}
#'   \item{weight}{Weight in kg.}
#'   \item{angioplasty}{1 = Has had an angioplasty; 0 = Has not had an angioplasty.}
#'   \item{bypas}{1 = Has had a bypass surgery; 0 = Has not had a bypass surgery.}
#'   \item{DIST_6WT_M0}{6-min walking test distance in m.}
#' }
#' @source APA & Co project
"INCLUSION"

#===============================================================================

#' Six-minute walking test performance at 6 months
#'
#' Participants data relating to the 6-min walking test 6 months after
#'     the end of the cardiac rehabilitation program.
#'
#' @format ## `VISIT_6M`
#' A data frame with 83 rows and 3 columns:
#' \describe{
#'   \item{patient}{Participant's ID.}
#'   \item{weight}{Weight in kg.}
#'   \item{DIST_6WT_M6}{6-min walking test distance in m.}
#' }
#' @source APA & Co project
"VISIT_6M"

#===============================================================================

#' Six-minute walking test performance at 12 months
#'
#' Participants data relating to the 6-min walking test 12 months after
#'     the end of the cardiac rehabilitation program.
#'
#' @format ## `VISIT_12M`
#' A data frame with 77 rows and 3 columns:
#' \describe{
#'   \item{patient}{Participant's ID.}
#'   \item{weight}{Weight in kg.}
#'   \item{DIST_6WT_M6}{6-min walking test distance in m.}
#' }
#' @source APA & Co project
"VISIT_12M"

#===============================================================================

#' IPAQ-SF
#'
#' Participants responses to the IPAQ-SF at 0, 6 and 12 months after the
#'     end of the cardiac rehabilitation program.
#'
#' @format ## `IPAQ`
#' A data frame with 243 rows and 13 columns:
#' \describe{
#'   \item{patient}{Participant's ID.}
#'   \item{num_visit}{1 = visit at the end of the cardiac rehabilitation program; 2 = visit at 6 months post program; 3 = visit at 12 months post program.}
#'   \item{bloc1_q1}{1 = The participant engaged in vigorous physical activity (>= 10  min bouts) during the last 7 days; 0 = The participant did not engage in vigorous physical activity (>= 10  min bouts) during the last 7 days.}
#'   \item{bloc2_q2}{Number of days with vigorous physical activity (>= 10  min bouts) during the last 7 days.}
#'   \item{total_hours_heavy}{Total time of vigorous physical activity (>= 10  min bouts) during the last 7 days (hours part); 999 = Did not answer.}
#'   \item{total_minutes_heavy}{Total time of vigorous physical activity (>= 10  min bouts) during the last 7 days (minutes part); 999 = Did not answer.}
#'   \item{bloc2_q1}{1 = The participant engaged in moderate physical activity (>= 10  min bouts) during the last 7 days; 0 = The participant did not engage in moderate physical activity (>= 10  min bouts) during the last 7 days.}
#'   \item{bloc2_q2}{Number of days with moderate physical activity (>= 10  min bouts) during the last 7 days.}
#'   \item{total_hours_moderate}{Total time of moderate physical activity (>= 10  min bouts) during the last 7 days (hours part); 999 = Did not answer.}
#'   \item{total_minutes_moderate}{Total time of moderate physical activity (>= 10  min bouts) during the last 7 days (minutes part); 999 = Did not answer.}
#'   \item{bloc3_q1}{1 = The participant engaged in walking activity (>= 10  min bouts) during the last 7 days; 0 = The participant did not engage in walking activity (>= 10  min bouts) during the last 7 days.}
#'   \item{bloc3_q2}{Number of days with walking activity (>= 10  min bouts) during the last 7 days.}
#'   \item{bouts_walk_7days}{Number of (consecutive or not) walking periods of 10  min during the last 7 days; 9999 = Did not answer.}
#'   }
#' @source APA & Co project
"IPAQ"

#===============================================================================

#' EMAPS
#'
#' Participants responses to the EMAPS at 0, 6 and 12 months after the
#'     end of the cardiac rehabilitation program.
#'
#' @format ## `EMAPS`
#' A data frame with 241 rows and 20 columns:
#' \describe{
#'   \item{patient}{Participant's ID.}
#'   \item{num_visit}{1 = visit at the end of the cardiac rehabilitation program; 2 = visit at 6 months post program; 3 = visit at 12 months post program.}
#'   \item{AP_q1 to AP_q18}{Answers to the EMAPS items.}
#'   }
#' @source APA & Co project
"EMAPS"

#===============================================================================

#' Barriers to physical activity
#'
#' Participants responses to the questionnaire assessing barriers to physical activity
#'     12 months after the end of the cardiac rehabilitation program.
#'
#' @format ## `BARRIERS`
#' A data frame with 77 rows and 12 columns:
#' \describe{
#'   \item{patient}{Participant's ID.}
#'   \item{num_visit}{1 = visit at the end of the cardiac rehabilitation program; 2 = visit at 6 months post program; 3 = visit at 12 months post program.}
#'   \item{trop_vieux}{"Too old"; 1 = "yes"; 0 = "no".}
#'   \item{manque_interet}{"Lack of interest"; 1 = "yes"; 0 = "no".}
#'   \item{effort_import_fatig}{"Heavy effort / too tired"; 1 = "yes"; 0 = "no".}
#'   \item{manque_temps}{"Lack of time"; 1 = "yes"; 0 = "no".}
#'   \item{meteo_defavorable}{"Unfavourable weather"; 1 = "yes"; 0 = "no".}
#'   \item{deplacements_diff}{"Difficulty to move"; 1 = "yes"; 0 = "no".}
#'   \item{cout_trop_eleve}{"Too costly"; 1 = "yes"; 0 = "no".}
#'   \item{crainte_blessures_douleurs}{"Fear of injury / pain"; 1 = "yes"; 0 = "no".}
#'   \item{isolement_faible_RS }{"Social isolation / weak social network"; 1 = "yes"; 0 = "no".}
#'   \item{autres}{"Other reasons".}
#'   \item{autre_precision_obstacle}{"Other details about barriers".}
#'   }
#' @source APA & Co project
"BARRIERS"

