#' Data for Monty Hall Problem
#'
#' Monty_hall is a data frame containing data to build a tree diagram for
#' the Monty Hall Problem. This data frame is in the format required by the
#' plot_tree_diagram() function.
#'
#' @format ## `Monty_hall`
#' A data frame with 27 rows and 6 columns. Each row represents a path in the
#' tree diagram.
#' \describe{
#'   \item{Level_1_Class}{Categories of level 1 variable `car`. `car`= Ci means the car was behind door i.}
#'   \item{Level_1_Prob}{Prob(car=Ci)}
#'   \item{Level_2_Class}{Categories of level 2 variable `pick`. `pick`= Pj means the participant picked door j.}
#'   \item{Level_2_Prob}{Prob(pick=Pj | car=Ci)}
#'   \item{Level_3_Class}{Categories of level 3 variable `monty`. `monty`= Mk means Monty opened door k.}
#'   \item{Level_3_Prob}{Prob(monty=Mk | car=Ci, pick=Pj)}
#' }
