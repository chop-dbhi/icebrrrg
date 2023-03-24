#' Single Odds Ratio Comparator
#'
#' Generate a Fisher's Exact Test summary table based on a single comparator between two cohorts in a single dataframe
#'
#' @param data A \code{data.frame} containing all necessary data for comparison.
#' @param case_cohort The first group to be compared from \code{data}. This should be an expression.
#' @param control_cohort The second group to be compared from \code{data}. This should be an expression.
#' @param comparator How the groups are compared. This variable should reference a column within \code{data}.
#'
#' @importFrom rlang enquo
#' @importFrom dplyr filter select group_by summarize n arrange desc slice pull
#' @importFrom stats fisher.test
#'
#' @return Returns a \code{dataframe} of Odds Ratios and summary data per cohort
#'
#' @examples
#' single_or(
#'   data = iris,
#'   case_cohort = Petal.Length > 1.2,
#'   control_cohort = Petal.Length <= 1.2,
#'   comparator = Species
#' )
#'
#' @export
#'
#' @seealso \code{\link{fisher.test}}

single_or <- function(data, case_cohort, control_cohort, comparator){

  # eval() returns a computed value from an expression
  # substitute() returns a parse tree for an expression (i.e. the 'characters' for that expression)
  case_cohort <- eval(substitute(case_cohort), data)
  control_cohort <- eval(substitute(control_cohort), data)

  # enquo returns single quoted expression to delay computation until explicitly activated (!! tells function when to 'activate')
  comparator <- enquo(comparator)

  group_1 <- data |> filter(case_cohort)
  group_2 <- data |> filter(control_cohort)

  count_1 <- group_1 |> filter(!!comparator != "", !is.na(!!comparator)) |> nrow()
  count_2 <- group_2 |> filter(!!comparator != "", !is.na(!!comparator)) |> nrow()

  compare_matrix <- matrix(ncol = 2, nrow = 2) |> as.data.frame()
  names(compare_matrix) <- c("group 1", "group 2")
  rownames(compare_matrix) <- c("present", "absent")

  # Generate table for comparing cohort 1 with comparator against cohort 2 without stated comparator
  # Note: "n" may be larger than the sum of group a + b as it includes ALL patients w/ comparator
  or_table <- data |> select(!!comparator) |>
    filter(!!comparator != "", !is.na(!!comparator) ) |>
    group_by(!!comparator) |>
    summarize(n = n()) |>
    arrange(n |> desc())

  or_table$case_cohort_comps_yes <- NA
  or_table$case_cohort_comps_no <- NA
  or_table$control_cohort_comps_yes <- NA
  or_table$control_cohort_comps_no <- NA
  or_table$p_value <- NA
  or_table$CIU <- NA
  or_table$OR <- NA
  or_table$CIL <- NA
  or_table$one_OR <- NA

  for (i in seq_len(NROW(or_table))) {

    feature <- or_table |> select(!!comparator) |> slice(i) |> pull(1)

    feature_1 <- group_1 |> filter(!!comparator == feature) |> nrow()
    feature_2 <- group_2 |> filter(!!comparator == feature) |> nrow()

    compare_matrix[1, 1] <- feature_1
    compare_matrix[2, 1] <- count_1 - feature_1
    compare_matrix[1, 2] <- feature_2
    compare_matrix[2, 2] <- count_2 - feature_2

    print(paste0("Fisher test for: ", feature))
    print(fisher.test(compare_matrix |> select(1, 2)))

    or_table$case_cohort_comps_yes[i] <- compare_matrix[1, 1]
    or_table$case_cohort_comps_no[i] <- compare_matrix[2, 1]
    or_table$control_cohort_comps_yes[i] <- compare_matrix[1, 2]
    or_table$control_cohort_comps_no[i]<- compare_matrix[2, 2]

    fish <- fisher.test(compare_matrix |> select(1, 2))
    or_table$p_value[i] <- fish$p.value
    or_table$CIU[i] <- fish$conf.int[2]
    or_table$OR[i] <- fish$estimate
    or_table$CIL[i] <- fish$conf.int[1]
    or_table$one_OR[i] <- 1 / fish$estimate

  }
  return(or_table)
}
