### -###-###-###-###-###-###-###-###-###-###-###-###-###-###
## Codename - sum_totals.R                               ##
## Version - 0.8
## Data Release - Annual Inpatient/Daycase Information   ##
## Original Authors - Chris Deans                        ##
## Orginal Date - June 2019                              ##
## Latest Update Author - Chris Deans                    ##
## Latest Update Date - 14/06/2019                       ##
##                                                       ##
## Type - Function                                       ##
## Written/run on - R Studio SERVER                      ##
## R version - 3.5.1 (2018-07-02)                        ##
## Dependancies: dplyr                                   ##
##                                                       ##
## Description - Function to sum variables in a data     ##
##  frame which are in each group, with totals.          ##
##                                                       ##
## Usage:                                                ##
##    sumtotals(data,                                    ##
##              vars = c("stays","los"),                 ##
##              groups = c("hb","age"))                  ##
##                                                       ##
### -###-###-###-###-###-###-###-###-###-###-###-###-###-###

### Documentation ----
#' Sum values by group with totals
#'
#' sum_totals() is wrapper for the dplyr functions which reduces a data frame to the provided groups, replacing the other set of provided variables with their sums over the groups and adding totals.
#' @param x a tbl() to count.
#' @param vars Variables to sum up.
#' @param groups Variables to group by. These will be converted to character so that a new value 'Total' can be added.
#' @param na.rm logical. Sould missing values (including NaN) be removed?
#' @return An ungrouped data frame.
#' @note Currently this function converts all of the grouping variables to character so that the value 'Total' can be added.
#' @examples
#' # Using one grouping variable will sum a variable for each
#' # value in the group and add an extra row for 'Total'
#' starwars %>%
#'   sum_totals("mass", "homeworld")
#'
#' # na.rm can be used to exclude NAs from the summing variables
#' starwars %>%
#'   sum_totals("mass", "homeworld", na.rm = TRUE)
#'
#' # Multiple grouping and summing variables can be specified.
#' # In such cases, all possible subtotals are also calculated.
#' starwars %>%
#'   sum_totals(c("mass", "height"), c("homeworld", "species", "gender"), na.rm = TRUE)
#'
#' # Including a new variable name in the summing variable will add
#' # a count with that name
#' starwars %>%
#'   sum_totals(c("n_char", "mass", "height"), c("homeworld", "species", "gender"), na.rm = TRUE)
#' @export

### Function definition ----
sum_totals <- function(x, vars, groups, na.rm = FALSE) {
  `%>%` <- magrittr::`%>%`

  # If any variables in vars don't exist, create them and set them to 1
  for (newvar in vars[!(vars %in% names(x))]) {
    x <- dplyr::mutate(x, !!newvar := 1)
  }

  # Convert grouping variables to character (so that "Total" can be added)
  x <- dplyr::mutate_at(x, groups, as.character)

  # Initial count over all groups
  z <- dplyr::group_by_at(x, groups) %>%
    dplyr::summarise_at(vars, sum, na.rm = na.rm) %>%
    dplyr::ungroup()

  # For each grouping variable (one at a time):
  #  - Change all values to "Total"
  #  - aggregate up to get totals over the other groups
  #  - bind this onto the previous results
  for (cat in groups) {
    z <- z %>%
      dplyr::mutate(!!cat := "Total") %>%
      dplyr::group_by_at(groups) %>%
      dplyr::summarise_at(vars, sum, na.rm = na.rm) %>%
      dplyr::ungroup() %>%
      dplyr::bind_rows(z)
  }

  # Put rows into correct order (totals first)
  z <- z %>%
    dplyr::mutate_at(groups, list(sorter = ~ ifelse(. == "Total", "", .))) %>%
    dplyr::arrange_at(vars(tidyselect::ends_with("sorter"))) %>%
    dplyr::select(-tidyselect::ends_with("sorter"))

  return(z)
}
