#' Package Available
#'
#' Internal function to check if a specified package is installed.

#' @param x Character. Name of the package to be checked.
#' @return Logical. Is the specified package installed?
#' @keywords internal

package_available <- function(x) requireNamespace(x, quietly = TRUE)


#' Skim Data
#'
#' This function skims a data frame. It provides information about each variable such as its type, range, missing values, and distribution (if the \pkg{skimr} package is available).
#'
#' @param x A data frame to be skimmed.
#'
#' @return A data frame with the skimmed data. Each row corresponds to a variable in the input data.frame and contains the following columns:
#' \itemize{
#'   \item variable: the name of the variable
#'   \item type: the type of the variable
#'   \item range: the range of the variable if it is numeric, the number of levels if it is a factor, or the number of unique values if it is a character
#'   \item missing: the number of missing values in the variable
#'   \item distribution: the distribution of the variable if it is numeric and the \pkg{skimr} package is available
#' }
#'
#' @examples
#' \dontrun{
#' skim_data(df)
#' }
#' @export

skim_data <- function(x) {
  # x <- papaja:::default_label.data.frame(x)
  labels <- unlist(tinylabels::variable_labels(x))

  data_range <- function(y) {
    if (inherits(y, "numeric") || inherits(y, "integer")) {
      return(paste0("[", min(y, na.rm = TRUE), ", ", max(y, na.rm = TRUE), "]"))
    } else if (inherits(y, "factor")) {
      return(nlevels(y))
    } else if (inherits(y, "character")) {
      return(length(unique(y[!is.na(y)])))
    } else {
      return("")
    }
  }

  modal_level <- function(z) {
    lvls <- table(z)
    names(lvls[which.max(lvls)])
  }

  data_center <- function(y) {
    if (inherits(y, "numeric") || inherits(y, "integer")) {
      return(median(y, na.rm = TRUE))
    } else if (inherits(y, "factor")) {
      return(modal_level(y))
    } else if (inherits(y, "character")) {
      return(modal_level(y))
    } else {
      return("")
    }
  }

  data_iqr <- function(y) {
    if (inherits(y, "numeric") || inherits(y, "integer")) {
      return(IQR(y, na.rm = TRUE))
    } else {
      return(NA)
    }
  }

  skimmed_data <- data.frame(
    variable = names(labels)
    , label = ifelse(labels == names(labels), NA, labels)
    , type = sapply(x, function(y) paste(class(y)[class(y) != "tiny_labelled"], collapse = ", "))
    , center = sapply(x, data_center)
    , iqr = sapply(x, data_iqr)
    , range = sapply(x, data_range)
    , missing = sapply(x, function(y) sum(is.na(y)))
    , stringsAsFactors = FALSE
  )

  # if(isTRUE(package_available("skimr"))) {
  #   skimmed_data <- cbind(
  #     skimmed_data
  #     , distribution = sapply(x, function(y) if (is.numeric(y)) skimr::inline_hist(y) else "")
  #   )
  # }

  skimmed_data
}
