#' Detect Outliers Using the Interquartile Range Method
#'
#' Identifies potential outliers in a numeric vector using the interquartile
#' range (IQR), calculated as the third quartile minus the first quartile.
#' Values below the first quartile minus 1.5 times the IQR or above the third
#' quartile plus 1.5 times the IQR are flagged. Missing values remain missing
#' in the returned logical vector.
#'
#' @param x A numeric vector.
#' @return A logical vector indicating whether each value is an outlier.
#'   Missing values in `x` produce missing values in the result.
#' @examples
#' x <- c(1, 2, 3, 100)
#' detect_outliers_iqr(x)
#' @export
#' @importFrom stats quantile
#' @keywords univariate
#' @seealso [run_outlier_analysis()]
detect_outliers_iqr <- function(x) {
  if (!is.numeric(x)) {
    stop("'x' must be a numeric vector.", call. = FALSE)
  }
  result <- rep(NA, length(x))
  observed <- !is.na(x) & is.finite(x)
  if (!any(observed)) {
    return(as.logical(result))
  }
  quartiles <- stats::quantile(x[observed], probs = c(0.25, 0.75), names = FALSE)
  spread <- quartiles[2L] - quartiles[1L]
  lower <- quartiles[1L] - 1.5 * spread
  upper <- quartiles[2L] + 1.5 * spread
  result[observed] <- x[observed] < lower | x[observed] > upper
  as.logical(result)
}

#' Run Outlier Detection and Export Results
#'
#' Reads a comma-separated values (CSV) file, detects potential outliers in
#' numeric columns using the interquartile range (IQR) method, and writes two
#' files to a directory specified by the user: a CSV file containing rows with
#' outliers and an 'Excel' workbook with outlier values highlighted and an
#' outlier-count summary. The function does not create files in the working
#' directory unless that directory is explicitly supplied as `output_dir`.
#'
#' @param file Path to the input CSV file.
#' @param output_dir Existing directory in which output files will be written.
#' @param verbose Logical; if `TRUE`, display progress messages. Defaults to
#'   `FALSE`.
#' @return Invisibly returns a list with `summary`, `outliers`, and `files`
#'   elements. `files` contains the paths of the two output files.
#' @examples
#' example_file <- system.file("extdata", "example_data.csv",
#'                             package = "DetectOutlier")
#' if (nzchar(example_file)) {
#'   result <- run_outlier_analysis(example_file, output_dir = tempdir())
#' }
#' @export
#' @importFrom utils read.csv write.csv
#' @importFrom openxlsx createWorkbook addWorksheet writeData createStyle addStyle saveWorkbook
#' @seealso [detect_outliers_iqr()]
run_outlier_analysis <- function(file, output_dir, verbose = FALSE) {
  if (!is.character(file) || length(file) != 1L || !file.exists(file)) {
    stop("'file' must name an existing input file.", call. = FALSE)
  }
  if (!is.character(output_dir) || length(output_dir) != 1L ||
      !dir.exists(output_dir)) {
    stop("'output_dir' must name an existing directory.", call. = FALSE)
  }
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("'verbose' must be TRUE or FALSE.", call. = FALSE)
  }

  data <- utils::read.csv(file, stringsAsFactors = FALSE, check.names = FALSE)
  numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]

  outlier_summary <- data.frame(
    Variable = numeric_cols,
    Outlier_Count = integer(length(numeric_cols)),
    stringsAsFactors = FALSE
  )
  outlier_flags <- vector("list", length(numeric_cols))
  names(outlier_flags) <- numeric_cols
  for (i in seq_along(numeric_cols)) {
    col <- numeric_cols[i]
    flags <- detect_outliers_iqr(data[[col]])
    outlier_flags[[i]] <- flags
    outlier_summary$Outlier_Count[i] <- sum(flags, na.rm = TRUE)
  }

  if (length(numeric_cols)) {
    any_outlier <- Reduce(`|`, lapply(outlier_flags, function(z) z %in% TRUE))
  } else {
    any_outlier <- rep(FALSE, nrow(data))
  }
  outlier_rows <- data[any_outlier, , drop = FALSE]
  outlier_file <- file.path(output_dir, "outlier_rows.csv")
  excel_file <- file.path(output_dir, "data_with_bold_outliers.xlsx")
  utils::write.csv(outlier_rows, outlier_file, row.names = FALSE, na = "")

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Data")
  openxlsx::writeData(wb, "Data", data)
  bold_style <- openxlsx::createStyle(textDecoration = "bold", fontColour = "#FF0000")
  for (col in numeric_cols) {
    flags <- detect_outliers_iqr(data[[col]])
    if (any(flags, na.rm = TRUE)) {
      openxlsx::addStyle(
        wb, "Data", style = bold_style,
        rows = which(flags %in% TRUE) + 1L,
        cols = match(col, names(data)),
        gridExpand = FALSE, stack = TRUE
      )
    }
  }
  openxlsx::addWorksheet(wb, "Outlier_Summary")
  openxlsx::writeData(wb, "Outlier_Summary", outlier_summary)
  openxlsx::saveWorkbook(wb, excel_file, overwrite = TRUE)

  if (verbose) {
    message("Outlier analysis completed. CSV: ", outlier_file,
            " | Excel workbook: ", excel_file)
  }
  invisible(list(summary = outlier_summary, outliers = outlier_rows,
                 files = c(outlier_rows = outlier_file, excel = excel_file)))
}
