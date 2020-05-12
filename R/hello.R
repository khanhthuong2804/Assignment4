
#' title : Assignment4
#'@title Assignment4
#'@author kt
#'@docType
#'@description package to build function as assignment on coursera
#'
# check existence of data file. if the file doesn't exist there is stop message
#'Create file of data
system.file("extdata","Assignment4", package = "Assignment4")
#' @param filename a string with the file name
#' @return to return the file
#' @examples
#' \dontrun
#' {
# fars_read("accident_2015.csv.bz2")
#' }
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @example
#' fars_read(accident_2015.csv)
#' @export    export the function, so users will have direct access to it when they load the package


fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}




#'Create custom filename
#'
#'
#' @param year is used in the filename string
#'
#' @return This function returns a string with the filename customized with given year
#'
#' @examples
#' make_filename(2015)
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Read data


#' @param years displayed
#' @return returns a list with the month and year columns from the file
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate select
#' @example
#' fars_read_years(2015)
#' @export
#'
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}


#'Summarize the data selected by the fars_read_years function
#'
#' @param years is used
#' @return returns a list of the summarized data for one or more years
#' @importFrom magrittr "%>%"
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @example
#' fars_summarize_years(2015)
#' @export
#'
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#'mapping datapoints and state

#' @param state what state data is to be displayed
#' @param year of data is to be displayed
#'
#' @return returns a map of the given state and the datapoints
#' @importFrom magrittr "%>%"
#' @importFrom maps map
#' @importFrom graphics points
#' @example
#' fars_map_state()
#' @export
#'
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}


