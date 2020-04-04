#'Read a CSV file
#'
#'By default, this function reads and converts a CSV file into a dataframe.
#'It takes the CSV file as the only argument and it doesn't show the reading
#'progress bar.
#'
#'@param filename A string of characters referencing the path and name of
#'the CSV file to be processed
#'
#'@return This function returns a dplyr dataframe if the file exists.
#'Otherwize, it returns an error like "file '\code{filename}' doesn't
#'exist".
#'
#'@importFrom readr read_csv
#'@importFrom dplyr tbl_df
#'
#'@export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#'make_filename creates a dynamic filename with a year given in input.
#'The filename appear like "accident_{year}.csv.bz2". This function is
#'call in "fars_read_years".
#'
#'@param year A integer which represents the desired year
#'
#'@return If the input type is a integer or can be convert into it, this
#'function return the filename "accident_{year}.csv.bz2".Else, returns
#'(1) the file "accident_NA.csv.bz2" and (2) a warning message
#'(NAs introduced by coercion)
#'
#'@examples
#'make_filename(2013)
#'make_filename("2015")
#'
#'@export
make_filename <- function(year) {
  year <- as.integer(year)
  filename <- sprintf("accident_%d.csv.bz2", year)
  system.file("extdata", filename, package = "roadAccidentsStudy")
}


#'Groups together in a new dataframe all accidents by
#'selecting only the month and the year referenced in the archive files
#'"accident_{year}.csv.bz2" from the target years given in the input.
#'This function calls make_file_name and fars_read.
#'
#'@param years An integer vector with all the years to extract
#'
#'@return If the input is an integer vector as required, it returns
#'a dataframe with n+1 columns : MONTH and n years. Otherwise, when there
#'are no archive files with the target year, it returns a warning
#'like "invalid year: {year}" and NULL value.
#'
#'@examples
#'fars_read_years(c("2013", "2014")) -> success
#'fars_read_years(c(2013, 2014)) -> success
#'
#'@importFrom dplyr mutate select
#'
#'@export
fars_read_years <- function(years) {
  MONTH <- NULL
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


#'fars_summarize_years counts for each year given in input,
#'the number of accidents per MONTH. This function calls
#'fars_read_years.
#'
#'@param years An integer vector with all the target years
#'
#'@return A dataframe with n+1 column : MONTH and n year.
#'Values of n year is the accident number for specific a month.
#'If the year doesn't exist in the files, return warning
#'
#'@examples
#'fars_summarize_years(c(2014, 2015)) -> success
#'
#'@importFrom dplyr bind_rows group_by
#'@importFrom tidyr spread
#'@importFrom magrittr %>%
#'
#'@export
fars_summarize_years <- function(years) {
  year <- MONTH <- n <- NULL
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
  dplyr::group_by(year, MONTH) %>%
  dplyr::summarize(n = dplyr::n()) %>%
  tidyr::spread(year, n)
}


#'fars_map_state return a point graphic representing the distribution
#' of accidents for a given state number and for a given year. This
#' function calls make_filename, fars_read.
#'
#'@param state.num Interger which represents a state
#'@param year Integer which represents a target year
#'
#'@return If the inputs are correct, it returns graphic points where
#'graphic represents the state and each point represent a accident.
#'If state number is unknown in the file, it returns a error like
#'"invalid STATE number : {state.num}". If there is no accident this
#'year in this state, return "no accidents to plot" and NULL value
#'
#'@examples
#'fars_map_state(1, 2013)
#'
#'@importFrom dplyr filter
#'@importFrom maps map
#'@importFrom graphics points
#'
#'@export
fars_map_state <- function(state.num, year) {
  STATE <- NULL
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
