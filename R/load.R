#' Load the Most Recent File of a Specified Type from a Folder
#'
#' This function searches a specified folder for files of a given type (e.g., CSV, SAV) 
#' and loads the most recently modified file. Optionally, the function can also 
#' filter files based on a specified pattern in their filenames. If no files 
#' are found, or if the file type is unsupported, the function will return an appropriate message.
#'
#' @param folder A character string specifying the path to the folder containing the files.
#' @param filetype A character string indicating the file type to search for (e.g., "csv", "sav").
#' @param pattern An optional character string representing a regular expression to filter the files by name.
#'                If `NULL`, no filtering based on filename will occur.
#' 
#' @return If a supported file is found, the function returns the loaded data as a data frame 
#'         (or a tibble if reading a CSV file). If no supported file is found or if the file type 
#'         is unsupported, the function will return `NULL` or the name of the most recent file.
#'
#' @details
#' The function currently supports loading SPSS (.sav) and CSV (.csv) files using the 
#' `haven::read_spss()` and `readr::read_csv()` functions, respectively. If an unsupported 
#' file type is specified, the function returns the name of the most recent file instead of 
#' loading the data.
#'
#' @import haven
#' @import readr
#' @export
#'
#' @examples
#' \dontrun{
#' # Load the most recent CSV file in the "data" folder
#' data <- load_most_recent_file("data", "csv")
#'
#' # Load the most recent SAV file in the "data" folder that contains "experiment" in its filename
#' data <- load_most_recent_file("data", "sav", pattern = "experiment")
#' }
load_most_recent_file <- function(folder, filetype, pattern = NULL) {
  # Create the file pattern based on the file extension
  file_pattern <- paste0("\\.", filetype, "$")
  
  # Get all files of the specified file type in the folder
  all_files <- list.files(folder, pattern = file_pattern, full.names = TRUE)
  
  # If a pattern is specified, filter the files based on the pattern
  if (!is.null(pattern)) {
    filtered_files <- all_files[grepl(pattern, all_files)]
  } else {
    filtered_files <- all_files
  }
  
  # Get the most recent file based on modification time
  if (length(filtered_files) > 0) {
    file_info <- file.info(filtered_files)
    most_recent_file <- filtered_files[which.max(file_info$mtime)]
    message <- paste0("Most recent file downloaded at ", max(file_info$mtime))
    print(message)
    
    # Load the file based on its type
    if (filetype == "sav") {
      data <- haven::read_spss(most_recent_file)
    } else if (filetype == "csv") {
      data <- readr::read_csv(most_recent_file)
    } else {
      # Issue a warning and output the file name instead of loading data
      return(most_recent_file)
      stop("Unsupported file type. Outputting file name rather than data file.")
    }
    
    return(data)
  } else {
    print(paste("No", filetype, "files found that match the pattern."))
    return(NULL)
  }
}