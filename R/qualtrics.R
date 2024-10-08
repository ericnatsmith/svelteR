#' Process Qualtrics Loop Data
#'
#' This function processes SPSS data downloaded from Qualtrics when questions are based on a loop.
#' It separates out the loop row (prefix) and the field in each loop (suffix), reformats the data,
#' and outputs a long dataframe.
#'
#' @param dataframe The original dataframe to process.
#' @param number_range An optional vector of numbers (after "A") indicating which columns to process. Defaults to all numbers.
#' @param questions_to_remove An optional character vector of questions to remove.
#' @param loop_cols An optional character vector of length 2, specifying the names of the resulting columns describing the loop item. Defaults to \code{c("loop_group", "loop_item")}. If these names already exist in the dataframe, suffixes \code{"_N"} will be added to make them unique.
#'
#' @return A reformatted long dataframe.
#'
#' @examples
#' \dontrun{
#' processed_data <- process_qualtrics_loop_data(
#'   dataframe = d_practices_orig,
#'   questions_to_remove = c("my_loop", "Practice_timing_1", "Practice_timing_2"),
#'   loop_cols = c("loop_group", "loop_item")
#' )
#' }
#'
#' @importFrom tidyr pivot_longer pivot_wider separate
#' @importFrom dplyr mutate filter across rename first
#' @export
process_qualtrics_loop_data <- function(dataframe, number_range = NULL, questions_to_remove = NULL, loop_cols = c("loop_group", "loop_item")) {
  
  # Check if loop_cols exist in dataframe, if so, modify names
  existing_cols <- names(dataframe)
  loop_cols_original <- loop_cols
  suffix <- 1
  while(any(loop_cols %in% existing_cols)) {
    loop_cols <- paste0(loop_cols_original, "_", suffix)
    suffix <- suffix + 1
  }
  
  # Generate column names to select based on "A" and number_range
  # If number_range is NULL, select all columns starting with "A"
  if (is.null(number_range)) {
    cols_to_select <- names(dataframe)[grepl("^A\\d+", names(dataframe))]
  } else {
    number_range <- as.character(number_range)
    number_pattern <- paste(number_range, collapse = "|")
    col_pattern <- paste0("^A(", number_pattern, ")")
    cols_to_select <- names(dataframe)[grepl(col_pattern, names(dataframe))]
  }
  
  if(length(cols_to_select) == 0) {
    stop("No columns match the specified number_range.")
  }
  
  # Convert selected columns to character
  dataframe <- dplyr::mutate(dataframe, dplyr::across(dplyr::all_of(cols_to_select), as.character))
  
  # Pivot longer
  dataframe_long <- tidyr::pivot_longer(
    dataframe,
    cols = dplyr::all_of(cols_to_select),
    names_to = "question_item",
    values_to = "answer"
  ) %>%
    dplyr::filter(!is.na(answer) & answer != "")
  
  # Separate the question_item column into two columns: group and question
  dataframe_long <- tidyr::separate(dataframe_long, question_item, into = c("group", "question"), sep = "_", extra = "merge", fill = "right")
  
  # Remove specified questions
  if(!is.null(questions_to_remove)) {
    dataframe_long <- dplyr::filter(dataframe_long, !question %in% questions_to_remove)
  }
  
  # Fix up item names for easier merging
  dataframe_long <- dplyr::mutate(dataframe_long, question = gsub("_of_\\d+_", "", question))
  
  # Separate question into question and item
  dataframe_long <- tidyr::separate(dataframe_long, question, into = c("question", "item"), sep = "_(?!.*_)", extra = "merge", remove = FALSE, fill = "right")
  
  # Pivot wider
  dataframe_wide <- tidyr::pivot_wider(
    dataframe_long,
    names_from = question,
    values_from = answer,
    values_fn = ~ dplyr::first(.)
  )
  
  # Rename "group" and "item" columns to loop_cols[1] and loop_cols[2]
  dataframe_wide <- dplyr::rename(dataframe_wide, !!loop_cols[1] := group, !!loop_cols[2] := item)
  
  return(dataframe_wide)
}
