#' Convert Character Vector to Proper Case
#'
#' This function transforms each element of a character vector to proper case, 
#' where the first letter of each word is capitalized and all other letters 
#' are in lowercase.
#'
#' @param name_vector A character vector where each element represents a name or phrase to be converted to proper case.
#'
#' @return A character vector with the same length as `name_vector`, where each element is converted to proper case.
#'
#' @details
#' The function splits each element of the character vector into words, applies 
#' proper case to each word (i.e., capitalizing the first letter and making 
#' the rest lowercase), and then joins the words back into a single string.
#'
#' @examples
#' # Convert a vector of names to proper case
#' names <- c("john DOE", "jane smith", "MARy JOHNSON")
#' proper_names <- to_proper_case(names)
#' print(proper_names)
#'
#' @export
to_proper_case <- function(name_vector) {
  # Apply proper case transformation to each element of the character vector
  sapply(name_vector, function(name) {
    # Split the string into words
    words <- strsplit(name, " ")[[1]]
    
    # Apply proper case to each word
    proper_case_words <- sapply(words, function(word) {
      # Capitalize the first letter and make the rest lowercase
      paste(toupper(substring(word, 1, 1)), tolower(substring(word, 2)), sep = "")
    })
    
    # Combine the words back into a single string
    paste(proper_case_words, collapse = " ")
  }, USE.NAMES = FALSE)
}

#' Retain Attributes in Merged Columns
#'
#' This function ensures that when merging columns in data frames, attributes 
#' such as variable labels and value labels (from labelled data) are retained 
#' for the first element of the column.
#'
#' @param x A vector (usually a column in a data frame) that may have labelled attributes 
#'          (such as those created by the `haven` package).
#'
#' @return If `x` is labelled, the function returns the first element of `x` 
#'         with its attributes (e.g., variable label, value labels, and class) retained.
#'         If `x` is not labelled, the function simply returns the first element of `x`.
#'
#' @details
#' The function checks whether the input vector `x` has labelled attributes (using `haven::is.labelled()`). 
#' If it does, the function extracts and retains the original attributes, including the variable label 
#' and value labels, and applies them to the first element of the vector.
#'
#' This is useful when merging data where you want to preserve variable labels for specific columns 
#' in the resulting data frame.
#'
#' @importFrom haven is.labelled
#' @importFrom tidyr pivot_wider
#' @export
#'
#' @examples
#' \dontrun{
#' # Example using labelled data from the haven package and pivot_wider
#' library(haven)
#' library(tidyr)
#'
#' # Create a labelled data frame
#' df <- data.frame(
#'   id = c(1, 1, 2, 2),
#'   time = c("pre", "post", "pre", "post"),
#'   score = labelled(c(10, 12, 9, 11), labels = c(Low = 9, High = 12), label = "Score")
#' )
#'
#' # Use pivot_wider to reshape data and retain attributes in merged columns
#' df_wide <- pivot_wider(df, names_from = time, values_from = score, 
#'                        values_fn = retain_attributes)
#'
#' # Check the attributes of the reshaped data
#' attributes(df_wide$post)
#' }
retain_attributes <- function(x) {
  require(haven)
  if (is.labelled(x)) {
    attributes_val <- attributes(x)
    return(structure(first(x), .Names = names(first(x)), .Label = attributes_val$label, labels = attributes_val$labels, class = class(x)))
  } else {
    first(x)
  }
}
