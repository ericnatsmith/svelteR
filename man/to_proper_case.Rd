% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vectors.R
\name{to_proper_case}
\alias{to_proper_case}
\title{Convert Character Vector to Proper Case}
\usage{
to_proper_case(name_vector)
}
\arguments{
\item{name_vector}{A character vector where each element represents a name or phrase to be converted to proper case.}
}
\value{
A character vector with the same length as \code{name_vector}, where each element is converted to proper case.
}
\description{
This function transforms each element of a character vector to proper case,
where the first letter of each word is capitalized and all other letters
are in lowercase.
}
\details{
The function splits each element of the character vector into words, applies
proper case to each word (i.e., capitalizing the first letter and making
the rest lowercase), and then joins the words back into a single string.
}
\examples{
# Convert a vector of names to proper case
names <- c("john DOE", "jane smith", "MARy JOHNSON")
proper_names <- to_proper_case(names)
print(proper_names)

}
