# Function that changes numbers to counts
count_them <- function(supposedly_a_count) {
  
  if (!checkmate::test_numeric(supposedly_a_count, lower = 0, finite = TRUE)) {
    stop("Only non-negatives and final numbers possible!")
  }
  
  if (checkmate::testScalarNA(supposedly_a_count)) {
    stop("NA")
  }
  
  if (!checkmate::test_count(supposedly_a_count)) {
    warning("rounding ", supposedly_a_count,
            " to the nearest integer.")
    supposedly_a_count <- round(supposedly_a_count) 
    # QUESTION: Isn't this what I'm not supposed to do? (overwrite a parameter)
    # Or when is it ok?
  }
  as.integer(supposedly_a_count)
}
