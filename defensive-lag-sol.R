
# Function, that returns version of its vector input x, which starts n values 
# behind the original and fills the beginning with NA
lag <- function(x, n = 1L) {
  
  # checks
  checkmate::assert_vector(x)
  checkmate::assert_true(length(x) >= n)
  checkmate::assert_true(length(x) >= 1)
  
  
  if (is.data.frame(x) == TRUE) {
    return(NA)
  }
  
  if (is.list(x) == TRUE) {
    warning("Only the first entry of each list element is used!")
  }
  
  if (is.matrix(x) == TRUE) {
    warning("Matrix has been converted to vector first!")
  }
  
  xlen <- length(x)
  c(rep(NA, n), x[seq_len(xlen - n)])
}
lag(5)
