# Function that calculates the mean of each numeric or date column
col_means <- function(df, ...) {
  
  data <- as.data.frame(df)

  # Handling empty data.frames (warning)
  if (nrow(data) == 0 | ncol(data) == 0) {
    warning("Empty input!")
    return(data.frame())
  }  
  
  # Warning for matrix columns
  matrix <- vapply(data, is.matrix, logical(1))
  if (ncol(data[matrix]) > 0) {
    warning("For each of the following columns of class matrix only one mean has
            been calculated: ", names(data[matrix]))
  }
  
  # Warning for list columns
  list <- vapply(data, is.list, logical(1))
  if (ncol(data[list]) > 0) {
    warning("The following columns of class list have been ignored: ", 
            names(data[list]))
  }
  
  # Warning for factor columns
  factor <- vapply(data, is.factor, logical(1))
  if (ncol(data[factor]) > 0) {
    warning("The following columns of type factor have been ignored: ",
            names(data[factor]))
  }

  # Detecting numeric columns
  numeric <- vapply(data, is.numeric, logical(1))
  numeric_cols <- data[numeric]

  # Function to detect a date
  is_date <- function(x) {
    inherits(x, "Date")
  }
  
  # Detecting date columns
  date <- vapply(data, is_date, logical(1))
  date_cols <- data[date]
  
  # Calculating the mean for numeric and date columns
  data.frame(lapply(c(numeric_cols, date_cols), mean, ...))
}

