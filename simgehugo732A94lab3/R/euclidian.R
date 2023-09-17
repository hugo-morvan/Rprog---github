#' Euclidean's Algorithm
#'
#' @param input1 a numeric scalars
#' @param input2 a numeric scalars
#' @return greatest common divisor
#' @export
#' @examples
#' euclidean(123612, 1389234791)

euclidean <- function(input1, input2){
  # Asserting that inputs are numeric scalars
  stopifnot(
    is.numeric(input1), is.vector(input1), length(input1) == 1,
    is.numeric(input2), is.vector(input2), length(input2) == 1
  )

  min_inp <- min(input1, input2) #the max from the input
  max_inp <- max(input1, input2) #the min from the input
  remainder <- max_inp %% min_inp #initial remainder

  # repeat the proc
  repeat{
    if (remainder != 0){
      max_inp <- min_inp
      min_inp <- remainder
      remainder <- max_inp %% min_inp
    } else{
      return(abs(min_inp))
      break
    }
  }
}
