# The euclidean algorithm

euclidean <- function(input1, input2){
  min_inp <- min(input1, input2)
  max_inp <- max(input1, input2)
  remainder <- max_inp %% min_inp #finding the inital remainder
  
  repeat{
    if (remainder != 0){
      max_inp <- min_inp
      min_inp <- remainder
      remainder <- max_inp %% min_inp
    } else{
      return(min_inp)
      break
    }
  }
}

euclidean(123612, 13892347912)
euclidean(100, 1000)

