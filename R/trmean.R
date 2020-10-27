#' Trimmed Mean
#'
#' This function can compute the trimmed mean
#' The user must provide a vector
#'
#' @param x numeric vector
#' @param s numeric positive integer
#' @param l numberic positive integer
#' @export
#' @author Edem Defor
#' @examples
#' trmean(c(1:5),1,2)

trmean = function(x,s,l){

  if(length(x)< s + l + 1)            #checking the condition
  {
    stop("x must be at least s+l+1")  #stop there's an error
  }

  n = length(x)

  y= sort(x)

  y = y[(s+1):(n-l)]                  #compute trimmed vector

  return(mean(y))                     #return trimmed mean
}


