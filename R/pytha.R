#' Pythagoras Theorem
#'
#' This function can compute the third value in any pythagorean set
#' The user must provide either a & b or a & c or b  c
#'
#' @param a numeric positive integer
#' @param b numeric positive integer
#' @export
#' @author Edem Defor
#' @examples
#' pytha(4,5)



pytha <- function(a, b)
{
  sides <- c(a, b)
  if(any(sides < 0))
  {
    stop("sides must be positive")          #stop there's an error
  } else if(!is.numeric(x = sides))
  {
    stop(("sides can not be non-numeric"))  #stop there's an error
  }
  else if(a > b)
  {
    stop(("input numbers in ascending order"))  #stop there's an error
  }

  x = (sqrt(sum(sides ^ 2)))

  if(x%%1!=0 && (a < b))            #if the correct computation has not been done check and compute again
  {
    x=(sqrt(diff(sides ^ 2)))

  } else if(x%%1==0 && (a > b))     #if the correct computation has not been done check and compute again
  {
    x=(sqrt(diff(sides ^ 2)))
  }

  return(x)                         #return answer
}


