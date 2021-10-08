#' Greedy knapsack
#'
#' @param x Knapsack data frame
#' @param W Knapsack size
#'
#' @return elements and the sum of the corresponding values
#' @export
#'
greedy_knapsack <- function(x, W){
  stopifnot(
    is.data.frame(x),
    W > 0,
    identical(colnames(x), c("w", "v")),
    all(x[c("w","v")] > 0)
  )

  # Add ratio add sort
  x["ratio"] = x$v/x$w
  x<-x[rev(order(x$ratio)),]

  w <- x$w # weights
  v <- x$v # values
  n <- length(v) # iterations

  elements <- c()
  value <- 0
  for(i in c(1:n)){
    W <- W - w[i]
    if(W <= 0){
      return(list("value"=round(value,0), "elements"=elements))
    }
    else{
      value <- value + v[i]
      elements <- c(elements, as.integer(rownames(x[i,])))
    }
  }
 }
