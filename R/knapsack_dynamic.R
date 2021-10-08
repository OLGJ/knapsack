#' Dynamic knapsack
#'
#' @param x Knapsack data frame
#' @param W Knapsack size
#'
#' @return elements and the sum of the corresponding values
#' @export
#'
knapsack_dynamic <- function(x, W){
  # Correct input
  stopifnot(
    is.data.frame(x),
    is.numeric(W),
    identical(colnames(x), c("w", "v")),
    all(x[c("w","v")] > 0)
  )
  #params
  w <- x$w # weights
  v <- x$v # values
  n <- nrow(x) # distinct items
  m <- matrix(0, n+1, W+1) # plus 1 for row 0 and col 0

  for(i in 2:n+1){ # Skip first row and col as they represents 0
    for(j in 2:W+1){

      if(w[i-1] <= j){ # max value with item i, compare to j
        m[i, j] = max(m[i-1, j], m[i-1,j-w[i-1]]+v[i-1]) # pick larger of the two, -1 to account for +1
        }
      else{
        m[i, j] = m[i-1, j] # max value without item i
      }
    }}
  value <- m[i, j]
  elements <- c()

  remaining_score <- value
  limit <- j

  for(i in c((n+1):1)){
    if(remaining_score <= 0){
      break()
    }

    if(remaining_score == m[i-1, limit]){
      next
    }

    else{
      elements <- c(elements, i-1)
      remaining_score <- remaining_score - v[i-1]
      limit <- limit - w[i-1]
    }

  }
  return(list("value"=round(value,0), "elements"=rev(elements)))

}
