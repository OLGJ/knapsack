#' Brute force knapsack
#'
#' @param x Knapsack data frame
#' @param W Knapsack size
#' @param parallel Optional argument to run a parallel implementation
#' @return elements and the sum of the corresponding values
#' @import parallel
#' @export

brute_force_knapsack <- function(x, W, parallel = FALSE){
  # Correct input
  stopifnot(
    is.data.frame(x),
    W > 0,
    identical(colnames(x), c("w", "v")),
    all(x[c("w","v")] > 0)
  )
  # Params
  n <- nrow(x)
  w <- x$w
  v <- x$v
  solution <- replicate(n,0)
  solution_score <- 0
  combinations <- seq(1, 2^n-1) # 2^n-1

  if(parallel == FALSE){
    for(i in combinations){
      m <- intToBits(i)
      score <- sum(unlist(v[m==1], use.names=FALSE))
      weight <- sum(unlist(w[m==1], use.names=FALSE))

      if(score > solution_score & weight <= W){ # compare and check weightlimit
        solution <- m
        solution_score <- score
      }
    }
    sol <- list("value"=solution_score, "elements"=which(solution==1))

    return(sol)
  }
  else{
    ncores <- parallel::detectCores()-1
    cluster <- parallel::makeCluster(ncores, type = "PSOCK")

    parallel_brute <- function(i){
      m <- intToBits(i)
      weight <- sum(unlist(w[m==1], use.names=FALSE))

      if(weight <= W){ # compare and check weightlimit
        solution <- m
        solution_score <- sum(unlist(v[m==1], use.names=FALSE))
        return(list("value"=solution_score, "elements"=which(solution==1)))
      }
    }

    a <- parLapply(cluster, combinations, parallel_brute)
    stopCluster(cluster)
    b <- a[!sapply(a, is.null)]
    c <- unlist(b[which.max(sapply(b, `[`, 1))], use.names=FALSE)
    return(list("value"=c[1], "elements"=c(c[2:3])))
  }
}
