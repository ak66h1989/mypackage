randomplot <- function(n, dist=c("normal", "uniform")){
  #input validation
  dist <- match.arg(dist)
  stopifnot(n < 1e6)

  if(dist == "normal"){
    hist(rnorm(n))
  }

  if(dist == "uniform"){
    hist(runif(n))
  }

  #return nothing
  invisible();
}
