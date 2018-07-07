### Erlang Random Number Generator ( alpha, beta ) alpha-Erlang( 1/beta ) E=alpha/beta 
### or Gamma Random Number Generator ( alpha, 1/beta )
Erlang <- function( seed.er, n.er, alpha.er, beta.er){
  m <- 2^(31)-1
  ans <- rep(0,n.er)
  for( i in 1 : n.er){
    u.er <- LLG(seed.er, alpha.er )
    temp <- prod(u.er)
    ans[i] <- -log(temp)/beta.er
    seed.er <- u.er[alpha.er]*m
  }
  return(list(sample = ans, seed = seed.er))
}