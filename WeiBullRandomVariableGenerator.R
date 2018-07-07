###Weibull Random Number Generator
weibull <- function( seed.wei, n.wei, alpha.wei, beta.wei ){
  u.wei <- LLG(seed.wei, n.wei )
  return( (-log(u.wei))^(1/alpha.wei)*beta.wei )
}