Hexp <- function( seed.exp, n.exp, lambda.exp, prob.exp){
  F.exp <- cumsum( prob.exp )
  u.exp <- seed.exp
  ans <- temp <- rep(0, n.exp)
  temp <- 0
  m <- 2^(31)-1
  for( i in 1 : n.exp){
    u.exp <- LLG(seed.exp, 2 )
    temp <- which( ( u.exp$sample[1] < F.exp )==T)[1]
    ans[i] <- -log( u.exp$sample[2] )/lambda.exp[temp]
    seed.exp <- u.exp$sample[2]*m
  }
  return( list(sample = ans, seed = seed.exp) )
}