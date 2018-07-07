DiscR <- function( seed.disc, size.disc, x.disc, prob.disc){
  temp <- LLG(seed.disc, size.disc )
  u.disc <- temp$sample
  seed.disc <- temp$seed[size.disc]
  F.disc <- cumsum( prob.disc )
  ans <- rep(0, size.disc)
  for( i in 1 : length(u.disc) )
    ans[i] <- which( ( u.disc[i] < F.disc )==T)[1]
  return( list(sample = x.disc[ans], seed = seed.disc) )
}