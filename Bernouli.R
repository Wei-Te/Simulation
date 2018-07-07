### Bernouli Random Number Generator
Ber <- function( seed.Ber, n.Ber, prob.Ber = 1/2 ){
  u.Ber <- LLG(seed.Ber, n.Ber)
  result.Ber <- rep(0, n.Ber)
  result.Ber[which(u.Ber <= prob.Ber)] <- 1
  return( result.Ber )
}