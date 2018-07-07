### Binomial Random Number Generator
Bin <- function( seed.bin, n.bin, prob.bin, size.bin){
  ans<-rep(0, size.bin)
  m <- 2^(31)-1
  for( i in 1 : size.bin){
    u.bin <- LLG(seed.bin, n.bin )
    ans[i] <- length(which(u.bin <= prob.bin))
    seed.bin<- u.bin[n.bin]*m
  }
  return( ans )
}