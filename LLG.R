#### Learmonth-Lewis Generator (Uniform random variable)
LLG<-function(z.0, size.LLG){
  if(z.0 > 0){
    z.LLG <- c(z.0,rep(0,size.LLG))
    m.LLG <- 2^(31)-1
    for(i in 1 : size.LLG )
      z.LLG[i+1] <- (16807*z.LLG[i])%%m.LLG
    z.LLG <- z.LLG[-1]
    z.sample <- z.LLG/m.LLG
    return( list(sample = z.sample, seed = z.LLG) )
  }else{
    warning("z.0 must > 0")
  }
}