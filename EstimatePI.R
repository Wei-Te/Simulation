LLG<-function(z.0, size.LLG){
  if(z.0 > 0){
    z.LLG <- c(z.0,rep(0,size.LLG))
    m.LLG <- 2^(31)-1
    for(i in 1 : size.LLG )
      z.LLG[i+1] <- (16807*z.LLG[i])%%m.LLG
    z.LLG<-z.LLG[-1]
    z.LLG<-z.LLG/m.LLG
    return(z.LLG)
  }else{
    warning("z.0 must > 0")
  }
}

PI <- function( seed.pi, size.pi, n.pi, alpha.pi ){
  size.pi <- size.pi/n.pi
  temp.1 <- 2^(31)-1
  bin <- rep(0, size.pi)
  for( i in 1 : size.pi){
    X <- LLG(seed.pi, n.pi)
    seed.pi <- X[n.pi]*temp.1
    Y <- LLG(seed.pi, n.pi)
    bin[i] <- length(which((X^2 + Y^2) <= 1))
    seed.pi <- Y[n.pi]*temp.1
  }
  bin.bar <- mean(bin)
  bin.var <- sqrt((sum(bin^2)-size.pi*bin.bar^2)/(size.pi-1))
  temp.2 <- bin.var/sqrt(size.pi)*qnorm(1-alpha.pi/2)
  CI.pi <- matrix(c(bin.bar-temp.2,bin.bar+temp.2),
                  dimnames = list(c(),c(paste((alpha.pi/2)*100,"%"), 
                                        paste((1-alpha.pi/2)*100,"%"))), 
                  ncol = 2)
  return(list(pi.hat=bin.bar/n.pi*4, Confidence.Interval= CI.pi/n.pi*4))
}

# (a)
tic <- Sys.time()
PI(123, 1e5, 1e2, 0.05)
toc <- Sys.time()
toc-tic

# (b)
tic <- Sys.time()
PI(123, 1e7, 1e2, 0.05)
toc <- Sys.time()
toc-tic
