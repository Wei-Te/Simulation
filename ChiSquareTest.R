##########################################################################################################
#                   ~~~Revise Note~~~ 
# 03/19 : Revise the return value!!!
#         return( c(paste("chi-square = ", round(chi.sum, digits = 5), "<", round(qchisq(1-alpha.cst, df=k.cst-1), digits = 5), "=", "chi-square,", k.cst-1,",", 1-alpha.cst),
#         " This sample must come from the uniform distribution. ") )
#
##########################################################################################################
CST<-function(x.cst, k.cst = 100, alpha.cst = 0.05){
  n<-length(x.cst)
  if( k.cst >= 100 && n/k.cst >=5){
    
    sub <- seq(from=0, to=1, by=1/k.cst)
    f <- rep(0, k.cst)
    chi.sum <- 0
    for( j.cst in 1 : k.cst){
      for( i.cst in 1 : n){
        if( sub[ j.cst ] <= x.cst[i.cst] &&  x.cst[i.cst] <= sub[ (j.cst+1) ] )
          f[ j.cst ] <- f[ j.cst ] + 1
      }
      chi.sum <- chi.sum+ (f[j.cst]-n/k.cst)^2*(k.cst/n)
    }
    if(chi.sum < qchisq(1-alpha.cst, df=k.cst-1)){
      return( list( Test = round(chi.sum, digits = 5), 
                    Chisq = round(qchisq(1-alpha.cst, df=k.cst-1), digits = 5),
                    From.uniform = T))
    }else{
      return( list( Test = round(chi.sum, digits = 5), 
                    Chisq = round(qchisq(1-alpha.cst, df=k.cst-1), digits = 5),
                    From.uniform = F))
    }
  }else{
    warning("k must >= 100 & n/k must >= 5")
  }
}