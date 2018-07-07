##########################################################################################################
#                   ~~~Revise Note~~~ 
# 03/19 : Revise the return value!!!
#         return( c(paste("R = ", round(R.rt, digits = 5), "<", round(qchisq(1-alpha.rt, 6), digits = 5), 
#                 "=", "chi-square, 6,", 1-alpha.rt),
#                 "This sample satisfy the randomness."))
#
##########################################################################################################
run.test<-function(x.rt, alpha.rt = 0.05){
  r.rt<-rep(0,6)
  inc.rt<-1
  n.rt <- length(x.rt)
  for( i.rt.1 in 1 : n.rt ){
    temp.rt<-inc.rt
    if( i.rt.1 < n.rt ){
      if( x.rt[i.rt.1+1] >= x.rt[i.rt.1] ){
        inc.rt<-inc.rt+1
      }else{
        inc.rt<-1
      }
      if( inc.rt==1 ){
        if( temp.rt <= 6){
          r.rt[temp.rt]<-r.rt[temp.rt]+1
        }else{
          r.rt[6]<-r.rt[6]+1
        }
      }
    }else if( i.rt.1 == n.rt ){
      if( temp.rt <= 6){
        r.rt[temp.rt]<-r.rt[temp.rt]+1
      }else{
        r.rt[6]<-r.rt[6]+1
      }
    }
  }
  R.rt <- 0
  a.rt <- matrix( c( 4529.4, 9044.9, 13568,  18091,  22615,  27892,
                     9044.9,  18097, 27139,  36187,  45234,  55789,
                     13568,  27139, 40721,  54281,  67852,  83685,
                     18091,  36187, 54281,  72414,  90470, 111580,
                     22615,  45234, 67852,  90470, 113262, 139476,
                     27892,  55789, 83685, 111580, 139476, 172860),
                  ncol = 6, nrow = 6)
  b.rt <- c(1/6, 5/24, 11/120, 19/720, 29/5040, 1/840)
  for( i.rt in 1 : 6 ){
    for( j.rt in 1 : 6 ){
      R.rt<-R.rt+a.rt[i.rt, j.rt]*
        (r.rt[i.rt]-n.rt*b.rt[i.rt])*
        (r.rt[j.rt]-n.rt*b.rt[j.rt])/n.rt
    }
  }
  if( R.rt < qchisq(1-alpha.rt, 6) ){
    return( list( Test = round(R.rt, digits = 5), 
                  Chisq = round(qchisq(1-alpha.rt, 6), digits = 5),
                  Randomness = T))
  }else{
    return( list( Test = round(R.rt, digits = 5), 
                  Chisq = round(qchisq(1-alpha.rt, 6), digits = 5),
                  Randomness = F))
  }
}