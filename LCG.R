LCG<-function(z.0, n.LCG, m.LCG, a.LCG, c.LCG){
  z.LCG<-c(z.0)
  for(i in 1 : n.LCG ){
    temp.LCG<-(a.LCG*z.LCG[i]+c.LCG)%%m.LCG
    z.LCG<-c(z.LCG,temp.LCG)
  }
  z.LCG<-z.LCG[-1]
  z.LCG<-z.LCG/m.LCG
  return(z.LCG)
}
n=1000
set.seed(99)
m=sample(100,1)
a=sample(100,1)
c=sample(100,1)
Z<-LCG(6,4000,2^63-1,17^5,0)
hist(Z)