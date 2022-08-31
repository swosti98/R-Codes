normalplot<-function(m,sd,region=0){
  x<-seq(m-(3.5)*sd,m+(3.5)*sd,length=1000)
  y<-dnorm(x,m,sd)
  plot(x,y,type="l",xlab="",ylab="", bty="n", yaxt="n")
  h <- dnorm(m,m,sd)
  z<-x[x>region[1]]
  z<-z[z<region[2]]
  polygon(c(region[1],z,region[2]),
          c(0,dnorm(z,m,sd),0),col="gray")
  abline(v=m)
  abline(h=0)}
normalplot(0,1)
