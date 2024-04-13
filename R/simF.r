
# Simple effort model ===========================
# From Carruthers et al. (2012) - function extracted from his package DLMtool in R
#' @param nsim - number of simulated effort datasets
#' @param Esd - inter-annual effort variability
#' @param nyears - number of years for simulated effort
#' @param dFmin - minimum final effort gradient. Three types of effort trends flat effort trend (mean effort graident over nyears = -0.02 and 0.02), increasing effort trend (mean effort graident over nyears > 0.02), and decreasing effort trend (mean effort graident over nyears < -0.02)
#' @param dFmax - maximum final effort gradient 
#' @param bb - intercept or start effort
#' @param scale  - to scale effort when standardized to 1
getFhist<-function(nsim,Esd,nyears,dFmin,dFmax,bb,scale){
    ne<-nsim*10                                           # Number of simulated effort datasets
    dEfinal<-runif(ne,dFmin,dFmax)                        # Sample the final gradient in effort
    a<-(dEfinal-bb)/nyears                                # Derive slope to get there from intercept
    a<-array(a,dim=c(ne,nyears))                          # Slope array
    bb<-array(bb,dim=c(ne,nyears))                        # Intercept array
    x<-array(rep(1:nyears,each=ne),dim=c(ne,nyears))      # Year array
    dE<-a*x+bb                                            # Change in effort
    E<-array(NA,dim=c(ne,nyears))                         # Define total effort array
    E[,1]<-dE[,1]
    for(y in 2:nyears){
        E[,y]<-apply(dE[,1:y],1,sum)
    }
    
    E<-scale*E/array(apply(E,1,mean),dim=c(ne,nyears))          # Standardise Effort to average 1
    
    cond<-apply(E,1,min)>0
    pos<-(1:ne)[cond]
    pos<-pos[1:nsim]
    
    E<-E[pos,]                                            # Sample only those without negative effort
    Emu<--0.5*Esd^2
    Eerr<-array(exp(rnorm(nyears*nsim,rep(Emu,nyears),rep(Esd,nyears))),c(nsim,nyears))
    E*Eerr
}

getSeasonFrate<-function(Fsim=Fsim,nAnnualFrate=nyears){        
  SeasonFrate <- NULL
  for (j in 1:nAnnualFrate)
  { 
    SeasonFrate[(1+4*(j-1)):(4*j)]<- rep(Fsim[j]/4,4)     # equal rate among season    
  }        
  return(SeasonFrate)        
}


# pdf(file="Simulated Fishing mortality.pdf",width=8,height=10)
# par(mfrow=c(3,1),mar=c(4,4,0.5,0.5), oma=c(1.5,1.5,0.5,0.5),cex=1.2)
 
# scale = 0.1 /1.323
#  Edat<-getFhist(nsim=100,Esd=0.001,nyears=75,dFmin=-0.07,dFmax=0.07,bb=-0.1,scale=scale)
#  matplot(x=seq(1949,2023,1),t(Edat),type='l',ylim=c(0,0.45),xlab="",ylab="")
#  D<-round(mean(apply(Edat,1,mean)),2)
#  legend('topleft',legend=paste("Mean F=",D,sep=""),bty='n')
 
#  scale = 0.2 /1.323
#  Edat<-getFhist(nsim=100,Esd=0.001,nyears=75,dFmin=-0.07,dFmax=0.07,bb=-0.1,scale=scale)
#  matplot(x=seq(1949,2023,1),t(Edat),type='l',ylim=c(0,0.45),xlab="",ylab="Fishing mortality")
#  D<-round(mean(apply(Edat,1,mean)),2)
#  legend('topleft',legend=paste("Mean F=",D,sep=""),bty='n')
 
#  scale = 0.3 /1.323
#  Edat<-getFhist(nsim=100,Esd=0.001,nyears=75,dFmin=-0.07,dFmax=0.07,bb=-0.1,scale=scale)
#  matplot(x=seq(1949,2023,1),t(Edat),type='l',ylim=c(0,0.45),xlab="Year",ylab="")
#  D<-round(mean(apply(Edat,1,mean)),2)
#  legend('topleft',legend=paste("Mean F=",D,sep=""),bty='n')
 
#  dev.off() 


 