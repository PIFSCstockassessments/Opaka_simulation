library(r4ss)
library(tidyverse)
library(ss3sim)

main.dir <- this.path::here(.. = 1)
source(file.path(main.dir, "R", "simF.r"))
dat <- SS_readdat_3.30(file = file.path(main.dir, "OM_sim_data", "data.ss"))
dat$catch$catch <- 1
head(dat$catch)

dat$CPUE$obs <- ifelse(dat$CPUE$index == 1, 1, dat$CPUE$obs)
dat$CPUE$se_log <- ifelse(dat$CPUE$index == 1, 0.2, dat$CPUE$se_log)
SS_writedat_3.30(dat, outfile = file.path(main.dir, "OM_sim_data", "data.ss"), overwrite = T)

dat <- SS_readdat_3.30(file = file.path(main.dir, "OM_sim_data", "data.ss_new"), section = 3)
SS_writedat_3.30(dat, outfile = file.path(main.dir, "OM_sim_data", "with_sim_data", "data.ss"), overwrite = T)
rep <- SS_output(dir = file.path(main.dir, "OM_sim_data"))
rep <- SS_output(dir = file.path(main.dir, "OM_sim_data", "with_sim_data"))
SS_plots(rep)

## Create F timeseries with no variability
scale = 0.1 /1.323 #scales how large F gets
#all effort trends
Edat<-getFhist(nsim=10,Esd=0.001,nyears=75,dFmin=-0.07,dFmax=0.07,bb=-0.1,scale=scale)
matplot(x=seq(1949,2023,1),t(Edat),type='l',ylim=c(0,0.2),xlab="",ylab="")
D<-round(mean(apply(Edat,1,mean)),2)
legend('topleft',legend=paste("Mean F=",D,sep=""),bty='n')
 
#flat effort trends (effort gradients between -0.02 and 0.02)
Edat<-getFhist(nsim=10,Esd=0.001,nyears=75,dFmin=-0.02,dFmax=0.02,bb=-0.1,scale=scale)
matplot(x=seq(1949,2023,1),t(Edat),type='l',ylim=c(0,0.2),xlab="",ylab="")
D<-round(mean(apply(Edat,1,mean)),2)
legend('topleft',legend=paste("Mean F=",D,sep=""),bty='n')

#increasing effort trends
Edat<-getFhist(nsim=10,Esd=0.001,nyears=75,dFmin=-0.1,dFmax=-0.02,bb=-0.1,scale=scale)
matplot(x=seq(1949,2023,1),t(Edat),type='l',ylim=c(0,0.2),xlab="",ylab="")
D<-round(mean(apply(Edat,1,mean)),2)
legend('topleft',legend=paste("Mean F=",D,sep=""),bty='n')

#decreasing effort trends
Edat<-getFhist(nsim=10,Esd=0.001,nyears=75,dFmin=0.02,dFmax=0.07,bb=-0.1,scale=scale)
matplot(x=seq(1949,2023,1),t(Edat),type='l',ylim=c(0,0.2),xlab="",ylab="")
D<-round(mean(apply(Edat,1,mean)),2)
legend('topleft',legend=paste("Mean F=",D,sep=""),bty='n')
#changing intercept to positive allows for negative min and max values to create decreasing trend
Edat<-getFhist(nsim=10,Esd=0.001,nyears=75,dFmin=-0.1,dFmax=-0.02,bb=0.1,scale=scale)
matplot(x=seq(1949,2023,1),t(Edat),type='l',ylim=c(0,0.2),xlab="",ylab="")
D<-round(mean(apply(Edat,1,mean)),2)
legend('topleft',legend=paste("Mean F=",D,sep=""),bty='n')

#all effort trends again
Edat<-getFhist(nsim=10,Esd=0.001,nyears=75,dFmin=-0.07,dFmax=0.07,bb=0.1,scale=scale)
matplot(x=seq(1949,2023,1),t(Edat),type='l',ylim=c(0,0.2),xlab="",ylab="")
D<-round(mean(apply(Edat,1,mean)),2) #mean F = 0.08
legend('topleft',legend=paste("Mean F=",D,sep=""),bty='n')

#increase F scale 
scale = 0.2 /1.323
Edat<-getFhist(nsim=10,Esd=0.001,nyears=75,dFmin=-0.07,dFmax=0.07,bb=0.1,scale=scale)
matplot(x=seq(1949,2023,1),t(Edat),type='l',ylim=c(0,0.4),xlab="",ylab="")
D<-round(mean(apply(Edat,1,mean)),2) #mean F = 0.15
legend('topleft',legend=paste("Mean F=",D,sep=""),bty='n')


## Steps for simulating fishery data
## 1. Have OM set up like ss3sim cod-om with dummy input data for all fishery fleets
## 2. Run with no estimation or hessian 
## 3. Modify starter to use data_expval.ss (rename if you want to first) and run ss -nohess. This provides the baseline model to compare simulated Fs to.
## 4. Simulate F time series with `getFhist()`
## 5. use `ss3sim::change_f()` to add F timeseries to control file and turn on/off extra settings
## 6. Run model again with no estimation or hessian
## 7. data_expval.ss contains expected catch and CPUE (with no error) based on the selectivity defined of the fishery (make sure Number of data files = 2)
## 8. Use expected data values in OM and re-run model 

dat <- r4ss::SS_readdat(
  system.file("extdata", "models", "cod-om", "codOM.dat", package = "ss3sim"),
  verbose = FALSE
)
ctl <- r4ss::SS_readctl(
  system.file("extdata", "models", "cod-om", "codOM.ctl", package = "ss3sim"),
  verbose = FALSE, use_datlist = TRUE, datlist = dat
)
# Using original vector-style inputs
newctl <- change_f(years = 1:50, fleets = 1, fvals = 0.2, ctl_list = ctl)
SS_writectl_3.30(newctl, outfile = file.path(main.dir, "ss3sim-example", "cod-om", "f_control.ss"))
cod_oms <- SSgetoutput(dirvec = c(file.path(main.dir, "ss3sim-example", "cod-om"), 
file.path(main.dir, "ss3sim-example", "change_f_OM")))
cod_oms_sum <- SSsummarize(cod_oms)
SSplotComparisons(cod_oms_sum)
SS_plots(cod_oms$replist1)


# steps 1-3
rep1 <- SS_output(dir = file.path(main.dir, "ss3sim-example", "cod-om"))
SS_plots(rep1)

# step 4
scale = 0.1 /1.323
Edat<-getFhist(nsim=10,Esd=0.001,nyears=100,dFmin=-0.07,dFmax=0.07,bb=0.1,scale=scale)
matplot(x=seq(1949,2048,1),t(Edat),type='l',ylim=c(0,0.2),xlab="",ylab="Fishing mortality", lwd = 3,cex.lab=1.5,cex.axis=1.5)
D<-round(mean(apply(Edat,1,mean)),2)
legend('topleft',legend=paste("Mean F=",D,sep=""),bty='n',cex = 2)

# step 5
dat <- r4ss::SS_readdat(
  file.path(main.dir, "ss3sim-example", "cod-om-simF", "data_new.ss"),
  verbose = FALSE
)
ctl <- r4ss::SS_readctl(
  file.path(main.dir, "ss3sim-example", "cod-om-simF", "codOM.ctl"),
  verbose = FALSE, use_datlist = TRUE, datlist = dat
)
newctl <- change_f(years = 1:100, fleets = 1, fvals = Edat[1,], ctl_list = ctl)
SS_writectl_3.30(newctl, outfile = file.path(main.dir, "ss3sim-example", "cod-om-simF", "codOM.ctl"), overwrite = TRUE)

# step 8
rep2 <- SS_output(dir = file.path(main.dir, "ss3sim-example", "cod-om-simF"))
SS_plots(rep2)

## create loop for steps 5-8
scale = 0.1 /1.323
Edat<-getFhist(nsim=10,Esd=0.001,nyears=100,dFmin=-0.12,dFmax=0.12,bb=0.1,scale=scale)
# matplot(x=seq(1,100,1),t(Edat),type='l',ylim=c(0,0.2),xlab="",ylab="Fishing mortality", lwd = 3,cex.lab=1.5,cex.axis=1.5)
# D<-round(mean(apply(Edat,1,mean)),2)
# legend('topleft',legend=paste("Mean F=",D,sep=""),bty='n')
# legend("topright", legend = paste0("Effort series ", seq(1,10)), lty = 1:10, col = 1:10, lwd = 2)
cols <- r4ss::rich.colors.short(n=10)
plot(x = seq(1,100), y = Edat[1,], type = "l",ylim = c(0,.2),ylab = "Fishing Mortality", col = cols[1], lwd = 3)#increasing
lines(x = seq(1, 100), y = Edat[2,], col = cols[2], lwd = 3) #flat
lines(x = seq(1, 100), y = Edat[3,], col = cols[3], lwd = 3) #flat
lines(x = seq(1, 100), y = Edat[4,], col = cols[4], lwd = 3) #increasing
lines(x = seq(1, 100), y = Edat[5,], col = cols[5], lwd = 3) #decreasing
lines(x = seq(1, 100), y = Edat[6,], col = cols[6], lwd = 3) #
lines(x = seq(1, 100), y = Edat[7,], col = cols[7], lwd = 3) #flat
lines(x = seq(1, 100), y = Edat[8,], col = cols[8], lwd = 3) #increasing
lines(x = seq(1, 100), y = Edat[9,], col = cols[9], lwd = 3) #flat
lines(x = seq(1, 100), y = Edat[10,], col = cols[10], lwd = 3) #decreasing
D<-round(mean(apply(Edat,1,mean)),2)
legend('topleft',legend=paste("Mean F=",D,sep=""),bty='n')
legend("topright", legend = paste0("Series ", seq(1,10)), col = cols, lwd = 2)

for(i in 1:nrow(Edat)){

    new.dir <- file.path(main.dir, "ss3sim-example", paste0("cod-om-simF-", i))
    dir.create(new.dir)
    files_to_cp <- c("codOM.ctl", "data_new.ss", "forecast.ss", "starter.ss", "ss.exe")
    file.copy(from = file.path(main.dir, "ss3sim-example", "cod-om", files_to_cp), 
            to = new.dir, overwrite = T)

    dat <- r4ss::SS_readdat(
    file.path(new.dir, "data_new.ss"),
    verbose = FALSE
    )
    ctl <- r4ss::SS_readctl(
    file.path(new.dir, "codOM.ctl"),
    verbose = FALSE, use_datlist = TRUE, datlist = dat
    )

    newctl <- change_f(years = 1:100, fleets = 1, fvals = Edat[i,], ctl_list = ctl)

    SS_writectl_3.30(newctl, outfile = file.path(new.dir, "codOM.ctl"), overwrite = TRUE)

    r4ss::run(dir = new.dir, exe = "ss", extras = "-nohess")
    file.rename(from = file.path(new.dir, "data_expval.ss"), to = file.path(new.dir, "data_newF.ss"))
    start <- SS_readstarter(file.path(new.dir, "starter.ss"))
    start$datfile <- "data_newF.ss"
    SS_writestarter(start, dir = new.dir, overwrite = T)
    r4ss::run(dir = new.dir, exe = "ss", extras = "-nohess", skipfinished = F)

}

simFreps <- SSgetoutput(dirvec = paste0(file.path(main.dir, "ss3sim-example"), "/cod-om-simF-", seq(1,10)))
simFsum <- SSsummarize(simFreps)
SSplotComparisons(simFsum)

rep_d <- SS_output(dir = file.path(main.dir, "ss3sim-example", "cod-om-simF-8"))
SS_plots(rep_d)
rep_f <- SS_output(dir = file.path(main.dir, "ss3sim-example", "cod-om-simF-10"))
SS_plots(rep_f)
rep_i <- SS_output(dir = file.path(main.dir, "ss3sim-example", "cod-om-simF-6"))
SS_plots(rep_i)

simFreps <- SSgetoutput(dirvec = paste0(file.path(main.dir, "ss3sim-example"), "/cod-om-simF-", c(8,10,6)))
simFsum <- SSsummarize(simFreps)
SSplotComparisons(simFsum, subplots = 1, legendlabels = c("Decreasing E", "Flat E", "Increasing E"), col = cols[c(8,10,6)])
SSplotComparisons(simFsum, subplots = 7, legendlabels = c("Decreasing E", "Flat E", "Increasing E"), col = cols[c(8,10,6)])
head(simFsum$indices)
simFsum$indices  |> 
ggplot(aes(x = Yr, group = as.factor(imodel))) + 
geom_point(aes(y = Obs, color = as.factor(imodel))) +
geom_line(aes(y = Exp, color = as.factor(imodel))) +
theme_classic() +
scale_color_manual(values = cols[c(8,10,6)], labels = c("Decreasing E", "Flat E", "Increasing E")) +
labs(x = "Year", y = "Index", color = "")  +
theme(text = element_text(size = 16))


