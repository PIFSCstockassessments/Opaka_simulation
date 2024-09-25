# dir_here = file.path(proj_dir, "runs", "osg_runs", "F=high_sigmaR=0.1", "1_H")
# setwd(dir_here)
## R script for running likelihood profiles - up
library(r4ss)

dir_here = getwd()
dir_profile = file.path(dir_here, "profile_up")
dir.create(dir_profile)
r0_maxdiff = 1
r0_step = 0.01

tmp_starter = SS_readstarter(file = file.path(dir_here, "starter.ss"), verbose = FALSE)
tmp_starter$ctlfile = "control_modified.ss"
tmp_starter$prior_like = 1
tmp_starter$init_values_src = 1
SS_writestarter(tmp_starter, dir = dir_here, file = "starter.ss", overwrite = TRUE, verbose = FALSE, warn = FALSE)
rm(list=c("tmp_starter"))

tmp_par = SS_readpar_3.30(parfile=paste0(dir_here,"/ss3.par"), 
                          datsource=paste0(dir_here,"/ss3.dat"), 
                          ctlsource=paste0(dir_here,"/control.ss_new"), verbose = FALSE) ## CTL file name, 
r0_original = tmp_par$SR_parms$ESTIM[1]
r0_up_vec = seq(from=r0_original,to=r0_original+r0_maxdiff,by=r0_step)
rm(list=c("tmp_par"))

FileList=list.files()
FileList=setdiff(FileList,c("Start.tar.gz","profile_up"))
file.copy(paste0(dir_here,"/",FileList),dir_profile,overwrite=TRUE)
profile_up = r4ss::profile(dir = dir_profile, oldctlfile = "control.ss_new", newctlfile = "control_modified.ss", 
                       string = "SR_LN(R0)", profilevec = r0_up_vec,
                       usepar = TRUE,globalpar = FALSE,parstring = "# SR_parm[1]:",
                       saveoutput = FALSE,overwrite = TRUE,exe = "ss_linux",verbose = FALSE) 

write.csv(profile_up,file="profile_up.csv")
unlink(setdiff(list.files(), c("profile_up.csv", "Start.tar.gz")), recursive=TRUE)
