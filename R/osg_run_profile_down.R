# dir_here = file.path(proj_dir, "runs", "osg_runs", "F=high_sigmaR=0.1", "1_H")
# setwd(dir_here)
## R script for running likelihood profiles - down
library(r4ss)

dir_here = getwd()
dir_profile = file.path(dir_here, "profile_down")
dir.create(dir_profile)
r0_maxdiff = 1
r0_step = 0.01

om = strsplit(getwd(),split = "/")[[1]][8]
em_miss = strsplit(getwd(), split = "/")[[1]][9]

tmp_starter = SS_readstarter(file = file.path(dir_here, "starter.ss"), verbose = FALSE)
tmp_starter$ctlfile = "control_modified.ss"
tmp_starter$prior_like = 1
tmp_starter$init_values_src = 1
SS_writestarter(tmp_starter, dir = dir_here, file = "starter.ss", overwrite = TRUE, verbose = FALSE, warn = FALSE)
rm(list=c("tmp_starter"))

tmp_ctl = SS_readctl_3.30(file = file.path(dir_here, "control.ss_new"), datlist = file.path(dir_here, "ss3.dat"), verbose = FALSE)
tmp_ctl$size_selex_parms$PHASE[9] <- 1
SS_writectl_3.30(tmp_ctl, outfile = "control.ss_new", verbose = FALSE, overwrite = TRUE)
rm(list=c("tmp_ctl"))

tmp_par = SS_readpar_3.30(parfile=paste0(dir_here,"/ss3.par"), 
                          datsource=paste0(dir_here,"/ss3.dat"), 
                          ctlsource=paste0(dir_here,"/control.ss_new"), verbose = FALSE) ## CTL file name, 
SS_writepar_3.30(tmp_par, outfile = "ss.par")
r0_original = tmp_par$SR_parms$ESTIM[1]
r0_down_vec = seq(from=r0_original,to=r0_original-r0_maxdiff,by=-r0_step)
rm(list=c("tmp_par"))

FileList=list.files()
FileList=setdiff(FileList,c("Start.tar.gz","profile_up","ss3.par"))
file.copy(paste0(dir_here,"/",FileList),dir_profile,overwrite=TRUE)
profile_down = profile(dir = dir_profile, oldctlfile = "control.ss_new", newctlfile = "control_modified.ss", 
                       string = "SR_LN(R0)", profilevec = r0_down_vec,
                       usepar = TRUE,globalpar = FALSE,parstring = "# SR_parm[1]:",
                       saveoutput = FALSE,overwrite = TRUE,exe = "ss_linux",verbose = FALSE) 
profile_down$om <- om
profile_down$em <- strsplit(em_miss,split = "_")[[1]][1]
profile_down$misspec_id <- strsplit(em_miss,split = "_")[[1]][2]
write.csv(profile_down,file="profile_down.csv")
unlink(setdiff(list.files(), c("profile_down.csv", "Start.tar.gz")), recursive=TRUE)
