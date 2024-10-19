# dir_here = file.path(proj_dir, "runs", "osg_runs", "F=high_sigmaR=0.1", "1_H")
# setwd(dir_here)
## R script for running likelihood profiles - down
library(r4ss)

dir_here = getwd()
dir_profile = file.path(dir_here, "profile_down")
dir.create(dir_profile)
r0_maxdiff = 1
r0_step = 0.05

tmp_starter = SS_readstarter(file = file.path(dir_here, "starter.ss"), verbose = FALSE)
tmp_starter$ctlfile = "control_modified.ss"
tmp_starter$prior_like = 1
tmp_starter$init_values_src = 1
SS_writestarter(tmp_starter, dir = dir_here, file = "starter.ss", overwrite = TRUE, verbose = FALSE, warn = FALSE)
rm(list=c("tmp_starter"))

tmp_par = SS_readpar_3.30(parfile=paste0(dir_here,"/ss.par"), 
                          datsource=paste0(dir_here,"/ss3.dat"), 
                          ctlsource=paste0(dir_here,"/control.ss_new"), verbose = FALSE) ## CTL file name, 

r0_original = tmp_par$SR_parms$ESTIM[1]
r0_down_vec = seq(r0_original - 1, r0_original + 1, by = 0.2) #seq(from=r0_original,to=r0_original-r0_maxdiff,by=-r0_step)
rm(list=c("tmp_par"))

FileList=list.files()
FileList=setdiff(FileList,c("Start.tar.gz","profile_up"))
file.copy(paste0(dir_here,"/",FileList),dir_profile,overwrite=TRUE)
profile_down = profile(dir = dir_profile, oldctlfile = "control.ss_new", newctlfile = "control_modified.ss", 
                       string = "SR_LN(R0)", profilevec = r0_down_vec,
                       usepar = TRUE,globalpar = FALSE,parstring = "# SR_parm[1]:",
                       saveoutput = FALSE,overwrite = TRUE,exe = "ss",verbose = FALSE,extras="-nohess -nox") 

likes <- profile(
  dir = dir_profile,
  newctlfile = "control_modified.ss",
  string = "SR_LN",
  profilevec = r0_down_vec,
  exe = "ss",
  verbose = FALSE,
  extras="-nohess -nox",
  usepar = TRUE,
  parstring = "# SR_parm[1]:"
)


write.csv(profile_down,file=file.path(dir_profile,"profile_down.csv"))
unlink(setdiff(list.files(), c("profile_down.csv", "Start.tar.gz")), recursive=TRUE)

head(profile_down)
profile_down %>%
filter(converged == T) %>% 
mutate(Max_total = 70.2947) %>% 
mutate(across("TOTAL":"Recruitment", ~./Max_total)) %>% 
ggplot(aes(x = Value)) + 
geom_line(aes(y = TOTAL)) +
geom_line(aes(y = Recruitment), color = "orange")


#need to add calculating change in likelihood
pivot_longer(cols = c("TOTAL", "Catch", "Equil_catch", "Survey", "Length_comp", "Age_comp", "Recruitment")) %>% 

ggplot(aes(x = Value, y = value)) + 
geom_line(aes(group = name, color = name))

profile_mods <- SSgetoutput(dirvec = dir_profile, keyvec = c(1:11))
profile_mods_sum <- SSsummarize(profile_mods)
SSplotProfile(profile_mods_sum,
  profile.string = "SR_LN",
  profile.label = "SR_LN(R0)"
)
profile_mods_sum$likelihoods

scen_mods <- SSgetoutput(dirvec = c(
    file.path(main.dir, "HRF_5_yrfwd", "1", "em"),
    file.path(main.dir, "SQ_5_yrfwd", "1", "em")
))
scen_mods_sum <- SSsummarize(scen_mods)
SSplotComparisons(scen_mods_sum)
