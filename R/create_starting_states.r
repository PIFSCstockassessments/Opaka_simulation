### Create 100 unique starting scenarios for Opaka Simulation
##

library(r4ss)
library(this.path)
library(tidyverse)

main.dir <- this.path::here(.. = 1)

model.dir <- file.path(main.dir, "SSMSE_models")
om.dir <- file.path(model.dir, "OM")
dir.mcmc <- file.path(model.dir, "mcmc")
dir.create(dir.mcmc)

#create setseed.csv for simulations
#set.seed <- data.frame(Run = seq(1, 100), Seed = floor(runif(100, min = 1, max = 10000)))
#write.csv(set.seed, file.path(main.dir, "Inputs", "setseed.csv"), row.names = FALSE)
set.seed <- read.csv(file.path(main.dir, "Inputs", "setseed.csv"))

## Read in starter file and change settings for running MCMC 
start <- SS_readstarter(file = file.path(om.dir, "starter.ss"))
start$init_values_src <- 0
start$run_display_detail <- 0
start$MCMCburn <- 100
start$MCMCthin <- 10
SS_writestarter(start, dir = om.dir, overwrite = TRUE)

forecast <- SS_readforecast(file = file.path(om.dir, "forecast.ss"))
forecast$Nforecastyrs <- 0
forecast$Forecast <- 0
SS_writeforecast(forecast, dir = om.dir, overwrite = TRUE)

files. <- c("data_updated.ss", "control.ss", "ss.exe", "forecast.ss", "starter.ss")
file.copy(file.path(om.dir, files.), dir.mcmc, overwrite = TRUE)

### create new folders
iterations <- seq(1, 10, by = 1)
dir.it <- paste0(model.dir, "/start_pop_", iterations)
sapply(dir.it, dir.create)

#Run ss with mcmc and mcsave first then run ss -mceval to estimate posteriors
shell(paste("cd/d", dir.mcmc, "&& ss -mcmc 11000 -mcsave 10 >NUL 2>&1", sep = " "))
shell(paste("cd/d", dir.mcmc, "&& ss -mceval >NUL 2>&1", sep = " "))

files.new <- list("data.ss", "control.ss", "forecast.ss", "starter.ss", "ss.exe", "ss.par")

for(i in 1:length(dir.it)){
  files.path <- file.path(dir.mcmc, files.new)
  file.copy(files.path, file.path(dir.it[i]), overwrite = TRUE)
}

rep.mcmc <- SS_output(om.dir, dir.mcmc = dir.mcmc)
mcmc <- rep.mcmc$mcmc

#Create IDs for pars by group to match up to mcmc labels
pars <- SS_readpar_3.30(parfile = file.path(om.dir, "ss.par"), 
                        datsource = file.path(om.dir, "data.ss"), 
                        ctlsource = file.path(om.dir, "control.ss"))

mg <- row.names(pars$MG_parms)[c(1)]
sr <- row.names(pars$SR_parms)[1:2]
#s <- row.names(pars$S_parms)
recdevs <- paste0("Main_RecrDev_", pars$recdev1[,1])
recdevs.fore <- paste0("ForeRecr_", pars$recdev_forecast[,1])
#q <- row.names(pars$Q_parms)
#f <- paste("F_fleet", 
#           pars$F_rate$fleet, 
#           "YR",
#           pars$F_rate$year, 
#           "s", 
#           pars$F_rate$seas, 
#           sep = "_")
f <- "init_F"
mcmc.cols <- colnames(rep.mcmc$mcmc)

## Subset mcmc.cols
mcmc.sr <- mcmc[which(str_detect(mcmc.cols, "SR"))]
mcmc.mg <- mcmc[which(str_detect(mcmc.cols, "NatM_"))] #|L_at_
#mcmc.s <- mcmc[which(str_detect(mcmc.cols, "Age|Size"))]
mcmc.f <- mcmc[which(str_detect(mcmc.cols, "InitF_"))]
#mcmc.q <- mcmc[which(str_detect(mcmc.cols, "LnQ"))]
mcmc.recdevs <- mcmc[which(str_detect(mcmc.cols, paste0(recdevs, collapse = "|")))]
mcmc.recfore <- mcmc[which(str_detect(mcmc.cols, paste0(recdevs.fore, collapse = "|")))]

#### All of this needs to be repeated for 100 directories (using i to indicate which iteration(row of mcmc) to use)
####################################################################################
#Read in pars file from iteration directory
for(i in 1:length(iterations)){

pars <- SS_readpar_3.30(parfile = file.path(dir.it[i], "ss.par"), 
                        datsource = file.path(dir.it[i], "data.ss"), 
                        ctlsource = file.path(dir.it[i], "control.ss"))


#check to make sure you have the right number of values to replace
if(length(pars$init_F) == ncol(mcmc.f)){
  
  pars$init_F <- mcmc.f[i,]
  
}else{
  message("Mismatched number of initialF values to replace original values")
}

if(nrow(pars$recdev1) == ncol(mcmc.recdevs)){
  
  pars$recdev1[,2] <- t(mcmc.recdevs[i,])
  
}else{
  message("Mismatched number of rec.dev values to replace original values")
}

#if(nrow(pars$recdev_forecast) == ncol(mcmc.recfore)){
  
  #pars$recdev_forecast[,2] <- mcmc.recfore[i,]
#}else{
 # message("Mismatch of forecast recdevs to replace original values")
#}

#if(ncol(mcmc.q) == 1){
  
#  pars$Q_parms[which(row.names(pars$Q_parms) == colnames(mcmc.q)), 2] <- mcmc.q[i,]
  
#}else{
#  message("More than 1 Q value is present")
#}

if(ncol(mcmc.sr) == 2){
  
  pars$SR_parms[which(row.names(pars$SR_parms) == colnames(mcmc.sr)[1]),2] <- mcmc.sr[i,1]
  pars$SR_parms[which(row.names(pars$SR_parms) == colnames(mcmc.sr)[2]),2] <- mcmc.sr[i,2]
  
}else{
  message("More than 2 SR variables present")
}

#S_parms <- pars$S_parms %>% 
#  filter(str_detect(row.names(.), "BLK", negate = TRUE)) %>% 
#  filter(str_detect(row.names(.), "VIDEO|SEAMAP|Age")) %>% 
#  filter(str_detect(row.names(.), "SMP_BYC|COMP", negate = TRUE)) %>% 
#  tibble::rownames_to_column(var = "ID")

#if(nrow(S_parms) == ncol(mcmc.s)){
  
#  S_parms$ESTIM <- t(mcmc.s[i,])
#  S_parms <- pars$S_parms %>% 
#    tibble::rownames_to_column(var = "ID") %>% 
#    mutate(order = seq(1, nrow(.))) %>% 
#    merge(S_parms, by = "ID", all.x = TRUE) %>% 
#    mutate(INIT.x = ifelse(is.na(INIT.y), INIT.x, INIT.y),
#           ESTIM.x = ifelse(is.na(ESTIM.y), ESTIM.x, ESTIM.y)) %>% 
#    arrange(order) %>% 
#    select(c(ID, INIT.x, ESTIM.x)) %>% 
#    tibble::column_to_rownames(var = "ID") %>% 
#    rename(INIT = INIT.x,
#      ESTIM = ESTIM.x)
  
#  pars$S_parms[,1:2] <- S_parms[,1:2]
#}else{
#  message("Mismatch number of selectivity variables to replace originial values")
#}

if(ncol(mcmc.mg) == 1){
  
  pars$MG_parms[which(str_detect(row.names(pars$MG_parms), "NatM")), 2] <- mcmc.mg[i,1]
  pars$MG_parms[which(str_detect(row.names(pars$MG_parms), colnames(mcmc.mg)[2])), 2] <- mcmc.mg[i,2]
  
}else{
  message("More than 2 MG values are present")
}

#if(ncol(mcmc.q) == 3){

#  pars$Q_parms[which(row.names(pars$Q_parms) == colnames(mcmc.q)[1]),2] <- mcmc.q[i,1]
#  pars$Q_parms[which(row.names(pars$Q_parms) == colnames(mcmc.q)[2]),2] <- mcmc.q[i,2]
#  pars$Q_parms[which(row.names(pars$Q_parms) == colnames(mcmc.q)[3]),2] <- mcmc.q[i,3]

#}else{
#  message("Mismatch in catchability parameters")
#}

#Save new pars to directory
SS_writepar_3.30(pars, outfile = file.path(dir.it[i], "ss.par"), overwrite = TRUE)

### rerun model to get starting states
starter.i <- SS_readstarter(file = file.path(dir.it[i], "starter.ss"))
starter.i$init_values_src <- 1                             
starter.i$last_estimation_phase <- 0
starter.i$seed <- set.seed[i,2]
starter.i$N_bootstraps <- 3
SS_writestarter(starter.i, dir = file.path(dir.it[i]), overwrite = TRUE)

shell(paste("cd/d", dir.it[i], "&& ss -nohess >NUL 2>&1", sep = " "))
}

### Compare starting states ######
dir.create(path = file.path(model.dir, "starting_states_comp_plots"))
big.reps <- SSgetoutput(dirvec = dir.it, verbose = FALSE) 
sum.reps <- SSsummarize(big.reps)
SSplotComparisons(sum.reps, print = TRUE, plotdir = file.path(model.dir, "starting_states_comp_plots"))

## visualize parameter distributions
mcmc_pars <- bind_cols(mcmc.sr, mcmc.mg, mcmc.f) |> 
pivot_longer(cols = everything(), names_to = "Parameter", values_to = "Value") 

original.rep <- SS_output(dir = file.path(model.dir, "EM"))
SS_plots(original.rep)
original.vals <- original.rep$parameters |> 
filter(Label %in% unique(mcmc_pars$Parameter)) |> 
select(c("Label", "Value")) |> 
rename(Parameter = Label)

mcmc_pars |> 
ggplot() + 
geom_density(aes(x  = Value)) +
facet_wrap(~Parameter, scales = "free") +
geom_vline(aes(xintercept = Value), original.vals, col = "red") +
theme_classic() +
theme(text = element_text(size = 16))

## Fit EM to OM bootstrapped data
em.files <- list("control.ss", "forecast.ss", "starter.ss", "ss.exe")

for(i in 1:length(iterations)){
  
  dat <- SS_readdat_3.30(file = file.path(dir.it[i], "data.ss_new"), section = 3)
  dir.EM <- file.path(dir.it[i], "EM")
  dir.create(dir.EM)
  files.path <- file.path(model.dir, "EM", em.files)
  file.copy(files.path, file.path(dir.it[i], "EM"), overwrite = TRUE)
  SS_writedat_3.30(dat, outfile = file.path(dir.it[i], "EM", "data.ss"), overwrite = T)
  run(dir = file.path(dir.it[i], "EM"), exe = "ss")
  repi <- SS_output(dir = file.path(dir.it[i], "EM"), verbose = F, printstats = F)
  SS_plots(repi)

}

om_em <- SSgetoutput(dirvec = c(file.path(dir.it[1]), file.path(dir.it[1], "EM")))
om_em_sum <- SSsummarize(om_em)
SSplotComparisons(om_em_sum, legendlabels = c("OM", "EM"))
rep <- SS_output(dir = dir.it[1])
 SS_plots(rep)

head(rep$cpue)
head(repi$cpue)
index_df <- cbind(rep$cpue$Fleet, rep$cpue$Yr, rep$cpue$Obs, rep$cpue$Exp, repi$cpue$Obs, repi$cpue$Exp)
colnames(index_df) <- c("Fleet","Yr", "Obs_OM", "Exp_OM", "Obs_EM", "Exp_EM")
head(index_df)

index_df |> 
as.data.frame() |> 
pivot_longer(cols = c("Obs_OM", "Obs_EM", "Exp_OM", "Exp_EM"), values_to = "Value", names_to = "Model")  |> 
separate(col = "Model",)
pivot_wider(cols = c("Model", "Value"), values_from = Value, names_from = Model)
