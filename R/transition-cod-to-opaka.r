library(r4ss)
library(tidyverse)
library(ss3sim)

main.dir <- this.path::here(.. = 1)
## Checking the original EM estimates the original OM 
om_dir <- file.path(main.dir, "models", "cod-om")
em_dir <- file.path(main.dir, "models", "cod-em")

# Creating input data structures ----
F0 <- list(
  years = 1:100,
  fleets = 1,
  fvals = c(rep(0, 25), rep(0.114, 75))
)

index1 <- list(
  fleets = c(2), years = list(seq(62, 100, by = 2)),
  sds_obs = list(0.1)
)

lcomp1 <- list(
  fleets = c(1, 2), Nsamp = list(50, 100),
  years = list(26:100, seq(62, 100, by = 2))
)

agecomp1 <- list(
  fleets = c(1, 2), Nsamp = list(50, 100),
  years = list(26:100, seq(62, 100, by = 2))
)

# Testing ss3sim_base function ----
##Function copies the OM and EM input files to new directory. Then is inserts dummy data for the amount of years you specifiy for each year and save that as ss3.dat. Then runs model to generate data_expval.ss (with init_scrc = 0) and recdevs and F values specified in ctl file. Then that data file is sent to EM directory. Each data point in EM/ss3.dat file is sampled from the OM/data_expval.ss with specified error. For example: if catch in year 50 = 4950 in OM (ie true catch), then in EM/ss3.dat, catch in year 50 = rnorm(mu = 4950, sd = .1). In control file, there are no recdevs or F specified (they are estimated).

ss3sim_base(
  iterations = 1,
  scenarios = "D1-E0-F0-cod", # name as desired
  f_params = F0,
  index_params = index1,
  lcomp_params = lcomp1,
  agecomp_params = agecomp1,
  om_dir = om_dir,
  em_dir = em_dir
)

ex_mods <- SSgetoutput(dirvec = c(
    file.path(main.dir, "D1-E0-F0-cod", "1", "om"),
    file.path(main.dir, "D1-E0-F0-cod", "1", "em")
))
ex_mods_sum <- SSsummarize(ex_mods)
SSplotComparisons(ex_mods_sum) ## EM matches very closely to OM and fits OM data tightly

# Changing cod-om and cod-em to opaka specs ----
# All changes were made manually by first matching the models/codOM.ctl or models/codOM.dat file to the No_size_comp/OM/control.ss_new or /data.ss files. Then models/codEM.ctl file was changed to match models/codOM.ctl. Changes were implemented step-wise and new directories were created at each step and OM/EM comparison plots were created to ensure they were still matching well.
# Life history ----
ss3sim_base(
  iterations = 1,
  scenarios = "01-LH-trans-cod", 
  f_params = F0,
  index_params = index1,
  lcomp_params = lcomp1,
  agecomp_params = agecomp1,
  om_dir = om_dir,
  em_dir = em_dir
)
lh_mods <- SSgetoutput(dirvec = c(
    file.path(main.dir, "01-LH-trans-cod", "1", "om"),
    file.path(main.dir, "01-LH-trans-cod", "1", "em")
))
lh_mods_sum <- SSsummarize(lh_mods)
SSplotComparisons(lh_mods_sum, print = TRUE, legendlabels = c("OM", "EM"), plotdir = file.path(main.dir, "01-LH-trans-cod", "1"))

# Stock Recruitment ----
ss3sim_base(
  iterations = 1,
  scenarios = "02-SR-trans-cod", 
  f_params = F0,
  index_params = index1,
  lcomp_params = lcomp1,
  agecomp_params = agecomp1,
  om_dir = om_dir,
  em_dir = em_dir
)
sr_mods <- SSgetoutput(dirvec = c(
    file.path(main.dir, "02-SR-trans-cod", "1", "om"),
    file.path(main.dir, "02-SR-trans-cod", "1", "em")
))
sr_mods_sum <- SSsummarize(sr_mods)
SSplotComparisons(sr_mods_sum, print = TRUE, legendlabels = c("OM", "EM"), plotdir = file.path(main.dir, "02-SR-trans-cod", "1"))

# Length Selectivity ----
#change the length selectivity of fishery and survey (use bfish camera first bc its asymptotic) and length bins of length comp data
ss3sim_base(
  iterations = 1,
  scenarios = "03-Selex-trans-cod", 
  f_params = F0,
  index_params = index1,
  lcomp_params = lcomp1,
  agecomp_params = agecomp1,
  om_dir = om_dir,
  em_dir = em_dir
)
sx_mods <- SSgetoutput(dirvec = c(
    file.path(main.dir, "03-Selex-trans-cod", "1", "om"),
    file.path(main.dir, "03-Selex-trans-cod", "1", "em")
))
sx_mods_sum <- SSsummarize(sx_mods)
SSplotComparisons(sx_mods_sum, print = TRUE, legendlabels = c("OM", "EM"), plotdir = file.path(main.dir, "03-Selex-trans-cod", "1"))

# Add growth platoons ----
#change growth platoon
ss3sim_base(
  iterations = 1,
  scenarios = "04-GP-trans-cod", 
  f_params = F0,
  index_params = index1,
  lcomp_params = lcomp1,
  agecomp_params = agecomp1,
  om_dir = om_dir,
  em_dir = em_dir
)
gp_mods <- SSgetoutput(dirvec = c(
    file.path(main.dir, "04-GP-trans-cod", "1", "om"),
    file.path(main.dir, "04-GP-trans-cod", "1", "em")
))
gp_mods_sum <- SSsummarize(gp_mods)
SSplotComparisons(gp_mods_sum, print = TRUE, legendlabels = c("OM", "EM"), plotdir = file.path(main.dir, "04-GP-trans-cod", "1"))

# Change age selectivity ----
#change age selectivity and age bins 
ss3sim_base(
  iterations = 1,
  scenarios = "05-age-trans-cod", 
  f_params = F0,
  index_params = index1,
  lcomp_params = lcomp1,
  agecomp_params = agecomp1,
  om_dir = om_dir,
  em_dir = em_dir
)
age_mods <- SSgetoutput(dirvec = c(
    file.path(main.dir, "05-age-trans-cod", "1", "om"),
    file.path(main.dir, "05-age-trans-cod", "1", "em")
))
age_mods_sum <- SSsummarize(age_mods)
SSplotComparisons(age_mods_sum, print = TRUE, legendlabels = c("OM", "EM"), plotdir = file.path(main.dir, "05-age-trans-cod", "1"))

# Non-commercial fleet ----
F2 <- list(
  years = list(1:75, 1:75),
  fleets = c(1, 3), 
  fvals = list(rep(0.114, 75), rep(0.114, 75))
)

index2 <- list(
  fleets = 1:2, years = list(seq(1, 75, by = 1), seq(69, 75, by = 1)),
  seas = list(7,1), sds_obs = list(0.1, 0.1)
)

lcomp2 <- list(
  fleets = c(2), Nsamp = list(45),
  years = list(seq(69, 75, by = 1))
)

agecomp2 <- list(
  fleets = c(2), Nsamp = list(45),
  years = list(seq(69, 75, by = 1))
)

ss3sim_base(
  iterations = 1,
  scenarios = "06-non-comm-trans-cod", 
  f_params = F2,
  index_params = index2,
  lcomp_params = lcomp2,
  agecomp_params = agecomp2,
  om_dir = om_dir,
  em_dir = em_dir
)

nc_mods <- SSgetoutput(dirvec = c(
    file.path(main.dir, "06-non-comm-trans-cod", "1", "om"),
    file.path(main.dir, "06-non-comm-trans-cod", "1", "em")
))
nc_mods_sum <- SSsummarize(nc_mods)
SSplotComparisons(nc_mods_sum, print = TRUE, legendlabels = c("OM", "EM"), plotdir = file.path(main.dir, "06-non-comm-trans-cod", "1"))

# Add BFISH research fishing fleet ----
F2 <- list(
  years = list(1:75, 1:75),
  fleets = c(1, 3), 
  fvals = list(rep(0.114, 75), rep(0.114, 75))
)

index2 <- list(
  fleets = 1:2, years = list(seq(1, 75, by = 1), seq(69, 75, by = 1)),
  seas = list(7,1), sds_obs = list(0.1, 0.1)
)

lcomp2 <- list(
  fleets = c(2), Nsamp = list(45),
  years = list(seq(69, 75, by = 1))
)

agecomp2 <- list(
  fleets = c(2), Nsamp = list(45),
  years = list(seq(69, 75, by = 1))
)

ss3sim_base(
  iterations = 1,
  scenarios = "07-resfish-trans-cod", 
  f_params = F2,
  index_params = index2,
  lcomp_params = lcomp2,
  agecomp_params = agecomp2,
  om_dir = om_dir,
  em_dir = em_dir
)
bf_mods <- SSgetoutput(dirvec = c(
    file.path(main.dir, "07-resfish-trans-cod", "1", "om"),
    file.path(main.dir, "07-resfish-trans-cod", "1", "em")
))
bf_mods_sum <- SSsummarize(bf_mods)
SSplotComparisons(bf_mods_sum, print = TRUE, legendlabels = c("OM", "EM"), plotdir = file.path(main.dir, "07-resfish-trans-cod", "1"))

# Custom F timeseries ----
## Put in F timeseries from modified Opaka SS model (removed size comps) and see how it behaves
rep <- SS_output(dir = file.path(main.dir, "models", "02-no-size-comp"))
head(rep$timeseries)
head(rep$exploitation)

F_frs <- rep$exploitation$FRS
F_noncomm <- rep$exploitation$Non_comm

F3 <- list(
  years = list(1:75, 1:75),
  fleets = c(1, 3), 
  fvals = list(F_frs, F_noncomm)
)

index3 <- list(
  fleets = c(1,2,4), years = list(seq(1, 75, by = 1), seq(69, 75, by = 1), seq(69, 75, by =1)),
  seas = list(7,1,1), sds_obs = list(0.25, 0.25, 0.25)
)

lcomp3 <- list(
  fleets = c(2,4), Nsamp = list(45,45),
  years = list(seq(69, 75, by = 1), seq(69, 75, by = 1))
)

agecomp3 <- list(
  fleets = c(2,4), Nsamp = list(45,45),
  years = list(seq(69, 75, by = 1), seq(69, 75, by = 1))
)

ss3sim_base(
  iterations = 1,
  scenarios = "08b-trueF-trans-cod", 
  f_params = F3,
  index_params = index3,
  lcomp_params = lcomp3,
  agecomp_params = agecomp3,
  om_dir = om_dir,
  em_dir = em_dir
)

## Modify original models to match the years in ss3sim model (instead of 1949-2023, 1-75)
# data_orig <- SS_readdat_3.30(file = file.path(main.dir, "models", "01_original_model", "data.ss"))
# data_orig$styr <- 1
# data_orig$endyr <- 75
# data_orig$catch <- data_orig$catch  |> mutate(year = ifelse(year == -999, year, year - 1948))
# data_orig$CPUE$year <- data_orig$CPUE$year - 1948
# data_orig$lencomp$Yr <- data_orig$lencomp$Yr - 1948
# data_orig$agecomp$Yr <- data_orig$agecomp$Yr - 1948
# data_orig$sizefreq_data_list[[1]]$Yr <- data_orig$sizefreq_data_list[[1]]$Yr - 1948
# SS_writedat_3.30(data_orig, outfile = file.path(main.dir, "models", "01_original_model", "data_updated.ss"))
# ctl <- SS_readctl_3.30(file = file.path(main.dir, "models", "01_original_model", "control.ss"), use_datlist = T, datlist = file.path(main.dir, "models", "01_original_model", "data_updated.ss"))
# ctl$last_early_yr_nobias_adj <- 1
# ctl$first_yr_fullbias_adj <- 26
# ctl$last_yr_fullbias_adj <- 74
# ctl$first_recent_yr_nobias_adj <- 75
# ctl$MainRdevYrFirst <- 1
# ctl$MainRdevYrLast <- 75
# SS_writectl_3.30(ctl, outfile = file.path(main.dir, "models", "01_original_model", "control_updated.ss"), overwrite = TRUE)
# start <- SS_readstarter(file = file.path(main.dir, "models", "01_original_model", "starter.ss"))
# start$datfile <- "data_updated.ss"
# start$ctlfile <- "control_updated.ss"
# SS_writestarter(start, file.path(main.dir, "models", "01_original_model"), overwrite = TRUE)

trueF_mods <- SSgetoutput(dirvec = c(
    file.path(main.dir, "08b-trueF-trans-cod", "1", "om"),
    file.path(main.dir, "08b-trueF-trans-cod", "1", "em"),
    file.path(main.dir, "models", "02-no-size-comp"),
    file.path(main.dir, "models", "01_original_model")
))
trueF_mods_sum <- SSsummarize(trueF_mods)
SSplotComparisons(trueF_mods_sum, print = TRUE, legendlabels = c("OM", "EM", "No Size Comp", "Original"), plotdir = file.path(main.dir, "08b-trueF-trans-cod", "1"))
# Tuned the rec dev adjustment bias and changed age selectivity pattern to 10 ----
ss3sim_base(
  iterations = 1:5,
  scenarios = "09-recdevs_ageselex-trans-cod", 
  f_params = F3,
  index_params = index3,
  lcomp_params = lcomp3,
  agecomp_params = agecomp3,
  om_dir = om_dir,
  em_dir = em_dir
)

recdevs_mods <- SSgetoutput(dirvec = c(
    #file.path(main.dir, "09-recdevs_ageselex-trans-cod", "2", "om"),
    #file.path(main.dir, "09-recdevs_ageselex-trans-cod", "2", "em"),
    #file.path(main.dir, "09-recdevs_ageselex-trans-cod", "3", "em"),
    #file.path(main.dir, "09-recdevs_ageselex-trans-cod", "4", "em"),
    #file.path(main.dir, "09-recdevs_ageselex-trans-cod", "5", "em"),
    file.path(main.dir, "models", "03-modified-og"),
    file.path(main.dir, "models", "02-no-size-comp")
))
recdevs_mods_sum <- SSsummarize(recdevs_mods)
SSplotComparisons(recdevs_mods_sum)#, legendlabels = c("Rep_2", "Rep_5", "2 no age", "Original_nosize")) #, print = TRUE, plotdir = file.path(main.dir, "08-trueF-trans-cod", "1"))

# FIMS em model ----
### Side work: Creating an EM for FIMS case study #################################
#only using 2 fleets, FRS and BFISH camera for now, need catch, cpue, and age comps
F_frs <- rep$exploitation$FRS

F_fims <- list(
  years = list(1:75),
  fleets = c(1), 
  fvals = list(F_frs)
)

index_fims <- list(
  fleets = c(1,2), years = list(seq(1, 75, by = 1), seq(69, 75, by = 1)),
  seas = list(7,1), sds_obs = list(0.1, 0.25)
)

lcomp_fims <- list(
  fleets = c(2), Nsamp = list(45),
  years = list(seq(69, 75, by = 1))
)

agecomp_fims <- list(
  fleets = c(1,2), Nsamp = list(100,45),
  years = list(seq(26, 75, by = 1), seq(69, 75, by = 1))
)

ss3sim_base(
  iterations = 1,
  scenarios = "FIMS-em", 
  f_params = F_fims,
  index_params = index_fims,
  lcomp_params = lcomp_fims,
  agecomp_params = agecomp_fims,
  om_dir = om_dir,
  em_dir = em_dir
)
## copy FIMS-em folder over to Opaka-FIMS-Case-Study directory and save Rdata objects for the FIMS case study scripts ###############################################

bootdat_mods <- SSgetoutput(dirvec = c(
    file.path(main.dir, "models", "02-no-size-comp"),
    file.path(main.dir, "models", "boot-02"),
    file.path(main.dir, "09-recdevs_ageselex-trans-cod", "2", "em")
))
bootdat_sum <- SSsummarize(bootdat_mods)
SSplotComparisons(bootdat_sum)
