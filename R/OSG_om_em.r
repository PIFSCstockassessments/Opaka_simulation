## Script to create OM/EM pairs for given sampling strategies, F, and recdevs. This script is for running code on OSG. 
library(r4ss)
library(magrittr)
library(dplyr)
library(ss3sim)

#Set up
main.dir <- getwd()
source(file.path(main.dir, "R", "get_fits.r"))
set.seed <- read.csv(file.path(main.dir, "Inputs", "setseed.csv"))
sas_full <- read.csv(file.path(main.dir, "Inputs", "sas.csv"))
load(file.path(main.dir, "Inputs", "constantF_mat.RData"))
load(file.path(main.dir, "Inputs", "recdevs_mat.RData"))
niter <- 10
#u_vec vector of ratios for calculating non-commercial catch
#u_vec <- c(rep(1.8, 26), rep(1.47, 29), rep(1.03, 70))

om_dir <- file.path(main.dir, "models", "opaka-om")
em_dir <- file.path(main.dir, "models", "opaka-em")
nyears <- 80
nyears_fwd <- 5
scen <- "SQ"
sas <- sas_full %>% filter(Scen_name == scen)
files.keep <- c("ss.par", "starter.ss", "forecast.ss", "em.ctl", "control.ss_new", "ss3.dat")

for(I in 1:niter){
    # Get F-vector 
    
    F_list <- list(
        years = list(1:nyears, 1:nyears),
        fleets = c(1, 2), 
        fvals = list(F_comm_df[1:nyears,I], F_noncomm_df[1:nyears,I])
        )
    #create sampling scheme for indices of abundance
    index <- list(
        fleets = c(1,3), years = list(seq(1, nyears, by = 1), seq(69, nyears, by = 1)),
        seas = list(7,1), 
        sds_obs = list(0.13, sas[which(sas$N_years == nyears_fwd), "Resfish_index_CV"])
        )
    lcomp <- list(
        fleets = c(3), Nsamp = list(sas[which(sas$N_years == nyears_fwd), "Neff_len_Resfish"]),
        years = list(seq(69, nyears, by = 1))
        )

    agecomp <- list(
        fleets = c(3), Nsamp = list(sas[which(sas$N_years == nyears_fwd), "Neff_age_Resfish"]),
        years = list(seq(69, nyears, by = 1))
        )

    ss3sim_base(
        iterations = I,
        scenarios = paste(scen, nyears_fwd, "yrfwd", sep = "_"), 
        f_params = F_list,
        index_params = index,
        lcomp_params = lcomp,
        agecomp_params = agecomp,
        om_dir = om_dir,
        em_dir = em_dir,
        user_recdevs = full_recdevs[1:nyears,],
        bias_adjust = T
        )

    om_path <- file.path(main.dir, paste(scen, nyears_fwd, "yrfwd", sep = "_"), I, "om")
    em_path <- file.path(main.dir, paste(scen, nyears_fwd, "yrfwd", sep = "_"), I, "em")

    #rerun om and em to get ssb on right scale (issue with ss3sim function, doesn't allow Ngender = -1)
    om_dat <- SS_readdat_3.30(file = file.path(om_path, "ss3.dat"))
    om_dat$Nsexes <- -1
    SS_writedat(om_dat, file.path(om_path, "ss3.dat"), overwrite = T)
    r4ss::run(dir = om_path, exe = "ss", extras = "-nohess", skipfinished = F)

    em_dat <- SS_readdat_3.30(file = file.path(em_path, "ss3.dat"))
    em_dat$Nsexes <- -1
    SS_writedat(em_dat, file.path(em_path, "ss3.dat"), overwrite = T)
    r4ss::run(dir = em_path, exe = "ss", skipfinished = F)
    #clean for next run
    rm(list = c("om_dat", "em_dat"))
    
    #get results out of models for comparisons
    get_results_all(
        overwrite_files = T,
        user_scenarios = paste(scen, nyears_fwd, "yrfwd", sep = "_")
    )   
    get_fits_all(
        overwrite_files = T,
        user_scenarios = paste(scen, nyears_fwd, "yrfwd", sep = "_")
    )

    #remove xtra files
    unlink(setdiff(list.files(em_path, full.names = T), file.path(em_path,files.keep)), recursive=TRUE)
    unlink(setdiff(list.files(om_path, full.names = T), file.path(om_path,files.keep)), recursive=TRUE)

}

