## Script to create OM/EM pairs for given sampling strategies, F, and recdevs. 
library(r4ss) #needs to be v1.46.1 to work with ss3sim
library(magrittr)
library(dplyr)
#pak::pkg_install("MOshima-PIFSC/ss3sim") for version that includes hyperstability function
library(ss3sim)
library(snowfall)

#Set up
main.dir <- getwd() 
#source(file.path(main.dir, "R", "get_fits.r"))
set.seed <- read.csv(file.path(main.dir, "Inputs", "setseed.csv"))
sas_full <- read.csv(file.path(main.dir, "Inputs", "sas.csv"))
load(file.path(main.dir, "Inputs", "constantF_mat.RData"))
load(file.path(main.dir, "Inputs", "recdevs_mat.RData"))
load(file.path(main.dir, "Inputs", "poor_recdevs_mat.RData"))
niter <- 10
#u_vec vector of ratios for calculating non-commercial catch
#u_vec <- c(rep(1.8, 26), rep(1.47, 29), rep(1.03, 70))
nyears <- 90
nyears_fwd <- 15
om_dir <- file.path(main.dir, "models", paste0("opaka-om-", nyears_fwd)) #-5 -15 -25 -50
em_dir <- file.path(main.dir, "models", paste0("opaka-em-", nyears_fwd)) #-5 -15 -25 -50

scen <- "SQhyperstable"
sas <- sas_full %>% filter(Scen_name == scen)
#files.keep <- c("ss.par", "starter.ss", "forecast.ss", "em.ctl", "control.ss_new", "ss3.dat")

#for(I in 1:niter){
wrapper_fn <- function(I, main.dir = main.dir, nyears = nyears, nyears_fwd = nyears_fwd, scen = scen, sas = sas, full_recdevs = full_recdevs, om_dir = om_dir, em_dir = em_dir){   
    
    # Get F-vector 
    F_list <- list(
        years = list(1:nyears, 1:nyears),
        fleets = c(1, 2), 
        fvals = list(F_comm_df[1:nyears,I], F_noncomm_df[1:nyears,I])
    )
    #create sampling scheme for indices of abundance
    index <- list(
        fleets = c(1, 3), 
        years = list(seq(1, nyears, by = 1), seq(69, nyears, by = 1)),
        seas = list(7,1), 
        sds_obs = list(0.13, sas[which(sas$N_years == nyears_fwd), "Resfish_index_CV"]) 
    )

    # index <- list(
    #     fleets = c(1, 1, 3), 
    #     years = list(seq(1, 75, by = 1), seq(76, nyears, by = 1), seq(69, nyears, by = 1)),
    #     seas = list(7,7,1), 
    #     sds_obs = list(0.13, 0.13, sas[which(sas$N_years == nyears_fwd), "Resfish_index_CV"]), 
    #     beta = list(1, 0.1, 1),
    #     bias = "hyperstable"
    # )

    lcomp <- list(
        fleets = c(3), Nsamp = list(sas[which(sas$N_years == nyears_fwd), "Neff_len_Resfish"]),
        years = list(seq(69, nyears, by = 1))
    )

    agecomp <- list(
        fleets = c(3), Nsamp = list(sas[which(sas$N_years == nyears_fwd), "Neff_age_Resfish"]),
        years = list(seq(69, nyears, by = 1))
    )
    
    seed <- set.seed[I,2]

    ss3sim_base(
        iterations = I,
        scenarios = paste(scen, nyears_fwd, "yrfwd", sep = "_"), 
        f_params = F_list,
        index_params = index,
        lcomp_params = lcomp,
        #agecomp_params = agecomp,
        om_dir = om_dir,
        em_dir = em_dir,
        user_recdevs = full_poor_recdevs,
        bias_adjust = T,
        seed = seed
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

}

A = proc.time()
sfInit(parallel = TRUE, cpus = 10)
sfLibrary(ss3sim)
sfLibrary(r4ss)
sfExport("F_comm_df")
sfExport("F_noncomm_df")
sfExport("set.seed")
sfLapply(1:niter, wrapper_fn, main.dir = main.dir, nyears = nyears, nyears_fwd = nyears_fwd, scen = scen, sas = sas, full_recdevs = full_poor_recdevs, om_dir = om_dir, em_dir = em_dir)
sfStop()
B = proc.time()
(B-A)/60

#get results out of models for comparisons
all_scenario_names <- paste(sas_full$Scen_name, sas_full$N_years, "yrfwd", sep = "_")
get_results_all(
    overwrite_files = T,
    user_scenarios = all_scenario_names,
    filename_prefix = "all_scens"
)   
get_fits_all(
    overwrite_files = F,
    user_scenarios = all_scenario_names,
    filename_prefix = "all_scens"
)


# #remove xtra files
# files.keep <- c("ss.par", "starter.ss", "forecast.ss", "em.ctl", "control.ss_new", "ss3.dat")
# unlink(setdiff(list.files(em_path, full.names = T), file.path(em_path,files.keep)), recursive=TRUE)
# unlink(setdiff(list.files(om_path, full.names = T), file.path(om_path,files.keep)), recursive=TRUE)