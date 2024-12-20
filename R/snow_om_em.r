## Script to create OM/EM pairs for given sampling strategies, F, and recdevs. 
library(r4ss) #needs to be v1.46.1 to work with ss3sim
library(magrittr)
library(dplyr)
#pak::pkg_install("MOshima-PIFSC/ss3sim") for version that includes hyperstability function
library(ss3sim) #be sure to use ss3.exe v3.30.19
library(snowfall)
library(stringr)

#Set up
main.dir <- getwd() 
#source(file.path(main.dir, "R", "get_fits.r"))
set.seed <- read.csv(file.path(main.dir, "Inputs", "setseed.csv"))
sas_full <- read.csv(file.path(main.dir, "Inputs", "sas.csv"))
load(file.path(main.dir, "Inputs", "constantF_mat.RData"))
load(file.path(main.dir, "Inputs", "increaseF_mat.RData"))
load(file.path(main.dir, "Inputs", "recdevs_mat.RData"))
load(file.path(main.dir, "Inputs", "poor_recdevs_mat.RData"))
source(file.path(main.dir, "R", "get_fits.r"))
niter <- 1
#u_vec vector of ratios for calculating non-commercial catch
#u_vec <- c(rep(1.8, 26), rep(1.47, 29), rep(1.03, 70))
nyears <- 100
nyears_fwd <- 25
om_dir <- file.path(main.dir, "models", paste0("opaka-om-", nyears_fwd, "-FRS-only")) #-5 -15 -25 -50
em_dir <- file.path(main.dir, "models", paste0("opaka-em-", nyears_fwd, "-FRS-only")) #-5 -15 -25 -50

scen <- "FRS_only"
sas <- sas_full %>% filter(Scen_name == scen)
#files.keep <- c("ss.par", "starter.ss", "forecast.ss", "em.ctl", "control.ss_new", "ss3.dat")

#for(I in 1:niter){
wrapper_fn <- function(I, main.dir = main.dir, nyears = nyears, nyears_fwd = nyears_fwd, scen = scen, sas = sas, F_comm_df = F_comm_df, F_noncomm_df = F_noncomm_df, full_recdevs = full_recdevs, om_dir = om_dir, em_dir = em_dir){   
    
    # Get F-vector 
    F_list <- list(
        years = list(1:nyears, 1:nyears),
        fleets = c(1, 2), 
        fvals = list(F_comm_df[1:nyears,I], F_noncomm_df[1:nyears,I])
    )
    #create sampling scheme for indices of abundance
    index <- list(
        fleets = c(1), 
        years = list(seq(1, nyears, by = 1)),
        seas = list(7), 
        sds_obs = list(0.2),
        sds_out = list(0.13) 
    )

    # lcomp <- list(
    #     fleets = c(3), Nsamp = list(c(rep(30, 7), rep(sas[which(sas$N_years == nyears_fwd), "Neff_len_Resfish"], nyears_fwd))),
    #     years = list(seq(69, nyears, by = 1))
    # )

    agecomp <- list(
        fleets = c(1), Nsamp = list(10),
        years = list(seq(69, 70, by = 1))
    )
    
    seed <- set.seed[I,2]

    ss3sim_base(
        iterations = I,
        scenarios = paste(scen, nyears_fwd, "yrfwd", sep = "_"), 
        f_params = F_list,
        index_params = index,
        #lcomp_params = lcomp,
        agecomp_params = agecomp,
        om_dir = om_dir,
        em_dir = em_dir,
        user_recdevs = full_recdevs,
        bias_adjust = F,
        seed = seed
    )

    # om_path <- file.path(main.dir, paste(scen, nyears_fwd, "yrfwd", sep = "_"), I, "om")
    # #rerun om and em to get ssb on right scale (issue with ss3sim function, doesn't allow Ngender = -1)
    # # om_dat <- SS_readdat_3.30(file = file.path(om_path, "ss3.dat"))
    # # om_dat$Nsexes <- -1
    # # SS_writedat(om_dat, file.path(om_path, "ss3.dat"), overwrite = T)
    # # r4ss::run(dir = om_path, exe = "ss3", extras = "-nohess", skipfinished = F)
    # # em_dat$Nsexes <- -1
    #for hyperstable scenario, copy data from normal rec models and use it for EM
    if(str_detect(scen, "hyperstable")){

        scen_prefix <- strsplit(scen, "hyperstable")[[1]][1]
        regular_em <- SS_readdat_3.30(file.path(main.dir, paste(scen_prefix, nyears_fwd, "yrfwd", sep = "_"), I, "em", "ss3.dat"))
        em_path <- file.path(main.dir, paste(scen, nyears_fwd, "yrfwd", sep = "_"), I, "em")
        #replace catch and CPUE for commercial fishery
        em_dat <- SS_readdat_3.30(file = file.path(em_path, "ss3.dat"))
        em_dat$catch <- regular_em$catch
        em_dat$CPUE$obs[1:nyears] <- regular_em$CPUE$obs[1:nyears]
        SS_writedat(em_dat, file.path(em_path, "ss3.dat"), overwrite = T)
        r4ss::run(dir = em_path, exe = "ss3", skipfinished = F, verbose = F)
        
        #clean for next run
        rm(list = c("om_dat", "em_dat"))

    }

}

A = proc.time()
sfInit(parallel = TRUE, cpus = 10)
sfLibrary(ss3sim)
sfLibrary(r4ss)
sfLibrary(stringr)
sfExport("set.seed")
sfLapply(1:niter, wrapper_fn, main.dir = main.dir, nyears = nyears, nyears_fwd = nyears_fwd, 
    scen = scen, sas = sas, F_comm_df = F_comm_df, F_noncomm_df = F_noncomm_df, full_recdevs = full_poor_recdevs, om_dir = om_dir, em_dir = em_dir)
sfStop()
B = proc.time()
(B-A)/60


HRF_dir <- file.path(main.dir, "HRF_25_yrfwd")
SQ_dir <- file.path(main.dir, "SQ_25_yrfwd")

for(i in 1:niter){

    HRF_dir_i <- file.path(HRF_dir, i)
    SQ_dir_i <- file.path(SQ_dir, i)
    



}


#get results out of models for comparisons
all_scenario_names <- paste(sas_full$Scen_name, sas_full$N_years, "yrfwd", sep = "_")
get_results_all(
    overwrite_files = T,
    user_scenarios =  c("SQ_25_yrfwd", "HRF_25_yrfwd"),
    filename_prefix = "all_scens"
)   
get_fits_all(
    overwrite_files = T,
    user_scenarios = c("SQ_25_yrfwd", "HRF_25_yrfwd"), #all_scenario_names,
    filename_prefix = "all_scens"
)


# #remove xtra files
# files.keep <- c("ss.par", "starter.ss", "forecast.ss", "em.ctl", "control.ss_new", "ss3.dat")
# unlink(setdiff(list.files(em_path, full.names = T), file.path(em_path,files.keep)), recursive=TRUE)
# unlink(setdiff(list.files(om_path, full.names = T), file.path(om_path,files.keep)), recursive=TRUE)

devtools::install_github('Cole-Monnahan-NOAA/adnuts')
library(adnuts)
m<-"ss3"
p<-file.path(main.dir, "SQ_0_yrfwd", "2", "em", "adnuts_model")
#make sure to run model with -hbf before fitting
fit_model <- adnuts::sample_nuts(model=m, path=p,  iter=1000, warmup=400, 
          chains=3, cores=4,control=list(metric='mle', max_treedepth=5),mceval=TRUE)

summary(fit_model)
str(fit_model$monitor)
summary(fit_model$monitor$n_eff)

post <- extract_samples(fit_model)
str(post)
install.packages("shinystan")
library(shinystan)
launch_shinyadmb(fit_model)

plot_marginals(fit_model)
pairs_admb(fit_model)
