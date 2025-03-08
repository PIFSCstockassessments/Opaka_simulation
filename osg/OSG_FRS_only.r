## Script to create OM/EM pairs for given sampling strategies, F, and recdevs. 
library(r4ss) #needs to be v1.46.1 to work with ss3sim
library(magrittr)
library(dplyr)
library(ss3sim) #be sure to use ss3.exe v3.30.19
library(ss3diags)
print(getwd())

#get args from Bash environment (for OSG)
args <- commandArgs(trailingOnly = TRUE)
#should be 
print(args) 

set.seed <- read.csv("setseed.csv")
sas_full <- read.csv("sas.csv")
load("constantF_mat.RData")
# load("increaseF_mat.RData")
load("recdevs_mat.RData")

#Variables
nyears <- 100
nyears_fwd <- 25
scen <- "FRS_only"

#Template OM and EM files
om_dir <- paste0("opaka-om-", nyears_fwd, "-FRS-only", "/")
em_dir <- paste0("opaka-em-", nyears_fwd, "-FRS-only", "/")

#Get iteration number
I <- as.numeric(tail(strsplit(args[1], "/")[[1]], n = 1))

sas <- sas_full %>% filter(Scen_name == scen)

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
        agecomp_params = agecomp,
        om_dir = file.path(main.dir, "models", om_dir),
        em_dir = file.path(main.dir, "models", em_dir),
        user_recdevs = full_recdevs,
        bias_adjust = T,
        seed = seed
    )

 if(str_detect(scen, "hyperstable")){

        scen_prefix <- strsplit(scen, "hyperstable")[[1]][1]
        regular_em <- SS_readdat_3.30(file.path("normal_rec.dat"))
        em_path <- file.path(getwd(), paste(scen, nyears_fwd, "yrfwd", sep = "_"), I, "em")
        #replace catch and CPUE for commercial fishery
        em_dat <- SS_readdat_3.30(file = file.path(em_path, "ss3.dat"))
        em_dat$catch <- regular_em$catch
        em_dat$CPUE$obs[1:nyears] <- regular_em$CPUE$obs[1:nyears]
        SS_writedat(em_dat, file.path(em_path, "ss3.dat"), overwrite = T)
        r4ss::run(dir = em_path, exe = "ss3", skipfinished = F, verbose = F)
        
        #clean for next run
        rm(list = c("om_dat", "em_dat"))

    }
    

