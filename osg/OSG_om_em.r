## Script to create OM/EM pairs for given sampling strategies, F, and recdevs. 
library(r4ss) #needs to be v1.46.1 to work with ss3sim
library(magrittr)
library(dplyr)
library(ss3sim) #be sure to use ss3.exe v3.30.19

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
nyears <- 90
nyears_fwd <- 15
scen <- "SQ"

#Template OM and EM files
om_dir <- paste0("opaka-om-", nyears_fwd, "/")
em_dir <- paste0("opaka-em-", nyears_fwd, "/")

#Get iteration number
I <- tail(strsplit(args[1], "/")[[1]], n = 1)

sas <- sas_full %>% filter(Scen_name == scen)

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
    sds_obs = list(0.2, 0.10),
    sds_out = list(0.13, sas[which(sas$N_years == nyears_fwd), "Resfish_index_CV"]) 
)

lcomp <- list(
    fleets = c(3), Nsamp = list(c(rep(30, 7), rep(sas[which(sas$N_years == nyears_fwd), "Neff_len_Resfish"], nyears_fwd))),
    years = list(seq(69, nyears, by = 1))
)

seed <- set.seed[I,2]

ss3sim_base(
    iterations = I,
    scenarios = paste(scen, nyears_fwd, "yrfwd", sep = "_"), 
    f_params = F_list,
    index_params = index,
    lcomp_params = lcomp,
    om_dir = om_dir,
    em_dir = em_dir,
    user_recdevs = full_recdevs,
    bias_adjust = T,
    seed = seed
)

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
    

# extract model output
ss_report <- try(SS_output(dir=getwd()),silent=TRUE) 

# save output
save(ss_report,file="ss_report.RData")