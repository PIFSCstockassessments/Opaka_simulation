## Script to create OM/EM pairs for given sampling strategies, F, and recdevs. 
library(r4ss) #needs to be v1.46.1 to work with ss3sim
library(magrittr)
library(dplyr)
library(ss3sim) #be sure to use ss3.exe v3.30.19
library(stringr)
print(getwd())
#main.dir <- this.path::here(.. = 1) 

#get args from Bash environment (for OSG)
args <- commandArgs(trailingOnly = TRUE)
#should be 
print(args) 

set.seed <- read.csv("Inputs/setseed.csv")
sas_full <- read.csv("Inputs/sas.csv")
F_df <- read.csv("Inputs/simple_F.csv")
recdev_df <- read.csv("recdev_df.csv")
effN <- read.csv("Inputs/effN.csv")
load("Inputs/constantF_mat.RData")
#load("increaseF_mat.RData")
load("Inputs/recdevs_mat.RData")
load("Inputs/poor_recdevs_mat.RData")

#Variables
nyears <- 100
nyears_fwd <- 25
scen <- "HRF_SQ"

#Template OM and EM files
om_dir <- paste0("opaka-om-", nyears_fwd, "_FRSlencomp") #, "-selex/" "-FRS-only" , "-R0_trend"
em_dir <- paste0("opaka-em-", nyears_fwd, "_FRSlencomp")

#Get iteration number
I <- as.numeric(tail(strsplit(args[1], "/")[[1]], n = 1))
print(I)
sas <- sas_full %>% filter(Scen_name == scen)

# Get F-vector 
F_list <- list(
    years = list(1949:2048, 1949:2048),
    fleets = c(1, 2), 
    fvals = list(F_df[2:101,"FRS"], F_df[2:101,"Noncomm"])
    #fvals = list(F_comm_df[1:nyears,I], F_noncomm_df[1:nyears,I])
)
#create sampling scheme for indices of abundance
index <- list(
    fleets = c(1, 3), 
    years = list(seq(1949, 2023, by = 1), seq(2017, 2048, by = 1)),
    seas = list(7,1), 
    sds_out = list(.2, sas[which(sas$N_years == nyears_fwd), "Resfish_index_CV"]),
    sds_obs = list(0.2, 0.15)
    # sds_obs = list(0.02, sas[which(sas$N_years == nyears_fwd), "Resfish_sd_obs"]),
    # sds_out = list(0.203, sas[which(sas$N_years == nyears_fwd), "Resfish_index_CV"]) 
)

lcomp <- list(
    fleets = c(1,3), Nsamp = list(c(rep(35, 21), effN$effN), rep(30, 32)),
    #Nsamp = list(c(rep(30, 7), rep(sas[which(sas$N_years == nyears_fwd), "Neff_len_Resfish"], nyears_fwd))),
    years = list(seq(1949, 2023), seq(2017, 2048, by = 1))
)

seed <- set.seed[I,2]
# rec_devs_mat <- matrix(data = 0, nrow = 75, ncol = 100)
# new_recs <- rnorm(nrow(recdev_df), mean = recdev_df$Value, sd = 0.1)
# rec_devs_mat[,I] <- c(rep(0.001,16), new_recs)
full_recdevs <- rbind(matrix(data = 0, nrow = 16, ncol = 100), full_recdevs)


ss3sim_base(
    iterations = I,
    scenarios = "HRF_SQ_rebuild_relax", #paste(scen, nyears_fwd, "yrfwd", sep = "_"), 
    f_params = F_list,
    index_params = index,
    lcomp_params = lcomp,
    om_dir = file.path(main.dir, "models", om_dir),
    em_dir = file.path(main.dir, "models", em_dir),
    user_recdevs = NULL,
    bias_adjust = T,
    seed = seed
)
