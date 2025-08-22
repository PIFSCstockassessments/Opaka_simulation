## Script to create OM/EM pairs for given sampling strategies, F, and recdevs. 
library(r4ss) #needs to be v1.46.1 to work with ss3sim
library(magrittr)
library(dplyr)
library(ss3sim) #be sure to use ss3.exe v3.30.19
library(stringr)
print(getwd())

#get args from Bash environment (for OSG)
args <- commandArgs(trailingOnly = TRUE) 
print(args) 

set.seed <- read.csv("setseed.csv")
sas_full <- read.csv("sas.csv")
load("constantF_mat.RData")
load("poor_recdevs_mat.RData")
effN <- read.csv("effN.csv")

#Variables
nyears <- 100
nyears_fwd <- 25
scen <- "IRF_poorrec"

#Template OM and EM files
om_dir <- paste0("opaka-om-", nyears_fwd, "-r0trend")
em_dir <- paste0("opaka-em-", nyears_fwd, "-r0trend")

#Get iteration number
I <- as.numeric(tail(strsplit(args[1], "/")[[1]], n = 1))
print(I)
sas <- sas_full %>% filter(Scen_name == scen)

# Get F-vector 
F_list <- list(
    years = list(1949:2048, 1949:2048),
    fleets = c(1, 2), 
    fvals = list(F_comm_df[1:nyears,I], F_noncomm_df[1:nyears,I])
)
#create sampling scheme for indices of abundance
index <- list(
    fleets = c(1, 3), 
    years = list(seq(1949, 2023, by = 1), seq(2017, 2048, by = 1)),
    seas = list(7,1), 
    sds_obs = list(0.2, sas[which(sas$N_years == nyears_fwd), "Resfish_index_CV"]),
    sds_out = list(0.20, sas[which(sas$N_years == nyears_fwd), "Resfish_index_CV"]) 
)

lcomp <- list(
    fleets = c(1,3), 
    Nsamp = list(c(rep(35, 21), effN$effN), 
    rep(sas[which(sas$N_years == nyears_fwd), "Neff_len_Resfish"], 7+nyears_fwd)),
    years = list(seq(1949, 2023), seq(2017, 2048, by = 1))
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
    user_recdevs = full_poor_recdevs,
    bias_adjust = T,
    seed = seed
)
