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

set.seed <- read.csv("setseed.csv")
sas_full <- read.csv("sas.csv")
F_df <- read.csv("F_df.csv")
recdev_df <- read.csv("recdev_df.csv")
effN <- read.csv("effN.csv")
# load("Inputs/constantF_mat.RData")
# load("increaseF_mat.RData")
# load("Inputs/recdevs_mat.RData")
# load("Inputs/poor_recdevs_mat.RData")

#Variables
nyears <- 75
nyears_fwd <- 0
scen <- "HRF_SQ"

#Template OM and EM files
om_dir <- paste0("opaka-om") #, "-selex/" "-FRS-only" , "-R0_trend"
em_dir <- paste0("opaka-em")

#Get iteration number
I <- as.numeric(tail(strsplit(args[1], "/")[[1]], n = 1))
print(I)
sas <- sas_full %>% filter(Scen_name == scen)

# Get F-vector 
F_list <- list(
    years = list(1949:2023, 1949:2023),
    fleets = c(1, 2), 
    fvals = list(F_df[2:76,"FRS"], F_df[2:76,"Non_comm"])
    #fvals = list(F_comm_df[1:nyears,I], F_noncomm_df[1:nyears,I])
)
#create sampling scheme for indices of abundance
index <- list(
    fleets = c(1, 3), 
    years = list(seq(1949, 2023, by = 1), seq(2017, 2023, by = 1)),
    seas = list(7,1), 
    sds_out = list(.2, .15),
    sds_obs = list(0.08, 0.15)
    # sds_obs = list(0.2, sas[which(sas$N_years == nyears_fwd), "Resfish_sd_obs"]),
    # sds_out = list(0.08, sas[which(sas$N_years == nyears_fwd), "Resfish_index_CV"]) 
)

lcomp <- list(
    fleets = c(1,3), Nsamp = list(effN$effN, rep(30, 7)),
    # Nsamp = list(effN$effN, c(rep(30, 7), rep(sas[which(sas$N_years == nyears_fwd), "Neff_len_Resfish"], nyears_fwd))),
    years = list(seq(1949,2023,by=1), seq(2017, 2023, by = 1))
)

seed <- set.seed[I,2]

rec_devs_mat <- matrix(data = 0, nrow = 75, ncol = 100)
new_recs <- rnorm(nrow(recdev_df), mean = recdev_df$Value, sd = 0.1)
rec_devs_mat[,I] <- c(rep(0.001,16), new_recs)

ss3sim_base(
    iterations = I,
    scenarios = "HRF_SQ",  #paste(scen, nyears_fwd, "yrfwd", sep = "_"), 
    f_params = F_list,
    index_params = index,
    lcomp_params = lcomp,
    om_dir = om_dir,
    em_dir = em_dir,
    user_recdevs = rec_devs_mat,
    bias_adjust = T,
    seed = seed
)
