#install.packages("openMSE")
library(openMSE)
library(r4ss)
library(tidyverse)
ssdir <- file.path("openMSE_models/OM")
OM <- SS2OM(ssdir, nsim = 50, proyears = 50, reps = 5, maxF = 3, seed = 12, interval = 5, 
pstar = 0.5, model_discards = FALSE, gender = 1, Name = "Opaka_OM", Author = "M.Oshima")

rep <- SS_output(dir = ssdir)

## Investigating stock parameter values
# maximum age
OM@maxage #should be 43
# natural mortality, upper and lower bounds of uniform distribution
OM@M #in SS, fixed at 0.135
# inter-annual variation in M, CV of log-normal distribution, upper and lower bounds of uniform distribution
OM@Msd #in SS, M is fixed so Msd = 0
OM@SRrel
# initial number of unfished recruits (R0)
OM@R0 #exp(5.28427) R0 estimated from SS
# steepness, upper and lower bounds of uniform distribution
OM@h #should be 0.76
# process error, sigmaR
OM@Perr #in SS, fixed at 0.52
# Autocorrelation of rec devs, upper and lower bounds of uniform distribution
OM@AC #not used in SS
# Linf, upper and lower bounds of uniform distribution
OM@Linf #in SS, fixed at 67.5
# k, upper and lower bounds of uniform distribution
OM@K #in SS, fixed at 0.242
# t0, theoretical age at size 0, upper and lower bounds for uniform distribution
OM@t0 #not used in SS, L1 used instead
# inter-annual variation in Linf, upper and lower bounds for uniform distribution
OM@Linfsd #not TV in SS, fixed at 0
# inter-annual variation in k, upper and lower bounds for uniform distribution
OM@Ksd
# length-at-age CV, constant for all ages, upper and lower bounds for uniform distribution
OM@LenCV <- c(0.085, 0.085)
# alpha and beta parameters for length-weight relationship, single values
OM@a #in SS, 1.75e-5
OM@b #in SS, 2.99
# length at 50% maturity, upper and lower bounds for uniform distribution
OM@L50 #in SS, 40.7
OM@L50 <- c(40.7, 40.7)
# difference in lengths between 50% and 95% maturity, upper and lower bounds for uniform distribution
OM@L50_95 #in SS, -2.26
OM@L50_95 <- c(-2.26, -2.26)
# depletion (SSB_terminal_year/SSB0), upper and lower bounds for uniform distribution
OM@D #not sure where it got this value 0.4325 from in SS file, unless calculated externally
#OM@D <- c(0,0) #set to 0 to not use, can pass actual F time series in cpars (Hordyk 2021)
# percent of total habitat, fraction of population, and probability of staying in each area (MSEtool uses a 2-box spatial model)
OM@Size_area_1 #single area model, so total habitat is even in both boxes, upper and lower bounds for uniform distribution  
OM@Frac_area_1 #even fishing in all areas, so should be 50% probability, upper and lower bounds for uniform distribution 
OM@Prob_staying #even probability of staying vs moving

##Investigating fleet dynamics
# years since the fishery began
OM@nyears #1949-2023
OM@CurrentYr #2023

# getting combined selectivity
fish_sel <- rep$sizeselex  |> 
    filter(Yr == 2023 & Factor == "Lsel")  |> 
    filter(Fleet == 1 | Fleet == 3) |> 
    select(-c(1:5))
sel <- apply(fish_sel, 2, sum) # sum across fleets for each length bin
nsel <- (sel-min(sel))/(max(sel)-min(sel)) #normalize between 0 and 1
plot(x = seq(1.5, 94.5, by = 1), y = nsel, pch = 16)
nsel
# selectivity, shortest length at which 5% of the population is vulnerable to gear, upper and lower bounds for uniform distribution 
OM@L5 #need to combine F-at-lengths for commercial and non-commercial fleets and then standardize to 1 to get combined selectivity (Hordyk 2021)
OM@L5 <- c(31.5, 31.5) #normalized selectivity = 4.65e-2
# selectivity, shortest length at which 100% of the population is vulnerable to gear, upper and lower bounds for uniform distribution 
OM@LFS
OM@LFS <- c(57.5, 57.5) #normalized selectivity = 9.954e-2
# proportion of fish selected by the gear at asymptotic length (Linf), 1 = 100% and curve is logistic, <1 dome shaped selectivity, upper and lower bounds between 0 and 1
OM@Vmaxlen <- c(1,1)
# are selectivity and retention params use absolute lengths or relative to size of maturity, T/F
OM@isRel #use absolute lengths (not relative to maturity = F)
# shortest length at which 5% of the population is vulneralbe to retention by fleet, upper and lower bounds for uniform distribution
OM@LR5 <- c(31.5, 31.5) #use same as selectivity
# shortest length at which 100% of the population is vulnerable to retention by fleet, upper and lower bounds for uniform distribution
OM@LFR <- c(31.5, 31.5) #use same as selectivity
# proportion of fish retained at asymptotic length (Linf)
OM@Rmaxlen
# discard rate,  upper and lower bounds between 0 and 1
OM@DR #no discards so should be 0
# MPA for area 1? T/F
OM@MPA #should be false
# distribution of fishing in relation to vulnerable biomass across areas (1 = no targeting, <1 avoidance, >1 targeting), upper and lower bounds for uniform distribution
OM@Spat_targ

# historical fishery effort information for conditioning OM
# years of fishing
OM@EffYears #seq(1,75,by=1)
# apical total F by year (same for Lower and Upper, bc not estimating it)
OM@EffLower
OM@EffUpper

#F.df <- rep$derived_quants |> 
#    filter(str_detect(Label, "F_")) |> 
#    select(Label, Value) |> 
#    separate(col = "Label", into = c("F_", "Year"), sep = "_") |> 
#    mutate(Year = as.numeric(Year)) |> 
#    filter(!is.na(Year))

#OM@EffLower <- F.df$Value
#OM@EffUpper <- F.df$Value

# additional variability on catchability
OM@Esd
OM@qinc
OM@qcv


plot(OM)

Hist <- Simulate(OM)
Hist@AtAge[["N.Mortality"]]

plot(Hist)
Hist@TSdata$Biomass[,,1]
Hist@TSdata$SBiomass[,,1]
Hist@TSdata$RecDev
plot_SS2OM(Hist, ssdir)


