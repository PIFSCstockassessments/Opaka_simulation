#remotes::install_github("nmfs-fish-tools/SSMSE")
library(SSMSE)
library(r4ss)
library(ss3sim)
#library(foreach)
#library(doParallel)

# Create a folder for the output in the working directory.
run_SSMSE_dir <- file.path("SSMSE_models/run_SSMSE")
dir.create(run_SSMSE_dir)
om_dir <- file.path("SSMSE_models/OM")
# Copy SS inputs over from Opaka-FIMS-Case-Study directory and run model
#copy_SS_inputs(dir.old = file.path("..", "Opaka-FIMS-Case-Study", "Model", "01_original_model"), dir.new = om_dir, create.dir = TRUE, copy_exe = TRUE)
#run(om_dir, exe = "ss.exe")
rep <- SS_output(dir = om_dir)
SS_plots(rep)
## Trying to run a simple example
develop_OMs(OM_in_dir = om_dir, out_dir = run_SSMSE_dir, refit_OMs = T, hess = F, par_name = "SR_BH_steep", par_vals = 1)
#run(dir = file.path(run_SSMSE_dir, "OM_SR_BH_steep_1"), exe = "ss_opt_win", extras = "-maxfn 0 -phase 50 -nohess", skipfinished = F)

## Adding components to the OM for projections. In this case we want to add process error through recruitment devs and implementation error for catch
future_om_list <- create_future_om_list(list_length = 1)
# recruitment devs
future_om_list[[1]]$pars <- "rec_devs"
future_om_list[[1]]$scen <- c("replicate", "all")
future_om_list[[1]]$input$first_yr_averaging <- 1949
future_om_list[[1]]$input$last_yr_averaging <- 2023
future_om_list[[1]]$input$last_yr_orig_valu <- 2023
future_om_list[[1]]$input$first_yr_final_val <- 2024
future_om_list[[1]]$input$value <- "sd"
future_om_list[[1]]$input$last_yr_orig_valu <- NULL
# implementation error
### waiting on this for now

## Add obs error in sampling from OM
datfile <- SS_readdat_3.30(file.path(getwd(), "OM", "data.ss"))
sample_struct <- create_sample_struct(dat = datfile, nyrs = 5)
#evaluate and see what needs to be fixed
sample_struct
sample_struct$CPUE <- rbind(data.frame(Yr = seq(2024,2028), Seas = 7, FltSvy = 1, SE = NA), sample_struct$CPUE[-1,])
sample_struct$CPUE$SE <- 0.1
sample_struct$lencomp <- data.frame(
    Yr = rep(seq(2024,2028), 2),
    Seas = 1,
    FltSvy = rep(c(2,4), each = 5),
    Sex = 0,
    Part = 0, 
    Nsamp = 45
)

run_results_path <- file.path(run_SSMSE_dir, "results")
dir.create(run_results_path)
res <- run_SSMSE(
    scen_name_vec = "test",
    out_dir_scen_vec = run_results_path,
    iter_vec = 1,
    OM_in_dir_vec = run_SSMSE_dir,
    EM_in_dir_vec = file.path(getwd(), "EM"),
    MS_vec = "no_catch",
    nyrs_vec = 5,
    nyrs_assess_vec = 5,
    future_om_list = future_om_list[[1]], 
    run_parallel = F,
    sample_struct_list = sample_struct,
    seed = 123
)


oms <- SSgetoutput(dirvec = c(file.path(getwd(), "OM_full"), file.path(getwd(), "OM_no_size"), file.path(getwd(), "OM_test")))
om_comps <- SSsummarize(oms)
SSplotComparisons(om_comps)


create_OM(file.path(getwd(), "OM_test"),
                      overwrite = TRUE,
                      writedat = TRUE,
                      verbose = FALSE,
                      nyrs = 0,
                      nyrs_assess = 5,
                      nscen = 1,
                      scen_name = "test",
                      niter = 1,
                      future_om_dat = NULL,
                      verify_OM = TRUE,
                      sample_struct_hist = NULL,
                      sample_struct = NULL,
                      seed = 123)



om_mcmc <- SS_output(dir = file.path(getwd(), "Full_mod", "OM_mcmc"),
    dir.mcmc = file.path(getwd(), "Full_mod", "OM_mcmc"))
