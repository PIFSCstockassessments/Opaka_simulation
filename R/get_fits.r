#' Extract Stock Synthesis simulation output
#'
#' This high level function extracts fits from Stock Synthesis model runs. Give it a
#' directory which contains directories for different "scenario" runs, within
#' which are iterations. It writes two data.frames to file:
#' one for single scalar values (e.g., MSY) and a second
#' that contains output for each year of the same model (timeseries, e.g.,
#' biomass(year)). These can always be joined later.
#'
#' @param directory The directory which contains scenario folders with
#'   results.
#' @param overwrite_files A switch to determine if existing files should be
#'   overwritten, useful for testing purposes or if new iterations are run.
#' @param user_scenarios A character vector of scenarios that should be read
#'   in. Default is `NULL`, which indicates find all scenario folders in
#'   `directory`.
#' @param type A character string specifying if you want the results to be
#'   written to the disk and returned as a long or wide data frame, where the
#'   default is `"long"`.
#' @param filename_prefix A character string specifying a prefix to append to
#'   the filename. Defaults to "ss3sim".
#' @export
#' @return Returns a list of 3 dataframes: index, lencomp, and agecomp.
#' Creates two .csv files in the current working directory,
#' where the names of those files are based on `filename_prefix`
#' and the default leads to the following:
#' `ss3sim_lencomp.csv` and `ss3sim_index.csv`.

get_fits_all <- function(directory = getwd(), overwrite_files = FALSE,
                            user_scenarios = NULL, type = c("long", "wide"), filename_prefix = "ss3sim") {
  old_wd <- getwd()
  on.exit(setwd(old_wd))

  type <- match.arg(type, several.ok = FALSE)

  ## Choose whether to do all scenarios or the vector passed by user
  if (is.null(user_scenarios)) {
    scenarios <- get_scenarios(directory = directory)
  } else {
    temp_scenarios <- dir(path = directory, include.dirs = TRUE)
    scenarios <- user_scenarios[which(user_scenarios %in% temp_scenarios)]
    if (any(user_scenarios %in% temp_scenarios == FALSE)) {
      warning(paste(user_scenarios[which(user_scenarios %in%
        temp_scenarios == FALSE)], "not in directory\n"))
    }
  }

  if (length(scenarios) == 0) {
    stop("No scenarios found in:", directory)
  }
  message("Extracting fits from ", length(scenarios), " scenarios")

  ## Loop through each scenario in folder in serial
  agecomp.list <- lencomp.list <- index.list <-
    vector(mode = "list", length = length(scenarios))
  setwd(directory)
  for (i in seq_along(scenarios)) {
    scen <- scenarios[i]
    ## If the files already exist just read them in, otherwise get fits
    index.file <- file.path(scen, paste0("fits_index_", scen, ".csv"))
    lencomp.file <- file.path(scen, paste0("fits_lencomp_", scen, ".csv"))
    agecomp.file <- file.path(scen, paste0("fits_agecomp_", scen, ".csv"))
    ## Delete them if this is flagged on
    if (overwrite_files) {
      if (file.exists(index.file)) file.remove(index.file)
      if (file.exists(lencomp.file)) file.remove(lencomp.file)
      if (file.exists(agecomp.file)) file.remove(agecomp.file)
      get_fits_scenario(
        scenario = scen, directory = directory,
        overwrite_files = overwrite_files
      )
    }
    ## Check if still there and skip if already so, otherwise read in
    ## and save to file
    if (!file.exists(index.file) | !file.exists(lencomp.file) | !file.exists(agecomp.file)) {
      get_fits_scenario(
        scenario = scen, directory = directory,
        overwrite_files = overwrite_files
      )
    }
    index.list[[i]] <- tryCatch(suppressWarnings(utils::read.csv(index.file, stringsAsFactors = FALSE)), error = function(e) NA)
    lencomp.list[[i]] <- tryCatch(suppressWarnings(utils::read.csv(lencomp.file, stringsAsFactors = FALSE)), error = function(e) NA)
    agecomp.list[[i]] <- tryCatch(suppressWarnings(utils::read.csv(agecomp.file, stringsAsFactors = FALSE)), error = function(e) NA)
  }
  index.list <- index.list[which(!is.na(index.list))]
  lencomp.list <- lencomp.list[which(!is.na(lencomp.list))]
  agecomp.list <- agecomp.list[which(!is.na(agecomp.list))]
  ## Combine all scenarios together and save into big final files
  index.all <- add_colnames(index.list, bind = TRUE)
  lencomp.all <- add_colnames(lencomp.list, bind = TRUE)
  agecomp.all <- add_colnames(agecomp.list, bind = TRUE)
  if (type == "wide") {
    index.all <- convert_to_wide(index.all)
    lencomp.all <- convert_to_wide(lencomp.all)
    agecomp.all <- convert_to_wide(agecomp.all)
  }
  index.file.all <- paste0(filename_prefix, "_index.csv")
  lencomp.file.all <- paste0(filename_prefix, "_lencomp.csv")
  agecomp.file.all <- paste0(filename_prefix, "_agecomp.csv")
  if (file.exists(index.file.all) & !overwrite_files) {
    warning(
      index.file.all, " already exists and overwrite_files = FALSE, ",
      "so a new file was not written."
    )
  } else { # can write either way
    utils::write.csv(index.all, file = index.file.all, row.names = FALSE)
  }
  if (file.exists(lencomp.file.all) & !overwrite_files) {
    warning(
      lencomp.file.all, " already exists and overwrite_files = FALSE, ",
      "so a new file was not written."
    )
  } else { # can write either way
    utils::write.csv(lencomp.all, file = lencomp.file.all, row.names = FALSE)
  }
  if (file.exists(agecomp.file.all) & !overwrite_files) {
    warning(
      agecomp.file.all, " already exists and overwrite_files = FALSE, ",
      "so a new file was not written."
    )
  } else { # can write either way
    utils::write.csv(agecomp.all, file = agecomp.file.all, row.names = FALSE)
  }
  ret <- list(
    index = index.all,
    lencomp = lencomp.all,
    agecomp = agecomp.all
  )
}

#' Extract Stock Synthesis simulation results for one scenario
#'
#' Extract results from all iterations inside a supplied scenario folder.
#' The function writes the following .csv files to the scenario folder
#' 1. scalar metrics with one value per iteration
#'    (e.g. \eqn{R_0}, \eqn{h}),
#' 1. a timeseries data ('ts') which contains multiple values per iteration
#'    (e.g.  \eqn{SSB_y} for a range of years \eqn{y}), and
#' 1. residuals on the log scale from the surveys across all iterations;
#'    this functionality is currently disabled and not tested.
#'
#' @seealso
#' [get_fits_all()] loops through these .csv files and
#' combines them together into a single "final" dataframe.
#'
#' @param scenario A single character giving the scenario from which to
#'   extract results.
#' @param directory The directory which contains the scenario folder.
#' @param overwrite_files A boolean (default is `FALSE`) for whether to delete
#'   any files previously created with this function. This is intended to be
#'   used if iterations were added since the last time it was called, or any
#'   changes were made to this function.
#' @author Cole Monnahan and Kathryn L. Doering
#' @export
get_fits_scenario <- function(scenario, directory = getwd(),
                                 overwrite_files = FALSE) {
  ## This function moves the wd around so make sure to reset on exit,
  ## especially in case of an error
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  if (file.exists(normalizePath(directory, mustWork = FALSE))) {
    setwd(directory)
  }
  if (file.exists(file.path(scenario))) {
    setwd(file.path(scenario))
  } else {
    stop(paste("Scenario", scenario, "does not exist in", directory))
  }
  ## Stop if the files already exist or maybe delete them
  index.file <- paste0("fits_index_", scenario, ".csv")
  lencomp.file <- paste0("fits_lencomp_", scenario, ".csv")
  agecomp.file <- paste0("fits_agecomp_", scenario, ".csv")
  if (file.exists(index.file) | file.exists(lencomp.file) | file.exists(agecomp.file)) {
    if (overwrite_files) {
      ## Delete them and continue
      message("Files deleted for ", scenario)
      file.remove(index.file, lencomp.file, agecomp.file)
    } else {
      ## Stop the progress
      stop("Files already exist for ", scenario, "
              and overwrite_files=FALSE")
    }
  }

  ## Loop through each iteration and get fits from both models
  reps.dirs <- list.files(pattern = "[0-9]+$")
  reps.dirs <- as.character(sort(as.numeric(reps.dirs)))
  if (length(reps.dirs) == 0) {
    return(vector(mode = "list", length = length(3)))
  }
  message("Starting ", scenario, " with ", length(reps.dirs), " iterations")
  ## Get the number of columns for this scenario
  get_fits_scen_list <- lapply(reps.dirs, get_fits_iter)
  # use this function to turn to bind the list components into 1 df

  ## Combine them together
  scen_dfs <- lapply(c("index", "lencomp", "agecomp"), make_df,
    list_df = get_fits_scen_list
  )
  names(scen_dfs) <- c("index", "lencomp", "agecomp")
  index <- scen_dfs[["index"]]
  lencomp <- scen_dfs[["lencomp"]]
  agecomp <- scen_dfs[["agecomp"]]
  if (isTRUE(is.data.frame(index) & is.data.frame(lencomp) & is.data.frame(agecomp))) {
    index$scenario <- lencomp$scenario <- agecomp$scenario <- scenario

    ## Write them to file in the scenario folder
    index.exists <- file.exists(index.file)
    utils::write.table(
      x = index, file = index.file, append = index.exists,
      col.names = !index.exists, row.names = FALSE, sep = ","
    )
    lencomp.exists <- file.exists(lencomp.file)
    utils::write.table(
      x = lencomp, file = lencomp.file, append = lencomp.exists,
      col.names = !lencomp.exists, row.names = FALSE, sep = ","
    )
    agecomp.exists <- file.exists(agecomp.file)
    utils::write.table(
      x = agecomp, file = agecomp.file, append = agecomp.exists,
      col.names = !agecomp.exists, row.names = FALSE, sep = ","
    )
  }
  ret <- list(
    index = index,
    lencomp = lencomp,
    agecomp = agecomp
  )
}


#' Get fits for 1 iteration
#' modifed from `get_results_iter()`
#'
#' @param dir_1_iter The full or relative path to the Stock Synthesis iteration folder.
#'  Assumed to contain multiple model folders that contain "om" or "em"
#'  (not case sensitive) somewhere in the model file name. If specified,
#'  mod_dirs need not be specified.
#' @param mod_dirs The full or relative path to the Stock Synthesis model folders as a
#'  vector of characters. If specified, dir_1_iter need not be specified.
#' @param iter_name Name of the iteration, which will be appended to the
#'  dataframes . Defaults to NULL, in which case the iter_name will be the
#'  folder name of dir_1_iter or the folder name 1 level up from the first
#'  mod_dirs specified
#' @author Kathryn L. Doering
#' @export
#' @return A list of 3 data frames called index, lencomp, and agecomp. These lists contain information for
#'  multiple model runs (estimation models and operating models) for 1
#'  iteration.
get_fits_iter <- function(dir_1_iter = NULL, mod_dirs = NULL,
                             iter_name = NULL) {
  # checks
  if (is.null(dir_1_iter) & is.null(mod_dirs)) {
    stop("Please specify either dir_1_iter or mod_dirs.")
  }
  if (!is.null(dir_1_iter) & !is.null(mod_dirs)) {
    stop("Please specify only dir_1_iter or mod_dirs, leaving the other NULL.")
  }
  if (!is.null(dir_1_iter)) {
    dir_1_iter <- normalizePath(dir_1_iter)
  }
  if (!is.null(mod_dirs)) {
    mod_dirs <- normalizePath(mod_dirs)
  }
  # get the directories if not prespecified.
  if (!is.null(dir_1_iter)) {
    mod_dirs <- list.dirs(dir_1_iter, recursive = FALSE)
    mod_dirs <- grep("[oe]m", mod_dirs, value = TRUE, ignore.case = TRUE)
  }
  if (is.null(iter_name)) {
    iter_name <- basename(dirname(mod_dirs[1]))
  }

  # call get_fits_mod
  iter_list <- lapply(mod_dirs, get_fits_mod)
  return_iter <- lapply(c("index", "lencomp", "agecomp"), make_df,
    list_df = iter_list
  )
  names(return_iter) <- c("index", "lencomp", "agecomp")
  if (isTRUE(all(unlist(lapply(return_iter, function(x) is.data.frame(x)))) == TRUE)) {
    return_iter$index$iteration <- return_iter$lencomp$iteration <-
      return_iter$agecomp$iteration <- iter_name
  }
  # return the iteration level dfs as a list
  return_iter
}

#' Get fits for 1 model run
#' Modified from get_results_mod()
#'
#' @param dir The full or relative path to the Stock Synthesis model file folder. If not
#'  specified, uses the working directory.
#' @param is_EM Is this an estimation model? Defaults to NULL, which will look
#' for the letters "em" (lower or uppercase) to decide if this is an estimation
#' model or operating model.
#' @param is_OM Is this an operating model? Defaults to NULL, which will look
#' for the letters "om" (lower or uppercase) to decide if this is an estimation
#' model or operating model.
#' @return A list of 3 data frames called index, lencomp, and agecomp. These data frames contain fits for 1
#'  model run.
get_fits_mod <- function(dir = getwd(), is_EM = NULL, is_OM = NULL) {
  # Input checks:
  if (!file.exists(file.path(dir, "Report.sso")) |
    file.size(file.path(dir, "Report.sso")) == 0) {
    fits_mod <- list(
      index = NA,
      lencomp = NA,
      agecomp = NA
    )
    return(fits_mod)
  }
  # figure out if is EM and if forecast report should be read
  if (is.null(is_EM)) {
    if (length(grep("em", basename(dir), ignore.case = TRUE)) > 0) {
      is_EM <- TRUE
    } else {
      is_EM <- FALSE
    }
  }
  if (is.null(is_OM)) {
    if (length(grep("om", basename(dir), ignore.case = TRUE)) > 0) {
      is_OM <- TRUE
    } else {
      is_OM <- FALSE
    }
  }
  if (is_EM) {
    forecastTF <- ifelse(
      file.size(file.path(dir, "Forecast-report.sso")) %in% c(0, NA),
      FALSE, TRUE
    )
  } else {
    forecastTF <- FALSE
  }
  report <- r4ss::SS_output(file.path(dir),
    covar = FALSE, verbose = FALSE,
    forecast = forecastTF, warn = FALSE,
    readwt = FALSE, printstats = FALSE, NoCompOK = TRUE
  )
  ## Get dfs
  index <- get_fits_index(report)
  lencomp <- get_fits_lcomp(report)
  agecomp <- get_fits_acomp(report)
  # add additional values
  index$model_run <- lencomp$model_run <- agecomp$model_run <- basename(dir)

  # list to return
  fits_mod <- list(
    index = index,
    lencomp = lencomp,
    agecomp = agecomp
  )
}


#' Return the index fits from an iteration
#' 
#' @return 
#' A dataframe with the following columns:
#' * Fleet
#' * Fleet_name
#' * Area
#' * Seas
#' * Obs 
#' * Exp
#' * SE
#' * SE_input
#' * year

get_fits_index <- function(report.file){
    years <- report.file$startyr:(report.file$endyr +
    ifelse(is.na(report.file$nforecastyears),
      0,
      report.file$nforecastyears
    ))
    Obs_cols <- grep("Obs", colnames(report.file$cpue))
    Exp_cols <- grep("Exp", colnames(report.file$cpue))
    SE_cols <- grep("SE", colnames(report.file$cpue))
    other_cols <- which(colnames(report.file$cpue) %in%
        c("Yr", "Area", "Seas", "Fleet", "Fleet_name"))
    xx <- report.file$cpue[, c(other_cols, Obs_cols, Exp_cols, SE_cols)]
    xx <- xx[xx$Yr %in% years, ]
    xx$year <- xx$Yr
    xx$Yr <- NULL
    return(xx)

}

#' Return the length comp fits from an iteration
#' 
#' @return 
#' A dataframe with the following columns:
#' * Seas
#' * Fleet
#' * Area
#' * Obs 
#' * Exp
#' * Nsamp_adj
#' * Nsamp_in
#' * effN
#' * Lbin_lo
#' * Lbin_hi
#' * Lbin_range
#' * Lbin_mid
#' * SuprPer
#' * year
get_fits_lcomp <- function(report.file){
    years <- report.file$startyr:(report.file$endyr +
    ifelse(is.na(report.file$nforecastyears),
      0,
      report.file$nforecastyears
    ))
    Obs_cols <- grep("Obs", colnames(report.file$lendbase))
    Exp_cols <- grep("Exp", colnames(report.file$lendbase))
    Nsamp_cols <- grep("Nsamp", colnames(report.file$lendbase))
    effN_cols <- grep("effN", colnames(report.file$lendbase))
    Bin_cols <- grep("Bin", colnames(report.file$lendbase))
    SuprPer_cols <- grep("SuprPer", colnames(report.file$lendbase))
    other_cols <- which(colnames(report.file$lendbase) %in%
        c("Yr", "Area", "Seas", "Fleet"))
    xx <- report.file$lendbase[, c(other_cols, Obs_cols, Exp_cols, Nsamp_cols,effN_cols,Bin_cols,SuprPer_cols)]
    xx <- xx[xx$Yr %in% years, ]
    xx$year <- xx$Yr
    xx$Yr <- NULL
    if(nrow(xx) == 0){
      xx[1,] <- NA
    }
    return(xx)
}

#' Return the age comp fits from an iteration
#' 
#' @return 
#' A dataframe with the following columns:
#' * Seas
#' * Fleet
#' * Area
#' * Obs 
#' * Exp
#' * Nsamp_adj
#' * Nsamp_in
#' * effN
#' * Bin
#' * SuprPer
#' * year
get_fits_acomp <- function(report.file){
    years <- report.file$startyr:(report.file$endyr +
    ifelse(is.na(report.file$nforecastyears),
      0,
      report.file$nforecastyears
    ))
    Obs_cols <- grep("Obs", colnames(report.file$agedbase))
    Exp_cols <- grep("Exp", colnames(report.file$agedbase))
    Nsamp_cols <- grep("Nsamp", colnames(report.file$agedbase))
    effN_cols <- grep("effN", colnames(report.file$agedbase))
    Bin_cols <- grep("Bin", colnames(report.file$agedbase))
    SuprPer_cols <- grep("SuprPer", colnames(report.file$agedbase))
    other_cols <- which(colnames(report.file$agedbase) %in%
        c("Yr", "Area", "Seas", "Fleet"))
    xx <- report.file$agedbase[, c(other_cols, Obs_cols, Exp_cols, Nsamp_cols,effN_cols,Bin_cols,SuprPer_cols)]
    xx <- xx[xx$Yr %in% years, ]
    xx$year <- xx$Yr
    xx$Yr <- NULL
    if(nrow(xx) == 0){
      xx[1,] <- NA
    }
    return(xx)
}

#' Make a list of lists with dataframe components into a dataframes
#'
#' Bind together list of list components with the same name
#' @param list_name A name to subset from iter_list
#' @param list_df A list of dataframes. These need not have the same column
#'  names, as this function will fill in with NAs.
#' @author Kathryn L. Doering
#' @return A dataframe
make_df <- function(list_name, list_df) {
  list_df_comp <- lapply(list_df, function(x) x[[list_name]])
  # set anything not a dataframe (the NA values) as Null
  list_df_comp <- lapply(list_df_comp, function(x) {
    if (!is.data.frame(x)) {
      x <- NULL
    }
    x
  })
  # drop any empty elements
  list_df_comp <- purrr::compact(list_df_comp)
  if (length(list_df_comp) > 0) {
    all_nms <- unique(unlist(lapply(list_df_comp, names)))
    # this extra code is needed in case of extra colnames that are not in both
    # dataframes.
    df <- do.call(
      rbind,
      c(
        lapply(
          list_df_comp,
          function(x) {
            data.frame(
              c(x, vapply(
                setdiff(all_nms, names(x)),
                function(y) NA, NA
              )),
              stringsAsFactors = FALSE
            )
          }
        ),
        make.row.names = FALSE
      )
    )
  } else {
    df <- NA
  }

  df
}

add_colnames <- function(dfs, bind = FALSE, fillwith = NA) {
  vars <- unique(unlist(lapply(dfs, base::names)))
  newdfs <- lapply(dfs, function(x) {
    missing <- setdiff(vars, names(x))
    x[, missing] <- fillwith
    return(x)
  })
  if (bind) newdfs <- do.call("rbind", newdfs)
  return(newdfs)
}
