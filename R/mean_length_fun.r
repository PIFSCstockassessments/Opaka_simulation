## calculate mean length from lencomp df made with ss3sim summary function get_fits()
mean_length <- function(lencomp_df){

    # indx is string combining fleet, year, and potentially conditional bin
    lencomp_df$indx <- paste(lencomp_df[["Fleet"]], lencomp_df[["year"]], lencomp_df[["model_run"]],
    lencomp_df[["iteration"]], lencomp_df[["scenario"]]) 
    # unique strings in indx vector
    uindx <- unique(lencomp_df$indx)
# create empty data.frame to store information on each observation
    pldat <- matrix(0, length(uindx), 10,
      dimnames = list(
        uindx,
        c(
          "Obsmn", "Obslo", "Obshi", "semn", "Expmn", "Std.res",
          "ObsloAdj", "ObshiAdj", "Fleet", "Yr"
        )
      )
    )
    # Find the weighting factor for this combination of factors
    for (i in seq_along(uindx)) { # each row of pldat is an individual comp
      subdbase <- lencomp_df[lencomp_df$indx == uindx[i], ]
      xvar <- subdbase[["Bin"]]
      # observed mean
      pldat[i, "Obsmn"] <- sum(subdbase[["Obs"]] * xvar) / sum(subdbase[["Obs"]])
      # expected mean
      pldat[i, "Expmn"] <- sum(subdbase[["Exp"]] * xvar) / sum(subdbase[["Exp"]])

      # use adjusted input sample size for Francis or MI weighting options
      Nsamp <- subdbase[["Nsamp_adj"]]
      if ("Nsamp_DM" %in% names(subdbase) || any(is.na(subdbase[["Nsamp_DM"]]))) {
        # dirichlet multinomial newer format
        Nsamp <- subdbase[["Nsamp_DM"]]
      }

      # standard error of the mean
      pldat[i, "semn"] <- sqrt((sum(subdbase[["Exp"]] * xvar^2) / sum(subdbase[["Exp"]]) -
        pldat[i, "Expmn"]^2) / mean(Nsamp))
      # calculate confidence intervals and other stuff
      pldat[i, "Obslo"] <- pldat[i, "Obsmn"] - 2 * pldat[i, "semn"]
      pldat[i, "Obshi"] <- pldat[i, "Obsmn"] + 2 * pldat[i, "semn"]
      pldat[i, "Std.res"] <- (pldat[i, "Obsmn"] - pldat[i, "Expmn"]) / pldat[i, "semn"]
      pldat[i, "Fleet"] <- mean(subdbase[["Fleet"]])
      pldat[i, "Yr"] <- unique(subdbase[["year"]])
    #   pldat[i, "Lbin"] <- subdbase$"Lbin_lo"

    }
    Nmult <- 1 / var(pldat[, "Std.res"], na.rm = TRUE)

    # Find the adjusted confidence intervals
    for (i in seq_along(uindx)) {
      pldat[i, "ObsloAdj"] <- pldat[i, "Obsmn"] - 2 * pldat[i, "semn"] / sqrt(Nmult)
      pldat[i, "ObshiAdj"] <- pldat[i, "Obsmn"] + 2 * pldat[i, "semn"] / sqrt(Nmult)
    }
    return(pldat)
}
