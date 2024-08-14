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
    Lbin_cols <- grep("Lbin_", colnames(report.file$lendbase))
    SuprPer_cols <- grep("SuprPer", colnames(report.file$lendbase))
    other_cols <- which(colnames(report.file$lendbase) %in%
        c("Yr", "Area", "Seas", "Fleet"))
    xx <- report.file$lendbase[, c(other_cols, Obs_cols, Exp_cols, Nsamp_cols,effN_cols,Lbin_cols,SuprPer_cols)]
    xx <- xx[xx$Yr %in% years, ]
    xx$year <- xx$Yr
    xx$Yr <- NULL
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
    return(xx)
}