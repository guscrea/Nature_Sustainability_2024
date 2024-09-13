# function for building  data frame of ICR values in reliability testing

# inputs
# ka = the object holding icr estimates, produced by krippendorffs.alpha() function
# data_source = character string for name of data sources used in ICR (e.g. "FJC IDB" or "RESL")
# var_name = character string of name of variable that is being assessed for ICR
# df_icr = data frame to be created or already created and holding icr estimates.
# notes = character string of any notes to be displayed in table.

build_icr_df <- function(ka,
                         data_source,
                         var_name,
                         df_to_add_to,
                         notes){
  # get values from kripp. alpha output; make them into
  # a 1-row df. 
  df <- c(
    data_source,
    var_name,
    round(ka$alpha.hat[[1]],4),
    round(ka$L,4),
    round(ka$U,4),
    ka$level,
    notes
    )
  df <- data.frame(
    matrix(
      unlist(df),
      ncol = length(df)
      )
  )
  # create vector of column names for df; add names to df
  df_names <- c(
      "Data Source",
      "Variable",
      "α estimate",
      "Lower bound",
      "Upper bound",
      "Method",
      "Notes"
    )
  names(df) <- df_names
    
  # if df_to_add_to already exists, bind new values to already extant df
  if(!is.null(df_to_add_to)){
    df_out <- bind_rows(
      df_to_add_to,df
    )
  }
  # if df_to_add_to does not exist, rename new df as df_out
  else {
    df_out <- df
  }
  
  return(df_out)
}