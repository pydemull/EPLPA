recode_capl_interpretation_vars <- function(capl_var) {
  
  capl_var <- forcats::fct_na_value_to_level(capl_var, "Non available")
  capl_var <- forcats::fct_relevel(
    capl_var,
    "Non available",
    "beginning",
    "progressing",
    "achieving",
    "excelling"
  )
  capl_var <- fct_recode(
    capl_var,
    "Beginning" = "beginning",
    "Progressing" = "progressing",
    "Achieving" = "achieving",
    "Excelling" = "excelling"
  )
  
  return(capl_var)
}
