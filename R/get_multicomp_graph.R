get_multicomp_graph <- function(
    scores, 
    ssnonpartest_out, 
    skip = 9, 
    x_label = "Scores", 
    y_label = "All possible score combinations",
    point_size = 4,
    show_all_possible_combination = FALSE
    ) {

require(ggplot2)
  
# Get a dataframe allowing to build thereafter a ggplot2 grid to show
# all the possible score combinations

## Get the list of the score combinations
combs_list <- list()
for (i in seq_along(scores)) {
  
  ### Get a dataframe with the different score combinations
  combs <- combn(scores, i) |> as.data.frame() 
  
  ### Convert dataframe to a set of lists of score combinations in wide format
  ncols <- ncol(combs)
  score_list <- list()
  for (j in 1:ncols) {
    score_list[[j]] <- combs[, j]
  }
  
  ### Put the lists in a column of a new dataframe
  df_combs <- 
    data.frame(score_comb = sapply(score_list, paste, collapse = ", ")) |> 
    tibble::rowid_to_column() |> 
    dplyr::rename(id_comb = rowid) |> 
    dplyr::mutate(num_items = i) |> 
    dplyr::relocate(num_items)
  
  ### Assign final dataframe to initial list
  combs_list[[i]] <- df_combs
  
}

## Collapse the dataframes with all the possible score combinations
## into a single dataframe
df_combs <- dplyr::bind_rows(combs_list)

## Get the final grid with which the plot will be built
df_grid <-
  data.frame(scores_base = rep(scores, each = nrow(df_combs))) |> 
  dplyr::bind_cols(purrr::map_dfr(seq_along(scores), ~df_combs)) |> 
  dplyr::mutate(type = "default")


# Get a dataframe with all the significant score combinations

## Get the length of the ssnonpartest function output
out_end <- length(ssnonpartest_out)

## Extract the content of interest from the function output
new_out <- ssnonpartest_out[-c(1:skip, out_end)]

## Make a list with all the significant score combinations
res <- list()
for (i in seq_along(new_out)) {
  out_raw <- sub(".*The Hypothesis of equality using response variables  (.*) is rejected .*", "\\1", new_out[i])
  out_split <- strsplit(out_raw, " ")
  out_length <- length(out_split[[1]])
  res[[i]] <- data.frame(score_comb = sapply(out_split, paste, collapse = ", "))
}

## Convert the list of outputs to a dataframe
df_out <- 
  dplyr::bind_rows(res) |> 
  dplyr::mutate(type2 = "signif")

# Combine dataframes
df_final <- 
  df_grid |> dplyr::left_join(df_out) |> 
  dplyr::mutate(
    included = ifelse(scores_base %in% unlist(strsplit(score_comb, ", ")), "yes", "no"),
    scores_base = factor(scores_base, levels = scores),
    score_comb = factor(score_comb, levels = unique(df_grid$score_comb))
    )

# Add a column to the final dataframe to mark the rows that should be used to
# plot significant score combinations
df_final$included <- "no"
for (i in 1:nrow(df_grid)) {
  if (df_final$scores_base[i] %in% unlist(strsplit(as.character(df_final$score_comb[i]), ", "))
      && (!is.na(df_final$type2[i])) && df_final$type2[i] == "signif") {df_final$included[i] <- "yes"}
}

# Add a column to the final dataframe to mark the rows that should be used to
# plot all possible score combinations
df_final$included2 <- "no"
for (i in 1:nrow(df_grid)) {
  if (df_final$scores_base[i] %in% unlist(strsplit(as.character(df_final$score_comb[i]), ", ")))
      {df_final$included2[i] <- "yes"}
}

# Plot significant combinations among all possible combinations
  paste0(collapse = "")
df_final <- df_final |> 
  dplyr::mutate(
    add_0 = ifelse(num_items < 10, "0", ""),
    num_items = paste0(add_0, num_items, "-score combination(s)")
    )

if (isTRUE(show_all_possible_combination)) {
  p <-
    ggplot(data = df_final, aes(x = scores_base, y = score_comb)) +
    geom_tile(aes(fill = scores_base), color = "grey") +
    geom_point(data = df_final |> dplyr::filter(included2 == "yes"),
               aes(group = score_comb), size = point_size, color = "grey80") + 
    geom_line(data = df_final |> dplyr::filter(included2 == "yes"),
              aes(group = score_comb), linewidth = 1, color = "grey80") + 
    geom_point(aes(color = included, size = included)) +
    geom_line(data = df_final |> dplyr::filter(type2 == "signif" & included == "yes"),
              aes(group = score_comb, color = included), linewidth = 1) +
    labs(x = x_label, y = y_label) +
    theme_bw() +
    theme(
      panel.grid = element_blank()
    ) +
    scale_color_manual(values = c("grey80", "black"), labels = c("Non significant", "Significant")) +
    scale_size_manual(values = c(0, point_size), labels = c("Non significant", "Significant"))
} else {
  p <-
    ggplot(data = df_final |> dplyr::filter(type2 == "signif"), 
           aes(x = scores_base, y = score_comb)) +
    geom_tile(aes(fill = scores_base), color = "grey") +
    geom_point(aes(color = included, size = included)) +
    geom_line(data = df_final |> dplyr::filter(type2 == "signif" & included == "yes"),
              aes(group = score_comb, color = included), linewidth = 1) +
    labs(x = x_label, y = y_label) +
    theme_bw() +
    theme(
      panel.grid = element_blank()
    ) +
    scale_color_manual(values = c("grey80", "black"), labels = c("Non significant", "Significant")) +
    scale_size_manual(values = c(0, point_size), labels = c("Non significant", "Significant"))
  
}


return(p)

}
