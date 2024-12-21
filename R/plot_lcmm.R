#' Plot a latent-class mixed model to study the effect of the day of the week on behaviour
#'
#' @param data 
#' @param model 
#' @param y 
#' @param labs_x 
#' @param labs_y 
#'
#' @return
#' @export
#'
#' @examples
plot_lcmm <- function(data, model, y, labs_x, labs_y) {
  
  if (model$ng == 1) labels <- "Class 1"
  if (model$ng == 2) labels <- c("Class 1", "Class 2")
  if (model$ng == 3) labels <- c("Class 1", "Class 2", "Class 3")
  if (model$ng == 4) labels <- c("Class 1", "Class 2", "Class 3", "Class 4")
  if (model$ng == 5) labels <- c("Class 1", "Class 2", "Class 3", "Class 4", "Class 5")
  
  # Compute predictions for the day effect per latent class
  newdat <-
    (predictY(
      model,
      data.frame(day = c(
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday",
        "Sunday"
      )),
      var.time = "day",
      draws = TRUE
    )[1]) |>
    as.data.frame() |>
    mutate(day =  c(
      "Monday",
      "Tuesday",
      "Wednesday",
      "Thursday",
      "Friday",
      "Saturday",
      "Sunday"
    )
    ) |>
    pivot_longer(
      cols = c(everything(), -day),
      names_to = c("lines", "class"),
      names_pattern = "(.*)_(.*)",
      values_to = "pred"
    ) |>
    pivot_wider(
      names_from = lines, 
      values_from = pred
    )
  
  # Rename classes
  newdat$class <- factor(newdat$class,labels = labels)
  
  # Make plot
  p <-
    ggplot() +
    geom_line(data = data |>
                inner_join(model$pprob) |>
                mutate(class = factor(class, labels = labels)),
              aes(x = data$day, y = data[[y]], group = id, linetype = "Participant trajectories"),
              color = "grey",
              alpha = 0.3
    ) +
    geom_ribbon(data = newdat,
                aes(x = day, ymin = pred.lower.Ypred, ymax = pred.upper.Ypred,
                    fill = class, group = class), 
                alpha = 0.3
    ) +
    geom_line(data = newdat,
              aes(x = day, y = pred.Ypred, color = class, group = class)
    ) +
    facet_wrap(~ class, ncol = 1) +
    labs(x = labs_x,
         y = labs_y,
         color = "Model predictions [95% CI]",
         fill = "Model predictions [95% CI]",
         linetype = "Posterior classification"
    ) +
    theme_bw()
  
  return(p)
  
}
