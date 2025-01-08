plot_raincloud_by_fact <- function(
    data, 
    id, 
    x, 
    y, 
    factor, 
    labs_x, 
    labs_y, 
    labs_fact,
    col_vals, 
    fill_vals,
    add_means = FALSE
    ) {
  
p <-
  ggplot(data, aes(x = .data[[x]], y = .data[[y]], fill = .data[[factor]], color = .data[[factor]])) +
    geom_rain(
      rain.side = "l",
      id.long.var = id,
      boxplot.args = list(color = "black"),
      boxplot.args.pos = list(
        position = ggpp::position_dodgenudge(x = -0.05, width = 0.3), 
        width = 0.2
      ),
      point.args = list(alpha = 0.3),
      point.args.pos = list(
        position = ggpp::position_dodgenudge(x = 0.4, width = 0.1)
      ),
      line.args.pos = list(
        position = ggpp::position_dodgenudge(x = 0.4, width = 0.1)
      ),
      violin.args = list(alpha = 0.3)
    )  +
    scale_color_manual(values = col_vals) +
    scale_fill_manual(values = fill_vals) +
    labs(x = labs_x, y = labs_y, color = labs_fact, fill = labs_fact) +
    theme_bw() +
    theme(
      legend.title = element_text(face = "bold")
    )

  if (isTRUE(add_means)) {
    p <- p +
      stat_summary(
        aes(x = stage(day, after_scale = x + 0.4)),
        fun = "mean",
        geom = "point",
        position = position_dodge(0.3),
        size = 2
      ) +
      stat_summary(
        aes(x = stage(day, after_scale = x + 0.4)),
        fun.data = "mean_sdl",
        geom = "errorbar",
        fun.args = list(mult = 1),
        width = 0.05,
        position = position_dodge(0.3),
        linewidth = 1,
      ) +
      stat_summary(
        aes(
          x = stage(day, after_scale = x + 0.4),
          group = .data[[factor]],
          color = .data[[factor]]
        ),
        fun = "mean",
        geom = "line",
        linewidth = 1,
        position = position_dodge(0.3)
      )
    
  } else {
   p <- p +
      stat_summary(
        aes(
          group = .data[[factor]],
          color = .data[[factor]]
        ),
        fun = "median",
        geom = "line",
        position = ggpp::position_dodgenudge(x = 0.05, width = 0.3),
        linewidth = 1
      )
    
  }

return(p)
 
}
