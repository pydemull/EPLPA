plot_metric_by_day <- function(data, y, labs_x, labs_y) {

    ggplot(data = data, aes(x = day, y = .data[[y]], color = capl_interpretation)) +
    stat_halfeye(
      aes(fill = capl_interpretation), 
      point_interval = NULL,
      side = "left",
      alpha = 0.3,
      justification = 1.35
    ) +
    geom_point(
      aes(
        color = capl_interpretation
      ),
      alpha = 0.3,
      position = position_jitter(seed = 123, width = 0.05, height = 0)
    ) +
    geom_path(
      aes(
        group = id, 
        color = capl_interpretation
      ),
      alpha = 0.1,
      position = position_jitter(seed = 123, width = 0.05, height = 0)
    ) +
    stat_summary(
      aes(x = stage(day, after_scale(x - 0.20))),
      fun = "mean", 
      geom = "point",
      position = position_dodge(0.2)
    ) +
    stat_summary(
      aes(x = stage(day, after_scale(x - 0.20))),
      fun.data = "mean_sdl", 
      geom = "errorbar", 
      fun.args = list(mult = 1), 
      width = 0,
      position = position_dodge(0.2)
    ) +
    stat_summary(
      aes(
        x = stage(day, after_scale(x - 0.20)),
        group = capl_interpretation,
        color = capl_interpretation
      ),
      fun = "mean",
      geom = "line",
      position = position_dodge(0.2)
    ) +
    scale_color_manual(values = hue_pal()(5)[2:5]) +
    scale_fill_manual(values = hue_pal()(5)[2:5]) +
    labs(x = labs_x, y = labs_y, color = "CAPL-2 profile", fill = "CAPL-2 profile") +
    theme_bw()
  
}
