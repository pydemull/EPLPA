plot_capl_profile_by_sex <- function(data, y, labs_x, labs_y) {
  
 p <-
    ggplot(data = data, aes(x = score, y = .data[[y]], color = gender)) +
    geom_point(
      aes(
        color = gender
      ),
      alpha = 0.3,
      position = position_jitter(seed = 123, width = 0.05, height = 0)
    ) +
    geom_path(
      aes(
        group = id, 
        color = gender
      ),
      alpha = 0.1,
      position = position_jitter(seed = 123, width = 0.05, height = 0)
    ) +
    geom_boxplot(
      aes(x = stage(score, after_scale(x - 0.20))),
      position = position_dodge(0.2),
      width = 0.1
    ) +
    stat_summary(
      aes(
        x = stage(score, after_scale(x - 0.20)),
        group = gender,
        color = gender
      ),
      fun = "median",
      geom = "line",
      position = position_dodge(0.2),
      linewidth = 1
    ) +
    scale_color_manual(values = c("hotpink", "royalblue")) +
    scale_fill_manual(values = c("hotpink", "royalblue")) +
    labs(x = labs_x, y = labs_y, color = "Sex", fill = "Sex") +
    theme_bw() +
    theme(
      legend.title = element_text(face = "bold")
    )
  
 if("Physical \ncompetence" %in% data[["score"]]) {
   p <-
     p +
     stat_halfeye(
       aes(fill = gender), 
       point_interval = NULL,
       side = "left",
       alpha = 0.3,
       justification = 1.35
     ) 
 } else {
   p <-
     p +
     stat_histinterval(
       aes(fill = gender), 
       point_interval = NULL,
       side = "left",
       alpha = 0.3,
       justification = 1.35
     ) 
 }
 

   return(p)
 
}
