plot_score_distri <-
  function(data1,
           data2,
           item,
           by_sex = c("no", "yes"),
           type = c("cont", "disc"),
           color = NA,
           text_x = 0,
           text_y = 0.45,
           breaks_x = seq(0, 10, 2),
           limits_x = c(0, 10)
           ) {
    
    type <- match.arg(type)
    by_sex <- match.arg(by_sex)
 
  # WHOLE GROUP
    
    if (by_sex == "no") {
      if (type == "disc") {
        
        # Build plot for discrete variable
        g <-
          ggplot(data = data1 |> filter(Item == item) |> mutate(Score = as.factor(Score)), 
                 aes(x = Score)) +
          geom_bar(fill = color, color = "black") +
          geom_text(data = data2 |>  filter(Item == item),
                    aes(
                      x = 0.5,
                      y = length(unique(data1$identifiant)) * 5.9 / 7,
                      hjust = 0.2,
                      label = paste0("N=", n)
                    ),  color = color) +
          geom_text(stat='count', aes(y = after_stat(count / 2), label = after_stat(count)), color = "white") +
          coord_cartesian(ylim = c(0, length(unique(data1$identifiant)))) +
          theme_bw() +
          facet_wrap( ~ Item, scales = "free")  +
          labs(y = NULL) +
          theme(
            axis.text = element_text(color = color),
            axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            strip.background = element_rect(fill = color, color = color),
            strip.text = element_text(color = "white", face = "bold"),
            panel.border = element_rect(
              color = color,
              linewidth = ifelse(
                item %in% c(
                  "Physical Competence (/30)",
                  "Daily Behaviour (/30)",
                  "Motivation and Confidence (/30)",
                  "Knowledge and Understanding (/10)",
                  "Physical Literacy (/100)"
                ),
                1.5,
                0.3
              )
            )
          )
        
      } 
      
      if (type == "cont") {
        
        # Build plot for continuous variable
        g <- ggplot(data = data1 |> filter(Item == item), aes(x = 0, y = Score)) +
          geom_rain(fill = color,
                    point.args = rlang::list2(
                      alpha = 0.3,
                      color = color,
                      size = 2
                    )) +
          geom_text(data = data2 |>  filter(Item == item),
                    aes(
                      x = text_y,
                      y = text_x,
                      hjust = 0.2,
                      label = paste0("N=", n)
                    ), color = color) +
          facet_wrap( ~ Item, scales = "free")  +
          scale_y_continuous(breaks = breaks_x) +
          coord_flip(xlim = c(-0.1, 0.55), ylim = limits_x) +
          labs(x = NULL) +
          theme_bw() +
          theme(
            axis.text.x = element_text(color = color),
            axis.ticks.x = element_line(color = color),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            strip.background = element_rect(fill = color, color = color),
            strip.text = element_text(color = "white", face = "bold"),
            panel.border = element_rect(
              color = color,
              linewidth = ifelse(
                item %in% c(
                  "Physical Competence (/30)",
                  "Daily Behaviour (/30)",
                  "Motivation and Confidence (/30)",
                  "Knowledge and Understanding (/10)",
                  "Physical Literacy (/100)"
                ),
                1.5,
                0.3
              )
            )
          )
      }
    }
       
  # BY SEX
    
    if (by_sex == "yes") {
      if (type == "disc") {
        
        # Build plot for discrete variable
        g <-
          ggplot(data = data1 |> filter(Item == item) |> mutate(Score = as.factor(Score)), 
                 aes(x = Score)) +
          geom_bar(aes(fill = gender), position = "dodge", color = "black") +
          geom_text(data = data2 |>  filter(Item == item & gender == "girl"),
                    aes(
                      x = 0.5,
                      y = length(unique(data1$identifiant)) / 2 * 6.6 / 7,
                      hjust = 0.2,
                      label = paste0("N=", n)
                    ), color = "hotpink", fontface = "bold") +
          geom_text(data = data2 |>  filter(Item == item & gender == "boy"),
                    aes(
                      x = 0.5,
                      y = length(unique(data1$identifiant)) / 2 * 5.6 / 7,
                      hjust = 0.2,
                      label = paste0("N=", n)
                    ), color = "royalblue2", fontface = "bold") +
          geom_text(stat='count', aes(y = after_stat(count / 2), label = after_stat(count),
                                      group = gender), 
                    color = "white", position = position_dodge(width = .9)) +
          scale_fill_manual(values = c("hotpink", "royalblue2"), labels = c("Girl", "Boy")) +
          coord_cartesian(ylim = c(0, length(unique(data1$identifiant)) /2)) +
          theme_bw() +
          facet_wrap( ~ Item, scales = "free")  +
          labs(y = NULL, fill = "") +
          theme(
            axis.text = element_text(color = color),
            axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            strip.background = element_rect(fill = color, color = color),
            strip.text = element_text(color = "white", face = "bold"),
            panel.border = element_rect(
              color = color,
              linewidth = ifelse(
                item %in% c(
                  "Physical Competence (/30)",
                  "Daily Behaviour (/30)",
                  "Motivation and Confidence (/30)",
                  "Knowledge and Understanding (/10)",
                  "Physical Literacy (/100)"
                ),
                1.5,
                0.3
              )
            )
          )
        
      } 
      
      if (type == "cont") {
        
        # Build plot for continuous variable
        g <-
          ggplot(data = data1 |> filter(Item == item), aes(
            x = 1,
            y = Score,
            fill = gender,
            color = gender
          )) +
          geom_rain(
            alpha = .5,
            rain.side = 'r',
            boxplot.args = list(
              color = "black",
              outlier.shape = NA,
              width = 0.05
            ),
            boxplot.args.pos = list(position = ggpp::position_dodgenudge(x = .1, width = 0.08)),
            point.args = list(
              color = "black",
              shape = NA,
              size = 0
            )
          ) +
          geom_point(aes(x = 0.998),
                     alpha = 0.5,
                     position = position_jitterdodge(
                       jitter.width = 0.05,
                       dodge.width = 0.08,
                       seed = 123
                     )) +
          geom_text(data = data2 |>  filter(Item == item & gender == "girl"),
                    aes(
                      x = text_y,
                      y = text_x,
                      hjust = 0.2,
                      label = paste0("N=", n)
                    ), color = "hotpink", fontface = "bold") +
          geom_text(data = data2 |>  filter(Item == item & gender == "boy"),
                    aes(
                      x = text_y - 0.09,
                      y = text_x,
                      hjust = 0.2,
                      label = paste0("N=", n)
                    ), color = "royalblue2", fontface = "bold") +
          facet_wrap( ~ Item, scales = "free")  +
          scale_y_continuous(breaks = breaks_x) +
          scale_fill_manual(values = c("hotpink", "royalblue2"), labels = c("Girl", "Boy")) +
          scale_color_manual(values = c("hotpink", "royalblue2"), labels = c("Girl", "Boy")) +
          theme_bw() +
          coord_flip(xlim = c(0.9, 1.6), ylim = limits_x) +
          labs(x = NULL, fill = "", color = "") +
          theme(
            legend.position = "none",
            axis.text.x = element_text(color = color),
            axis.ticks.x = element_line(color = color),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            strip.background = element_rect(fill = color, color = color),
            strip.text = element_text(color = "white", face = "bold"),
            panel.border = element_rect(
              color = color,
              linewidth = ifelse(
                item %in% c(
                  "Physical Competence (/30)",
                  "Daily Behaviour (/30)",
                  "Motivation and Confidence (/30)",
                  "Knowledge and Understanding (/10)",
                  "Physical Literacy (/100)"
                ),
                1.5,
                0.3
              )
            )
          )
      }
    }
    
    
    return(g)
  }
