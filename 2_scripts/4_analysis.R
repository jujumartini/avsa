source("./2_scripts/1_functions.R")

# summary visit ------------------------------------------------------------

# check to see which columns have NAs
data_time <- read_csv(file = "./3_data/analysis/table_analysis_time.csv")
data_percentage <- read_csv(file = "./3_data/analysis/table_analysis_percentage.csv")

# seeing which columns have NAs
nacols <- function(df) {
  colnames(df)[unlist(lapply(df, function(x) anyNA(x)))]
}

nacols(data_time)
nacols(data_percentage)

# create visit summary table
create_visit_summary()



# IRR ---------------------------------------------------------------------

irr_table <- vroom(file = "./3_data/analysis/irr_table.csv",
                   delim = ",",
                   col_names = c("id_visit",
                                 "kripp_full",
                                 "kripp_gapless"))
mean(irr_table$kripp_full)
min(irr_table$kripp_full)
max(irr_table$kripp_full)

mean(irr_table$kripp_gapless)
min(irr_table$kripp_gapless)
max(irr_table$kripp_gapless)



# bias --------------------------------------------------------------------

create_bias_table()



# misclassification -------------------------------------------------------

create_miss_table()



# Figures: Plots ----------------------------------------------------------

# pos_figure_bias_minutes, save as 800 by 500 pixels
tbl_bias_time <- read_rds(path = "./4_results/posture_bias_time.rds")
{
  
  plot(1:3,
       1:3,
       xlab = "",
       ylab = "Bias (95% CI) in minutes",
       xlim = c(.5,3.5),
       ylim = range(tbl_bias_time[,c("Bias",
                                     "Lower_95_Bias",
                                     "Upper_95_Bias" )]),
       type = "n",
       axes = F,
       font.lab = 2)
  
  title(xlab = "Posture",
        font.lab = 2,
        line = 2.5)
  title(sub = paste0("* Mean event min from AP for sitting = ",
                     tbl_bias_time$AP_mean[1],
                     "', standing = ",
                     tbl_bias_time$AP_mean[2],
                     "', and movement = ",
                     tbl_bias_time$AP_mean[3],
                     "' *"), 
        font.sub = 1,
        adj = 0.5)
  
  axis(2)
  axis(1,
       at = 1:3,
       labels = tbl_bias_time$Posture)
  abline(h = 0,
         lty = 3)
  points(1:3,
         tbl_bias_time$Bias,
         pch = 16,
         cex = 1.5)
  
  for (i in 1:3) {
    
    lines(c(i,i),
          c(tbl_bias_time$Lower_95_Bias[i],tbl_bias_time$Upper_95_Bias[i]),
          lty = 2)
    text(i,
         tbl_bias_time$Bias[i],
         paste0(tbl_bias_time$Bias[i],"min"),
         pos = 4,
         cex = 1.25)
    
  }
}

# pos_figure_bias_percent, save as 800 by 500 pixels
tbl_bias_perc <- read_rds(path = "./4_results/posture_bias_perc.rds")
{
  
  plot(1:3,
       1:3,
       xlab = "",
       ylab = "Bias (95% CI) in minutes",
       xlim = c(.5,3.5),
       ylim = range(tbl_bias_perc[,c("Bias",
                                     "Lower_95_Bias",
                                     "Upper_95_Bias" )]),
       type = "n",
       axes = F,
       font.lab = 2)
  
  title(xlab = "Posture",
        font.lab = 2,
        line = 2.5)
  title(sub = paste0("* Mean event min from AP for sitting = ",
                     tbl_bias_perc$AP_mean[1],
                     "', standing = ",
                     tbl_bias_perc$AP_mean[2],
                     "', and movement = ",
                     tbl_bias_perc$AP_mean[3],
                     "' *"), 
        font.sub = 1,
        adj = 0.5)
  
  axis(2)
  axis(1,
       at = 1:3,
       labels = tbl_bias_perc$Posture)
  abline(h = 0,
         lty = 3)
  points(1:3,
         tbl_bias_perc$Bias,
         pch = 16,
         cex = 1.5)
  
  for (i in 1:3) {
    
    lines(c(i,i),
          c(tbl_bias_perc$Lower_95_Bias[i],tbl_bias_perc$Upper_95_Bias[i]),
          lty = 2)
    text(i,
         tbl_bias_perc$Bias[i],
         paste0(tbl_bias_perc$Bias[i],"%"),
         pos = 4,
         cex = 1.25)
    
  }
}

# pos_figure_miss_minutes, save as 850 by 600 (reshape2)
tbl_miss_time <- read_rds(path = "./4_results/posture_miss_time.rds")
{
  
  # read in only classifications
  graph <- tbl_miss_time[ , -which(colnames(tbl_miss_time) %in% c("AP_Total"))]
  
  # rename IMG to correct
  colnames(graph)[2] <- "Correct"
  
  # change table into variables that represent x, y, other
  graph <- graph %>% 
    melt(id.vars = "Posture")
  
  # clean
  colnames(graph)[2] <- "Classification"
  
  graph <- graph[graph$value != 0, ]
  
  graph$Posture <- factor(graph$Posture,
                          levels = c("Sit",
                                     "Stand",
                                     "Move"))
  
  # plot
  ggplot(data = graph) +
    geom_bar(mapping = aes(x = Posture,
                           y = value,
                           fill = Classification),
             stat = "identity") +
    geom_text(data = tbl_miss_time,
              mapping = aes(x = Posture,
                            y = AP_Total,
                            label = paste(AP_Total, "mins")),
              vjust = -0.5) +
    geom_text(mapping = aes(x = Posture,
                            y = value,
                            fill = Classification,
                            label = paste(value, "mins")),
              position = position_stack(vjust = 0.5)) +
    theme(plot.title = element_text(lineheight = 1,
                                    hjust = .5),
          text = element_text(size = 15)) +
    labs(title = "Total AP estimates classified by IMGs",
         x = "Posture",
         y = "Minutes") +
    scale_fill_manual(values = c("#3399FF",
                                 "#FF6666",
                                 "#9999FF",
                                 "#FF9933",
                                 "#99CC99"))
  
}

# pos_figure_miss_percent, save as 1024 by 700 (reshape2)
tbl_miss_perc <- read_rds(path = "./4_results/posture_miss_perc.rds")
{
  
  # read in only classifications
  graph <- tbl_miss_perc[ , -which(colnames(tbl_miss_perc) %in% c("AP_Total"))]
  
  # rename IMG to correct
  colnames(graph)[2] <- "Correct"
  
  # change table into variables that represent x, y, other
  graph <- graph %>% 
    melt(id.vars = "Posture")
  
  # clean
  colnames(graph)[2] <- "Classification"
  
  graph <- graph[graph$value != 0, ]
  
  graph$Posture <- factor(graph$Posture,
                          levels = c("Sit",
                                     "Stand",
                                     "Move"))
  
  # label
  lbl <- tbl_miss_perc
  lbl$label <- lbl$AP_Total/lbl$AP_Total*100
  
  # color
  brewer.pal(9, name = "PuBuGn")
  brewer.pal(5, name = "Greys")
  brewer.pal(5, name = "Set3")
  brewer.pal(5, name = "Spectral")
  # old color layout
  c("#3399FF",
    "#FF6666",
    "#9999FF",
    "#FF9933",
    "#99CC99")
  
  graph_colors <- brewer.pal(9, name = "PuBuGn")[c(1, 9, 5, 7, 3)]
  graph_colors <- brewer.pal(9, name = "PuBuGn")[c(1, 3, 5, 7, 9)]
  
  
  # need to calculate actual position of labels
  graph_pos <- graph[order(graph$Posture, graph$Classification, decreasing = TRUE), ]
  
  graph_pos <- group_by(graph_pos,Posture) %>%
    mutate(pos = cumsum(value) - (0.5 * value))
  
  # plot - B&W ----
  
  ggplot(data = graph) +
    geom_bar_pattern(
      mapping = aes(x = Posture,
                    y = value,
                    pattern = Classification,
                    pattern_alpha = Classification,
                    pattern_shape = Classification,
                    pattern_angle = Classification,
                    pattern_density = Classification
      ),
      color = "black",
      fill = "white",
      size = 1,
      stat = "identity",
      pattern_fill = "black",
      pattern_color = "black",
      pattern_spacing = 0.01
    ) +
    scale_pattern_manual(
      values = c("stripe",
                 "crosshatch",
                 "circle",
                 "stripe",
                 "stripe")
      # values = c("stripe",
      #            "crosshatch",
      #            "circle",
      #            "stripe",
      #            "stripe")
    ) +
    scale_pattern_angle_manual(
      values = c(0,
                 45,
                 45,
                 0,
                 90)
    ) +
    scale_pattern_alpha_manual(
      values = c(0, 1, 1, 1, 1)
    ) +
    scale_pattern_density_manual(
      values = c(0.2,
                 1.0,
                 0.2,
                 0.2,
                 0.2)
    ) +
    geom_text(
      data = lbl,
      mapping = aes(x = Posture,
                    y = label,
                    label = paste(AP_Total, "mins")),
      # fontface = "bold",
      vjust = -0.5
    ) +
    geom_label(
      data = graph_pos[!(graph_pos$Posture == "Sit" & graph_pos$Classification != "Correct"), ],
      mapping = aes(x = Posture,
                    y = pos,
                    label = paste0(value, "%")),
      fill = "white",
      label.size = 1,
      show.legend = FALSE
    ) +
    geom_label_repel(
      data = graph_pos[graph_pos$Posture == "Sit" & graph_pos$Classification != "Correct", ],
      mapping = aes(x = Posture,
                    y = pos,
                    label = paste0(value, "%")),
      fill = "white",
      box.padding = 0.25, # def = 0.25
      label.padding = 0.4, # def = 0.25
      point.padding = 0, # def
      label.r = 0.15, # def = 0.15
      label.size = 1, # def = 0.5
      segment.color = "black", # def
      segment.size = 2.0, # def = 0.5
      segment.alpha = 1.0, # def
      # arrow = arrow(angle = 30,
      #               length = unit(0.03,
      #                             "npc"),
      #               type = "closed",
      #               ends = "first"),
      force = 1,
      max.iter = 2000,
      # nudge_x = 0,
      # nudge_y = 0,
      xlim = c(0.6, 1.4),
      ylim = c(16, NA),
      show.legend = FALSE,
      direction = "x"
    ) +
    geom_point(
      data = graph_pos[graph_pos$Posture == "Sit" & graph_pos$Classification != "Correct", ],
      mapping = aes(x = Posture,
                    y = pos),
      size = 3,
      shape = 21,
      fill = "white",
      color = "black",
      show.legend = FALSE
    ) +
    scale_y_continuous(
      name = "% of Total activPAL Estimates",
      breaks = waiver(),
      labels = waiver(),
      limits = NULL,
      expand = expansion(mult = c(0.05, .05)) # for cont, mult 5% = default
    ) +
    scale_x_discrete(
      name = waiver(),
      breaks = waiver(),
      labels = waiver(),
      limits = NULL,
      expand = expansion(add = c(0.54, 0.54)) # for disc, add 0.6 units = default
    ) +
    labs(
      title = "Proportion of Total activPAL estimates classified by Images",
      x = "Posture",
      y = "% of Total activPAL Estimates"
    ) +
    # scale_fill_manual(
    #   values = c("white",
    #              "white",
    #              "white",
    #              "white",
    #              "white")
    # ) +
    theme(
      line = element_line(
        color = "black", # def = black
        size = 1, # def = 0.5
        linetype = NULL, # def = solid
        lineend = NULL, # def = square
        arrow = NULL, # def = none
        inherit.blank = FALSE
      ),
      rect = element_rect(
        fill = NULL, # def varies
        color = NULL, # ???
        size = NULL, # border size
        linetype = NULL,
        inherit.blank = FALSE
      ),
      text = element_text(
        family = NULL,
        face = NULL,
        color = "black",
        size = 15,
        hjust = NULL,
        vjust = NULL,
        angle = NULL,
        lineheight = NULL,
        margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        debug = FALSE,
        inherit.blank = FALSE
      ),
      title = element_text(
        face = "bold",
        debug = TRUE
      ),
      plot.title = element_text(face = "bold",
                                hjust = 0.5),
      # axis.title.x.bottom = element_text(hjust = 0.50),
      
      axis.ticks.length.y.left = unit(-.5, units = "cm"), # -.8 w/ .6
      axis.ticks.length.x.bottom = unit(0, units = "cm"),
      
      axis.ticks.y.left = element_line(color = "black"),
      
      axis.text.y.left = element_text(color = "black",
                                      margin = margin(r = 0.6, unit = "cm")),
      axis.text.x.bottom = element_text(color = "black",
                                        margin = margin(t = -0.45, unit = "cm")),
      
      
      panel.background = element_rect(fill = "White", # def = "grey92"
                                      color = NA)
      # panel.grid.major.y = element_line(color = "black",
      #                                   size = 1, # 0.5 = default
      #                                   linetype = "solid",
      #                                   lineend = "square",
      #                                   arrow = NULL,
      #                                   inherit.blank = FALSE),
    )
  
  
  
  
  # plot - color w/ white labels ----
  
  ggplot(data = graph) +
    geom_bar(
      mapping = aes(x = Posture,
                    y = value,
                    fill = Classification),
      color = "black",
      size = 1,
      stat = "identity"
    ) +
    geom_text(
      data = lbl,
      mapping = aes(x = Posture,
                    y = label,
                    label = paste(AP_Total, "mins")),
      vjust = -0.5
    ) +
    geom_label(
      data = graph_pos[!(graph_pos$Posture == "Sit" & graph_pos$Classification != "Correct"), ],
      mapping = aes(x = Posture,
                    y = pos,
                    # fill = Classification,
                    label = paste0(value, "%")),
      # position = position_stack(vjust = 0.5),
      
      label.size = 1,
      show.legend = FALSE
    ) +
    geom_label_repel(
      data = graph_pos[graph_pos$Posture == "Sit" & graph_pos$Classification != "Correct", ],
      mapping = aes(x = Posture,
                    y = pos, # value if you want color in label
                    # fill = Classification,
                    label = paste0(value, "%")),
      # position = position_stack(vjust = 0.5),
      box.padding = 0.25, # def = 0.25
      label.padding = 0.4, # def = 0.25
      point.padding = 0, # def
      label.r = 0.15, # def = 0.15
      label.size = 1, # def = 0.5
      segment.color = "black", # def
      segment.size = 2.0, # def = 0.5
      segment.alpha = 1.0, # def
      # arrow = arrow(angle = 30,
      #               length = unit(0.03,
      #                             "npc"),
      #               type = "closed",
      #               ends = "first"),
      force = 1,
      max.iter = 2000,
      # nudge_x = 0,
      # nudge_y = 0,
      xlim = c(0.6, 1.4),
      ylim = c(16, NA),
      show.legend = FALSE,
      direction = "x"
    ) +
    geom_point(
      data = graph_pos[graph_pos$Posture == "Sit" & graph_pos$Classification != "Correct", ],
      mapping = aes(
        x = Posture,
        y = pos #value if you want color in label
        # fill = Classification
      ),
      # position = position_stack(vjust = 0.5),
      size = 3,
      shape = 21,
      fill = "white",
      color = "black",
      show.legend = FALSE
    ) +
    scale_y_continuous(
      name = "% of Total activPAL Estimates",
      breaks = waiver(),
      labels = waiver(),
      limits = NULL,
      expand = expansion(mult = c(0.05, 0.05)) # for cont, mult 5% = default
    ) +
    scale_x_discrete(
      name = waiver(),
      breaks = waiver(),
      labels = waiver(),
      limits = NULL,
      expand = expansion(add = c(0.54, 0.54)) # for disc, add 0.6 units = default
    ) +
    labs(
      title = "Proportion of Total activPAL estimates classified by Images",
      x = "Posture",
      y = "% of Total activPAL Estimates"
    ) +
    theme(
      line = element_line(
        color = "black", # def = black
        size = 1, # def = 0.5
        linetype = NULL, # def = solid
        lineend = NULL, # def = square
        arrow = NULL, # def = none
        inherit.blank = FALSE
      ),
      rect = element_rect(
        fill = NULL, # def varies
        color = NULL, # ???
        size = NULL, # border size
        linetype = NULL,
        inherit.blank = FALSE
      ),
      text = element_text(
        family = NULL,
        face = NULL,
        color = "black",
        size = 15,
        hjust = NULL,
        vjust = NULL,
        angle = NULL,
        lineheight = NULL,
        margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        debug = FALSE,
        inherit.blank = FALSE
      ),
      title = element_text(
        face = "bold",
        debug = TRUE
      ),
      plot.title = element_text(face = "bold",
                                hjust = 0.5),
      
      axis.ticks.length.y.left = unit(-.5, units = "cm"), # -.8 w/ .6
      axis.ticks.length.x.bottom = unit(0, units = "cm"),
      
      axis.ticks.y.left = element_line(color = "black"),
      
      axis.text.y.left = element_text(color = "black",
                                      margin = margin(r = 0.6, unit = "cm")),
      axis.text.x.bottom = element_text(color = "black",
                                        margin = margin(t = -0.45, unit = "cm")),
      
      
      panel.background = element_rect(fill = "White", # def = "grey92"
                                      color = NA)
    ) +
    # scale_fill_brewer(
    #   palette = "Spectral",
    #   direction = -1
    # ) 
    scale_fill_manual(
      values = graph_colors
    ) 
    
  
  # plot - color w/ colored labels ----
  
  ggplot(data = graph) +
    geom_bar(
      mapping = aes(x = Posture,
                    y = value,
                    fill = Classification),
      color = "black",
      size = 1,
      stat = "identity"
    ) +
    geom_text(
      data = lbl,
      mapping = aes(x = Posture,
                    y = label,
                    label = paste(AP_Total, "mins")),
      vjust = -0.5
    ) +
    geom_label(
      data = graph_pos[!(graph_pos$Posture == "Sit" & graph_pos$Classification != "Correct"), ],
      mapping = aes(x = Posture,
                    y = value,
                    fill = Classification,
                    label = paste0(value, "%")),
      position = position_stack(vjust = 0.5),
      label.size = 1,
      show.legend = FALSE
    ) +
    geom_label_repel(
      data = graph_pos[graph_pos$Posture == "Sit" & graph_pos$Classification != "Correct", ],
      mapping = aes(x = Posture,
                    y = value, # value if you want color in label
                    fill = Classification,
                    label = paste0(value, "%")),
      position = position_stack(vjust = 0.5),
      box.padding = 0.25, # def = 0.25
      label.padding = 0.4, # def = 0.25
      point.padding = 0, # def
      label.r = 0.15, # def = 0.15
      label.size = 1, # def = 0.5
      segment.color = "black", # def
      segment.size = 2.0, # def = 0.5
      segment.alpha = 1.0, # def
      # arrow = arrow(angle = 30,
      #               length = unit(0.03,
      #                             "npc"),
      #               type = "closed",
      #               ends = "first"),
      force = 1,
      max.iter = 2000,
      # nudge_x = 0,
      # nudge_y = 0,
      xlim = c(0.6, 1.4),
      ylim = c(16, NA),
      show.legend = FALSE,
      direction = "x"
    ) +
    geom_point(
      data = graph_pos[graph_pos$Posture == "Sit" & graph_pos$Classification != "Correct", ],
      mapping = aes(
        x = Posture,
        y = value, #value if you want color in label
        fill = Classification
      ),
      position = position_stack(vjust = 0.5),
      show.legend = FALSE
    ) +
    scale_y_continuous(
      name = "% of Total activPAL Estimates",
      breaks = waiver(),
      labels = waiver(),
      limits = NULL,
      expand = expansion(mult = c(0.05, 0.05)) # for cont, mult 5% = default
    ) +
    scale_x_discrete(
      name = waiver(),
      breaks = waiver(),
      labels = waiver(),
      limits = NULL,
      expand = expansion(add = c(0.54, 0.54)) # for disc, add 0.6 units = default
    ) +
    labs(
      title = "Proportion of Total activPAL estimates classified by Images",
      x = "Posture",
      y = "% of Total activPAL Estimates"
    ) +
    scale_fill_manual(
      values = graph_colors
    ) +
    theme(
      line = element_line(
        color = "black", # def = black
        size = 1, # def = 0.5
        linetype = NULL, # def = solid
        lineend = NULL, # def = square
        arrow = NULL, # def = none
        inherit.blank = FALSE
      ),
      rect = element_rect(
        fill = NULL, # def varies
        color = NULL, # ???
        size = NULL, # border size
        linetype = NULL,
        inherit.blank = FALSE
      ),
      text = element_text(
        family = NULL,
        face = NULL,
        color = "black",
        size = 15,
        hjust = NULL,
        vjust = NULL,
        angle = NULL,
        lineheight = NULL,
        margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        debug = FALSE,
        inherit.blank = FALSE
      ),
      title = element_text(
        face = "bold",
        debug = TRUE
      ),
      plot.title = element_text(face = "bold",
                                hjust = 0.5),
      # axis.title.x.bottom = element_text(hjust = 0.50),
      
      axis.ticks.length.y.left = unit(-.5, units = "cm"), # -.8 w/ .6
      axis.ticks.length.x.bottom = unit(0, units = "cm"),
      
      axis.ticks.y.left = element_line(color = "black"),
      
      axis.text.y.left = element_text(color = "black",
                                      margin = margin(r = 0.6, unit = "cm")),
      axis.text.x.bottom = element_text(color = "black",
                                        margin = margin(t = -0.45, unit = "cm")),
      
      
      panel.background = element_rect(fill = "White", # def = "grey92"
                                      color = NA)
      # panel.grid.major.y = element_line(color = "black",
      #                                   size = 1, # 0.5 = default
      #                                   linetype = "solid",
      #                                   lineend = "square",
      #                                   arrow = NULL,
      #                                   inherit.blank = FALSE),
    )
  
  
  
  # plot - combined WINNER----
  ggplot(data = graph) +
    geom_bar_pattern(
      mapping = aes(x = Posture,
                    y = value,
                    fill = Classification,
                    pattern = Classification,
                    pattern_alpha = Classification,
                    pattern_shape = Classification,
                    pattern_angle = Classification,
                    pattern_density = Classification
      ),
      color = "black",
      size = 1,
      stat = "identity",
      pattern_fill = "black",
      pattern_color = "black",
      pattern_spacing = 0.01
    ) +
    scale_pattern_manual(
      # values = c("stripe",
      #            "crosshatch",
      #            "circle",
      #            "stripe",
      #            "stripe")
      values = c("stripe",
                 "circle",
                 "stripe",
                 "stripe",
                 "crosshatch")
    ) +
    scale_pattern_angle_manual(
      # values = c(0,
      #            45,
      #            45,
      #            0,
      #            90)
      values = c(0,
                 45,
                 0,
                 90,
                 45)
    ) +
    scale_pattern_alpha_manual(
      values = c(0, 
                 0.2, 
                 0.2, 
                 0.2, 
                 0.2)
    ) +
    scale_pattern_density_manual(
      values = c(0.2,
                 0.2,
                 0.2,
                 0.2,
                 0.2)
    ) +
    geom_text(
      data = lbl,
      mapping = aes(x = Posture,
                    y = label,
                    label = paste(AP_Total, "mins")),
      vjust = -0.5
    ) +
    geom_label(
      data = graph_pos[!(graph_pos$Posture == "Sit" & graph_pos$Classification != "Correct"), ],
      mapping = aes(x = Posture,
                    y = pos,
                    # fill = Classification,
                    label = paste0(value, "%")),
      # position = position_stack(vjust = 0.5),
      label.padding = unit(0.3, "lines"),
      label.size = 1,
      show.legend = FALSE
    ) +
    geom_label_repel(
      data = graph_pos[graph_pos$Posture == "Sit" & graph_pos$Classification != "Correct", ],
      mapping = aes(x = Posture,
                    y = pos, # value if you want color in label
                    # fill = Classification,
                    label = paste0(value, "%")),
      # position = position_stack(vjust = 0.5),
      box.padding = 0.25, # def = 0.25
      label.padding = 0.3, # def = 0.25
      point.padding = 0, # def
      label.r = 0.15, # def = 0.15
      label.size = 1, # def = 0.5
      segment.color = "black", # def
      segment.size = 2.0, # def = 0.5
      segment.alpha = 1.0, # def
      # arrow = arrow(angle = 30,
      #               length = unit(0.03,
      #                             "npc"),
      #               type = "closed",
      #               ends = "first"),
      force = 1,
      max.iter = 2000,
      # nudge_x = 0,
      # nudge_y = 0,
      xlim = c(0.55, 1.45),
      ylim = c(16, NA),
      show.legend = FALSE,
      direction = "x"
    ) +
    geom_point(
      data = graph_pos[graph_pos$Posture == "Sit" & graph_pos$Classification != "Correct", ],
      mapping = aes(
        x = Posture,
        y = pos #value if you want color in label
        # fill = Classification
      ),
      # position = position_stack(vjust = 0.5),
      size = 3,
      shape = 21,
      fill = "white",
      color = "black",
      show.legend = FALSE
    ) +
    scale_y_continuous(
      name = "% of Total activPAL Estimates",
      breaks = waiver(),
      labels = waiver(),
      limits = NULL,
      expand = expansion(mult = c(0.05, 0.05)) # for cont, mult 5% = default
    ) +
    scale_x_discrete(
      name = waiver(),
      breaks = waiver(),
      labels = waiver(),
      limits = NULL,
      expand = expansion(add = c(0.54, 0.54)) # for disc, add 0.6 units = default
    ) +
    labs(
      title = "Proportion of Total activPAL estimates classified by Images",
      x = "Posture",
      y = "% of Total activPAL Estimates"
    ) +
    theme(
      line = element_line(
        color = "black", # def = black
        size = 1, # def = 0.5
        linetype = NULL, # def = solid
        lineend = NULL, # def = square
        arrow = NULL, # def = none
        inherit.blank = FALSE
      ),
      rect = element_rect(
        fill = NULL, # def varies
        color = NULL, # ???
        size = NULL, # border size
        linetype = NULL,
        inherit.blank = FALSE
      ),
      text = element_text(
        family = NULL,
        face = NULL,
        color = "black",
        size = 17, #### CHANGED
        hjust = NULL,
        vjust = NULL,
        angle = NULL,
        lineheight = NULL,
        margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        debug = FALSE,
        inherit.blank = FALSE
      ),
      title = element_text(
        face = "bold",
        debug = FALSE
      ),
      plot.title = element_text(face = "bold",
                                hjust = 0.5),
      
      axis.ticks.length.y.left = unit(-.5, units = "cm"), # -.8 w/ .6
      axis.ticks.length.x.bottom = unit(0, units = "cm"),
      
      axis.ticks.y.left = element_line(color = "black"),
      
      axis.text.y.left = element_text(color = "black",
                                      margin = margin(r = 0.6, unit = "cm")),
      axis.text.x.bottom = element_text(color = "black",
                                        margin = margin(t = -0.45, unit = "cm")),
      
      
      panel.background = element_rect(fill = "White", # def = "grey92"
                                      color = NA)
    ) +
    scale_fill_manual(
      values = graph_colors
    )
  
  
}

ggsave(filename = "pos_fig_mis_percent2.jpeg",
       plot = last_plot(),
       device = "jpeg",
       path = "./5_figures/",
       scale = 1,
       width = 6.299,
       height = 4.011,
       units = "in",
       dpi = 600)

ggsave(filename = "pos_fig_mis_percent2.png",
       plot = last_plot(),
       device = png,
       path = "./5_figures/",
       scale = 1,
       width = 1024,
       height = 652,
       dpi = 600,
       limitsize = FALSE)

# Figures: Tables ---------------------------------------------------------

# pos_table_bias_minutes, save as 750 by 175 pixels
tbl_bias_time <- read_rds(path = "./4_results/posture_bias_time.rds")
{
  
  tbl <- tbl_bias_time
  
  # merge mean and sd columns, /u00b1 is plus/minus symbol
  tbl$AP <- paste(tbl$AP_mean,
                  tbl$AP_sd,
                  sep = " \u00b1 ")
  
  tbl$IMG <- paste(tbl$IMG_mean,
                   tbl$IMG_sd,
                   sep = " \u00b1 ")
  
  # clean
  tbl <- tbl[, c("Posture",
                 "IMG",
                 "AP",
                 "Bias",
                 "SE",
                 "Lower_95_Bias",
                 "Upper_95_Bias")]
  
  colnames(tbl) <- c("Posture",
                     "Image min \u00b1 SD",
                     "activPAL min \u00b1 SD",
                     "Bias",
                     "Standard Error",
                     "Lower 95% CI",
                     "Higher 95% CI")
  
  # format
  formattable(tbl,
              list(Posture = formatter("span",
                                       style = x ~ style(font.weight = "bold"))),
              align = c("l", "c", "c", "c", "c", "c", "c"))
  
}

# pos_table_bias_percent, save as 650 by 175 pixels
tbl_bias_perc <- read_rds(path = "./4_results/posture_bias_perc.rds")
{
  
  tbl <- tbl_bias_perc
  
  # merge mean and sd columns, /u00b1 is plus/minus symbol
  tbl$AP <- paste(tbl$AP_mean,
                  tbl$AP_sd,
                  sep = " \u00b1 ")
  
  tbl$IMG <- paste(tbl$IMG_mean,
                   tbl$IMG_sd,
                   sep = " \u00b1 ")
  
  # add percent symbol
  tbl$Bias <- paste0(tbl$Bias,
                     c("%", "%", "%"))
  
  tbl$SE <- paste0(tbl$SE,
                   c("%", "%", "%"))
  
  tbl$Lower_95_Bias <- paste0(tbl$Lower_95_Bias,
                              c("%", "%", "%"))
  
  tbl$Upper_95_Bias <- paste0(tbl$Upper_95_Bias,
                              c("%", "%", "%"))
  
  # clean
  tbl <- tbl[, c("Posture",
                 "AP",
                 "IMG",
                 "Bias",
                 "SE",
                 "Lower_95_Bias",
                 "Upper_95_Bias")]
  
  colnames(tbl) <- c("Posture",
                     "AP min \u00b1 SD",
                     "IMG min \u00b1 SD",
                     "% Bias",
                     "% SE",
                     "Lower 95% CI %",
                     "Higher 95% CI %")

  # format
  formattable(tbl,
              list("Posture" = formatter("span",
                                         style = x ~ style(font.weight = "bold"))),
              align = c("l", "c", "c", "c", "c", "c", "c"))
  
}

# pos_table_miss_minutes, save as 600 by 175 pixels
tbl_miss_time <- read_rds(path = "./4_results/posture_miss_time.rds")
{
  
  tbl <- tbl_miss_time
  
  # clean
  colnames(tbl) <- c("Posture",
                     "Total AP min",
                     "Correct",
                     "Transition",
                     "Sit",
                     "Stand",
                     "Move")
  
  # format
  formattable(tbl,
              list(Posture = formatter("span",
                                       style = x ~ style(font.weight = "bold"))),
              align = c("l", "c", "c", "c", "c", "c", "c"))
  
}

# pos_table_miss_percent, save as 600 by 175 pixels
tbl_miss_perc <- read_rds(path = "./4_results/posture_miss_perc.rds")
{
  
  tbl <- tbl_miss_perc
  
  # add percent symbol
  tbl$IMG <- paste0(tbl$IMG,
                    c("%", "%", "%"))
  
  tbl$Transition <- paste0(tbl$Transition,
                           c("%", "%", "%"))
  
  tbl$Sit <- paste0(tbl$Sit,
                    c("%", "%", "%"))
  
  tbl$Stand <- paste0(tbl$Stand,
                      c("%", "%", "%"))
  
  tbl$Move <- paste0(tbl$Move,
                     c("%", "%", "%"))
  
  # clean
  colnames(tbl) <- c("Posture",
                     "Total AP min",
                     "% Correct",
                     "% Transition",
                     "% Sit",
                     "% Stand",
                     "% Move")
  
  # format
  formattable(tbl,
              list("Posture" = formatter("span",
                                         style = x ~ style(font.weight = "bold"))),
              align = c("l", "c", "c", "c", "c", "c", "c"))
  
}



# sedentary vs upright ----------------------------------------------------

create_sedentary_tables()

# sed_table_bias_minutes, save as 650 by 145 pixels
tbl_bias_time <- read_rds(path = "./4_results/sedentary_bias_time.rds")
{
  
  tbl <- tbl_bias_time
  
  # merge mean and sd columns, /u00b1 is plus/minus symbol
  tbl$AP <- paste(tbl$AP_mean,
                  tbl$AP_sd,
                  sep = " \u00b1 ")
  
  tbl$IMG <- paste(tbl$IMG_mean,
                   tbl$IMG_sd,
                   sep = " \u00b1 ")
  
  # clean
  tbl <- tbl[, c("Posture",
                 "AP",
                 "IMG",
                 "Bias",
                 "SE",
                 "Lower_95_Bias",
                 "Upper_95_Bias")]
  
  colnames(tbl) <- c("Posture",
                     "AP min \u00b1 SD",
                     "IMG min \u00b1 SD",
                     "Bias",
                     "SE",
                     "Lower 95% CI",
                     "Higher 95% CI")
  
  # format
  formattable(tbl,
              list(Posture = formatter("span",
                                       style = x ~ style(font.weight = "bold"))),
              align = c("l", "c", "c", "c", "c", "c", "c"))
  
}

# sed_figure_miss_percent, save as 800 by 600 (reshape2)
tbl_miss_perc <- read_rds(path = "./4_results/sedentary_miss_perc.rds")
{
  
  # read in only classifications
  graph <- tbl_miss_perc[ , -which(colnames(tbl_miss_perc) %in% c("AP_Total"))]
  
  # rename IMG to correct
  colnames(graph)[2] <- "Correct"
  
  # change table into variables that represent x, y, other
  graph <- graph %>% 
    melt(id.vars = "Posture")
  
  # clean
  colnames(graph)[2] <- "Classification"
  
  graph <- graph[graph$value != 0, ]
  
  graph$Posture <- factor(graph$Posture,
                          levels = c("Sedentary",
                                     "Upright"))
  
  # label
  lbl <- tbl_miss_perc
  lbl$label <- lbl$AP_Total/lbl$AP_Total*100
  
  # plot
  ggplot(data = graph) +
    geom_bar(mapping = aes(x = Posture,
                           y = value,
                           fill = Classification),
             stat = "identity") +
    geom_text(data = lbl,
              mapping = aes(x = Posture,
                            y = label,
                            label = paste(AP_Total, "mins")),
              vjust = -0.5) +
    geom_text(mapping = aes(x = Posture,
                            y = value,
                            fill = Classification,
                            label = paste0(value, "%")),
              position = position_stack(vjust = 0.5)) +
    theme(plot.title = element_text(lineheight = 1,
                                    hjust = .5),
          text = element_text(size = 15)) +
    labs(title = "Proportion of Total AP estimates classified by IMGs",
         x = "Posture",
         y = "% of Total AP Estimates") +
    scale_fill_manual(values = c("#3399FF",
                                 "#FF6666",
                                 "#9999FF",
                                 "#FF9933",
                                 "#99CC99"))
  
}
