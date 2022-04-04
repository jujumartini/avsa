source("./2_scripts/1_functions.R")

# summary visit ------------------------------------------------------------

# # check to see which columns have NAs
# data_time <- read_csv(file = "./3_data/analysis/table_analysis_time.csv")
# data_percentage <- read_csv(file = "./3_data/analysis/table_analysis_percentage.csv")
# 
# # seeing which columns have NAs
# nacols <- function(df) {
#   colnames(df)[unlist(lapply(df, function(x) anyNA(x)))]
# }
# 
# nacols(data_time)
# nacols(data_percentage)

# create visit summary table
create_averages_table(
  fpa_processed = "./3_data/analysis",
  fpa_results = "./4_results",
  fnm_tbl_minutes = "table_processed_minutes.csv",
  fnm_tbl_percent = "table_processed_percentage.csv",
  fnm_tbl_summary = "table_averages.csv",
  fnm_rds_summary = "table_averages.rds"
)



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

create_bias_table(
  fpa_processed = "./3_data/analysis",
  fpa_results = "./4_results",
  fnm_tbl_minutes = "table_processed_minutes.csv",
  fnm_tbl_bias_minutes = "table_bias_minutes.csv",
  fnm_tbl_bias_percent = "table_bias_percent.csv",
  fnm_rds_bias_minutes = "table_bias_minutes.rds",
  fnm_rds_bias_percent = "table_bias_percent.rds"
)



# Confusion Matrix Table -------------------------------------------------------

create_confusion_table(
  fpa_processed = "./3_data/analysis",
  fpa_results = "./4_results",
  fnm_tbl_minutes = "table_processed_minutes.csv",
  fnm_tbl_upright = "table_upright_percentage.csv",
  fnm_tbl_cnfuse_minutes = "table_cnfuse_minutes.csv",
  fnm_tbl_cnfuse_percent = "table_cnfuse_percent.csv",
  fnm_tbl_cnfuse_upright = "table_cnfuse_upright.csv",
  fnm_rds_cnfuse_minutes = "table_cnfuse_minutes.rds",
  fnm_rds_cnfuse_percent = "table_cnfuse_percent.rds",
  fnm_rds_cnfuse_upright = "table_cnfuse_upright.rds"
)



# Bland-Altman Plot: AP as x-axis -----------------------------------------
# 1024x500 pixels, 92 dpi = 11.13x5.43in
tbl_minutes <- 
  vroom(
    file = "./3_data/analysis/table_processed_minutes.csv",
    delim = ",",
    progress = FALSE
  )
tbl_bias_minutes <-
  readr::read_rds(path = "./4_results/table_bias_minutes.rds")

# library(blandr)

# points: using ap as x-axis
gpt_bland_points <- 
  tibble(
    posture = c(rep("sit",
                    times = nrow(tbl_minutes)),
                rep("stand",
                    times = nrow(tbl_minutes)),
                rep("move",
                    times = nrow(tbl_minutes))),
    ap      = c(tbl_minutes$event_ap_sit,
                tbl_minutes$event_ap_sta,
                tbl_minutes$event_ap_mov),
    diff    = c(tbl_minutes$event_img_sit - tbl_minutes$event_ap_sit,
                tbl_minutes$event_img_sta - tbl_minutes$event_ap_sta,
                tbl_minutes$event_img_mov - tbl_minutes$event_ap_mov),
    .rows = nrow(tbl_minutes) * 3
  ) 

# gpt_bland_points$posture <- 
#   gpt_bland_points$posture %>% 
#   forcats::as_factor()

# lines
gpt_bland_lines <- 
  tbl_bias_minutes[, c("posture",
                       "bias",
                       "se")]
gpt_bland_lines$loa_lower <- 
  gpt_bland_lines$bias -
  2 * gpt_bland_lines$se
gpt_bland_lines$loa_upper <- 
  gpt_bland_lines$bias +
  2 * gpt_bland_lines$se
# gpt_bland_lines$posture <- 
#   gpt_bland_lines$posture %>% 
#   forcats::as_factor()

# text
gpt_bland_text <- 
  gpt_bland_lines[, !(colnames(gpt_bland_lines) == "se")]
# gpt_bland_text$position <- 
#   c(75, 55, 57)
# gpt_bland_text$text_upper <- 
#   paste("ULOA = ",
#         gpt_bland_text$loa_upper,
#         "minutes",
#         sep = " ")
# gpt_bland_text$text_bias <- 
#   paste("BIAS  =",
#         gpt_bland_text$bias,
#         "minutes",
#         sep = " ")
# gpt_bland_text$text_lower <- 
#   paste("LLOA =",
#         gpt_bland_text$loa_lower,
#         "minutes",
#         sep = " ")

gpt_bland_text$text_uloa_1 <- 
  rep("ULOA",
      times = 3)
gpt_bland_text$text_uloa_2 <- 
  rep("=",
      times = 3)
gpt_bland_text$text_uloa_3 <- 
  gpt_bland_text$loa_upper %>% 
  as.character()
gpt_bland_text$text_uloa_4 <- 
  rep("minutes",
      times = 3)
gpt_bland_text$text_bias_1 <- 
  rep("BIAS  ",
      times = 3)
gpt_bland_text$text_bias_2 <- 
  rep("=",
      times = 3)
gpt_bland_text$text_bias_3 <- 
  gpt_bland_text$bias %>% 
  as.character()
gpt_bland_text$text_bias_4 <- 
  rep("minutes",
      times = 3)
gpt_bland_text$text_lloa_1 <- 
  rep("LLOA",
      times = 3)
gpt_bland_text$text_lloa_2 <- 
  rep("=",
      times = 3)
gpt_bland_text$text_lloa_3 <- 
  gpt_bland_text$loa_lower %>% 
  as.character()
gpt_bland_text$text_lloa_4 <- 
  rep("minutes",
      times = 3)

gpt_bland_text <- 
  gpt_bland_text[, !(colnames(gpt_bland_text) %in% c("bias", "loa_lower", "loa_upper"))]

lgl_uloa_1 <- (
  gpt_bland_text$text_uloa_3 %>% 
    str_length()
) == 4
lgl_bias_1 <- (
  gpt_bland_text$text_bias_3 %>% 
    str_length()
) == 4
lgl_lloa_1 <- (
  gpt_bland_text$text_lloa_3 %>% 
    str_length()
) == 4
lgl_uloa_2 <- (
  gpt_bland_text$text_uloa_3 %>% 
    str_length()
) == 5
lgl_bias_2 <- (
  gpt_bland_text$text_bias_3 %>% 
    str_length()
) == 5
lgl_lloa_2 <- (
  gpt_bland_text$text_lloa_3 %>% 
    str_length()
) == 5

gpt_bland_text$text_uloa_3[lgl_uloa_1] <- 
  paste0("  ",
         gpt_bland_text$text_uloa_3[lgl_uloa_1])
gpt_bland_text$text_bias_3[lgl_bias_1] <- 
  paste0("  ",
         gpt_bland_text$text_bias_3[lgl_bias_1])
gpt_bland_text$text_lloa_3[lgl_lloa_1] <- 
  paste0("  ",
         gpt_bland_text$text_lloa_3[lgl_lloa_1])
gpt_bland_text$text_uloa_3[lgl_upper_2] <- 
  paste0(" ",
         gpt_bland_text$text_uloa_3[lgl_upper_2])
gpt_bland_text$text_bias_3[lgl_bias_2] <- 
  paste0(" ",
         gpt_bland_text$text_bias_3[lgl_bias_2])
gpt_bland_text$text_lloa_3[lgl_lloa_2] <- 
  paste0(" ",
         gpt_bland_text$text_lloa_3[lgl_lloa_2])

gpt_bland_text$text_uloa_3 <- 
  paste0(" ",
         gpt_bland_text$text_uloa_3)
gpt_bland_text$text_bias_3 <- 
  paste0(" ",
         gpt_bland_text$text_bias_3)
gpt_bland_text$text_lloa_3[3] <- 
  paste0(" ",
         gpt_bland_text$text_lloa_3[3])
gpt_bland_text$text_bias_3[c(1, 3)] <- 
  paste0(" ",
         gpt_bland_text$text_bias_3[c(1, 3)])
gpt_bland_text$text_uloa_3[2] <- 
  paste0(" ",
         gpt_bland_text$text_uloa_3[2])

test <- 
  gpt_bland_text %>% 
  melt(id.vars = "posture",
       value.name = "text")
test$posture <- 
  gpt_bland_text$posture %>% 
  str_to_title() %>% 
  forcats::as_factor()
test <- 
  test[order(test$posture),]
max_sit <- 
  gpt_bland_points$ap[gpt_bland_points$posture == "Sit"] %>% 
  max()
max_sta <- 
  gpt_bland_points$ap[gpt_bland_points$posture == "Stand"] %>% 
  max()
max_mov <- 
  gpt_bland_points$ap[gpt_bland_points$posture == "Move"] %>% 
  max()
test$x <- 
  c(
    rep(c((max_sit * 0.47), 
          (max_sit * 0.57),
          (max_sit * 0.67),
          (max_sit * 0.85)),
        times = 3),
    rep(c((max_sta * 0.47), 
          (max_sta * 0.57),
          (max_sta * 0.67),
          (max_sta * 0.85)),
        times = 3),
    rep(c((max_mov * 0.47), 
          (max_mov * 0.57),
          (max_mov * 0.67),
          (max_mov * 0.85)),
        times = 3)
  )
c(rep(c(53.5, 65, 75, 95),
      times = 3),
  rep(c(40, 48, 55, 69),
      times = 3),
  rep(c(41, 49, 57, 72),
      times = 3))
test$y <- 
  rep(c(38, 35, 32),
      each = 4)
# rep(c(80, 74, 68),
#     each = 4)
# test$color <- 
#   rep(c("#2171B5", "black", "#2171B5"),
#       each = 12)
gpt_bland_rect <- 
  tibble(
    posture = c("sit",
                "stand",
                "move"),
    xmin    = c((max_sit * 0.38),
                (max_sta * 0.38),
                (max_mov * 0.38)), # c(44.5, 33, 34)
    # c(47.5, 36, 37)
    # c(37, 27, 28)
    xmax    = c((max_sit * 0.95),
                (max_sta * 0.95),
                (max_mov * 0.95)
    ) # c(105.5, 77, 80)
    # c(102.5, 74, 77)
    # c(113, 83, 85)
  )
# gpt_bland_text$xmin <- 
#   c(44.5, 33, 34)
# c(47.5, 36, 37)
# c(37, 27, 28)
# gpt_bland_text$xmax <- 
#   c(105.5, 77, 80)
# c(102.5, 74, 77)
# c(113, 83, 85)


# Outliers
gpt_bland_outliers <- 
  gpt_bland_points[
    (
      gpt_bland_points$posture == "sit" &
        (gpt_bland_points$diff > 
           gpt_bland_lines$loa_upper[gpt_bland_lines$posture == "sit"])
    ) |
      (
        gpt_bland_points$posture == "stand" &
          (gpt_bland_points$diff > 
             gpt_bland_lines$loa_upper[gpt_bland_lines$posture == "stand"])
      ) |
      (
        gpt_bland_points$posture == "move" &
          (gpt_bland_points$diff > 
             gpt_bland_lines$loa_upper[gpt_bland_lines$posture == "move"])
      ) |
      (
        gpt_bland_points$posture == "sit" &
          (gpt_bland_points$diff < 
             gpt_bland_lines$loa_lower[gpt_bland_lines$posture == "sit"])
      ) |
      (
        gpt_bland_points$posture == "stand" &
          (gpt_bland_points$diff < 
             gpt_bland_lines$loa_lower[gpt_bland_lines$posture == "stand"])
      ) |
      (
        gpt_bland_points$posture == "move" &
          (gpt_bland_points$diff < 
             gpt_bland_lines$loa_lower[gpt_bland_lines$posture == "move"])
      ), ]

# Title Posture.
gpt_bland_points$posture <- 
  gpt_bland_points$posture %>% 
  str_to_title() %>% 
  forcats::as_factor()
gpt_bland_lines$posture <- 
  gpt_bland_lines$posture %>% 
  str_to_title() %>% 
  forcats::as_factor()
# gpt_bland_text$posture <- 
#   gpt_bland_text$posture %>% 
#   str_to_title() %>% 
#   forcats::as_factor()
gpt_bland_outliers$posture <- 
  gpt_bland_outliers$posture %>% 
  str_to_title() %>% 
  forcats::as_factor()
gpt_bland_rect$posture <- 
  gpt_bland_rect$posture %>% 
  str_to_title() %>% 
  forcats::as_factor()

# Final (after testing within plot)
gpt_bland_lines <- 
  gpt_bland_lines %>% 
  melt(id.vars = "posture",
       measure.vars = c("loa_upper","bias", "loa_lower"),
       value.name = "value")

# # Finding visits that have outliers
# paste(
#   tbl_minutes$id[tbl_minutes$event_ap_sit %in% 
#                    gpt_bland_outliers$ap[gpt_bland_outliers$posture == "Sit"]],
#   tbl_minutes$visit[tbl_minutes$event_ap_sit %in% 
#                       gpt_bland_outliers$ap[gpt_bland_outliers$posture == "Sit"]]
# )
# paste(
#   tbl_minutes$id[tbl_minutes$event_ap_sta %in% 
#                    gpt_bland_outliers$ap[gpt_bland_outliers$posture == "Stand"]],
#   tbl_minutes$visit[tbl_minutes$event_ap_sta %in% 
#                       gpt_bland_outliers$ap[gpt_bland_outliers$posture == "Stand"]]
# )
# paste(
#   tbl_minutes$id[tbl_minutes$event_ap_mov %in% 
#                    gpt_bland_outliers$ap[gpt_bland_outliers$posture == "Move"]],
#   tbl_minutes$visit[tbl_minutes$event_ap_mov %in%
#                       gpt_bland_outliers$ap[gpt_bland_outliers$posture == "Move"]]
# )
# test <- 
#   tbl_minutes[
#     tbl_minutes$event_ap_sit %in% 
#       gpt_bland_outliers$ap[gpt_bland_outliers$posture == "Sit"] |
#       tbl_minutes$event_ap_sta %in% 
#       gpt_bland_outliers$ap[gpt_bland_outliers$posture == "Stand"] |
#       tbl_minutes$event_ap_mov %in%
#       gpt_bland_outliers$ap[gpt_bland_outliers$posture == "Move"], c(1,2,14:19)
#     ]

# PLOT
ggplot(
  data = gpt_bland_points
) +
  geom_point(
    mapping = aes(x = ap,
                  y = diff)
  ) +
  # geom_point(
  #   data = gpt_bland_outliers,
  #   mapping = aes(x = ap,
  #                 y = diff),
  #   color = "#DE2D26"
  # ) +
  # geom_text_repel(
  #   data = gpt_bland_outliers,
  #   mapping = aes(x = ap,
  #                 y = diff,
  #                 label = round(diff, digits = 2)),
  #   direction = "both"
  # ) +
  facet_wrap(
    facets = vars(posture),
    scales = "free_x"
  ) +
  scale_y_continuous(
    breaks = c(-30, -15, 0, 15, 30),
    # c(-75, -60, -45, -30, -15, 0, 15, 30, 45, 60, 75)
    limits = c(-40, 40) # use max and min on gpt_bland_points$diff 
    # c(-71.06667, 83.7) c(-34.28333, 31.91667)
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "solid",
    alpha = 0.2
  ) +
  geom_hline( # works 
    data     = gpt_bland_lines,
    mapping  = aes(yintercept = value),
    color    = rep(c("#2171B5", "black", "#2171B5"), each = 3),
    size     = 1, # mm, def = 0.5
    linetype = rep(c("dashed", "dashed", "dashed"), each = 3)
  ) +
  # geom_label(
  #   data     = gpt_bland_text,
  #   mapping  = aes(label = )
  # ) +
  geom_text(
    data = test,
    mapping = aes(x = x,
                  y = y,
                  label = text),
    color = rep(rep(c("#2171B5", "black", "#2171B5"),
                    each = 4),
                times = 3),
    size     = 11 / .pt, # mm, 1pt = 0.35mm, 11pt standard
    family   = "sans",
    fontface = "plain"
  ) +
  # geom_text(
  #   data     = gpt_bland_text,
  #   mapping  = aes(x = position,
  #                  label = text_upper),
  #   y = 80,
  #   color    = "#2171B5",
  #   size     = 11 / .pt, # mm, 1pt = 0.35mm, 11pt standard
  #   family   = "sans",
  #   fontface = "plain"
  # ) +
  # geom_text(
  #   data     = gpt_bland_text,
  #   mapping  = aes(x = position,
  #                  label = text_bias),
  #   y = 74,
  #   color    = "black",
  #   size     = 11 / .pt, # mm, 1pt = 0.35mm, 11pt standard
  #   family   = "sans",
  #   fontface = "plain"
  # ) +
  # geom_text(
  #   data     = gpt_bland_text,
  #   mapping  = aes(x = position,
  #                  label = text_lower),
  #   y = 68,
  #   color    = "#2171B5",
  #   size     = 11 / .pt, # mm, 1pt = 0.35mm, 11pt standard
  #   family   = "sans",
  #   fontface = "plain"
  # ) +
  geom_rect(
    data = gpt_bland_rect,
    mapping = aes(xmin = xmin,
                  xmax = xmax),
    ymin = 30, # 64 before
    ymax = 40, # 84 before
    color = "black",
    size = 0.5,
    fill = NA
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
      size = 11, # pt, def = 11
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
    
    axis.ticks.length.y.left   = unit(-1.5, units = "mm"),
    axis.ticks.length.x.bottom = unit(-1.5, units = "mm"),
    
    axis.ticks.y.left = element_line(color = "black"),
    
    axis.text.y.left   = element_text(
      color = "black",
      margin = margin(r = 2, unit = "mm")
    ),
    axis.text.x.bottom = element_text(
      color = "black",
      margin = margin(t = 2, unit = "mm")
    ),
    panel.background = element_rect(fill = "White", # def = "grey92"
                                    color = "black",
                                    size = 1),
    panel.grid       = element_line(color = NA),
    # plot.background = 
    strip.text       = element_text(face = "bold",
                                    size = 11),
    strip.background = element_rect(fill = NA,
                                    color = NA)
  ) +
  labs(
    title = waiver(),
    x = "activPAL Estimates (Minutes)",
    y = "Difference in Minutes\n(Image - activPAL)"
  )
╟ggsave("test",
       plot = last_plot(),
       device = "pdf",
       path = getwd(),
       width = "11.13",
       height = "5.43",
       units = "in")
# 
# outliers
# lst_diff_upper <- 
#   list(
#     gpt_bland_points$diff[gpt_bland_points$posture == "sit" &
#                             (gpt_bland_points$diff > 
#                                gpt_bland_text$loa_upper[1])],
#     gpt_bland_points$diff[gpt_bland_points$posture == "stand" &
#                             (gpt_bland_points$diff > 
#                                gpt_bland_text$loa_upper[2])],
#     gpt_bland_points$diff[gpt_bland_points$posture == "move" &
#                             (gpt_bland_points$diff > 
#                                gpt_bland_text$loa_upper[3])]
#   )
# lst_diff_lower <- 
#   list(
#     gpt_bland_points$diff[gpt_bland_points$posture == "sit" &
#                             (gpt_bland_points$diff < 
#                                gpt_bland_text$loa_lower[1])],
#     gpt_bland_points$diff[gpt_bland_points$posture == "stand" &
#                             (gpt_bland_points$diff < 
#                                gpt_bland_text$loa_lower[2])],
#     gpt_bland_points$diff[gpt_bland_points$posture == "move" &
#                             (gpt_bland_points$diff < 
#                                gpt_bland_text$loa_lower[3])]
#   )
# lst_ap_upper <- 
#   list(
#     gpt_bland_points$ap[gpt_bland_points$posture == "sit" &
#                             (gpt_bland_points$diff > 
#                                gpt_bland_text$loa_upper[1])],
#     gpt_bland_points$ap[gpt_bland_points$posture == "stand" &
#                             (gpt_bland_points$diff > 
#                                gpt_bland_text$loa_upper[2])],
#     gpt_bland_points$ap[gpt_bland_points$posture == "move" &
#                             (gpt_bland_points$diff > 
#                                gpt_bland_text$loa_upper[3])]
#   )
# lst_ap_lower <- 
#   list(
#     gpt_bland_points$ap[gpt_bland_points$posture == "sit" &
#                             (gpt_bland_points$diff < 
#                                gpt_bland_text$loa_lower[1])],
#     gpt_bland_points$ap[gpt_bland_points$posture == "stand" &
#                             (gpt_bland_points$diff < 
#                                gpt_bland_text$loa_lower[2])],
#     gpt_bland_points$ap[gpt_bland_points$posture == "move" &
#                             (gpt_bland_points$diff < 
#                                gpt_bland_text$loa_lower[3])]
#   )
# times_upper <- 
#   sapply(lst_diff_upper, 
#          FUN = length)
# times_lower <- 
#   sapply(lst_diff_lower, 
#          FUN = length)
# gpt_bland_outliers <- 
#   tibble(
#     posture   = c(rep(c("sit", "stand", "move"),
#                       times = times_upper),
#                   rep(c("sit", "stand", "move"),
#                       times = times_lower)),
#     direction = c(rep("upper",
#                       times = sum(times_upper)),
#                   rep("lower",
#                       times = sum(times_lower))),
#     ap        = c(unlist(lst_ap_upper),
#                   unlist(lst_ap_lower)),
#     diff      = c(unlist(lst_diff_upper),
#                   unlist(lst_diff_lower)),
#     .rows = length(c(unlist(lst_diff_upper),
#                      unlist(lst_diff_lower)))
#   )
# gpt_bland_outliers$posture <- 
#   gpt_bland_outliers$posture %>% 
#   forcats::as_factor()
# x <- tbl_minutes$event_ap_sit
# y <- tbl_minutes$event_ap_sit - tbl_minutes$event_img_sit
# 
# blandr.display.and.draw(
#   method1 = tbl_minutes$event_ap_sit,
#   method2 = tbl_minutes$event_img_sit,
#   plotter = "ggplot"
# )
# 
# 
# 
# stat <- 
# blandr.statistics(
#   method1 = tbl_minutes$event_ap_sit,
#   method2 = tbl_minutes$event_img_sit,
#   sig.level = 0.95
# )
# 
# stat$differences
# stat$method1
# 
# df <- 
#   structure(
#     list(
#       Lightbox = c(84L, 67L, 80L, 63L, 76L, 66L, 79L, 81L, 77L, 82L,
#                    84L, 67L, 80L, 63L, 76L, 66L, 79L, 81L, 77L, 82L, 
#                    84L, 67L, 80L, 63L, 76L, 66L, 79L, 81L, 77L, 82L, 
#                    84L, 67L, 80L, 63L, 76L, 66L, 79L, 81L, 77L, 82L,
#                    84L, 67L, 80L, 63L, 76L, 66L, 79L, 81L, 77L, 82L),
#       variable = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
#                              2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
#                              3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
#                              4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L,
#                              5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L),
#                            .Label = c("S1", "S2", "S3", "S4", "S5"),
#                            class = "factor"),
#       value = c(82L, 65L, 73L, 50L, 50L, 50L, 72L, 56L, 76L, 78L,
#                 88L, 66L, 71L, 60L, 54L, 55L, 63L, 68L, 73L, 75L,
#                 73L, 65L, 76L, 57L, 51L, 57L, 75L, 65L, 69L, 66L, 
#                 77L, 67L, 79L, 58L, 55L, 56L, 77L, 66L, 73L, 80L, 
#                 78L, 62L, 78L, 52L, 63L, 59L, 71L, 64L, 69L, 89L),
#       mean  = c(83, 66, 76.5, 56.5, 63, 58, 75.5, 68.5, 76.5, 80,
#                 86, 66.5, 75.5, 61.5, 65, 60.5, 71, 74.5, 75, 78.5, 
#                 78.5, 66, 78, 60, 63.5, 61.5, 77, 73, 73, 74, 
#                 80.5, 67, 79.5, 60.5, 65.5, 61, 78, 73.5, 75, 81, 
#                 81, 64.5, 79, 57.5, 69.5, 62.5, 75, 72.5, 73, 85.5), 
#       diff  = c(2L, 2L, 7L, 13L, 26L, 16L, 7L, 25L, 1L, 4L,
#                 -4L, 1L, 9L, 3L, 22L, 11L, 16L, 13L, 4L, 7L,
#                 11L, 2L, 4L, 6L, 25L, 9L, 4L, 16L, 8L, 16L,
#                 7L, 0L, 1L, 5L, 21L, 10L, 2L, 15L, 4L, 2L,
#                 6L, 5L, 2L, 11L, 13L, 7L, 8L, 17L, 8L, -7L)
#     ),
#     .Names = c("Lightbox", "variable", 
#                "value", "mean", "diff"),
#     row.names = c(NA, -50L), class = "data.frame"
#   )
# 
# melt(df)
# geom_hline(data     = gpt_bland_lines,
#            mapping  = aes(yintercept = vars(upper_loa,
#                                             bias,
#                                             lower_loa)),
#            linetype = c(2, 1, 2),
#            color    = c("blue", "black", "blue")) +
#   geom_hline(data     = gpt_bland_lines,
#              mapping  = aes(yintercept = vars(upper_loa,
#                                               bias,
#                                               lower_loa)),
#              linetype = c(2, 1, 2),
#              color    = c("blue", "black", "blue")) 
#   # annotate(
#   geom = "rect",
#   xmin = 28, xmax = 85,
#   ymin = 66, ymax = 83,
#   color = "black",
#   size = 1,
#   fill = NA,
#   alpha = 1
# )
# test <- 
#   gpt_bland_lines %>% 
#   melt(id.vars = "posture",
#        measure.vars = c("loa_upper","bias", "loa_lower"))
# 
# ggplot(
#   data = gpt_bland_points[gpt_bland_points$posture == "sit", ]
# ) +
#   geom_point(
#     mapping = aes(x = ap,
#                   y = diff)
#   ) +
#   geom_hline( # works 
#     data     = gpt_bland_lines[gpt_bland_lines$posture == "sit", ],
#     mapping  = aes(yintercept = value),
#     color    = c("blue", "black", "blue"),
#     size     = 1, # mm, def = 0.5
#     linetype = c("dashed", "solid", "dashed")
#   ) +
#   geom_text(
#     data     = gpt_bland_text[gpt_bland_text$posture == "sit", ],
#     mapping  = aes(x = position,
#                    y = loa_upper,
#                    label = text_upper),
#     color    = "blue",
#     size     = 11 / .pt, # mm, 1pt = 0.35mm, 11pt standard
#     nudge_x  = 4,
#     nudge_y  = 4,
#     family   = "sans",
#     fontface = "plain"
#   )
  # geom_text(
  #   data     = gpt_bland_text,
  #   mapping  = aes(x = position,
  #                  y = loa_upper,
  #                  label = text_upper),
  #   color    = "blue",
  #   size     = 11 / .pt, # mm, 1pt = 0.35mm, 11pt standard
  #   nudge_x  = 4,
  #   nudge_y  = 4,
  #   family   = "sans",
  #   fontface = "plain"
  # )
# gpt_bland_text$text_upper <- 
#   paste("Upper level of agreement =",
#         gpt_bland_text$loa_upper,
#         "minutes",
#         sep = " ")
# gpt_bland_text$text_bias <- 
#   paste("Bias =",
#         gpt_bland_text$bias,
#         "minutes",
#         sep = " ")
# gpt_bland_text$text_lower <- 
#   paste("Lower level of agreement =",
#         gpt_bland_text$loa_lower,
#         "minutes",
#         sep = " ")
# c(
#   paste("Upper level of agreement =",
#         gpt_bland_text$value[gpt_bland_text$variable == "loa_upper"],
#         "minutes",
#         sep = " "),
#   paste("Bias =",
#         gpt_bland_text$value[gpt_bland_text$variable == "bias"],
#         "minutes",
#         sep = " "),
#   paste("Lower level of agreement =",
#         gpt_bland_text$value[gpt_bland_text$variable == "loa_lower"],
#         "minutes",
#         sep = " ")
# )
# gpt_bland_text <- 
#   gpt_bland_text[, !(colnames(gpt_bland_text) %in% c("se"))]


# gpt_bland_text %>% 
#   melt(id.vars = c("posture", "text_upper","text_bias", "text_lower"),
#        measure.vars = c("text_upper","text_bias", "text_lower"),
#        value.name = "position")
# geom_hline(data     = gpt_bland_lines,
#            mapping  = aes(yintercept = c(loa_upper,
#                                          bias,
#                                          loa_lower)),
#            linetype = c(2, 1, 2),
#            color    = c("blue", "black", "blue")) +

plt_bland
  ggplot(
    data = gpt_bland_points
    ) +
    geom_point(
      mapping = aes(x = ap,
                    y = diff)
    ) +
    geom_hline(
      data     = gpt_bland_lines,
      mapping  = aes(yintercept = loa_upper),
      color    = "blue",
      size     = 0.5, # mm
      linetype = "dashed"
    ) +
    geom_text(
      data     = gpt_bland_lines,
      mapping  = aes(x = 50,
                     y = loa_upper,
                     label = paste("Upper Level of Agreement =",
                                   loa_upper,
                                   "minutes",
                                   sep = " ")),
      size     = 11 / .pt, # mm, 1pt = 0.35mm, 11pt standard
      nudge_x  = 5,
      nudge_y  = 5,
      family   = "sans",
      fontface = "plain"
    ) +
    geom_hline(
      data     = gpt_bland_lines,
      mapping  = aes(yintercept = bias),
      color    = "black",
      size     = 0.5, # mm
      linetype = "solid"
    ) +
    geom_text(
      data     = gpt_bland_lines,
      mapping  = aes(x = 50,
                     y = bias,
                     label = paste("Bias =",
                                   bias,
                                   "minutes",
                                   sep = " ")),
      size     = 11 / .pt, # mm, 1pt = 0.35mm, 11pt standard
      nudge_y  = 5,
      family   = "sans",
      fontface = "plain"
    ) +
    geom_hline(
      data     = gpt_bland_lines,
      mapping  = aes(yintercept = loa_lower),
      color    = "blue",
      size     = 0.5, # mm
      linetype = "dashed"
    ) +
    geom_text(
      data     = gpt_bland_lines,
      mapping  = aes(x = 50,
                     y = loa_lower,
                     label = paste("Lower Level of Agreement =",
                                   loa_lower,
                                   "minutes",
                                   sep = " ")),
      size     = 11 / .pt, # mm, 1pt = 0.35mm, 11pt standard
      nudge_y  = 5,
      family   = "sans",
      fontface = "plain"
    ) +
    facet_wrap(
      facets = vars(posture)
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
        size = 11, # pt, def = 11
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
      
      axis.ticks.length.y.left = unit(-1.5, units = "mm"), # -.8 w/ .6
      axis.ticks.length.x.bottom = unit(-1.5, units = "mm"),

      axis.ticks.y.left = element_line(color = "black"),

      axis.text.y.left = element_text(
        color = "black",
        margin = margin(r = 2, unit = "mm")
      ),
      axis.text.x.bottom = element_text(
        color = "black",
        margin = margin(t = 2, unit = "mm")
      ),
      
      
      panel.background = element_rect(fill = "White", # def = "grey92"
                                      color = "black"),
      panel.grid = element_line(color = NA),
      # plot.background = 
      strip.text = element_text(size = 11)
    ) +
  labs(
    title = waiver(),
    x = "activPAL Estimates",
    y = "Difference\n(Annotation - activPAL)"
  )
  
    geom_hline(data     = gpt_bland_lines,
               mapping  = aes(yintercept = bias),
               linetype = 1,
               color    = "black") +
    geom_hline(data     = gpt_bland_lines,
               mapping  = aes(yintercept = lower_loa),
               linetype = 2,
               color    = "blue") +
    geom_text(data     = gpt_bland_lines,
              mapping  = aes(x = 50,
                             y = c(upper_loa,
                                   bias,
                                   lower_loa),
                             label = c(paste("Upper Level of Agreement", upper_loa,
                                       bias,
                                       lower_loa)))) +
    #           geom_hline(data     = gpt_bland_lines,
    #            mapping  = aes(yintercept = vars(upper_loa,
    #                                             bias,
    #                                             lower_loa)),
    #            linetype = c(2, 1, 2),
    #            color    = c("blue", "black", "blue")) +
    # geom_hline(data     = gpt_bland_lines,
    #            mapping  = aes(yintercept = vars(upper_loa,
    #                                             bias,
    #                                             lower_loa)),
    #            linetype = c(2, 1, 2),
    #            color    = c("blue", "black", "blue")) +

  facet_wrap(facets = vars(posture))
  

plot(
  tbl_minutes$event_ap_sit,
  (tbl_minutes$event_ap_sit - tbl_minutes$event_img_sit)
      )
plot(
  tbl_minutes$event_ap_sit,
  (tbl_minutes$event_img_sit - tbl_minutes$event_ap_sit)
)



(tbl_minutes$event_img_sit - tbl_minutes$event_ap_sit) %>% 
  mean()

plot(
  (tbl_minutes$event_ap_sit + tbl_minutes$event_img_sit) / 2,
  (tbl_minutes$event_ap_sit - tbl_minutes$event_img_sit)
)


abline(0, 1)


# Bland-Altman Plot: Mean as x-axis ---------------------------------------
# 1024x500 pixels, 92 dpi = 11.13x5.43in
tbl_minutes <- 
  vroom(
    file = "./3_data/analysis/table_processed_minutes.csv",
    delim = ",",
    progress = FALSE
  )
tbl_bias_minutes <-
  readr::read_rds(path = "./4_results/table_bias_minutes.rds")

# Points: using mean as x-axis
gpt_bland_points <- 
  tibble(
    posture = c(rep("sit",
                    times = nrow(tbl_minutes)),
                rep("stand",
                    times = nrow(tbl_minutes)),
                rep("move",
                    times = nrow(tbl_minutes))),
    ap      = c((tbl_minutes$event_ap_sit + tbl_minutes$event_img_sit) / 2,
                (tbl_minutes$event_ap_sta + tbl_minutes$event_img_sta) / 2,
                (tbl_minutes$event_ap_mov + tbl_minutes$event_img_mov) / 2),
    diff    = c(tbl_minutes$event_img_sit - tbl_minutes$event_ap_sit,
                tbl_minutes$event_img_sta - tbl_minutes$event_ap_sta,
                tbl_minutes$event_img_mov - tbl_minutes$event_ap_mov),
    .rows = nrow(tbl_minutes) * 3
  ) 

# gpt_bland_points$posture <- 
#   gpt_bland_points$posture %>% 
#   forcats::as_factor()

# lines
gpt_bland_lines <- 
  tbl_bias_minutes[, c("posture",
                       "bias",
                       "se")]
gpt_bland_lines$loa_lower <- 
  gpt_bland_lines$bias -
  2 * gpt_bland_lines$se
gpt_bland_lines$loa_upper <- 
  gpt_bland_lines$bias +
  2 * gpt_bland_lines$se
# gpt_bland_lines$posture <- 
#   gpt_bland_lines$posture %>% 
#   forcats::as_factor()

# text
gpt_bland_text <- 
  gpt_bland_lines
gpt_bland_text$position <- 
  c(75, 55, 57)
gpt_bland_text$text_upper <- 
  paste("ULOA = ",
        gpt_bland_text$loa_upper,
        "minutes",
        sep = " ")
gpt_bland_text$text_bias <- 
  paste("BIAS  =",
        gpt_bland_text$bias,
        "minutes",
        sep = " ")
gpt_bland_text$text_lower <- 
  paste("LLOA =",
        gpt_bland_text$loa_lower,
        "minutes",
        sep = " ")
gpt_bland_text$xmin <- 
  c(44.5, 33, 34)
c(47.5, 36, 37)
c(37, 27, 28)
gpt_bland_text$xmax <- 
  c(105.5, 77, 80)
c(102.5, 74, 77)
c(113, 83, 85)

# Outliers
gpt_bland_outliers <- 
  gpt_bland_points[
    (
      gpt_bland_points$posture == "sit" &
        (gpt_bland_points$diff > 
           gpt_bland_text$loa_upper[gpt_bland_text$posture == "sit"])
    ) |
      (
        gpt_bland_points$posture == "stand" &
          (gpt_bland_points$diff > 
             gpt_bland_text$loa_upper[gpt_bland_text$posture == "stand"])
      ) |
      (
        gpt_bland_points$posture == "move" &
          (gpt_bland_points$diff > 
             gpt_bland_text$loa_upper[gpt_bland_text$posture == "move"])
      ) |
      (
        gpt_bland_points$posture == "sit" &
          (gpt_bland_points$diff < 
             gpt_bland_text$loa_lower[gpt_bland_text$posture == "sit"])
      ) |
      (
        gpt_bland_points$posture == "stand" &
          (gpt_bland_points$diff < 
             gpt_bland_text$loa_lower[gpt_bland_text$posture == "stand"])
      ) |
      (
        gpt_bland_points$posture == "move" &
          (gpt_bland_points$diff < 
             gpt_bland_text$loa_lower[gpt_bland_text$posture == "move"])
      ), ]

# Title Posture.
gpt_bland_points$posture <- 
  gpt_bland_points$posture %>% 
  str_to_title() %>% 
  forcats::as_factor()
gpt_bland_lines$posture <- 
  gpt_bland_lines$posture %>% 
  str_to_title() %>% 
  forcats::as_factor()
gpt_bland_text$posture <- 
  gpt_bland_text$posture %>% 
  str_to_title() %>% 
  forcats::as_factor()
gpt_bland_outliers$posture <- 
  gpt_bland_outliers$posture %>% 
  str_to_title() %>% 
  forcats::as_factor()

# Final (after testing within plot)
gpt_bland_lines <- 
  gpt_bland_lines %>% 
  melt(id.vars = "posture",
       measure.vars = c("loa_upper","bias", "loa_lower"),
       value.name = "value")
gpt_bland_text$text_bias[1] <- 
  paste("BIAS  =   ",
        gpt_bland_text$bias[1],
        "minutes",
        sep = " ")
gpt_bland_text$text_bias[2] <- 
  paste("BIAS  =  ",
        gpt_bland_text$bias[2],
        "minutes",
        sep = " ")
gpt_bland_text$text_bias[3] <- 
  paste("BIAS  =   ",
        gpt_bland_text$bias[3],
        "minutes",
        sep = " ")
gpt_bland_text$text_lower[3] <- 
  paste("LLOA =",
        "-30.40",
        "minutes",
        sep = " ")



# PLOT
ggplot(
  data = gpt_bland_points
) +
  geom_point(
    mapping = aes(x = ap,
                  y = diff)
  ) +
  geom_point(
    data = gpt_bland_outliers,
    mapping = aes(x = ap,
                  y = diff),
    color = "#DE2D26"
  ) +
  geom_text_repel(
    data = gpt_bland_outliers,
    mapping = aes(x = ap,
                  y = diff,
                  label = round(diff, digits = 2)),
    direction = "x"
  ) +
  facet_wrap(
    facets = vars(posture),
    scales = "free_x"
  ) +
  scale_y_continuous(
    breaks = c(-75, -60, -45, -30, -15, 0, 15, 30, 45, 60, 75),
    limits = c(-71.06667, 83.7)
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "solid",
    alpha = 0.2
  ) +
  geom_hline( # works 
    data     = gpt_bland_lines,
    mapping  = aes(yintercept = value),
    color    = rep(c("#2171B5", "black", "#2171B5"), each = 3),
    size     = 1, # mm, def = 0.5
    linetype = rep(c("dashed", "dashed", "dashed"), each = 3)
  ) +
  # geom_label(
  #   data     = gpt_bland_text,
  #   mapping  = aes(label = )
  # ) +
  geom_text(
    data     = gpt_bland_text,
    mapping  = aes(x = position,
                   label = text_upper),
    y = 80,
    color    = "#2171B5",
    size     = 11 / .pt, # mm, 1pt = 0.35mm, 11pt standard
    family   = "sans",
    fontface = "plain"
  ) +
  geom_text(
    data     = gpt_bland_text,
    mapping  = aes(x = position,
                   label = text_bias),
    y = 74,
    color    = "black",
    size     = 11 / .pt, # mm, 1pt = 0.35mm, 11pt standard
    family   = "sans",
    fontface = "plain"
  ) +
  geom_text(
    data     = gpt_bland_text,
    mapping  = aes(x = position,
                   label = text_lower),
    y = 68,
    color    = "#2171B5",
    size     = 11 / .pt, # mm, 1pt = 0.35mm, 11pt standard
    family   = "sans",
    fontface = "plain"
  ) +
  geom_rect(
    data = gpt_bland_text,
    mapping = aes(xmin = xmin,
                  xmax = xmax),
    ymin = 64,
    ymax = 84,
    color = "black",
    size = 0.5,
    fill = NA
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
      size = 11, # pt, def = 11
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
    
    axis.ticks.length.y.left   = unit(-1.5, units = "mm"),
    axis.ticks.length.x.bottom = unit(-1.5, units = "mm"),
    
    axis.ticks.y.left = element_line(color = "black"),
    
    axis.text.y.left   = element_text(
      color = "black",
      margin = margin(r = 2, unit = "mm")
    ),
    axis.text.x.bottom = element_text(
      color = "black",
      margin = margin(t = 2, unit = "mm")
    ),
    panel.background = element_rect(fill = "White", # def = "grey92"
                                    color = "black",
                                    size = 1),
    panel.grid       = element_line(color = NA),
    # plot.background = 
    strip.text       = element_text(face = "bold",
                                    size = 11),
    strip.background = element_rect(fill = NA,
                                    color = NA)
  ) +
  labs(
    title = waiver(),
    x = "Mean",
    y = "Difference\n(Image - activPAL)"
  )
ggplot2::ggsave()
# 
# outliers
# lst_diff_upper <- 
#   list(
#     gpt_bland_points$diff[gpt_bland_points$posture == "sit" &
#                             (gpt_bland_points$diff > 
#                                gpt_bland_text$loa_upper[1])],
#     gpt_bland_points$diff[gpt_bland_points$posture == "stand" &
#                             (gpt_bland_points$diff > 
#                                gpt_bland_text$loa_upper[2])],
#     gpt_bland_points$diff[gpt_bland_points$posture == "move" &
#                             (gpt_bland_points$diff > 
#                                gpt_bland_text$loa_upper[3])]
#   )
# lst_diff_lower <- 
#   list(
#     gpt_bland_points$diff[gpt_bland_points$posture == "sit" &
#                             (gpt_bland_points$diff < 
#                                gpt_bland_text$loa_lower[1])],
#     gpt_bland_points$diff[gpt_bland_points$posture == "stand" &
#                             (gpt_bland_points$diff < 
#                                gpt_bland_text$loa_lower[2])],
#     gpt_bland_points$diff[gpt_bland_points$posture == "move" &
#                             (gpt_bland_points$diff < 
#                                gpt_bland_text$loa_lower[3])]
#   )
# lst_ap_upper <- 
#   list(
#     gpt_bland_points$ap[gpt_bland_points$posture == "sit" &
#                             (gpt_bland_points$diff > 
#                                gpt_bland_text$loa_upper[1])],
#     gpt_bland_points$ap[gpt_bland_points$posture == "stand" &
#                             (gpt_bland_points$diff > 
#                                gpt_bland_text$loa_upper[2])],
#     gpt_bland_points$ap[gpt_bland_points$posture == "move" &
#                             (gpt_bland_points$diff > 
#                                gpt_bland_text$loa_upper[3])]
#   )
# lst_ap_lower <- 
#   list(
#     gpt_bland_points$ap[gpt_bland_points$posture == "sit" &
#                             (gpt_bland_points$diff < 
#                                gpt_bland_text$loa_lower[1])],
#     gpt_bland_points$ap[gpt_bland_points$posture == "stand" &
#                             (gpt_bland_points$diff < 
#                                gpt_bland_text$loa_lower[2])],
#     gpt_bland_points$ap[gpt_bland_points$posture == "move" &
#                             (gpt_bland_points$diff < 
#                                gpt_bland_text$loa_lower[3])]
#   )
# times_upper <- 
#   sapply(lst_diff_upper, 
#          FUN = length)
# times_lower <- 
#   sapply(lst_diff_lower, 
#          FUN = length)
# gpt_bland_outliers <- 
#   tibble(
#     posture   = c(rep(c("sit", "stand", "move"),
#                       times = times_upper),
#                   rep(c("sit", "stand", "move"),
#                       times = times_lower)),
#     direction = c(rep("upper",
#                       times = sum(times_upper)),
#                   rep("lower",
#                       times = sum(times_lower))),
#     ap        = c(unlist(lst_ap_upper),
#                   unlist(lst_ap_lower)),
#     diff      = c(unlist(lst_diff_upper),
#                   unlist(lst_diff_lower)),
#     .rows = length(c(unlist(lst_diff_upper),
#                      unlist(lst_diff_lower)))
#   )
# gpt_bland_outliers$posture <- 
#   gpt_bland_outliers$posture %>% 
#   forcats::as_factor()
# x <- tbl_minutes$event_ap_sit
# y <- tbl_minutes$event_ap_sit - tbl_minutes$event_img_sit
# 
# blandr.display.and.draw(
#   method1 = tbl_minutes$event_ap_sit,
#   method2 = tbl_minutes$event_img_sit,
#   plotter = "ggplot"
# )
# 
# 
# 
# stat <- 
# blandr.statistics(
#   method1 = tbl_minutes$event_ap_sit,
#   method2 = tbl_minutes$event_img_sit,
#   sig.level = 0.95
# )
# 
# stat$differences
# stat$method1
# 
# df <- 
#   structure(
#     list(
#       Lightbox = c(84L, 67L, 80L, 63L, 76L, 66L, 79L, 81L, 77L, 82L,
#                    84L, 67L, 80L, 63L, 76L, 66L, 79L, 81L, 77L, 82L, 
#                    84L, 67L, 80L, 63L, 76L, 66L, 79L, 81L, 77L, 82L, 
#                    84L, 67L, 80L, 63L, 76L, 66L, 79L, 81L, 77L, 82L,
#                    84L, 67L, 80L, 63L, 76L, 66L, 79L, 81L, 77L, 82L),
#       variable = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
#                              2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
#                              3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
#                              4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L,
#                              5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L),
#                            .Label = c("S1", "S2", "S3", "S4", "S5"),
#                            class = "factor"),
#       value = c(82L, 65L, 73L, 50L, 50L, 50L, 72L, 56L, 76L, 78L,
#                 88L, 66L, 71L, 60L, 54L, 55L, 63L, 68L, 73L, 75L,
#                 73L, 65L, 76L, 57L, 51L, 57L, 75L, 65L, 69L, 66L, 
#                 77L, 67L, 79L, 58L, 55L, 56L, 77L, 66L, 73L, 80L, 
#                 78L, 62L, 78L, 52L, 63L, 59L, 71L, 64L, 69L, 89L),
#       mean  = c(83, 66, 76.5, 56.5, 63, 58, 75.5, 68.5, 76.5, 80,
#                 86, 66.5, 75.5, 61.5, 65, 60.5, 71, 74.5, 75, 78.5, 
#                 78.5, 66, 78, 60, 63.5, 61.5, 77, 73, 73, 74, 
#                 80.5, 67, 79.5, 60.5, 65.5, 61, 78, 73.5, 75, 81, 
#                 81, 64.5, 79, 57.5, 69.5, 62.5, 75, 72.5, 73, 85.5), 
#       diff  = c(2L, 2L, 7L, 13L, 26L, 16L, 7L, 25L, 1L, 4L,
#                 -4L, 1L, 9L, 3L, 22L, 11L, 16L, 13L, 4L, 7L,
#                 11L, 2L, 4L, 6L, 25L, 9L, 4L, 16L, 8L, 16L,
#                 7L, 0L, 1L, 5L, 21L, 10L, 2L, 15L, 4L, 2L,
#                 6L, 5L, 2L, 11L, 13L, 7L, 8L, 17L, 8L, -7L)
#     ),
#     .Names = c("Lightbox", "variable", 
#                "value", "mean", "diff"),
#     row.names = c(NA, -50L), class = "data.frame"
#   )
# 
# melt(df)
# geom_hline(data     = gpt_bland_lines,
#            mapping  = aes(yintercept = vars(upper_loa,
#                                             bias,
#                                             lower_loa)),
#            linetype = c(2, 1, 2),
#            color    = c("blue", "black", "blue")) +
#   geom_hline(data     = gpt_bland_lines,
#              mapping  = aes(yintercept = vars(upper_loa,
#                                               bias,
#                                               lower_loa)),
#              linetype = c(2, 1, 2),
#              color    = c("blue", "black", "blue")) 
#   # annotate(
#   geom = "rect",
#   xmin = 28, xmax = 85,
#   ymin = 66, ymax = 83,
#   color = "black",
#   size = 1,
#   fill = NA,
#   alpha = 1
# )
# test <- 
#   gpt_bland_lines %>% 
#   melt(id.vars = "posture",
#        measure.vars = c("loa_upper","bias", "loa_lower"))
# 
# ggplot(
#   data = gpt_bland_points[gpt_bland_points$posture == "sit", ]
# ) +
#   geom_point(
#     mapping = aes(x = ap,
#                   y = diff)
#   ) +
#   geom_hline( # works 
#     data     = gpt_bland_lines[gpt_bland_lines$posture == "sit", ],
#     mapping  = aes(yintercept = value),
#     color    = c("blue", "black", "blue"),
#     size     = 1, # mm, def = 0.5
#     linetype = c("dashed", "solid", "dashed")
#   ) +
#   geom_text(
#     data     = gpt_bland_text[gpt_bland_text$posture == "sit", ],
#     mapping  = aes(x = position,
#                    y = loa_upper,
#                    label = text_upper),
#     color    = "blue",
#     size     = 11 / .pt, # mm, 1pt = 0.35mm, 11pt standard
#     nudge_x  = 4,
#     nudge_y  = 4,
#     family   = "sans",
#     fontface = "plain"
#   )
# geom_text(
#   data     = gpt_bland_text,
#   mapping  = aes(x = position,
#                  y = loa_upper,
#                  label = text_upper),
#   color    = "blue",
#   size     = 11 / .pt, # mm, 1pt = 0.35mm, 11pt standard
#   nudge_x  = 4,
#   nudge_y  = 4,
#   family   = "sans",
#   fontface = "plain"
# )
# gpt_bland_text$text_upper <- 
#   paste("Upper level of agreement =",
#         gpt_bland_text$loa_upper,
#         "minutes",
#         sep = " ")
# gpt_bland_text$text_bias <- 
#   paste("Bias =",
#         gpt_bland_text$bias,
#         "minutes",
#         sep = " ")
# gpt_bland_text$text_lower <- 
#   paste("Lower level of agreement =",
#         gpt_bland_text$loa_lower,
#         "minutes",
#         sep = " ")
# c(
#   paste("Upper level of agreement =",
#         gpt_bland_text$value[gpt_bland_text$variable == "loa_upper"],
#         "minutes",
#         sep = " "),
#   paste("Bias =",
#         gpt_bland_text$value[gpt_bland_text$variable == "bias"],
#         "minutes",
#         sep = " "),
#   paste("Lower level of agreement =",
#         gpt_bland_text$value[gpt_bland_text$variable == "loa_lower"],
#         "minutes",
#         sep = " ")
# )
# gpt_bland_text <- 
#   gpt_bland_text[, !(colnames(gpt_bland_text) %in% c("se"))]


# gpt_bland_text %>% 
#   melt(id.vars = c("posture", "text_upper","text_bias", "text_lower"),
#        measure.vars = c("text_upper","text_bias", "text_lower"),
#        value.name = "position")
# geom_hline(data     = gpt_bland_lines,
#            mapping  = aes(yintercept = c(loa_upper,
#                                          bias,
#                                          loa_lower)),
#            linetype = c(2, 1, 2),
#            color    = c("blue", "black", "blue")) +

plt_bland
ggplot(
  data = gpt_bland_points
) +
  geom_point(
    mapping = aes(x = ap,
                  y = diff)
  ) +
  geom_hline(
    data     = gpt_bland_lines,
    mapping  = aes(yintercept = loa_upper),
    color    = "blue",
    size     = 0.5, # mm
    linetype = "dashed"
  ) +
  geom_text(
    data     = gpt_bland_lines,
    mapping  = aes(x = 50,
                   y = loa_upper,
                   label = paste("Upper Level of Agreement =",
                                 loa_upper,
                                 "minutes",
                                 sep = " ")),
    size     = 11 / .pt, # mm, 1pt = 0.35mm, 11pt standard
    nudge_x  = 5,
    nudge_y  = 5,
    family   = "sans",
    fontface = "plain"
  ) +
  geom_hline(
    data     = gpt_bland_lines,
    mapping  = aes(yintercept = bias),
    color    = "black",
    size     = 0.5, # mm
    linetype = "solid"
  ) +
  geom_text(
    data     = gpt_bland_lines,
    mapping  = aes(x = 50,
                   y = bias,
                   label = paste("Bias =",
                                 bias,
                                 "minutes",
                                 sep = " ")),
    size     = 11 / .pt, # mm, 1pt = 0.35mm, 11pt standard
    nudge_y  = 5,
    family   = "sans",
    fontface = "plain"
  ) +
  geom_hline(
    data     = gpt_bland_lines,
    mapping  = aes(yintercept = loa_lower),
    color    = "blue",
    size     = 0.5, # mm
    linetype = "dashed"
  ) +
  geom_text(
    data     = gpt_bland_lines,
    mapping  = aes(x = 50,
                   y = loa_lower,
                   label = paste("Lower Level of Agreement =",
                                 loa_lower,
                                 "minutes",
                                 sep = " ")),
    size     = 11 / .pt, # mm, 1pt = 0.35mm, 11pt standard
    nudge_y  = 5,
    family   = "sans",
    fontface = "plain"
  ) +
  facet_wrap(
    facets = vars(posture)
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
      size = 11, # pt, def = 11
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
    
    axis.ticks.length.y.left = unit(-1.5, units = "mm"), # -.8 w/ .6
    axis.ticks.length.x.bottom = unit(-1.5, units = "mm"),
    
    axis.ticks.y.left = element_line(color = "black"),
    
    axis.text.y.left = element_text(
      color = "black",
      margin = margin(r = 2, unit = "mm")
    ),
    axis.text.x.bottom = element_text(
      color = "black",
      margin = margin(t = 2, unit = "mm")
    ),
    
    
    panel.background = element_rect(fill = "White", # def = "grey92"
                                    color = "black"),
    panel.grid = element_line(color = NA),
    # plot.background = 
    strip.text = element_text(size = 11)
  ) +
  labs(
    title = waiver(),
    x = "activPAL Estimates",
    y = "Difference\n(Annotation - activPAL)"
  )



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
tbl_cnfuse_percent <- 
  readr::read_rds(path = "./4_results/table_cnfuse_percent.rds")
{

  # read in only classifications
  tib_graph <- 
    tbl_cnfuse_percent[ , c("posture",
                            "correct",
                            "sit",
                            "stand",
                            "move",
                            "gap",
                            "transition",
                            "unknown")] %>% 
    reshape2::melt(id.vars = "posture",
                   variable.name = "annotation",
                   value.name = "percentage")

  # clean
  colnames(tib_graph) <- 
    colnames(tib_graph) %>% 
    str_to_title()
  colnames(tib_graph)[1] <- "activPAL Posture"
  tib_graph$`activPAL Posture` <- 
    tib_graph$`activPAL Posture` %>% 
    str_to_title()
  tib_graph$Annotation <- 
    tib_graph$Annotation %>% 
    str_to_title()
  
  tib_graph <- tib_graph[tib_graph$Percentage != 0, ]

  tib_graph$Posture <- factor(tib_graph$Posture,
                              levels = c("Sit",
                                         "Stand",
                                         "Move"))
  tib_graph$`activPAL Posture` <- 
    tib_graph$`activPAL Posture` %>% 
  forcats::as_factor()

  classification = Annotation
  value = Percentage
  
  # Specific ggplot tibbles
  
  gpt_ap_total <- 
    tibble(
      `activPAL Posture` = str_to_title(tbl_cnfuse_percent$posture),
      ap_total = tbl_cnfuse_percent$ap_total,
      position = 100,
      .rows = 3
    )
  gpt_label <- 
    tib_graph[order(tib_graph$`activPAL Posture`,
                    tib_graph$Annotation,
                    decreasing = TRUE), ] %>% 
    group_by(`activPAL Posture`) %>%
    mutate(Position = cumsum(Percentage) - (0.5 * Percentage))
  ind_repel <- 
    which(
      gpt_label$`activPAL Posture` == "Sit" & 
        gpt_label$Annotation != "Correct"
    )
  gpt_label_normal <- 
    gpt_label[-ind_repel, ]
  gpt_label_repel <- 
    gpt_label[ind_repel, ]
  
  # # label
  # lbl <- tbl_cnfuse_percent
  # lbl$label <- lbl$ap_total/lbl$ap_total*100
  # 
  # # color
  # brewer.pal(9, name = "PuBuGn")
  # brewer.pal(5, name = "Greys")
  # brewer.pal(5, name = "Set3")
  # brewer.pal(5, name = "Spectral")
  # # old color layout
  # c("#3399FF",
  #   "#FF6666",
  #   "#9999FF",
  #   "#FF9933",
  #   "#99CC99")
  # 
  # graph_colors <- brewer.pal(9, name = "PuBuGn")[c(1, 9, 5, 7, 3)]
  # graph_colors <- brewer.pal(9, name = "PuBuGn")[c(1, 3, 5, 7, 9)]
  gpt_bar_color <- 
    brewer.pal(
      n = 7,
      name = "PuBuGn"
    )


  # need to calculate actual position of labels
  # tib_graph_pos <- tib_graph[order(tib_graph$`activPAL Posture`, tib_graph$Annotation, decreasing = TRUE), ]
  # 
  # tib_graph_pos <- 
  #   tib_graph_pos %>% 
  #   group_by(`activPAL Posture`) %>%
  #   mutate(pos = cumsum(Percentage) - (0.5 * Percentage))

  ggplot(data = tib_graph[tib_graph$`activPAL Posture` == "Move", -1]) +
    geom_bar_pattern(
      mapping = aes(x = "",
                    y = Percentage,
                    fill = Annotation,
                    pattern = Annotation,
                    pattern_alpha = Annotation,
                    pattern_shape = Annotation,
                    pattern_angle = Annotation,
                    pattern_density = Annotation
      ),
      width = 1,
      color = "black",
      # size = 1,
      stat = "identity"
      # pattern_fill = "black",
      # pattern_color = "black",
      # pattern_spacing = 0.01
    ) +
    coord_polar("y", start = 0)
  
  
  # plot - combined WINNER----
  ggplot(data = tib_graph) +
    geom_bar_pattern(
      mapping = aes(x = `activPAL Posture`,
                    y = Percentage,
                    fill = Annotation,
                    pattern = Annotation,
                    pattern_alpha = Annotation,
                    pattern_shape = Annotation,
                    pattern_angle = Annotation,
                    pattern_density = Annotation
      ),
      color = "black",
      size = 1,
      stat = "identity",
      pattern_fill = "black",
      pattern_color = "black",
      pattern_spacing = 0.01
    ) +
    # scale_pattern_manual(
    #   # values = c("stripe",
    #   #            "crosshatch",
    #   #            "circle",
    #   #            "stripe",
    #   #            "stripe")
    #   values = c("stripe",
    #              "circle",
    #              "stripe",
    #              "stripe",
    #              "crosshatch")
    # ) +
    # scale_pattern_angle_manual(
    #   # values = c(0,
    #   #            45,
    #   #            45,
    #   #            0,
    #   #            90)
    #   values = c(0,
    #              45,
    #              0,
    #              90,
    #              45)
    # ) +
    # scale_pattern_alpha_manual(
    #   values = c(0,
    #              0.2,
    #              0.2,
    #              0.2,
    #              0.2)
    # ) +
    # scale_pattern_density_manual(
    #   values = c(0.2,
    #              0.2,
    #              0.2,
    #              0.2,
    #              0.2)
    # ) +
    geom_text(
      data = lbl,
      mapping = aes(x = `activPAL Posture`,
                    y = label,
                    label = paste(AP_Total, "mins")),
      vjust = -0.5
    ) +
    geom_label(
      data = tib_graph_pos[!(tib_graph_pos$`activPAL Posture` == "Sit" & tib_graph_pos$Annotation != "Correct"), ],
      mapping = aes(x = `activPAL Posture`,
                    y = pos,
                    # fill = Annotation,
                    label = paste0(Percentage, "%")),
      # position = position_stack(vjust = 0.5),
      label.padding = unit(0.3, "lines"),
      label.size = 1,
      show.legend = FALSE
    ) +
    geom_label_repel(
      data = tib_graph_pos[tib_graph_pos$`activPAL Posture` == "Sit" & tib_graph_pos$Annotation != "Correct", ],
      mapping = aes(x = `activPAL Posture`,
                    y = pos, # Percentage if you want color in label
                    # fill = Annotation,
                    label = paste0(Percentage, "%")),
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
      data = tib_graph_pos[tib_graph_pos$`activPAL Posture` == "Sit" & tib_graph_pos$Annotation != "Correct", ],
      mapping = aes(
        x = `activPAL Posture`,
        y = pos #Percentage if you want color in label
        # fill = Annotation
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
      x = "`activPAL Posture`",
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
