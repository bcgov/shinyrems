# Copyright 2019 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

detected <- function (value, limit) {
  value > 0 & (is.na(limit) | value > limit)
}

# make_label <- function(data){
#   paste0(unique(data$Variable),
#          " (", unique(data$Units), ")")
# }
#
# make_labeller <- function(data){
#   ggplot2::as_labeller(setNames(make_label(data),
#                                 unique(data$Variable)))
# }

multiple_units <- function(data){
  length(unique(data$Units)) > 1
}

# plot_scaffold <- function(data){
#   data %<>% dplyr::mutate(Detected = detected(Value, DetectionLimit))
#   data$Detected %<>% factor(levels = c(TRUE, FALSE))
#
#   gp <- ggplot2::ggplot(data, ggplot2::aes(x = Date, y = Value)) +
#     ggplot2::scale_color_discrete(drop = FALSE) +
#     ggplot2::scale_alpha_discrete(range = c(1, 1/3), drop = FALSE) +
#     ggplot2::expand_limits(y = 0)
#
#   if(!multiple_units(data)){
#     gp <- gp +
#       ggplot2::facet_wrap(~EMS_ID,
#                  ncol = 1,
#                  scales = "free_y") +
#       ggplot2::ylab(unique(data$Units))
#   } else {
#     gp <- gp +
#       ggplot2::facet_grid(EMS_ID~Variable, ncol = 1,
#                  scales = "free_y") +
#       ggplot2::ylab(NULL) +
#       ggplot2::theme(strip.background = ggplot2::element_blank(),
#             strip.placement = "outside")
#   }
# }
#
# ems_scatter_plot <- function(data){
#   gp <- plot_scaffold(data)
#   gp + ggplot2::geom_point(size = 1, ggplot2::aes(color = Outlier,
#                                                   alpha = Detected))
# }
#
# ems_timeseries_plot <- function(data){
#   gp <- plot_scaffold(data)
#   gp + ggplot2::geom_point(size = 1, ggplot2::aes(color = Outlier,
#                                                   alpha = Detected)) +
#     ggplot2::geom_line(size = 0.3)
# }
#
# ems_boxplot <- function(data){
#   if(!multiple_units(data)){
#     gp <- ggplot2::ggplot(data) +
#       facet_wrap(~EMS_ID) +
#       ggplot2::geom_boxplot(ggplot2::aes(x = Variable, y = Value)) +
#       ggplot2::ylab(make_label(data))
#   } else {
#     gp <- ggplot2::ggplot(data) +
#       ggplot2::geom_boxplot(ggplot2::aes(x = factor(0), y = Value)) +
#       ggplot2::facet_wrap(EMS_ID~Variable, ncol = 1,
#                  scales = "free_y",
#                  strip.position = "left",
#                  labeller = make_labeller(data)) +
#       ggplot2::ylab(NULL) +
#       ggplot2::theme(strip.background = ggplot2::element_blank(),
#             strip.placement = "outside",
#             axis.title.x = ggplot2::element_blank(),
#             axis.text.x = ggplot2::element_blank(),
#             axis.ticks.x = ggplot2::element_blank())
#   }
#   gp
# }

ems_plots <- function(data, plot_type, date_range){
  lapply(unique(data$Units), function(x){
    dat <- data %>% dplyr::filter(Units == x)
    dat %<>% dplyr::mutate(Detected = detected(Value, DetectionLimit))
    dat$Detected %<>% factor(levels = c(TRUE, FALSE))

    gp <- ggplot2::ggplot(dat, ggplot2::aes(x = Date, y = Value)) +
      ggplot2::scale_color_discrete(drop = FALSE) +
      ggplot2::scale_alpha_discrete(range = c(1, 1/3), drop = FALSE) +
      ggplot2::expand_limits(y = 0) +
      ggplot2::facet_wrap(~EMS_ID, ncol = 1,
                          scales = "free_y") +
      ggplot2::ylab(unique(dat$Units)) +
      ggplot2::theme(legend.position = "bottom")

    if(plot_type == "scatter")
      return(gp + ggplot2::geom_point(size = 1, ggplot2::aes(alpha = Detected,
                                                             color = Variable)) +
               ggplot2::scale_x_date(limits = as.Date(date_range))
      )

    if(plot_type == "timeseries")
       return(gp + ggplot2::geom_point(size = 1, ggplot2::aes(alpha = Detected,
                                                              color = Variable)) +
              ggplot2::geom_line(size = 0.3, ggplot2::aes(color = Variable)) +
              ggplot2::scale_x_date(limits = as.Date(date_range))
       )

    gp + ggplot2::geom_boxplot(ggplot2::aes(x = Variable, y = Value))
  }) %>% setNames(unique(data$Units))
}

ems_summary_table <- function(data){
  summarise_wqdata(data)
}

download_plots <- function(plots, path){
  if(length(plots) > 1)
    return({
      for(i in unique(names(plots))){
        dir.create(i)
      }
      for(i in plots){
        ggplot2::ggsave(path, plot = i, device = "png",
                        width = get_width(), height = get_height(), dpi = get_dpi())
      }
      zip(path, names(plots))
      for(i in names(plots)){
        unlink(i, recursive = TRUE)
      }
    })
}

# multiplot <- function(plots = NULL, file, cols=1) {
#   library(grid)
#
#   numPlots = length(plots)
#   layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
#                    ncol = cols, nrow = ceiling(numPlots/cols))
#
#   if(numPlots==1) return(print(plots[[1]]))
#
#   grid.newpage()
#   pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
#
#   for (i in 1:numPlots) {
#     matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#     print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#                                     layout.pos.col = matchidx$col))
#   }
# }






