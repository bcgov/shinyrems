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

make_label <- function(data){
  paste0(unique(data$Variable),
         " (", unique(data$Units), ")")
}

make_labeller <- function(data){
  ggplot2::as_labeller(setNames(make_label(data),
                                unique(data$Variable)))
}

multiple_units <- function(data){
  length(unique(data$Units)) > 1
}

plot_scaffold <- function(data){
  data %<>% dplyr::mutate(Detected = detected(Value, DetectionLimit))
  data$Detected %<>% factor(levels = c(TRUE, FALSE))
  data$Outlier %<>% factor(levels = c(TRUE, FALSE))

  gp <- ggplot2::ggplot(data, ggplot2::aes(x = Date, y = Value)) +
    ggplot2::scale_color_discrete(drop = FALSE) +
    ggplot2::scale_alpha_discrete(range = c(1, 1/3), drop = FALSE) +
    ggplot2::expand_limits(y = 0)

  if(!multiple_units(data)){
    gp <- gp +
      ggplot2::facet_wrap(~Variable,
                 ncol = 1,
                 scales = "free_y") +
      ggplot2::ylab(unique(data$Units))
  } else {
    gp <- gp +
      ggplot2::facet_wrap(~Variable, ncol = 1,
                 scales = "free_y",
                 strip.position = "left",
                 labeller = make_labeller(data)) +
      ggplot2::ylab(NULL) +
      ggplot2::theme(strip.background = ggplot2::element_blank(),
            strip.placement = "outside")
  }
}

ems_scatter_plot <- function(data){
  gp <- plot_scaffold(data)
  gp + ggplot2::geom_point(size = 1, ggplot2::aes(color = Outlier,
                                                  alpha = Detected))
}

ems_timeseries_plot <- function(data){
  gp <- plot_scaffold(data)
  gp + ggplot2::geom_point(size = 1, ggplot2::aes(color = Outlier,
                                                  alpha = Detected)) +
    ggplot2::geom_line(size = 0.3)
}

ems_boxplot <- function(data){
  if(!multiple_units(data)){
    gp <- ggplot2::ggplot(data) +
      ggplot2::geom_boxplot(ggplot2::aes(x = Variable, y = Value)) +
      ggplot2::ylab(make_label(data))
  } else {
    gp <- ggplot2::ggplot(data) +
      ggplot2::geom_boxplot(ggplot2::aes(x = factor(0), y = Value)) +
      ggplot2::facet_wrap(~Variable, ncol = 1,
                 scales = "free_y",
                 strip.position = "left",
                 labeller = make_labeller(data)) +
      ggplot2::ylab(NULL) +
      ggplot2::theme(strip.background = ggplot2::element_blank(),
            strip.placement = "outside",
            axis.title.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank())
  }
  gp
}





