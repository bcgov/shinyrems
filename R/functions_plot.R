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

multiple_units <- function(data){
  length(unique(data$Units)) > 1
}

ems_plots <- function(data, plot_type, geom, date_range,
                      point_size, line_size,
                      facet, colour){

  lapply(unique(data$Units), function(x){
    dat <- data %>% dplyr::filter(Units == x)
    dat %<>% dplyr::mutate(Detected = detected(Value, DetectionLimit))
    dat$Detected %<>% factor(levels = c(TRUE, FALSE))

    gp <- ggplot2::ggplot(dat, ggplot2::aes(x = Date, y = Value)) +
      ggplot2::scale_color_discrete(drop = FALSE) +
      ggplot2::expand_limits(y = 0) +
      ggplot2::facet_wrap(facet, ncol = 1,
                          scales = "free_y") +
      ggplot2::ylab(unique(dat$Units)) +
      ggplot2::theme(legend.position = "bottom")

    if(plot_type == "scatter"){
      if("show points" %in% geom){
        gp <- gp + ggplot2::geom_point(size = point_size,
                                       ggplot2::aes_string(shape = "Detected",
                                                    color = colour)) +
          ggplot2::scale_x_date(limits = as.Date(date_range))
      }
      if("show lines" %in% geom){
        gp <- gp + ggplot2::geom_line(size = line_size, ggplot2::aes_string(color = colour))
      }
    }

    if(plot_type == "boxplot"){
      gp + ggplot2::geom_boxplot(ggplot2::aes(x = Variable, y = Value))
    }
    gp
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

