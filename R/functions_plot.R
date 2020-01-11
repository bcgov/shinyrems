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
                      facet, colour, timeframe){

  lapply(unique(data$Units), function(x){
    dat <- data[data$Units == x,]
    dat$Detected <- detected(data$Value, data$DetectionLimit)
    dat$EMS_ID <- data$EMS_ID_Renamed
    dat$Detected %<>% factor(levels = c(TRUE, FALSE))
    dat <- dat[dat$Date >= as.Date(date_range[1]) & dat$Date <= as.Date(date_range[2]),]
    dat$Timeframe <- factor(get_timeframe(dat$Date, timeframe))

    gp <- ggplot2::ggplot(dat, ggplot2::aes_string(x = "Date", y = "Value")) +
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
                                                    color = colour))
      }
      if("show lines" %in% geom){
        gp <- gp + ggplot2::geom_line(size = line_size, ggplot2::aes_string(color = colour))
      }
    }

    if(plot_type == "boxplot"){
      gp <- gp + ggplot2::geom_boxplot(ggplot2::aes_string(x = "Timeframe",
                                                y = "Value",
                                                fill = colour)) +
        ggplot2::xlab(timeframe)
    }
    gp
   }) %>% setNames(unique(data$Units))
}

plot_outlier <- function(data, by, point_size){
  data$Detected <- detected(data$Value, data$DetectionLimit)
  data$Detected %<>% factor(levels = c(TRUE, FALSE))
  data$Outlier %<>% factor(levels = c(TRUE, FALSE))
  gp <- ggplot2::ggplot(data, ggplot2::aes_string(x = "Date",
                                                  y = "Value",
                                                  color = "Outlier",
                                                  shape = "Detected")) +
    ggplot2::geom_point(ggplot2::aes_string(),
                          size = point_size) +
    ggplot2::scale_color_discrete(drop = FALSE) +
    ggplot2::expand_limits(y = 0)

  if("EMS_ID" %in% by){
    if(length(unique(data$Variable)) == 1){
      gp <- gp + ggplot2::facet_grid(EMS_ID~Variable, scales = "free")
    } else {
      gp <- gp + ggplot2::facet_grid(Variable~EMS_ID, scales = "free")
    }
  } else {
    gp <- gp + ggplot2::facet_wrap(~Variable, scales = "free", ncol = 1)
  }
  gp
}

get_timeframe <- function(date, x = "Year"){
  date <- dttr2::dtt_date(date)
  if(x == "Year")
    return(dttr2::dtt_year(date))
  if(x == "Year-Month")
    return(substr(date, 1, 7))
  if(x == "Month")
    return(dttr2::dtt_month(date))
  dttr2::dtt_season(date)
}

ems_summary_table <- function(data){
  summarise_wqdata(data)
}

