# Copyright 2020 Province of British Columbia
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

detected <- function(value, limit) {
  value > 0 & (is.na(limit) | value > limit)
}

multiple_units <- function(data) {
  length(unique(data$Units)) > 1
}

### takes aggregated data with EMS_ID_Rename col
ems_plot_data <- function(data, date_range, timeframe){
  data$Detected <- detected(data$Value, data$DetectionLimit)
  # data$Station <- data$Site_Renamed
  data$Detected %<>% factor(levels = c(TRUE, FALSE))
  data <- data[data$Date >= as.Date(date_range[1]) & data$Date <= as.Date(date_range[2]), ]
  data$Timeframe <- factor(get_timeframe(data$Date, timeframe))
  if("UPPER_DEPTH" %in% names(data))
    data$UPPER_DEPTH %<>% as.factor()
  if("LOWER_DEPTH" %in% names(data))
    data$LOWER_DEPTH %<>% as.factor()
  data
}

ems_plot_base <- function(data, facet, ncol, scales){
  Date <- rlang::sym("Date")
  Value <- rlang::sym("Value")

  scales <- ifelse(scales, "fixed", "free")
  gp <- ggplot2::ggplot(data, ggplot2::aes(x = Date, y = Value)) +
    # ggplot2::scale_color_discrete(drop = FALSE) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::facet_wrap(facet,
                        ncol = ncol,
                        scales = scales
    ) +
    ggplot2::ylab(unique(data$Units)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
}

ems_plot_add_guideline <- function(gp, guideline){
  UpperLimit <- rlang::sym("UpperLimit")
  Date <- rlang::sym("Date")
  Guideline <- rlang::sym("Guideline")

  gp <- gp +
    ggplot2::geom_line(
      data = guideline,
      ggplot2::aes(x = Date, y = UpperLimit, linetype = Guideline),
      linewidth = 0.8,
      color = "black"
    ) +
    ggplot2::scale_linetype_manual(
      values=c("dashed", "dotted", "dotdash", "longdash", "twodash", "solid")
    )
  gp
}

ems_plot_add_geom <- function(gp, plot_type, geom,
                              point_size, line_size,
                              colour, timeframe, palette){
  Detected <- rlang::sym("Detected")
  colour <- rlang::sym(colour)

  if (plot_type == "scatter") {
    if ("show points" %in% geom) {
      gp <- gp + ggplot2::geom_point(
        size = point_size,
        ggplot2::aes(
          shape = Detected,
          color = !!colour
        )
      ) +
        ggplot2::scale_colour_brewer(palette = palette)
    }
    if ("show lines" %in% geom) {
      gp <- gp + ggplot2::geom_line(
        size = line_size,
        ggplot2::aes(color = !!colour)
      ) +
        ggplot2::scale_colour_brewer(palette = palette)
    }
  }

  if (plot_type == "boxplot") {
    Timeframe <- rlang::sym("Timeframe")
    Value <- rlang::sym("Value")

    gp <- gp + ggplot2::geom_boxplot(ggplot2::aes(
      x = Timeframe,
      y = Value,
      fill = !!colour
    )) +
      ggplot2::xlab(timeframe) +
      ggplot2::scale_fill_brewer(palette = palette)
  }
  gp
}

plot_outlier <- function(data, by, point_size, ncol) {
  Date <- rlang::sym("Date")
  Value <- rlang::sym("Value")
  Outlier <- rlang::sym("Outlier")
  Detected <- rlang::sym("Detected")

  data$Detected <- detected(data$Value, data$DetectionLimit)
  data$Detected %<>% factor(levels = c(TRUE, FALSE))
  data$Outlier %<>% factor(levels = c(TRUE, FALSE))
  gp <- ggplot2::ggplot(data, ggplot2::aes(
    x = Date,
    y = Value,
    color = Outlier,
    shape = Detected
  )) +
    ggplot2::geom_point(ggplot2::aes(),
      size = point_size
    ) +
    ggplot2::scale_color_discrete(drop = FALSE) +
    ggplot2::expand_limits(y = 0) +
    # ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")

  if ("Station" %in% by) {
    gp <- gp + ggplot2::facet_wrap("Station", scales = "free", ncol = ncol)
  } else if ("EMS_ID" %in% by) {
    gp <- gp + ggplot2::facet_wrap("EMS_ID", scales = "free", ncol = ncol)
  } else {
    gp <- gp + ggplot2::facet_wrap("Variable", scales = "free", ncol = ncol)
  }
  gp
}

get_timeframe <- function(date, x = "Year") {
  date <- dttr2::dtt_date(date)
  if (x == "Year") {
    return(dttr2::dtt_year(date))
  }
  if (x == "Year-Month") {
    ch <- format(sort(date), "%Y-%b")
    fac <- factor(ch, unique(ch))
    return(fac)
  }
  if (x == "Month") {
    return(factor(month.abb[dttr2::dtt_month(date)], levels = month.abb))
  }
  dttr2::dtt_season(date)
}
