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

ems_plot <- function(data, parameter){
  ggplot2::ggplot(data = data, ggplot2::aes_string(x = "COLLECTION_START", y = "RESULT",
                                            group = "MONITORING_LOCATION",
                                            color = "MONITORING_LOCATION")) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 0.5) +
    ggplot2::scale_color_discrete("Sites") +
    ggplot2::xlab("Date") +
    ggplot2::ylab(parameter) +
    ggplot2::theme(legend.position = "bottom",
                   legend.direction = 'vertical')
}





