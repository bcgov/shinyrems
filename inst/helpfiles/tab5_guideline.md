### Determining Water Quality Guideline

There are two options:

#### Set manually
It might be helpful to visit the [Water Quality Guideline app](https://bcgov-env.shinyapps.io/bc_wqg/) to find a guideline for a particular variable.

#### Calculate from data

Calculates the approved "short" or "long"-term upper water quality thresholds for freshwater life in British Columbia. Only limits whose conditions are met are returned. Note to use the long-term limits on daily values set term = "long-daily".

If a limit depends on another variable such as pH, Total Chloride, or Total Hardness and no value was recorded for the date of interest then the pH, Total Chloride or Total Hardness value is assumed to be the average recorded value over the 30 day period. The one exception is if the 'Get modelled estimate' box is checked, in which case a parametric model is used to predict the pH, Total Chloride and Total Hardness for all dates with a value of any variable. Existing values are replaced. If, in every year, there are less then 12 pH/Total Chloride/Total Hardness then an average value is taken. Otherwise, if there is only one year with 12 or more values a simple seasonal smoother is used. If there is two years with 12 or more values then a seasonal smoother with a trend is fitted. Otherwise a model with trend and a dynamic seasonal component is fitted.

When considering long-term limits there must be at least 5 values spanning 21 days. As replicates are averaged prior to calculating the limits each of the 5 values must be on a separate day. The first 30 day period begin at the date of the first reading while the next 30 day period starts at the date of the first reading after the previous period and so on. The only exception to this is if the user provides dates in which case each period extends for 30 days or until a provided date is reached. It is important to note that the averaging of conditional variables, the 5 in 30 rule and the assignment of 30 day periods occurs independently for all combination of factor levels in the columns specified by by.
