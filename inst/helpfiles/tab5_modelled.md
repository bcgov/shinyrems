### Get modelled estimate

A limit may depend on another variable such as pH, Total Chloride, or Total Hardness and in some cases no value was recorded for the date of interest.

If the **'Get modelled estimate box' is unchecked**:  

The pH, Total Chloride or Total Hardness value is assumed to be the average recorded value over the 30 day period. 

If the **'Get modelled estimate box' is checked**:  

A parametric model is used to predict the pH, Total Chloride and Total Hardness for all dates with a value of any variable. Existing values are replaced. If, in every year, there are less then 12 pH/Total Chloride/Total Hardness then an average value is taken. Otherwise, if there is only one year with 12 or more values a simple seasonal smoother is used. If there is two years with 12 or more values then a seasonal smoother with a trend is fitted. Otherwise a model with trend and a dynamic seasonal component is fitted.
