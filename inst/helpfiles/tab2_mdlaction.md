### Values below the detection limit

This determines what is done with any values below the detection limit.  

Options are:
* 'zero' - set the value to 0 (the default option)
* 'half the detection limit' - set the value to half the detection limit
* 'detection limit' - set the value equal to the detection limit
* 'missing value (NA)' - set the value to NA

When summarising the data in Tab 5. Plots/Statistics, missing values (NAs) can be excluded and values at or below the detection limits can be taken at face value or treated as censored. The underlying function being used in the wqbc R package is [set_non_detects](http://bcgov.github.io/wqbc/reference/set_non_detects.html).
