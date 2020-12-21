### Account for data censoring 

Treat values at or below detection limit at face value when calculating the summary table or account for these non-detects using using left-censored log-normal maximum-likelihood.

The underlying function being used in the wqbc R package is [summarise_wqdata](http://bcgov.github.io/wqbc/reference/summarise_wqdata.html).
