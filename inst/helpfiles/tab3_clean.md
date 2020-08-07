### Clean your data

This step does the following:
1. Checks that all variables in the Variable column can be recognised, and if necessary, replaces the variable name with a recognised one.
1. Checks that all values are non-negative and have standard units
1. Calculates a summary value (max, mean or median) for replicates, which are defined as two or more readings for the same variable on the same date. Data can also be summarized by additional columns such as Station.

There are two underlying functions in the `wqbc` R package being used: 
* `clean_wqdata`
* `standardize_wqdata`
