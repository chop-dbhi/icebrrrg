
<!-- README.md is generated from README.Rmd. Please edit that file -->

# icebrrrg

<!-- badges: start -->
<!-- badges: end -->

The goal of icebrrrg is to speed statistic analyses as well as use of
machine learning models through several helper functions. In other
words, this augments many standard functions, organizing them into
summary tables or providing additional structured data that can be
useful for expanding on existing analyses or machine learning models.

## Installation

You can install the development version of icebrrrg from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("chop-dbhi/icebrrrg")
```

## single_or()

Generate a Fisher’s Exact Test summary table based on a single
comparator between two cohorts in a single dataframe

### Example

This is a basic example of the single_or function:

``` r
library(icebrrrg)
## basic example code
icebrrrg::single_or(
  data = iris,
  case_cohort = Petal.Length > 1.2,
  control_cohort = Petal.Length <= 1.2,
  comparator = Species
  )
#> [1] "Fisher test for: setosa"
#> 
#>  Fisher's Exact Test for Count Data
#> 
#> data:  select(compare_matrix, 1, 2)
#> p-value = 0.01137
#> alternative hypothesis: true odds ratio is not equal to 1
#> 95 percent confidence interval:
#>  0.0000000 0.7320236
#> sample estimates:
#> odds ratio 
#>          0 
#> 
#> [1] "Fisher test for: versicolor"
#> 
#>  Fisher's Exact Test for Count Data
#> 
#> data:  select(compare_matrix, 1, 2)
#> p-value = 0.3017
#> alternative hypothesis: true odds ratio is not equal to 1
#> 95 percent confidence interval:
#>  0.3310645       Inf
#> sample estimates:
#> odds ratio 
#>        Inf 
#> 
#> [1] "Fisher test for: virginica"
#> 
#>  Fisher's Exact Test for Count Data
#> 
#> data:  select(compare_matrix, 1, 2)
#> p-value = 0.3017
#> alternative hypothesis: true odds ratio is not equal to 1
#> 95 percent confidence interval:
#>  0.3310645       Inf
#> sample estimates:
#> odds ratio 
#>        Inf
#> # A tibble: 3 × 11
#>   Species        n case_cohort_comps_yes case_cohort_comps_no control_cohort_co…
#>   <fct>      <int>                 <int>                <int>              <int>
#> 1 setosa        50                    46                  100                  4
#> 2 versicolor    50                    50                   96                  0
#> 3 virginica     50                    50                   96                  0
#> # … with 6 more variables: control_cohort_comps_no <int>, p_value <dbl>,
#> #   CIU <dbl>, OR <dbl>, CIL <dbl>, one_OR <dbl>
```

------------------------------------------------------------------------

# License Summary

Use of this software is available to academic and non-profit
institutions for research purposes subject to the terms of the 2-Clause
BSD License (see copy below). For use or transfers of the software to
commercial entities, please inquire with <kaufmanmc@chop.edu>. © 2023
CHOP. The FreeBSD Copyright

Copyright and License Information

Copyright (c) 2023, Children’s Hospital of Philadelphia CHOP Invention
2023-059

Authors: Michael Kaufman, Alexander Gonzalez, Julie Xian, Shridhar
Parthasarathy
