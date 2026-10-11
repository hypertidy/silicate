# silicate 0.7.2

This release fixes a regression (introduced in development) where string literals
were used inside dplyr data-masking verbs, which silently broke grouping,
filtering and joins. It also addresses the CRAN check notes for deprecated
`structure()` special names and lost braces in Rd, and registers several S3
methods.

Thank you very much.

## Test environments

* local ubuntu, R 4.6.1
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes





