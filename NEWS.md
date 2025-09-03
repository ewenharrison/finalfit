# finalfit 1.1.0
* Fix to all plotting functions as no longer aligning with previous dependencies.

# finalfit 1.0.9
* Adopt mice 3.17.0 broom convention for names of lower and upper bound of confidence interval #107
* Expand all plots vignette. 

# finalfit 1.0.8
* `ff_expand(`) approach for model simulation added. 
* bug fix (tidyselect complaint on checks) #93
* `or_plot()` and `coefficient_plot(`) now allowing confidence interval specification via `confint_level` #58
* Remove requirement for `ff_label()` to convert input to dataframe. 

# finalfit 1.0.72
* Remove requirement for `ff_label()` to convert input to dataframe. 

# finalfit 1.0.71
* bug fix (tidyselect complaint on checks) #93
* `or_plot()` and `coefficient_plot(`) now allowing confidence interval specification via `confint_level` #58

# finalfit 1.0.7
* bug fixes
* `fit2df.lmerMod()` set default `confint_sep = " to "`. 
* `boot_predict()` and `boot_compare()` confidence interval limits and methods options added. 

# finalfit 1.0.61
* bug fixes
* `fit2df.lmerMod()` set default `confint_sep = " to "`. 
* `boot_predict()` and `boot_compare()` confidence interval limits and methods options added. 

# finalfit 1.0.6
* new plots example vignette https://finalfit.org/articles/all_plots_examples.html
* bug fix for `cont_cut` argument in `finalfit()`. #78
* bug fix for column totals with weighted data. 
* correction to weighting of continuous explanatory variables in `summary_factorlist()`.
* fix bug to `coefficient_plot()` when passing `lmmixed()` object. 
* weighting now available directly in `finalfit()`. 
* formula parsed correctly in `lmuni()`, `lmmulti()`, `glmuni()`, `glmmulti()`, `coxphuni()`, `coxphmulti()`.

# finalfit 1.0.55
* bug fix for `cont_cut` argument in `finalfit()`. #78

# finalfit 1.0.54
* bug fix for column totals with weighted data. 

# finalfit 1.0.53
* correction to weighting of continuous explanatory variables in `summary_factorlist()`.
* fix bug to `coefficient_plot()` when passing `lmmixed()` object. 

# finalfit 1.0.52
* weighting now available directly in `finalfit()`. 
* formula parsed correctly in `lmuni()`, `lmmulti()`, `glmuni()`, `glmmulti()`, `coxphuni()`, coxphmulti().

# finalfit 1.0.5
* formula interface added to `finalfit()` and `summary_factorlist()`. 
* weights argument added to `summary_factorlist()` for weighted tables. 
* `finalfit()` updated and arguments to underlying models can now be passed directly. 

# finalfit 1.0.4
* `missing_plot()` bugs fixed, many thanks @nathansam. #72
* `finalfit()` for CPH models now provides column proportions by default, many thanks corneliushennch. #74
* `summary_factorlist_stratified()`: beta testing for stratified tables.
* `rm_empty_block()` added: remove rows where all specified variables are missing. 

# finalfit 1.0.3
* `add_row_total` in `summary_factorlist()` now can include proportion of complete data via `include_row_totals_percent` argument. 
* Robust standard error approach added to vignette.
* Support added for weights etc. to be included in `coxphmulti()`.
* "Univariable" mixed effects models including random effects added to vignette. 
* `summary_factorlist()`: non-parametric continuous variables now defaults to Q1 - Q3 rather than single figure IQR. 
* `ff_interaction()`: default factor separator changed from "|" to "_" and variable separator from "__" to "_" given incompatibilities with packages such as `brms`.
* Bug to `coefficient_plot()` fixed to bring back point estimates.
* `na_to_prop = FALSE` in `summary_factorlist()` to not include missing data in column proportions of categorical data.

# finalfit 1.0.2
* `ff_relabel_df()` added to allow passing data frame / tibble with labels directly at bottom of pipe. 

# finalfit 1.0.1

* `ff_relabel()` tightened to allow mismatch between available data and labels. 
* `missing_compare()` code updated to allow arguments to be passed to new `summary_factorlist()`. 
* Quadratic terms (`I(var1^2)` etc.) are now better supported in `finalfit()`. 
* `cluster()`, `frailty()` and `strata()` terms shown in `finalfit()` regression tables as an indicator they have been included in model. 
* `or_plot()` remove_ref bug fix. 
* `ff_newdata()` bug fix. 
* Minor changes to accommodate dplyr 1.0.0.
* Plots bug fix when using `remove_ref` argument. 

# finalfit 1.0.0

* First official release. 
* Re-write of major functions to remove older dependencies, improve performance and add functionality. 
* `summary_factorlist()` completely rewritten. New column and row summary functions. Alternative statistical tests included. Finer control over continuous variable behaviours. 

# finalfit 0.9.6

* Built on R 3.6.1.
* Better support for multiple imputation. `fit2df()` function for `mipo` objects. See missing data vignette/article for examples.
* Easier now to build on a `finalfit()` table by including `keep_fit_id = TRUE`. See `ff_merge()` documentation for details. 
* Issues with update of `tidyr::spread()` in `summary_factorlist()` so updated to `tidyr::pivot_wider()`.
* `ff_column_totals()` added to be used in combination with `summary_factorlist()`.
* `ff_row_totals()` added to be used in combination with `summary_factorlist()`.
* `ff_percent_only()` added to be used in combination with `summary_factorlist()`. #25
* `ff_remove_p()` can be applied to any condensed finalfit output to remove the p-value. #26
* `finalfit()` now takes `column = FALSE` to provide row proportions. #26
* `check_recode()` added. 
* `remove_labels()` now works for tibbles. #28
* `summary_factorlist()` includes argument `cont_range = TRUE` to include quartiles Q1 and Q3 when median for continuous variables. #29

# finalfit 0.9.5

* `data(wcgs)` added.
* `summary_factorlist()` geometric sd added. 
* AIC added to lm metrics.
* `ff_label()` now does not add class "labelled".
* Removed capacity for `glmmulti()` and `lmmulti()` to run multiple models from multiple dependent variables. It wasn't used and the list generated was inconvenient for passing output to other functions such as `ggfortify::autoplot()`.
* `ff_permute()` re-written to allow many more options for producing intermediate models. 

# finalfit 0.9.4

* `coxphuni()` and `coxphmulti()` now take the other `library(survival)` functions `survival::strata()` and `survival::cluster()`. 
* Fixed `hr_plot()` axis title edit option. 
* Add option to remove reference level (`remove_ref = TRUE`) to `or_plot()`, `hr_plot()` and `coefficient_plot()`. 
* `summary_factorlist()` digit rounding option added.
* `summary_factorlist()` geometric mean option added. 
* Level label removed for continuous variables from `or_plot()`, `hr_plot()` and `coeffient_plot()`. 

# finalfit 0.9.3

* Bug in `or_plot()` and `hr_plot()` introduced in 0.9.2 because of new total column specification. 

# finalfit 0.9.2

* Competing risks time-to-event regression now supported via `cmprsk::crr()`: `crruni()`, `crrmulti()` and `fit2df()`.
* Complex stratified sampling now supported via `library(survey)`: `svyglmuni()`, `svyglmmulti()` provide support for. #13
* `summary_factorlist()` total column now summarises continuous variables.  #17 #21
* `summary_factorlist()` can now take any `Hmisc:::summary.formula` argument, such as `catTest = catTestfisher`. 
*  `catTestfisher()` added. 
* `finalfit_permute()` added. 

# finalfit 0.9.1

* `glmuni()`, `glmmulti()`, `lmuni()`, `lmmulti()` now all take `weights` and any other `glm()` or `lm()` argument. #13
* `summary_factorlist()` rework. Now supports any number of factor levels in dependent. #14 #15
* `summary_factorlist()` now provides total count for continuous variable. #17

# finalfit 0.9.0

* `or_plot()` bug fix
* `ff_remove_ref()` added. #12
* `glmmixed()` and `lmmixed()` now support random gradient models, and all complex `lme4` specifications. 
* Data preparation vignette added.

# finalfit 0.8.9

* `ff_plot()` added
* `coefficient_plot()` added
* `variable_type()` added
* Compatibility for future `shinyfit` started. 
* `ff_relabel()` added.
* Error added to `finalfit()` for not-allowed colons (:) in factor levels. #10

# finalfit 0.8.8

* `ff_glimpse()` re-written to remove `psych` dependency
* `missing_glimpse()` added: single data frame describing all variables and missing values
* `ff_interaction()` added: create variable for an interaction between two factors
* `ff_label()` added: easily add label to variable in dataframe
* `ff_newdata()` modified to take dataframe without requirement for dependent and explanatory arguments
* `summary_factorlist()` modified to allow user to change number of unique factor levels at which a variable a continuous variable is converted to a factor (`cont_cut`). #9 
* `fit2df()` and its internal function `extract_fit` modified to take `confint_type` and `confint_level`. 

# finalfit 0.8.7

* New vignettes
* pkgdown website support
* `missing_predictorMatrix()` added for use with `mice`

# finalfit 0.8.6

* Bug fix

# finalfit 0.8.5

* Extended missing data functions added

# finalfit 0.8.4

* Bootstrap simulation functions added

# finalfit 0.8.3

* Bug fix

# finalfit 0.8.2

* Missing data functions added

# finalfit 0.8.1

## Bug fixes

* `lmuni()`, `lmmulti()`, `lmmixed()`, `glmuni()`, `glmmulti()`, `glmmixed()`, `coxphuni()`, `coxphmulti()`

## New functions

* `metrics_hoslem()` is the first of a number of 'metrics' functions which will be introduced. 

# finalfit 0.7.8

* First CRAN release
