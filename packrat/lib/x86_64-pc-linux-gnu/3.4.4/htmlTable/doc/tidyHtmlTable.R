## ---- message=FALSE------------------------------------------------------
library(magrittr)
library(tidyr)
library(dplyr)
library(htmlTable)
library(tibble)

td <- mtcars %>%
    rownames_to_column %>%
    select(rowname, cyl, gear, hp, mpg, qsec) %>%
    gather(per_metric, value, hp, mpg, qsec)


## ------------------------------------------------------------------------
tidy_summary <- td %>%
    group_by(cyl, gear, per_metric) %>% 
    summarise(Mean = round(mean(value), 1),
              SD = round(sd(value), 1),
              Min = round(min(value), 1),
              Max = round(max(value), 1)) %>%
    gather(summary_stat, value, Mean, SD, Min, Max) %>% 
    ungroup %>% 
    mutate(gear = paste(gear, "Gears"),
           cyl = paste(cyl, "Cylinders"))

## ------------------------------------------------------------------------
tidy_summary  %>% 
    tidyHtmlTable(header = "gear",
                 cgroup1 = "cyl",
                 cell_value = "value", 
                 rnames = "summary_stat",
                 rgroup = "per_metric")

## ------------------------------------------------------------------------
tidy_summary  %>% 
    tidyHtmlTable(header = "summary_stat",
                 cgroup1 = "per_metric",
                 cell_value = "value", 
                 rnames = "gear",
                 rgroup = "cyl")

