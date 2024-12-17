
#############################
# gtsummary iteration and combining tables
# created by: Olivia Schultes


rm(list = ls())

if (!require(pacman)) {install.packages("pacman"); require(pacman)}
pacman::p_load(tidyverse, gtsummary, gt, geepack)


# read in data
cheese_processed <- readRDS("cheese_processed.Rds")
cats_processed <- readRDS("cats_processed.Rds")
childcare_processed <- readRDS("childcare_processed.Rds")


# description of data, including data dictionaries

# cheese data: https://github.com/rfordatascience/tidytuesday/tree/main/data/2024/2024-06-04
# cats data: https://github.com/rfordatascience/tidytuesday/tree/main/data/2023/2023-01-31
# childcare data: https://github.com/rfordatascience/tidytuesday/tree/main/data/2023/2023-05-09







# outline

# show example of looping over multiple models to form a table

# combining tables using stack & merge

# transforming to gt and exporting as an rtf -> word

# further customization options in gt that are not available in gt summary 
# i.e., more powerful footnote aesthetics

# exporting not as rtf - look into rmarkdown (quarto too?)

# other 








##########################################
# let's go through an example of creating a table 1 and 2 for an analysis using the childcare dataset










###############################
# Table 1


# let's say for table 1 we are interested in looking at a subset of variables at study baseline (year 2008)

table1 = childcare_processed |>
  filter(study_year==2008) |>
  tbl_summary(include = c(total_pop, pr_f, unr_16, one_race_w, flfpr_20to64, flfpr_20to64_under6, flfpr_20to64_6to17, flfpr_20to64_under6_6to17),
              digits = all_continuous() ~ 0,
              label = c(unr_16 ~ "% population 16 years or older unemployed",
                        one_race_w ~ "% of population that identifies as only White",
                        pr_f ~ "% families below poverty line",
                        total_pop ~ "County population",
                        flfpr_20to64 ~ "% female population 20-64 years that participates in the labor force",
                        flfpr_20to64_under6 ~ "% female population 20-64 years with children under 6 years old that participates in the labor force",
                        flfpr_20to64_6to17 ~ "% female population 20-64 years with children 6-17 years old that participates in the labor force",
                        flfpr_20to64_under6_6to17 ~ "% female population 20-64 years with children under 6 and 6-17 years old that participates in the labor force"))

table1

# to export to word, we need to transform this object to a gt table and export as an .rtf file

table1_gt = table1 |>
  as_gt()

table1_gt |>
  gtsave(paste0("Output/table_1.rtf"))








################
# Table 2

# we might want to loop through a set of predictors to run multiple models with the same outcome
# we could do this with tbl_uvregression, but only with univariate models
# creating a function with tbl_regression will give us more flexibility


# in this case, our outcome is weekly childcare cost for infants
# we are interested in four similar predictors:
    # flfpr_20to64, % female population 20-64 years that participates in the labor force
    # flfpr_20to64_under6, % female population 20-64 years with children under 6 years old that participates in the labor force
    # flfpr_20to64_6to17, % female population 20-64 years with children 6-17 years old that participates in the labor force
    # flfpr_20to64_under6_6to17, % female population 20-64 years with children under 6 and 6-17 years old that participates in the labor force
# in each model, we will control for study year, county population and the % of families below the poverty line



# first, let's set up our function for each regression

func_tab2 = function(predictor) {
  
  # data
  func_dat = childcare_processed |>
    filter(!is.na(mc_infant) & !is.na(UQ(sym(predictor))))
  
  # formula
  formula <- as.formula(paste0("mc_infant ~ ", predictor, " + total_pop + pr_f + study_year"))
  
  # model
  model <- glm(formula, data = func_dat, family = gaussian)
  
  # labels
  if(predictor=="flfpr_20to64") {label = "% female population 20-64 years that participates in the labor force"
  } else if(predictor=="flfpr_20to64_under6") {label = "% female population 20-64 years with children under 6 years old that participates in the labor force"
  } else if(predictor=="flfpr_20to64_6to17") {label = "% female population 20-64 years with children 6-17 years old that participates in the labor force"
  } else if(predictor=="flfpr_20to64_under6_6to17") {label = "% female population 20-64 years with children under 6 and 6-17 years old that participates in the labor force"
  } else("Error: incorrect predictor argument.")
  
  # make table  
  tbl2 = tbl_regression(model,
                        include = paste0(predictor),             # note that this means we are only keeping our predictor of interest displayed in the table, even though the model is still controlled for total population & % family poverty!
                        pvalue_fun = ~style_sigfig(., digits = 3),
                        label = predictor ~ label) %>%
      modify_footnote(everything() ~ NA, abbreviation = TRUE)
  
  return(tbl2)
}


# to test our function, let's see what happens when we use the first predictor of interest
func_tab2("flfpr_20to64")

# perfect! looks like there is one row of output for this predictor, which is what we want
# note that the other two variables we controlled for are still included in the model; we just specifically told tbl_regression to only show us the coefficients for the variable of interest (the predictor)


# now let's create a vector with all the predictors of interest and loop through them using lapply
predictors_tab2 = c("flfpr_20to64", "flfpr_20to64_under6", "flfpr_20to64_6to17", "flfpr_20to64_under6_6to17")

results_tab2 <- lapply(predictors_tab2, func_tab2)

# now we have a list of the tables from each regression we ran
# we can combine results into one table vertically using tbl_stack
# after combining, we can edit some of the aesthetics of the nearly-final table

table2 = tbl_stack(results_tab2) %>%
  modify_header(estimate = "**Mean difference in weekly cost of infant childcare (USD)**") %>%
  modify_footnote(ci ~ "CI = Confidence Interval") |>
  modify_footnote(label ~ "All models adjusted for study year, county population and % families below poverty line.")


# the footnote capabilities are pretty good within the gtsummary package, but there are some things that are currently not possible
# for instance, gtsummary currently doesn't support putting footnotes in the labels of a particular variable
# if you run into trouble placing footnotes or making other aesthetic changes, I would recommend converting to gt and then modifying
# the gt package is older and has additional footnote/aesthetic capabilities

# looks great! now let's export
table2 |>
  as_gt() |>
  gtsave("Output/table_2.rtf")








################
# Table 3

# let's say we wanted to look at a binary outcome
# in this case, our binary outcome will be 
# our table 3 will have 2 components: first, a summary of the n (%) of counties with the outcome for each variable, and then the results of a regression


# first, we'll use tbl_summary to create the descriptive portion of the table

table3_summary = childcare_processed %>% 
  mutate(mc_infant_150 = ifelse(is.na(mc_infant_150), NA,
                                ifelse(mc_infant_150==0, "Not over 150 USD / week", "Over 150 USD / week"))) |>
  tbl_summary(include = c(flfpr_20to64, flfpr_20to64_under6, flfpr_20to64_6to17, flfpr_20to64_under6_6to17),
              by = mc_infant_150,
              label = list(flfpr_20to64 ~ "% female population 20-64 years that participates in the labor force",
                           flfpr_20to64_under6 ~ "% female population 20-64 years with children under 6 years old that participates in the labor force",
                           flfpr_20to64_6to17 ~ "% female population 20-64 years with children 6-17 years old that participates in the labor force",
                           flfpr_20to64_under6_6to17 ~ "% female population 20-64 years with children under 6 and 6-17 years old that participates in the labor force")
  ) 
  # modify_header(all_stat_cols() ~ "**{level}**") %>%



# now let's set up our function for each regression

func_tab3 = function(predictor) {
  
  # data
  func_dat = childcare_processed |>
    filter(!is.na(mc_infant_150) & !is.na(UQ(sym(predictor))))
  
  # formula
  formula <- as.formula(paste0("mc_infant_150 ~ ", predictor, " + total_pop + pr_f + study_year"))
  
  # model
  model <- glm(formula, data = func_dat, family = binomial)
  
  # labels
  if(predictor=="flfpr_20to64") {label = "% female population 20-64 years that participates in the labor force"
  } else if(predictor=="flfpr_20to64_under6") {label = "% female population 20-64 years with children under 6 years old that participates in the labor force"
  } else if(predictor=="flfpr_20to64_6to17") {label = "% female population 20-64 years with children 6-17 years old that participates in the labor force"
  } else if(predictor=="flfpr_20to64_under6_6to17") {label = "% female population 20-64 years with children under 6 and 6-17 years old that participates in the labor force"
  } else("Error: incorrect predictor argument.")
  
  # make table  
  tbl3 = tbl_regression(model,
                        include = paste0(predictor),
                        exponentiate = TRUE,               # we want exponentiated values displayed so our results are not on the log(OR) scale
                        pvalue_fun = ~style_sigfig(., digits = 3),
                        label = predictor ~ label) %>%
    modify_footnote(everything() ~ NA, abbreviation = TRUE)
  
  return(tbl3)
}


# testing our function
func_tab3("flfpr_20to64")



# now let's create a vector with all the predictors of interest and loop through them using lapply
predictors_tab3 = c("flfpr_20to64", "flfpr_20to64_under6", "flfpr_20to64_6to17", "flfpr_20to64_under6_6to17")

results_tab3 <- lapply(predictors_tab3, func_tab3)

# now we have a list of the tables from each regression we ran
# we can combine results into one table vertically using tbl_stack
# after combining, we can edit some of the aesthetics of the nearly-final table

table3_regression = tbl_stack(results_tab3) %>%
  modify_header(estimate = "**Mean difference in weekly cost of infant childcare (USD)**") %>%
  modify_footnote(ci ~ "CI = Confidence Interval") |>
  modify_footnote(estimate ~ "All models adjusted for study year, county population and % families below poverty line.")



# now let's combine the summary and regression tables horizontally using tbl_merge

table3 = tbl_merge(list(table3_summary, table3_regression),
                   tab_spanner = FALSE)


# looks great! some notes on combining tables:
    # it's usually best practice to stack, then merge if necessary
    # the labels must match exactly for tbl_merge to recognize that two rows are the same. if the labels are different, then parts of the table will appear as two different rows
    # we can specify spanning labels over portions of the tables we're combining using tab_spanner 


# now let's export
table3 |>
  as_gt() |>
  gtsave("Output/table_3.rtf")









