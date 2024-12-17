

#############################
# gtsummary basic functions
# created by: Olivia Schultes


rm(list = ls())

if (!require(pacman)) {install.packages("pacman"); require(pacman)}
pacman::p_load(tidyverse, gtsummary, geepack)


# read in data
cheese_processed <- readRDS("cheese_processed.Rds")
cats_processed <- readRDS("cats_processed.Rds")
childcare_processed <- readRDS("childcare_processed.Rds")



# outline

# walk through tbl_summary, with some different customization actions
    # including unknown
    # demonstrating automatic treatment of T/F y/n 1/0 as binary vars
    # use different journal styles

# tbl_uvregression, tbl_ multiple regression
    # showcase different types of regression (i.e., linear, logistic, poisson)
    # show example of incorporating more complex models (i.e. GEE)







#############
# tbl_summary

# tbl_summary is useful for creating a table 1 for your project
# let's create a simple table 1 for our cheese dataset

tbl_summary(data = cheese_processed,
            include = c(continent, milk_simple, firmness, rind_simple, color_simple, fat_percent))


# we have to specify which variables to include; by default, it will include all variables
# let's add nicer labels to each of our variables

tbl_summary(data = cheese_processed,
            include = c(continent, milk_simple, firmness, rind_simple, color_simple, fat_percent, vegetarian),
            label = c(continent ~ "Continent", 
                      milk_simple ~ "Milk",
                      firmness ~ "Firmness",
                      rind_simple ~ "Rind",
                      color_simple ~ "Color",
                      fat_percent ~ "Fat percent",
                      vegetarian ~ "Vegetarian"))

# by default, tbl_summary guesses each type of variable
    # variables coded T/F, yes/no, or 1/0 are interpreted as binary and are presented in one line (the T, yes or 1 values)
    # numeric variables with >10 unique values are treated as continuous and presented as Median (IQR)
    # numeric variables with <10 unique values are treated as categorical; you may need to tell tbl_summary to treat these as continuous
# tbl_summary has many aesthetic defaults for tables, but these can be changed
# let's customize the aesthetics to our liking!

tbl_summary(data = cheese_processed,
            include = c(continent, milk_simple, firmness, rind_simple, color_simple, fat_percent, vegetarian),
            label = c(continent ~ "Continent", 
                      milk_simple ~ "Milk",
                      firmness ~ "Firmness",
                      rind_simple ~ "Rind",
                      color_simple ~ "Color",
                      fat_percent ~ "Fat percent",
                      vegetarian ~ "Vegetarian"),
            statistic = list(all_continuous() ~ "{mean} ({sd})",    # changing continuous variables from median (iqr) to mean (sd)
                             all_categorical() ~ "{n} ({p})"),   # getting rid of percentage sign in cells
            digits = all_categorical() ~ 1,             # printing 1 decimal place for all categorical variables
            type = list(vegetarian ~ "categorical")) |>    # changing vegetarian to display like a categorical variable instead of a binary one
  add_stat_label() |>                        # place statistics labels after each variable name instead of in footnote
  remove_row_type(type = "missing")                     # remove rows denoting how many missing values there are per variable
  

# what if we want to present table 1 stratified by a key variable? Let's look at cheeses by firmness:
    # note: the add_overall command must come directly after the tbl_summary command in the pipe

tbl_summary(data = cheese_processed,
            include = c(continent, milk_simple, rind_simple, color_simple, fat_percent, vegetarian),
            by = firmness,
            label = c(continent ~ "Continent", 
                      milk_simple ~ "Milk",
                      # firmness ~ "Firmness",
                      rind_simple ~ "Rind",
                      color_simple ~ "Color",
                      fat_percent ~ "Fat percent",
                      vegetarian ~ "Vegetarian"),
            statistic = list(all_categorical() ~ "{n} ({p})")) |>
  add_overall(last = TRUE) |>
  remove_row_type(type = "missing") 



# to provide further customization, we can set themes by specifying a theme prior to creating a table
# themes can be stacked on each other
# let's call two themes at once

theme_gtsummary_journal("lancet")
theme_gtsummary_compact()

tbl_summary(data = cheese_processed,
            include = c(continent, milk_simple, rind_simple, color_simple, fat_percent, vegetarian),
            by = firmness,
            label = c(continent ~ "Continent", 
                      milk_simple ~ "Milk",
                      # firmness ~ "Firmness",
                      rind_simple ~ "Rind",
                      color_simple ~ "Color",
                      fat_percent ~ "Fat percent",
                      vegetarian ~ "Vegetarian"),
            statistic = list(all_categorical() ~ "{n} ({p})")) |>
  add_overall(last = TRUE) |>
  remove_row_type(type = "missing") 

# we can clear all themes using the following command

reset_gtsummary_theme()






##################
# tbl_uvregression

# we can perform statistical analyses within the functions that produce nicely formatted gtsummary tables
# tbl_uvregression allows us to run the same univariate regression on a set of predictors

# let's start with a simple linear regression
# what is the association between cheese firmness and fat content?

cheese_processed |>
  tbl_uvregression(method = glm,
                   y = fat_percent,
                   include = c(firmness))

# now let's look at a set of predictors

cheese_processed |>
  tbl_uvregression(method = glm,
                   y = fat_percent,
                   include = c(firmness, milk_simple, rind_simple, vegetarian))

# for categorical predictors coded as strings, the first group in alphabetical order is automatically considered the reference group
# let's change the reference group for the milk variable and make the labels look nicer

cheese_processed |>
  mutate(milk_simple = factor(milk_simple, 
                              levels = c("cow", "camel", "goat", "moose", "multiple", "sheep", "water buffalo"),
                              labels = c("Cow", "Camel", "Goat", "Moose", "Multiple", "Sheep", "Water buffalo"))) |>
  tbl_uvregression(method = glm,
                   y = fat_percent,
                   include = c(milk_simple), 
                   label = c(milk_simple ~ "Milk"))

# cow is a better comparison group to examine because it has more observations
# looks like moose cheeses have less fat than cow cheeses! who knew?

# frequently, changing variables as string or factors prior to using gtsummary functions is the way to go
# so for instance, we might want to create a factor, relevel a factor, or capitalize variables appropriately using mutate() prior to creating tables
# we can easily change the variable name labels within gtsummary; trickier for levels within a variable


# we can manipulate how the results are presented using helper functions after the table is created
# it can be helpful to check column names when using these functions
# let's save the table object then check the column names

cheese_tab1 = cheese_processed |>
  mutate(milk_simple = factor(milk_simple, 
                              levels = c("cow", "camel", "goat", "moose", "multiple", "sheep", "water buffalo"),
                              labels = c("Cow", "Camel", "Goat", "Moose", "Multiple", "Sheep", "Water buffalo"))) |>
  tbl_uvregression(method = glm,
                   y = fat_percent,
                   include = c(milk_simple), 
                   label = c(milk_simple ~ "Milk"))

show_header_names(cheese_tab1)



# now let's make some more aesthetic changes
# combining beta & CI into one column, relabeling, and removing ci and N columns
cheese_processed |>
  mutate(milk_simple = factor(milk_simple, 
                              levels = c("cow", "camel", "goat", "moose", "multiple", "sheep", "water buffalo"),
                              labels = c("Cow", "Camel", "Goat", "Moose", "Multiple", "Sheep", "Water buffalo"))) |>
  tbl_uvregression(method = glm,
                   y = fat_percent,
                   include = c(milk_simple), 
                   label = c(milk_simple ~ "Milk")) |>
  modify_table_styling(column = estimate,                       # combining beta and ci into 1 column
                       rows = !is.na(estimate),
                       cols_merge_pattern = "{estimate} ({conf.low}, {conf.high})") |>
  modify_column_hide(c(ci, stat_n)) |>                            # hiding ci and n columns (note this data is still in the table object, just hidden)
  modify_header(estimate ~ "**Mean difference (CI)**")       # new label for combined column


# we can also style the number of digits
cheese_processed |>
  mutate(milk_simple = factor(milk_simple, 
                              levels = c("cow", "camel", "goat", "moose", "multiple", "sheep", "water buffalo"),
                              labels = c("Cow", "Camel", "Goat", "Moose", "Multiple", "Sheep", "Water buffalo"))) |>
  tbl_uvregression(method = glm,
                   y = fat_percent,
                   include = c(milk_simple), 
                   pvalue_fun = ~style_sigfig(., digits = 3),   # 3 decimal places for p-value column
                   label = c(milk_simple ~ "Milk")) |>
  modify_table_styling(column = estimate,                     
                       rows = !is.na(estimate),
                       cols_merge_pattern = "{estimate} ({conf.low}, {conf.high})") |>
  modify_column_hide(c(ci, stat_n)) |>                      
  modify_header(estimate ~ "**Mean difference (CI)**")    


# if we are interested in testing an anova hypothesis for categorical predictors, we can specify this as well
cheese_processed |>
  mutate(milk_simple = factor(milk_simple, 
                              levels = c("cow", "camel", "goat", "moose", "multiple", "sheep", "water buffalo"),
                              labels = c("Cow", "Camel", "Goat", "Moose", "Multiple", "Sheep", "Water buffalo"))) |>
  tbl_uvregression(method = glm,
                   y = fat_percent,
                   include = c(milk_simple), 
                   pvalue_fun = ~style_sigfig(., digits = 3),   # 3 decimal places for p-value column
                   label = c(milk_simple ~ "Milk")) |>
  modify_table_styling(column = estimate,                     
                       rows = !is.na(estimate),
                       cols_merge_pattern = "{estimate} ({conf.low}, {conf.high})") |>
  modify_column_hide(c(ci, stat_n)) |>                      
  modify_header(estimate ~ "**Mean difference (CI)**") |>
  add_global_p()


# when testing the global hypothesis of whether the fat percentage of cows milk is significantly different from all other types of milk,
# we find that there is a significant difference!
# looks like cheese from cow's milk tends to have higher fat content...yummy




################
# tbl_regression

# we can also use the more flexible command tbl_regression to run models
# let's explore some data on cat hunting behavior

# first, we'll look at how year predicts weekly childcare costs for infants by county

tbl_regression(model = glm(mc_infant ~ study_year, 
                           data = childcare_processed, 
                           family = gaussian),             # assuming a normal distribution is the default
               pvalue_fun = ~style_sigfig(., digits = 3),
               label = c(study_year ~ "Year"))

# looks like childcare costs have gone up over time (shocker)

# what if we were interested in the binary outcome of costs >$150 per week?
# note that binary outcomes may be coded as 1/0 or T/F
# we will use the exponentiate argument to get outputs on the odds ratio scale rather than log odds ratio scale

tbl_regression(glm(mc_infant_150 ~ study_year, 
                   data = childcare_processed,  
                   family = binomial),
               exponentiate = TRUE,
               pvalue_fun = ~style_sigfig(., digits = 3),
               label = c(study_year ~ "Year"))

# we can add covariates to our model; let's add family poverty rate and labor force participation rate for females aged 20-64

tbl_regression(glm(mc_infant ~ study_year + pr_f + flfpr_20to64, 
                   data = childcare_processed),
               pvalue_fun = ~style_sigfig(., digits = 3),
               label = c(study_year ~ "Year",
                         pr_f ~ "Family Poverty %",
                         flfpr_20to64 ~ "Labor Force Participation, % Females 20-64"))


# we can also look at more complex types of models
# for instance, if we wanted to account for clustering at the state level, we can use a gee model with state-level clustering
# we would use the geeglm() function to specify the model rather than glm()

# reminders for gee: 
    # geeglm won't automatically drop observations with missing value (NAs must be removed)
    # observations must be sorted by cluster variable
    # function prefers if id variable is a factor or numeric variable (character id variables will take longer to run)

# note: model takes 3-4 minutes to run

tbl_regression(geeglm(mc_infant ~ study_year + pr_f + flfpr_20to64, 
                      data = childcare_processed |> filter(!is.na(mc_infant)) |> filter(str_detect(state_abbreviation, "A")==T),
                      id = state_abbreviation,
                      family = gaussian,
                      corstr = "exchangeable",
                      std.err = "san.se"),
               pvalue_fun = ~style_sigfig(., digits = 3),
               label = c(study_year ~ "Year",
                         pr_f ~ "Family Poverty %",
                         flfpr_20to64 ~ "Labor Force Participation, % Females 20-64"))


