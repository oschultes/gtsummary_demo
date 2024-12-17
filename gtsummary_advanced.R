
#############################
# gtsummary iteration and combining tables
# created by: Olivia Schultes


rm(list = ls())

if (!require(pacman)) {install.packages("pacman"); require(pacman)}
pacman::p_load(tidyverse, gtsummary, geepack)


# read in data
cheese_processed <- readRDS("cheese_processed.Rds")
cats_processed <- readRDS("cats_processed.Rds")
childcare_processed <- readRDS("childcare_processed.Rds")





# outline

# show example of looping over multiple models to form a table

# combining tables using stack & merge

# transforming to gt and exporting as an rtf -> word

# further customization options in gt that are not available in gt summary 
# i.e., more powerful footnote aesthetics

# exporting not as rtf - look into rmarkdown (quarto too?)

# other 











######################################################
# looping over tbl_regression to create custom outputs

# we might want to loop through a set of predictors to run multiple models with the same outcome
# to do this, let's figure out something first:







#############################
# stacking and merging tables


# demonstrate stack & merge: subset the data, produce tables, then stack or merge







##############################################################
# converting to gt, further customization and exporting tables







