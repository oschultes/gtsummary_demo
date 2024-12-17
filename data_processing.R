
########################################
# processing data for gtsummary tutorial

if (!require(pacman)) {install.packages("pacman"); require(pacman)}
pacman::p_load(tidyverse)







################### 
# cheese processing

# load our data
cheese <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-06-04/cheeses.csv')


# let's clean up our cheese shall we?

cheese_processed = cheese |>
  mutate(region = ifelse(!is.na(country) & str_detect(country, ","), "Multiple", country),
         region = case_when(country %in% c("Afghanistan", "Armenia", "Georgia", "India", "Iraq", "Israel", "Middle East", "Mongolia", "Turkey",
                                          "Bangladesh, India", "China, Nepal, Tibet", "China, Tibet", "Lebanon, Middle East") ~ "Asia",
                            country %in% c("Mauritania") ~ "Africa",
                            country %in% c("Great Britain", "England", "Scotland", "Wales", "England, Great Britain, United Kingdom",
                                           "England, United Kingdom", "Scotland, United Kingdom", "England, Scotland, United Kingdom",
                                           "England, Scotland, Wales", "Great Britain, Scotland, United Kingdom", "Great Britain, United Kingdom, Wales",
                                           "United Kingdom, Wales") ~ "United Kingdom",
                            country %in% c("Argentina", "Brazil", "Chile") ~ "South America",
                            country %in% c("Mexico", "Mexico and Caribbean") ~ "Mexico and Caribbean",
                            country %in% c("Bulgaria", "Croatia", "Czech Republic", "Hungary", "Lithuania", "Poland", "Romania", "Serbia",
                                          "Hungary, Poland, Slovakia") ~ "Eastern Europe",
                            country %in% c("Austria", "Belgium", "Cyprus", "Denmark", "Finland", "Germany", "Greece", "Holland", "Iceland", "Ireland", 
                                          "Netherlands", "Portugal", "Spain", "Sweden", "Switzerland", "Austria, Germany", "Belgium, Germany, Netherlands",
                                          "Denmark, Finland, Germany, Iceland, Norway, Sweden") ~ "Other Western Europe",
                            country %in% c("Australia", "New Zealand") ~ "Oceania",       
                             .default = region),
         continent = case_when(region %in% c("Canada", "United States", "Mexico and Caribbean") | 
                                 country %in% c("Canada, United States", "Mexico, United States") ~ "North America",
                               region %in% c("Eastern Europe", "France", "Italy", "Other Western Europe", "United Kingdom") | 
                                 country %in% c("France, Italy", "France, Switzerland") ~ "Europe",
                               .default = region),
         milk_simple = ifelse(!is.na(milk) & str_detect(milk, ","), "multiple", milk),
         firmness = ifelse(!is.na(type) & (str_detect(type, "hard") | str_detect(type, "firm")), "Hard or firm",
                           ifelse(!is.na(type) & str_detect(type, "soft"), "Soft", NA)),
         fat_percent = str_replace(fat_content, "%", ""),
         fat_percent = str_replace(fat_percent, " g/100g", ""),
         fat_percent = as.numeric(str_remove(fat_percent, "-.+")),
         rind_simple = case_when(rind %in% c("artificial", "plastic", "waxed") ~ "wax or plastic",
                                 rind %in% c("mold ripened") ~ "bloomy",
                                 rind %in% c("edible") ~ NA,
                                 rind %in% c("cloth wrapped", "leaf wrapped") ~ "cloth or leaf wrapped",
                                 .default = rind),
         color_simple = case_when(color %in% c("blue-grey") ~ "blue",
                                  color %in% c("brownish yellow") ~ "brown",
                                  color %in% c("pale white") ~ "white",
                                  color %in% c("cream", "ivory") ~ "cream/ivory",
                                  color %in% c("golden yellow") ~ "straw",
                                  color %in% c("golden orange") ~ "orange",
                                  color %in% c("green", "pink and white", "red") ~ "other",
                                  .default = color))



# export
write_rds(cheese_processed, "cheese_processed.Rds")








################# 
# cats processing

# load our data
cats_uk_reference <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-01-31/cats_uk_reference.csv')


# let's clean up our cat data shall we?

cats_processed = cats_uk_reference |>
  mutate(n_days_obs = difftime(deploy_off_date, deploy_on_date, units = "days"))


# export
write_rds(cats_processed, "cats_processed.Rds")








###########################
# childcare data processing

# load our data
childcare_costs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-05-09/childcare_costs.csv')
counties <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-05-09/counties.csv')


# merge datasets together

childcare_processed = childcare_costs |>
  left_join(counties, by = "county_fips_code") |>
  relocate(c(county_name, state_name, state_abbreviation), .after = county_fips_code) |>
  mutate(mc_infant_150 = ifelse(is.na(mc_infant), NA,
                                ifelse(mc_infant>150, 1, 0))) |>
  arrange(state_name, county_name, study_year) |>
  mutate(state_abbreviation = factor(state_abbreviation))

# export
write_rds(childcare_processed, "childcare_processed.Rds")



