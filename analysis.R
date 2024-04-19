##setup of analysis ----
##Load libraries to use 
library(dplyr)
library(ggplot2)
##Load data
individual <- readr::read_csv(
  here::here("data", "individual.csv")
)%>%
  select(stem_diameter, height, growth_form)

#check data
summary(individual)

## subsetting data to remove NA's and srt liana data 
analysis_df <- individual %>%
  filter(complete.cases(.), 
         growth_form != "liana")
#filters through rows to include complete cases of data only (remove NA), and 
#also remove noly liana growth forms 

#organise bar plots for better visualisation
#ascending order
#need to make vector of growth_forms by ordering growth form leves 
##make vector containing order we want from the levels
gf_levels <- table(analysis_df$growth_form) %>%
  sort() %>%
  names()

#now need to specify level order when we mutate growth form to a factor using gf_evels 
analysis_df <- analysis_df %>%
  mutate(growth_form = factor(growth_form,
                              levels = gf_levels
  ))


#check bar plot now when running in development R script
# check bar plot now the data has been sorted 
analysis_df %>%
  ggplot(aes(x = growth_form, colour = growth_form, fill = growth_form)) +
  geom_bar()

## VISUALISATION - CHANGE axes assignments 
analysis_df %>%
  ggplot(aes(y = growth_form, colour = growth_form, fill = growth_form)) +
  geom_bar(alpha = 0.5, show.legend = FALSE)
#show.legend = FALSE removes the colour descriptive
##alpha changes opacity

# want to double data amount
analysis_df %>%
  tidyr::pivot_longer(
    cols = c(stem_diameter, height),
    names_to = "var",
    values_to = "value"
  )

# visualise data
#analysis_df %>%
tidyr::pivot_longer(
  cols = c(stem_diameter, height),
  names_to = "var",
  values_to = "value"
) %>%
  ggplot(aes(x = log(value), y = growth_form, colour = growth_form, 
             fill = growth_form)) +
  geom_violin(alpha = 0.5, trim = T) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  facet_grid(~var)

#fit linear model to perform stats on findings 
lm_overall <- lm(log(stem_diameter) ~ log(height), analysis_df)
lm_overall

#prints output of linear model nicely in a tibble to evaluate overall fit of model
lm_overall %>%
  broom::glance()

#tidy() summarizes information about the components of a model
#lm components are the parameters associated with a regression i.e. the intercept and slope
lm_overall %>%
  broom::tidy()

## VISUALISE MODEL
analysis_df %>%
  ggplot(aes(x = log(height), y = log(stem_diameter))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm")

#CHANGE PLOT THEME
analysis_df %>%
  ggplot(aes(x = log(height), y = log(stem_diameter))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  xlab("Log of height (m)") +
  ylab("Log of stem diameter (cm)") +
  theme_linedraw()

# investigate interaction between diameter and height, by growth form
lm_growth <- lm(log(stem_diameter) ~ log(height) * growth_form, analysis_df)

#examing new lm growth model
lm_growth %>%
  broom::glance()
# r squared and p values now highter

# summarize model info using tidy
lm_growth %>%
  broom::tidy()

## visualise this new model
### apply a grouping to scatter plot through-> aesthetic colour.
analysis_df %>%
  ggplot(aes(x = log(height), y = log(stem_diameter), colour = growth_form)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm") +
  labs(
    x = "Log of height (m)",
    y = "Log of stem diameter (cm)",
    colour = "Growth forms"
  ) +
  theme_linedraw()