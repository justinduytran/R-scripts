# Header ------------------------------------------------------------------
#
# Script name: Animated population charts
# Script description: 
#
# Author: Justin Tran
#
# Notes:
#
#
#

# Dependencies ------------------------------------------------------------

if (!require("pacman"))
  install.packages("pacman")
pacman::p_load(readabs,
               tidyverse,
               lubridate,
               ggplot2,
               ggpol,
               cowplot,
               gganimate,
               gifski
               )

pacman::p_load_current_gh()
# devtools::source_url()

devtools::install_github("justinduytran/R-package")
pacman::p_load(justinduytran)

# @@ SCRIPT @@ ------------------------------------------------------------

# Jurisdiction parameter --------------------------------------------------

Jurisdiction = "Australia"

# ERP ABS data ------------------------------------------------------------

ERP <- read_abs("3101.0")

ERP_by_jurisdiction <- ERP |>
  filter(table_no %in% 3101051:3101059) |>
  mutate(
    jurisdiction = case_when(
      table_no == 3101051 ~ "New South Wales",
      table_no == 3101052 ~ "Victoria",
      table_no == 3101053 ~ "Queensland",
      table_no == 3101054 ~ "South Australia",
      table_no == 3101055 ~ "Western Australia",
      table_no == 3101056 ~ "Tasmania",
      table_no == 3101057 ~ "Northern Territory",
      table_no == 3101058 ~ "Australian Capital Territory",
      table_no == 3101059 ~ "Australia"
    )
  ) |>
  # mutate(
  #   jurisdiction_short = case_when(
  #     jurisdiction == "New South Wales" ~ "NSW",
  #     jurisdiction == "Victoria" ~ "VIC",
  #     jurisdiction == "Queensland" ~ "QLD",
  #     jurisdiction == "South Australia" ~ "SA",
  #     jurisdiction == "Western Australia" ~ "WA",
  #     jurisdiction == "Tasmania" ~ "TAS",
  #     jurisdiction == "Northern Territory" ~ "NT",
  #     jurisdiction == "Australian Capital Territory" ~ "ACT",
  #     jurisdiction == "Australia" ~ "AUS"
  #   )
  # ) |>
  select(date, series, value, jurisdiction) |>
  separate_series() |>
  mutate(series_3 = case_when(series_3 == "100 and over" ~ 100,
                              TRUE ~ as.numeric(series_3))) |>
  select(-series) |>
  rename("sex" = "series_2", "age" = "series_3") |>
  mutate(year = year(date)) |>
  group_by(jurisdiction, date, sex) |> 
  mutate(value_percent = value/sum(value)) |> 
  ungroup()
  
ERP_Jurisdiction <- ERP_by_jurisdiction |> 
  filter(jurisdiction == Jurisdiction)

# Pop Pyramid -------------------------------------------------------------

ERP_Jurisdiction_group5 <- ERP_Jurisdiction |>
  mutate(age_groups =
           cut(
             age,
             c(seq(0, 105, 5)),
             labels = c(paste0(seq(0, 95, 5), " - ", seq(4, 99, 5)), "100 and over"),
             right = F
           )) |>
  filter(sex != "Persons") |>
  group_by(date, year, age_groups, sex) |>
  summarise(value = sum(value)) |>
  group_by(date, year, sex) |>
  mutate(value_percent = value / sum(value) * 100) |>
  ungroup()

ERP_Jurisdiction_group5_pyramidformat <- ERP_Jurisdiction_group5 |>
  mutate(value_percent = case_when(sex == "Male" ~ -value_percent,
                                   sex == "Female" ~ value_percent))

pyramid <- ERP_Jurisdiction_group5_pyramidformat |>
  ggplot(aes(x = age_groups, y = value_percent, fill = sex)) +
  geom_col(show.legend = FALSE) +
  geom_col(
    ERP_Jurisdiction_group5_pyramidformat |>
      filter(year == 1971) |>
      select(-year),
    mapping = aes(x = age_groups, y = value_percent, fill = sex),
    fill = 'transparent',
    show.legend = FALSE
  ) +
  coord_flip() +
  facet_share(
    ~ factor(sex, levels = c("Male", "Female")),
    dir = "h",
    scales = "free",
    reverse_num = T
  ) +
  #scale_x_continuous(breaks = seq(0,100,10), labels = c(seq(0,90,10), "100 and over"))+
  theme_light() +
  labs(
    title = "{Jurisdiction}'s Population in {round(frame_time)}",
    subtitle = "Population = {round(ERP_Jurisdiction_total_pop$population[as.integer(frame_time)-1970]/1000000,2)} million
    Growth since 1971: {round((ERP_Jurisdiction_total_pop$population[as.integer(frame_time)-1970]/ERP_Jurisdiction_total_pop$population[1]-1)*100,2)}%",
    caption = "Data: Australian Bureau of Statistics,  National, state and territory population, Estimated Resident Population",
    x = NULL,
    y = "Percent of population in each age group"
  )

pyramid

pyramid_animated <- pyramid +
  transition_time(year)
  
animate(pyramid_animated,
  duration = 15,
  end_pause = 30,
  renderer = gifski_renderer(),
  nframes = 100,
  height = 15,
  width = 15,
  units = 'cm',
  res = 160
)

# Pop growth line ---------------------------------------------------------

ERP_Jurisdiction_total_pop <- ERP_Jurisdiction |> 
  filter(sex == "Persons") |> 
  group_by(date) |> 
  summarise(population = sum(value)) |> 
  ungroup() |> 
  mutate(year = year(date))

popgrowthchart <-  ERP_Jurisdiction_total_pop |> 
  ggplot(aes(x = year, y = population/1000000))+
  geom_line()+
  geom_line(
    ERP_Jurisdiction_total_pop |> 
      select(population, year) |> 
      rename("year_x" = "year"),
    mapping = aes(x = year_x)
  )+
  geom_point()+
  scale_y_continuous(limits = c(0, 10 * (
    max(ERP_Jurisdiction_total_pop$population / 1000000) %/% 10 +
      as.logical(max(
        ERP_Jurisdiction_total_pop$population / 1000000
      ) %%
        10)
  )))+
    theme_half_open()+
  labs(title = "{Jurisdiction}'s Population in {as.integer(frame_along)}",
       x = "Year",
       y = "Population (millions)")

popgrowthchart

popgrowthchart_anim <- popgrowthchart+
  transition_reveal(year)

animate(popgrowthchart_anim,
        duration = 10,
        end_pause = 20,
        renderer = gifski_renderer(),
        nframes = 100,
        height = 10,
        width = 20,
        units = 'cm',
        res = 75
)

  