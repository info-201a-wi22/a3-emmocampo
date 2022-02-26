# Emm Ocampo
# Assignment 3

# Load required packages.
library(tidyverse)
library(ggthemes)

# Load incarceration trends
incarceration_trends <- read.csv(
  file = "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv",
  header = TRUE, 
  stringsAsFactors = FALSE
)

# Load state data
states <- read.csv(
  file = "https://raw.githubusercontent.com/12ketan/List-of-US-States/master/states.csv",
  header = TRUE, 
  stringsAsFactors = FALSE
) %>% 
  mutate(state = str_to_lower(State)) %>%
  select(state, Abbreviation) %>% 
  rename(abb = Abbreviation)

# My data of interest, excludes years with unreliable prison data.
incarceration_data <- incarceration_trends %>%
  filter(!year == "2017", !year == "2018") %>% 
  select(year, state, total_pop, 
         total_jail_pop:other_race_jail_pop,
         total_prison_pop:white_male_prison_pop)

# Data frame used for proportional questions
prop_by_year <- incarceration_data %>% 
  group_by(year) %>% 
  summarise(female_prison_pop = sum(female_prison_pop, female_jail_pop, na.rm = TRUE),
            male_prison_pop = sum(male_prison_pop, male_jail_pop, na.rm = TRUE),
            total_prison_pop = female_prison_pop + male_prison_pop,
            total_black_female = sum(black_female_prison_pop, na.rm = TRUE),
            total_black_male = sum(black_male_prison_pop, na.rm = TRUE),
            total_white_female = sum(white_female_prison_pop, na.rm = TRUE),
            total_white_male = sum(white_male_prison_pop, na.rm = TRUE)) %>% 
  mutate(prop_female = (female_prison_pop/total_prison_pop)*100,
         prop_male = (male_prison_pop/total_prison_pop)*100)

# Proportion of female in jail min year
female_min_year <- prop_by_year %>% 
  filter(year == min(year)) %>% 
  pull(prop_female) %>% 
  round(2)

# Proportion of females in jail max year
female_max_year <- prop_by_year %>% 
  filter(year == max(year)) %>% 
  pull(prop_female) %>% 
  round(2)

# Change in proportion
diff_prop <- female_max_year - female_min_year

# Year with the highest proportion of females in prison
female_max_prop <- prop_by_year %>% 
  filter(prop_female == max(prop_female)) %>% 
  pull(year)

# Population of females in prison for the year above
female_max_pop <- prop_by_year %>% 
  filter(prop_female == max(prop_female)) %>%
  pull(female_prison_pop) %>% 
  round() %>% 
  prettyNum(big.mark = ",", scientific = FALSE)

# Year with the highest proportion
male_max_pop_year <- prop_by_year %>% 
  filter(male_prison_pop == max(male_prison_pop)) %>% 
  pull(year)

# Proportion for the year above
male_max_pop_prop <- prop_by_year %>% 
  filter(male_prison_pop == max(male_prison_pop)) %>% 
  pull(prop_male) %>% 
  round(2)

# Population for the year above
male_max_pop <- prop_by_year %>% 
  filter(male_prison_pop == max(male_prison_pop)) %>% 
  pull(male_prison_pop) %>% 
  round() %>% 
  prettyNum(big.mark = ",", scientific = FALSE)

# Suppresses scientific notation
options(scipen = 10000)

# Chart one

# My first chart comparing the number of prisoners by year.
chart_one <- ggplot(prop_by_year,
                    aes(x = year)) +
  geom_line(aes(y = total_black_female,
                color = "Black Female"),
            size = 1,) +
  geom_line(aes(y = total_black_male,
                color = "Black Male"),
            size = 1,) +
  geom_line(aes(y = total_white_female,
                color = "White Female"),
            size = 1,) +
  geom_line(aes(y = total_white_male,
                color = "White Male"),
            size = 1,) +
  labs(title = "Number of prisoners by year",
       subtitle = "from 1970 to 2016",
       caption = "Data Retrieved from the Vera Institute of Justice",
       x = "Year",
       y = "Number of prisoners",
       color = "Race") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text())

# Graph two

# Data to be used for second chart
prop_black_and_white <- incarceration_trends %>% 
  filter(year == "2016") %>% 
  group_by(year) %>% 
  summarise(total_prison_pop = sum(total_prison_pop, na.rm = TRUE),
            total_pop = sum(total_pop, na.rm = TRUE),
            total_black_prison_pop = sum(black_male_prison_pop, na.rm = TRUE),
            black_pop = sum(black_pop_15to64, na.rm = TRUE),
            white_pop = sum(white_pop_15to64, na.rm = TRUE),
            total_white_prison_pop = sum(white_male_prison_pop, na.rm = TRUE)) %>% 
  mutate(Black_Proportional = (total_black_prison_pop/total_prison_pop)*100,
         White_Proportional = (total_white_prison_pop/total_prison_pop)*100,
         Black_Pop_Prop = (black_pop/total_pop)*100,
         White_Pop_Prop = (white_pop/total_pop)*100)

# Preparing the data above for a bar chart
race <- c(rep("black", 2), rep("white", 2))
condition <- rep(c("population", "prison population"), 2)
values <- c(prop_black_and_white$Black_Pop_Prop, prop_black_and_white$Black_Proportional,
            prop_black_and_white$White_Pop_Prop, prop_black_and_white$White_Proportional)
graphing_frame <- data.frame(race, condition, values)


# My second chart comparing the proportion of prison population in 2016
graph_two <- ggplot(graphing_frame, aes(fill = condition, y = values, x = race)) + 
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Proportion of White and Black people incarcerated compared to population",
       subtitle = "In the year 2016",
       caption = "Data Retrieved from the Vera Institute of Justice",
       x = "Race",
       y = "Proportion in %",
       fill = "Condition") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text())


# Chart three

# Define a minimalist theme for maps
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )

mapping_data <- incarceration_data %>% 
  filter(year == "2016") %>% 
  group_by(state) %>% 
  summarise(female_prison_pop = sum(female_prison_pop, na.rm = TRUE)) %>% 
  rename(abb = state) %>% 
  left_join(states, by = "abb")

state_shape <- map_data("state") %>% 
  rename(state = region)  %>% 
  left_join(mapping_data, by = "state") %>% 
  group_by(state) %>% 
  select(long, lat, state, female_prison_pop, group)

female_prison_population <- ggplot(state_shape) +
  geom_polygon(mapping = aes(x = long, 
                             y = lat, 
                             group = group, 
                             fill = female_prison_pop), 
               color = "white") +
  coord_map() +
  scale_fill_continuous(low = "#e8d8d3", 
                        high = "#942c09") +
  blank_theme +
  labs(title = "Number of women in prison by state",
       subtitle = "In the year 2016",
       fill = "Number of women",
       caption = "Data Retrieved from the Vera Institute of Justice")