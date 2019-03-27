library(tidyverse)
library(scales)
library(magrittr)

abb <- tibble(
  state = state.name,
  abbrv = state.abb
)

# find region 5 states
abb <- filter(abb, abbrv %in% c("IL", "IN", "OH", "MI", "WI", "MN"))

# MIC ---------------------------------------------------------------------

micfils <- dir("data/", full.names = TRUE, pattern = "r2mic")

mic <- micfils %>%
  map_df(
    read_csv,
    na = c("DQ*", "-"),
    col_names = c("state", "year", "n_mic", "nonvic"),
    col_types = c("cddd")
  ) %>%
  arrange(state, year) %>%
  inner_join(abb, by = "state")

mic <- mic %>%
  group_by(state) %>%
  filter(!anyNA(n_mic)) %>%
  ungroup()

mic <- mic %>%
  group_by(year) %>%
  arrange(n_mic) %>%
  mutate(
    rank  = 1:length(n_mic),
    is_mi = state == "Michigan"
  ) %>%
  ungroup()

# 0.57 or lower is the standard

p0 <- mic %>%
  filter(year >= 2012) %>%
  ggplot(aes(x = rank, y = n_mic / 100, fill = is_mi)) +
  geom_col(width = .75) +
  geom_hline(yintercept = 0.0057, lty = "dotted") +
  geom_hline(yintercept = 0.0) +
  geom_text(aes(label = abbrv), hjust = -0.25) +
  scale_x_reverse(expand = c(0, 0)) +
  scale_y_continuous(name = "", limits = c(0, .01), labels = percent_format()) +
  scale_fill_manual(values = c("darkgrey", "darkblue")) +
  facet_wrap(~year, nrow = 2) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks.x = element_line(),
    axis.text.y = element_blank()
  ) +
  labs(
    x = "",
    title = "Maltreatment in Care, Round 2 Performance 2012 - 2017",
    subtitle = "National standard: less than or equal to 0.57%",
    caption = "Source: Children's Bureau (https://cwoutcomes.acf.hhs.gov/cwodatasite/recurrence/index)"
  ) +
  coord_flip()

p0

# Recurrence --------------------------------------------------------------

recfils <- dir("data/", full.names = TRUE, pattern = "recurstate")

rec <- recfils %>%
  map_df(
    read_csv,
    na = c("DQ*", "-"),
    col_names = c("state", "year", "n_rec", "nonrec", "n_uniq_vic"),
    col_types = c("cdddc")
  ) %>%
  arrange(state, year) %>%
  inner_join(abb, by = "state")

rec <- rec %>%
  group_by(state) %>%
  filter(!anyNA(n_rec)) %>%
  ungroup()

rec <- rec %>%
  group_by(year) %>%
  arrange(desc(n_rec)) %>%
  mutate(
    rank  = 1:length(n_rec),
    is_mi = state == "Michigan"
  ) %>%
  ungroup()

# 5.4% is the standard

p1 <- rec %>%
  filter(year >= 2012) %>%
  ggplot(aes(x = rank, y = (100 - n_rec) / 100, fill = is_mi)) +
  geom_col(width = .75) +
  geom_hline(yintercept = .054, lty = "dotted") +
  geom_hline(yintercept = 0.0) +
  geom_text(aes(label = abbrv), hjust = -0.25) +
  scale_x_reverse(expand = c(0, 0)) +
  scale_y_continuous(name = "", label = percent_format(), limits = c(0, .1)) +
  scale_fill_manual(values = c("darkgrey", "darkblue")) +
  facet_wrap(~year, nrow = 2) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks.x = element_line(),
    axis.text.y = element_blank()
  ) +
  labs(
    x = "",
    title = "Maltreatment Recurrence within 6 months, Round 2 Performance 2012 - 2017",
    subtitle = "National standard: less than or equal to 5.4%",
    caption = "Source: Children's Bureau (https://cwoutcomes.acf.hhs.gov/cwodatasite/recurrence/index)"
  ) +
  coord_flip()

p1

# save --------------------------------------------------------------------

ggsave(
  "region-5-states-r2-safety-recurrence.png",
  p0,
  units = "in",
  height = 6, width = 12
)

ggsave(
  "region-5-states-r2-safety-mic.png",
  p1,
  units = "in",
  height = 6, width = 12
)
