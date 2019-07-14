team_attendances %>% 
  filter(home_or_away == "Home") %>% 
  group_by(Team, season) %>% 
  summarise(med_att = median(attendance)) %>% 
  ggplot(aes(x=season, y= med_att, group = Team)) +
  geom_line() +
  geom_point() +
  ggtitle("MEDIAN HOME ATTENDANCES THROUGH THE YEARS") +
  facet_wrap(~ Team, ncol = 6) +
  theme_classic() +
  theme(panel.grid.major.x = element_line(linetype = 2, colour = "lightgrey"), panel.grid.major.y = element_line(linetype = 2, colour = "lightgrey"),
        panel.grid.minor.x = element_line(linetype = 2, colour = "lightgrey"))

ggsave("plots/med_attendance_team_and_season.png", width = 30, height = 22, units = "cm")




team_attendances %>% filter(Team == "St Kilda", home_or_away == "Home") %>% count(venue)
team_attendances %>% filter(Team == "Essendon", home_or_away == "Home") %>% count(venue)





# testing to see if removing Hawthorn's second "home" venue (York Park) makes a difference to the result
test <- team_attendances %>% 
  filter(!is.na(last_result),
         home_or_away == "Home",
         venue != "York Park") %>%
  group_by(Team) %>% 
  mutate(median_home_attendance = median(attendance)) %>% 
  group_by(Team, last_result, median_home_attendance) %>% 
  summarise(median_attendances = median(attendance)) %>% 
  spread(key = last_result, value = median_attendances) %>% 
  mutate(LIoA = round((Lost - median_home_attendance) / median_home_attendance, 4),
         WIoA = round((Won - median_home_attendance) / median_home_attendance, 4),
         OPI = WIoA - LIoA) %>% ungroup()


library(kableExtra)

test %>%
  rename(`Median Home Attendance` = median_home_attendance, `Attendance After Loss` = Lost, `Attendance After Win` = Won) %>% 
  mutate(
    LIoA = cell_spec(LIoA, "html", color = ifelse(LIoA < -.1, "red", "grey")),
    WIoA = cell_spec(WIoA, "html", color = ifelse(WIoA > .11, "red", ifelse( WIoA > .1, "orange", ifelse(WIoA < 0.015, "lightblue", "grey")))),
    OPI = cell_spec(OPI, "html", color = ifelse(OPI > .2, "red", ifelse(OPI > .16, "orange", ifelse(OPI < 0.01, "lightblue", "grey"))))) %>%
  select(Team, `Median Home Attendance`, `Attendance After Loss`, `Attendance After Win`, LIoA, WIoA, OPI) %>% 
  kable(format = "html", escape = F) %>%
  kable_styling("striped", full_width = F) 
