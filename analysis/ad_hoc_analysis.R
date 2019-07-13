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
