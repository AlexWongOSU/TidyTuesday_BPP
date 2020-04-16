library(Lahman)
library(gganimate)
library(dplyr)
library(reshape2)
library(tidyr)

data(Salaries)
data(People)

Richest.Players<- Salaries%>%
  group_by(yearID)%>%
  mutate(rank = rank(-salary, ties.method = "first")*1)%>%
  top_n(-30)

Richest.Players.withnames<- merge(Richest.Players, People, c("playerID"))

theme_set(theme_classic())

plot<- ggplot(Richest.Players.withnames, aes(x=rank, group = playerID, fill = as.factor(playerID)))+
  geom_tile(aes(y = salary/2,
            height = salary,
            width = 0.9), alpha = 0.8, color = NA)+
  ggtitle('{closest_state}')+
  geom_text(aes(y = 0, label = paste(nameLast, " ")), vjust = 0.2, hjust = 1)+
  geom_text(aes(y = salary, label = paste(scales::comma(salary))), hjust = 0, nudge_y = 300)+
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse()+
  guides(color = FALSE, fill = FALSE) +
  theme(axis.ticks.y = element_blank(),  # These relate to the axes post-flip
        axis.text.y  = element_blank(),
        legend.position = "none",
        plot.margin = margin(1,4,1,4, "cm"))+
  transition_states(yearID,
                    state_length = 2)+
  ease_aes("cubic-in-out")+
  xlab("")+
  ylab("")

animate(plot, fps = 25, duration = 35, width = 800, height = 600, end_pause = 10)