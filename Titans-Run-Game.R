library(tidyverse)
library(ggplot2)
library(ggthemes)
library(nflfastR)
library(nflreadr)
library(gridExtra)


###########################################################################
#############################  Custom 538 theme ################################

theme_538 <- function(base_size = 12, font = "Lato") {
  
  # Text setting
  txt <- element_text(size = base_size + 2, colour = "black", face = "plain")
  bold_txt <- element_text(
    size = base_size + 2, colour = "black",
    family = "Montserrat", face = "bold"
  )
  large_txt <- element_text(size = base_size + 4, color = "black", face = "bold")
  
  
  theme_minimal(base_size = base_size, base_family = font) +
    theme(
      # Legend Settings
      legend.key = element_blank(),
      legend.background = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      
      # Backgrounds
      strip.background = element_blank(),
      strip.text = large_txt,
      plot.background = element_blank(),
      plot.margin = unit(c(1, 1, 1, 1), "lines"),
      
      # Axis & Titles
      text = txt,
      axis.text = txt,
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      axis.title = bold_txt,
      plot.title = large_txt,
      
      # Panel
      panel.grid = element_line(colour = NULL),
      panel.grid.major = element_line(colour = "#D2D2D2"),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank()
    )
}


##########################################################################
data <- load_pbp(2021)

# Titans colors for graph aesthetics
ten_colors <- nflfastR::teams_colors_logos %>% 
  filter(team_abbr == "TEN")

# Rushing stats by all rushers
data %>% filter(rush == 1, !is.na(epa), posteam == "TEN", down <=4) %>% 
  group_by(rusher) %>% 
  summarise(epa = mean(epa), success_rate = mean(success), 
            ypc = mean(yards_gained), plays = n()) %>%
  filter(plays >=4) %>% 
  head(15)



##########################################################################
######################  Get Titans Rushing EPA by Week   ################## 
data %>% filter(rush == 1, !is.na(epa), posteam =="TEN", down <=4) %>% 
  group_by(week) %>% 
  summarise(mean_epa = mean(epa), success_rate = mean(success)) %>% 
  ggplot(aes(week, mean_epa))+
  geom_point(size=3, color = "darkblue")+
  geom_line(color = "lightblue", size = 1, alpha= 0.8)+
  geom_hline(yintercept = 0, alpha = 0.7, linetype ="dashed", color ="red")+
  geom_vline(xintercept = 9, linetype = "dashed", color = "red")+
  scale_x_continuous(breaks = seq(1,17,1))+
  scale_y_continuous(limits = c(-0.35,0.35),breaks = seq(-0.35,0.35,0.1))+
  ggthemes::theme_fivethirtyeight()+
  theme(plot.title = element_text(hjust = 0.5, color ="red"))+
  labs(title = "Titans Average  Rush EPA Per Week",
       x = "Week",
       y = "Average EPA",
       caption = "plot:@_ThomasT; data:nflfastR")+
  geom_label(label = "Henry IR",
             x = 9,
             y = 0.025, label.size = 1)



##########################################################################
#################Get Rushing Stats for 1st & 2nd half of season############

data %>% filter(week >=1, week <=8, rush == 1, !is.na(epa),
                posteam =="TEN", down<=4) %>%
  summarise(mean_epa = mean(epa), success_rate = mean(success),
            ypc=mean(yards_gained)) %>% 
  head(10)

data %>% filter(week >=9, week <=17, rush == 1, !is.na(epa),
                posteam =="TEN", down<=4) %>%
  summarise(mean_epa = mean(epa), success_rate = mean(success), 
            ypc=mean(yards_gained)) %>% 
  head(10)


##########################################################################
##############GEt EPA, Success rate & YPC by RB and plot together #########


z1 <- data %>% filter(posteam == "TEN", down<=4, !is.na(epa), rush ==1) %>% 
  group_by(rusher) %>% 
  summarise(mean_epa = mean(epa), success_rate = mean(success), ypc = mean(yards_gained),
            plays = n()) %>% 
  filter(plays>42) %>% 
  mutate('Average EPA' = mean_epa , 'Success Rate' = success_rate,
         'Yards Per Carry' = ypc) %>% 
  ggplot(aes( x= rusher,y = `Average EPA`))+
  geom_col(aes(fill = rusher),position = "dodge")+
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 2)+
  scale_fill_manual(values = c(ten_colors$team_color, 
                               ten_colors$team_color2,
                               ten_colors$team_color3))+
  scale_y_continuous(breaks = seq(-0.2, 0.15,0.02))+
  theme_538()+
  labs(title = "AVerage EPA")+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "white", size = 1),
    panel.ontop = TRUE,
    legend.position = "none",
    plot.title = element_text(hjust = 0.5))


z2 <- data %>% filter(posteam == "TEN", down<=4, !is.na(epa), rush ==1) %>% 
  group_by(rusher) %>% 
  summarise(mean_epa = mean(epa), success_rate = mean(success), ypc = mean(yards_gained),
            plays = n()) %>% 
  filter(plays>42) %>% 
  mutate('Average EPA' = mean_epa , 'Success Rate' = success_rate,
         'Yards Per Carry' = ypc) %>% 
  ggplot(aes( x= rusher,y = `Success Rate`))+
  geom_col(aes(fill=rusher),position = "dodge")+
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 2)+
  scale_fill_manual(values = c(ten_colors$team_color, 
                               ten_colors$team_color2,
                               ten_colors$team_color3))+
  scale_y_continuous(breaks = seq(0,0.4,0.05))+
  theme_538()+
  labs(title = "Success Rate")+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "white", size = 1),
    panel.ontop = TRUE,
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)) 


z3 <- data %>% filter(posteam == "TEN", down<=4, !is.na(epa), rush ==1) %>% 
  group_by(rusher) %>% 
  summarise(mean_epa = mean(epa), success_rate = mean(success), ypc = mean(yards_gained),
            plays = n()) %>% 
  filter(plays>42) %>% 
  mutate('Average EPA' = mean_epa , 'Success Rate' = success_rate,
         'Yards Per Carry' = ypc) %>% 
  ggplot(aes( x= rusher,y = `Yards Per Carry`))+
  geom_col(aes(fill=rusher),position = "dodge")+
  geom_hline(yintercept = 0, linetype = "solid", color ="red", size =2)+
  scale_fill_manual(values = c(ten_colors$team_color, ten_colors$team_color2, ten_colors$team_color3))+
  scale_y_continuous(breaks = seq(0,6.5,0.5))+
  theme_538()+
  labs(title = "Yards Per Carry")+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "white", size = 1),
    panel.ontop = TRUE,
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)) 



grid.arrange(z1, z2, z3, ncol =3)



