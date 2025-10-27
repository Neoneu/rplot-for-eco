# Setting
library("ggplot2")
library("dplyr")
library("tidyr")
library("cowplot")
library("ggridges")
library("ggbeeswarm")
#library("ggradar")
library("fmsb")
library("tidyverse")
library("lubridate")

mytheme <- theme(
  plot.title=element_text(face="bold.italic", size="14",
                          color="brown"), #指定图的标题应该为粗斜体棕色14号
  axis.title=element_text(face="bold.italic", size=10,
                          color="brown"),#轴的标题为粗斜体的棕色10
  axis.text=element_text(face="bold", size=9, 
                         color="darkblue"),#轴标签为粗体的深蓝色9号
  panel.background=element_rect(fill="white",color="darkblue"),#图片区域有白色的填充和深蓝色的边框
  panel.grid.major.y=element_line(color="grey",linetype=1),#主水平网格应该是灰色的实线
  panel.grid.minor.y=element_line(color="grey",linetype=2),#次水平网格应该是灰色的虚线
  panel.grid.minor.x=element_blank(), #垂直网格不输出
  legend.position="right") #图例展示在右部

# Other themes
# mytheme <- theme(
  # plot.title=element_text(face="bold.italic", size="14",
  #                        color="brown"), 
  # axis.title=element_text(face="bold.italic", size=10,
  #                        color="brown"),
  # axis.text=element_text(face="bold", size=9, 
  #                       color="darkblue"),
  # panel.background=element_rect(fill="white",color="darkblue"),
  # panel.grid.major.y=element_line(color="grey",linetype=1),
  # panel.grid.minor.y=element_line(color="grey",linetype=2),
  # panel.grid.minor.x=element_blank(),
  # legend.position="right")

# Other themes
# mytheme <- theme(
# plot.title=element_text(face="bold.italic", size="14",
#                        color="darkblue"), 
# axis.title=element_text(face="bold.italic", size=10,
#                        color="darkblue"),
# axis.text=element_text(face="bold", size=9, 
#                       color="darkblue"),
# panel.background=element_rect(fill="white",color="darkblue"),
# panel.grid.major.y=element_line(color="grey",linetype=1),
# panel.grid.minor.y=element_line(color="grey",linetype=2),
# panel.grid.minor.x=element_blank(),
# legend.position="right")

# Other themes
# mytheme <- theme(
# plot.title=element_text(face="bold.italic", size="14",
#                        color="brown"), 
# axis.title=element_text(face="bold.italic", size=10,
#                        color="brown"),
# axis.text=element_text(face="bold", size=9, 
#                       color="brown"),
# panel.background=element_rect(fill="white",color="brown"),
# panel.grid.major.y=element_line(color="grey",linetype=1),
# panel.grid.minor.y=element_line(color="grey",linetype=2),
# panel.grid.minor.x=element_blank(),
# legend.position="right")


# CPI
cpi = read.csv("./cpi.csv", header = FALSE, sep=',',  row.names = 1)
names(cpi) = seq(2011,2021,1)
cpi = as.data.frame(t(cpi))
sub = cpi[-12,]

sub%>% 
  mutate(Year = seq(2011,2021,1)) %>%
  pivot_longer(cols = colnames(sub), names_to = "country", values_to = "quantity") %>%
  ggplot(mapping =aes(x = Year, y = quantity, color = country)) +
  geom_line(stat = "identity", linewidth = 0.9)+
  scale_x_continuous(breaks = seq(2011,2021,1))+
  labs(x = 'Years', y = 'Consumer Price Index (2010 = 100)')+
  mytheme

# GDP

GDP = read.csv("./GDP.csv", header = FALSE, sep=',',  row.names = 1)
names(GDP) = seq(2010,2022,1)
GDP = as.data.frame(t(GDP))

sub = GDP[,1:5]
cn = colnames(sub)
sub$Year =  seq(2010,2022,1)
sub = pivot_longer(data = sub, cols = cn, names_to = "country", values_to = "quantity")
ggplot(data = sub, mapping = aes(x = Year, y = quantity, fill = country)) +
  geom_bar(position = "dodge", stat = 'identity') +
  labs(x = 'Years', y = 'GDP (trillion dollar)') +
  scale_x_continuous(breaks = seq(2010,2022,1)) +
  mytheme

# GDPgrowth

GDPg = read.csv("./GDPgrow.csv", header = FALSE, sep=',',  row.names = 1)
names(GDPg) = seq(2010,2022,1)
GDPg = as.data.frame(t(GDPg))

sub = GDPg[,7:12]
cn = colnames(sub)
sub$Year =  seq(2010,2022,1)
sub = pivot_longer(data = sub, cols = cn, names_to = "country", values_to = "quantity")
sub %>%
  ggplot(aes(x = Year, y=quantity, fill = country, color = country)) +
  geom_density(stat = 'identity') + 
  scale_x_continuous(breaks = seq(2010,2022,3))+
  labs(x = 'Years', y = 'GDP growth(annual %)')+
  facet_wrap(vars(country)) + 
  mytheme + 
  theme(strip.text = element_text(face="bold", size=14, color="darkblue", family = "serif")) +
  theme(legend.position = "none")

sub = GDPg[,13:15]
cn = colnames(sub)
sub$Year =  seq(2010,2022,1)
sub = pivot_longer(data = sub, cols = cn, names_to = "country", values_to = "quantity")
p=sub %>%
  ggplot(aes(x = Year, y=quantity, fill = country, color = country)) +
  geom_density(stat = 'identity') + 
  scale_x_continuous(breaks = seq(2010,2022,3))+
  labs(x = 'Years', y = 'GDP growth(annual %)')+
  facet_wrap(vars(country)) + 
  mytheme + 
  theme(strip.text = element_text(face="bold", size=14, color="darkblue", family = "serif")) +
  theme(legend.position = "none")
  
plot_grid(nrow = 2,p)

# GDPper

GDPper = read.csv("./Data/GDPper.csv", header = FALSE, sep=',',  row.names = 1)
names(GDPper) = seq(2010,2022,1)
GDPper = as.data.frame(t(GDPper))

sub = GDPper[,1:5]
cn = colnames(sub)
sub$Year =  seq(2010,2022,1)
sub = pivot_longer(data = sub, cols = cn, names_to = "country", values_to = "quantity")
ggplot(data = sub, mapping = aes(x = Year, y = quantity, fill = country)) +
geom_bar(position = "dodge", stat = 'identity') +
labs(x = 'Years', y = 'GDP per capita(current dollar)') +
scale_x_continuous(breaks = seq(2010,2022,1)) +
mytheme

# GDPpergrowth

GDPper = read.csv("./Data/GDPpergrow.csv", header = FALSE, sep=',',  row.names = 1)
names(GDPper) = seq(2010,2022,1)
GDPper = as.data.frame(t(GDPper))

sub = GDPper[,13:15]
cn = colnames(sub)
sub$Year =  seq(2010,2022,1)
sub = pivot_longer(data = sub, cols = cn, names_to = "country", values_to = "quantity")
p = sub %>%
  ggplot(aes(x = Year, y=quantity, fill = country, color = country)) +
  geom_density(stat = 'identity') + 
  scale_x_continuous(breaks = seq(2010,2022,3))+
  labs(x = 'Years', y = 'GDP per capita growth(annual %)')+
  facet_wrap(vars(country)) + 
  mytheme + 
  theme(strip.text = element_text(face="bold", size=14, color="darkblue", family = "serif")) +
  theme(legend.position = "none")

# Export

ex= read.csv("./ex.csv", header = FALSE, sep=',',row.names = 1)
names(ex) = seq(2010,2022,1)
ex = as.data.frame(t(ex))

sub = ex[2:7,]
cn = colnames(sub)
sub$Year =  seq(2011,2016,1)
sub = pivot_longer(data = sub, cols = cn, names_to = "country", values_to = "quantity")
ggplot(data = sub, mapping = aes(x="", y = quantity, fill = country))+
geom_bar(stat = "identity", width = 1) +
coord_polar(theta = "y") +
labs(x = "", y = "", title = "") +
theme(axis.ticks = element_blank()) +
theme(axis.text.x = element_blank()) +
scale_fill_discrete(breaks = sub$country) +
facet_wrap(Year~., nrow = 2) +
theme(strip.text = element_text(face="bold", size=14, color="darkblue")) +
mytheme

# high-tech export
he = read.csv("./highex.csv",header = FALSE, sep = ",",row.names = 1)
names(he) = seq(2010,2022,1)
he = as.data.frame(t(he))
he = he/100
he = he[-13,]

he_new = rbind(rep(0.45,15),rep(0.05,15),he[7:12,])

stars(he, nrow = 3, ncol = 4,
      full = T,               # 画半圆还是画圆
      draw.segments = F,      # 是否画成区域图
      key.loc = c(-0.8,5),      # 在(-1, 5)的位置放置参考图
      cex = 1)                # 标签大小   

radarchart(
  he_new, axistype = 1,
  # Customize the polygon
  pcol = c("#6EA2E6","#025259","#007101","#E6b318","#F29325","#D94F04"), plwd = 2, plty = 1,
  # Customize the grid
  cglcol = "grey", cglty = 1, cglwd = 0.8,
  # Customize the axis
  axislabcol = "black", 
  # Variable labels
  vlcex = 0.5, vlabels = colnames(he_new),
  caxislabels = c("5","15","25","35","45(%)")
  )
# Add an horizontal legend
legend(
  x = -0.9,y = 1.37,legend = rownames(he_new)[3:8], horiz = TRUE,
  bty = "n", pch = 20 , col = c("#6EA2E6","#025259","#007101","#E6b318","#F29325","#D94F04"),
  text.col = "black", cex = 0.5, pt.cex = 1
)

# R&D
re = read.csv("./re.csv", header = FALSE, sep = ',', row.names = 1)
names(re) = seq(2010,2017,1)
re = as.data.frame(t(re))
cn = colnames(re)
re$Year = seq(2010,2017,1)
re = pivot_longer(data = re, cols = cn, names_to = "country", values_to = "quantity")
re$gender = rep("male", length(re$Year))
re$quantity = re$quantity%/%500
re_new = data.frame(0,0,0,0)
names(re_new) = c("Year","country","quantity","gender")
for (i in 1:length(re$Year)) {
  j=1
  while (j<=re[i,3]) {
    re_new = rbind(re_new,re[i,])
    j=j+1
  }
}
re_new = re_new[-1,]

re_new %>%
  ggplot(aes(
    x = country,
    y = Year,
    colour = country,
    alpha = country
  )) +
  ggbeeswarm::geom_beeswarm() +
  coord_flip() +
  theme_minimal() +
  scale_y_continuous(breaks = seq(2010,2017,1)) +
  labs(
    title = "Researchers in R&D (per two thousand people)",
    colour = "country",
    alpha = "country",
    y = "Years"
  ) +
  mytheme

# nobel
df <- read_csv("./nobel_winners.csv")

nobel_winners <- df %>% # 去重
  mutate_if(is.character, tolower) %>%
  distinct_at(vars(full_name, prize_year, category), .keep_all = TRUE) %>%
  mutate(
    decade = 10 * (prize_year %/% 10),
    prize_age = prize_year - year(birth_date)
  ) 

nobel_winners_clean <- nobel_winners %>%  # 出生地与工作地
  mutate_at(
    vars(birth_country, death_country),
    ~ ifelse(str_detect(., "\\("), str_extract(., "(?<=\\().*?(?=\\))"), .)
  ) %>%
  mutate_at(
    vars(birth_country, death_country),
    ~ case_when(
      . == "scotland" ~ "united kingdom",
      . == "northern ireland" ~ "united kingdom",
      str_detect(., "czech") ~ "czechia",
      str_detect(., "germany") ~ "germany",
      TRUE ~ .
    )
  ) %>%
  select(full_name, prize_year, category, birth_date, birth_country, gender, organization_name, organization_country, death_country)

nobel_winners_clean %>% # 迁徙
  filter(prize_year >2000) %>%
  mutate(
    colour = case_when(
      organization_country == "united states of america" ~ "#FF2B4F",
      organization_country == "germany" ~ "#fcab27",
      organization_country == "united kingdom" ~ "#3686d3",
      organization_country == "france" ~ "#88398a",
      organization_country == "israel" ~ "#20d4bc",
      organization_country == "japan" ~ "brown",
      TRUE ~ "gray60"
    )
  ) %>%
  ggplot(aes(
    x = 0,
    y = fct_rev(factor(birth_country)),
    xend = organization_country,
    yend = 1,
    colour = colour,
    alpha = (colour != "gray60")
  )) +
  geom_curve(
    curvature = -0.5,
    arrow = arrow(length = unit(0.01, "npc"))
  ) +
  scale_x_discrete() +
  scale_y_discrete() +
  scale_color_identity() +
  scale_alpha_manual(values = c(0.1, 0.2), guide = F) +
  scale_size_manual(values = c(0.1, 0.4), guide = F) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#F0EFF1", colour = "#F0EFF1"),
    legend.position = "none",
    axis.text.x = element_text(angle = 40, hjust = 1),
    axis.title=element_text(face="bold.italic", size=10)
  ) +
  labs(x = "Country of organization", y = "Country of birth")

# unemployment (gender)
un = read.csv("./un.csv",header = FALSE, sep=',')
names(un) = c("country",seq(2011,2022,1))
un %>%
  mutate(gender = c("male","female","male","female","male","female","male","female",
                    "male","female","male","female","male","female","male","female",
                    "male","female","male","female","male","female","male","female","male","female"))%>%
  pivot_longer(cols = colnames(un)[-1], names_to = "Year", values_to = "quantity") %>%
  ggplot(aes(x = Year, y = country, fill = quantity)) +
  geom_tile(size = 0.7) +
  # geom_text(aes(label = scales::percent(prop, accuracy = .01))) +
  geom_text(aes(label = scales::number(quantity, accuracy = .01))) +
  facet_grid(vars(gender)) +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  mytheme +
  theme(strip.text = element_text(face="bold", size=14, color="darkblue"))+
  labs(title = "Unempolyment for male and female",
       x = "Years",
       y = "country")

# unemployment (edu)
un2 = read.csv("./un2.csv",header = FALSE, sep=',')
names(un2) = c("country",seq(2011,2022,1),"edu")
un2 %>%
  pivot_longer(cols = colnames(.)[-c(1,14)], names_to = "Year", values_to = "quantity") %>%
  ggplot(aes(x = Year, y = country, fill = quantity)) +
  geom_tile(size = 0.7) +
  # geom_text(aes(label = scales::percent(prop, accuracy = .01))) +
  geom_text(aes(label = scales::number(quantity, accuracy = .01))) +
  facet_grid(vars(edu)) +
  scale_fill_gradient(low = "#FDF4E9", high = "brown") +
  mytheme +
  theme(strip.text = element_text(face="bold", size=14, color="brown"))+
  labs(title = "Unempolyment for people of different educational background",
       x = "Years",
       y = "country")

# CO2
library("here")
library("sf")
library("countrycode")

# countrycode('Albania', 'country.name', 'iso3c')

co2per = read.csv("./co2.csv", header = TRUE, sep = ',')

co2iso <- co2per %>%
  filter(!is.na(Country)) %>%
  mutate(ISO3 = countrycode(Country,
                            origin = "country.name", destination = "iso3c"
  ))


global <-
  sf::st_read("./TM_WORLD_BORDERS_SIMPL-0.3.shp") %>%
  st_transform(4326)

global %>%
  full_join(co2iso, by = "ISO3") %>%
  ggplot() +
  geom_sf(aes(fill = n),
          color = "white",
          size = 0.1
  ) +
  labs(
    x = NULL, y = NULL,
    title = "CO2 emissions around the world",
    subtitle = expression(paste("Colors of map indicate ", CO[2], " emissions (metric tons per capita)",
                                " and grey means no data available.")),
    fill = expression(paste(CO[2], " emissions")),
    caption = "Data from 2019"
  ) +
  scale_fill_gradientn(colors = c("royalblue1", "magenta", "orange", "gold"), na.value = "white") +
   scale_fill_gradient(low = "wheat1", high = "red") +
  theme_void() +
  theme(
    legend.position = c(0.1, 0.3),
    plot.background = element_rect(fill = "gray"),
    plot.title=element_text(face="bold.italic", size="14")
  )

# PM2.5
pm = read.csv("./pm.csv", header = TRUE, sep = ',')

pmiso <- pm %>%
  filter(!is.na(Country)) %>%
  mutate(ISO3 = countrycode(Country,
                            origin = "country.name", destination = "iso3c"
  ))

global %>%
  full_join(pmiso, by = "ISO3") %>%
  ggplot() +
  geom_sf(aes(fill = n),
          color = "white",
          size = 0.1
  ) +
  labs(
    x = NULL, y = NULL,
    title = "PM2.5 exposure around the world",
    subtitle = "PM2.5 air pollution, mean annual exposure (micrograms per cubic meter), and white means no data available.",
    fill = "PM2.5 air pollution",
    caption = "Data from 2019"
  ) +
  scale_fill_gradientn(colors = c("royalblue1", "magenta", "orange", "gold"), na.value = "white") +
  #scale_fill_gradient(low = "wheat1", high = "red") +
  theme_void() +
  theme(
    legend.position = c(0.1, 0.3),
    plot.background = element_rect(fill = "gray"),
    plot.title=element_text(face="bold.italic", size="14")
  )

# air passenger
air1 = read.csv("./air1.csv", header = TRUE, sep = ',')
air1 = air1[,1:4]

a1 <- air1 %>%
  filter(!is.na(long)) %>%
  filter(!is.na(n)) %>%
  filter(!is.na(lat)) %>%
  filter(n >=10000) %>%
  mutate(ISO3 = countrycode(Country,
                            origin = "country.name", destination = "iso3c"
  ))

world <- map_data("world")

ggplot() +
  geom_polygon(
    data = world,
    aes(x = long, y = lat, group = group),
    fill = "grey", alpha = 0.3
  ) +
  geom_point(
    data = a1,
    aes(x = long, y = lat, size = n, color = n),
    stroke = F, alpha = 0.7
  ) +
  scale_size_continuous(
    name = "Numbers", trans = "log",
    range = c(1, 9),
    breaks = c(10000, 100000, 1000000, 10000000, 100000000),
    labels = c("10,000-99,999" ,"100,000-999,999", "1,000,000-9,999,999","10,000,000-99,999,999","100,000,000+")
  ) +
  scale_color_viridis_c(
    option = "inferno",
    name = "Numbers",
    trans = "log",
    breaks = c(10000, 100000, 1000000, 10000000, 100000000),
    labels = c("10,000-99,999" ,"100,000-999,999", "1,000,000-9,999,999","10,000,000-99,999,999","100,000,000+")
  ) +
  theme_void() +
  guides(colour = guide_legend()) +
  labs(
    title = "Air transport, passengers carried",
    subtitle = "",
    caption = "Data from 2019"
  ) +
  theme(
    legend.position = "bottom",
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#ffffff", color = NA),
    panel.background = element_rect(fill = "#ffffff", color = NA),
    legend.background = element_rect(fill = "#ffffff", color = NA),
    plot.title=element_text(face="bold.italic", size="14")
  )

# air freight
air2 = read.csv("./air2.csv", header = TRUE, sep = ',')
air2 = air2[,1:4]

a2 <- air2 %>%
  filter(!is.na(long)) %>%
  filter(!is.na(n)) %>%
  filter(!is.na(lat)) %>%
  mutate(ISO3 = countrycode(Country,
                            origin = "country.name", destination = "iso3c"
  ))

world <- map_data("world")

ggplot() +
  geom_polygon(
    data = world,
    aes(x = long, y = lat, group = group),
    fill = "grey", alpha = 0.3
  ) +
  geom_point(
    data = a2,
    aes(x = long, y = lat, size = n, color = n),
    stroke = F, alpha = 0.7
  ) +
  scale_size_continuous(
    name = "Size", trans = "log",
    range = c(1, 9),
    breaks = c(1, 10, 100, 1000, 10000),
    labels = c("1-9","10-99" ,"100-999", "1,000-9,999","10,000+")
  ) +
  scale_color_viridis_c(
    option = "viridis", # 渐变主题
    name = "Size",
    trans = "log",
    breaks = c(1, 10, 100, 1000, 10000),
    labels = c("1-9","10-99" ,"100-999", "1,000-9,999","10,000+")
  ) +
  theme_void() +
  guides(colour = guide_legend()) +
  labs(
    title = "Air transport, freight (million ton-km)",
    subtitle = "",
    caption = "Data from 2019"
  ) +
  theme(
    legend.position = "bottom",
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#ffffff", color = NA),
    panel.background = element_rect(fill = "#ffffff", color = NA),
    legend.background = element_rect(fill = "#ffffff", color = NA),
    plot.title=element_text(face="bold.italic", size="14")
  )

# population
po = read.csv("./pop.csv",header = TRUE, sep = ',')

ggplot(po, aes(x = age, y = ifelse(gender == "female", -rate, rate), fill = country)) + 
  geom_bar(stat = "identity") + 
  geom_hline(aes(yintercept=0), alpha = 0.15) +
  scale_y_continuous(labels = abs, limits = c(-10,10)) +
  xlab("Age") + ylab("Population (%)") + 
  coord_flip() +
  facet_wrap(country~., nrow = 3) +
  theme(strip.text = element_text(face="bold", size=9, color="black")) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))+
  theme(axis.text.y = element_text(size = 7))+
  mytheme

# population rate

pr = read.csv("./pr.csv", header = FALSE, sep = ',', row.names = 1)
names(pr) = seq(2010,2021,1)
pr = as.data.frame(t(pr))

pr %>%
  mutate(Year = seq(2010,2021,1)) %>%
  pivot_longer(cols = names(pr), names_to = "country", values_to = "quantity") %>%
  ggplot(aes(x = country, y = quantity))+
  geom_boxplot(aes(fill = country, color = country)) +
  mytheme