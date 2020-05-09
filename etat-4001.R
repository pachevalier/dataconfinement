library(tidyverse)
library(wesanderson)
library(ggthemes)
library(tricky)
library(tibbletime)
library(lubridate)
pal <- wes_palette("Zissou1", 21, type = "continuous")
table_fm <- read_csv(file = "data-raw/etat_4001_france_metropolitaine.csv") %>%
    mutate(
      date = ymd(paste0(date, "-01"))
      )
table_fm %>% glimpse()
table_fe <- read_csv(file = "data-raw/etat_4001_france_entiere.csv") %>%
  mutate(
    date = ymd(paste0(date, "-01"))
  )
table_index <- table_fm %>%
  distinct(index, index_libelle)

# table_departements <- read_csv(file = "data-raw/etat_4001_departements.csv.gz") %>%
#   mutate(
#     date = ymd(paste0(date, "-01"))
#   )

table_fm %>%
  filter(year(date) == 2019) %>%
  group_by(index, index_libelle) %>%
  summarise(n = sum(n)) %>%
  arrange(desc(n))

table_fm %>% 
  as_tbl_time(index = date) %>%
  arrange(date) %>%
  group_by(index, index_libelle) %>%
  mutate(lag12 = roll_lag12(n)) %>%
  filter(index == 27, year(date) >= 2019) %>%
  select(index, date, n, lag12)

table_effects <- table_fm %>% 
  arrange(date) %>%
  group_by(index, index_libelle) %>%
  mutate(
    lag12 = lag(n, n = 12, order_by = date),
    diff12 = n - lag12, 
    growth12 = (n - lag12) / lag12
    ) 
table_effects %>%
  filter(index == 27, year(date) >= 2019, month(date) %in% c(1:3))

table_effects %>%
  filter(year(date) == 2020, month(date) == 3, lag12 >= 5000) %>%
  ggplot() + 
  geom_col(
    mapping = aes(x = reorder(index_libelle, -growth12), y = growth12), 
    width = .7
      ) + 
  coord_flip() + 
  theme_tufte() + 
  scale_x_discrete(name = "Infraction") + 
  scale_y_continuous(
    name = "Evolution sur 12 mois", 
    labels = scales::percent_format(accuracy = 1)
    ) + 
  labs(
    title = "Evolution des infractions entre mars 2019 et mars 2020",
       subtitle = "Infractions avec plus de 5 000 actes en mars 2019, France métropolitaine",
       caption = "Source : Ministère de l'Intérieur, chiffres mensuels \nhttps://www.data.gouv.fr/fr/datasets/5617ad4dc751df6211cdbb49/\nversion du 17 avril 2020"
    ) 



table_effects %>% 
  filter(month(date) %in% 1:3, index == 27, year(date) >= 2000) %>%
  mutate(month = month(date), year = year(date)) %>%
  select(year, month, growth12) %>%
  pivot_wider(names_from = month, values_from = growth12, names_prefix = "month")




table_effects %>% 
  filter(month(date) %in% 1:3, index == 27) %>%
  ggplot() +
  geom_line(
    mapping = aes(x = year(date), y = growth12, group = month(date), colour = as.factor(month(date)))
      ) + 
  scale_x_continuous(name = 'Année') + 
  scale_colour_manual(name = "Mois", values = wes_palette("FantasticFox1")) +
  scale_y_continuous(name = "Taux de croissance sur 12 mois") + 
  theme_tufte()


# table_effects <- table_fm %>% 
#   group_by(index, index_libelle, month = month(date)) %>%
#   arrange(year(date)) %>%
#   mutate(
#     lag12 = lag(n),
#     diff12 = n - lag(n), 
#     growth12 = (n - lag(n)) / lag(n)
#     ) 
# table_effects %>%
#   filter(year(date) >= 2019, month %in% c(1,2,3), index == 27) %>%
#   arrange(month)

table_effects %>%
  filter(month == 3, year(date) == 2020, lag12 > 100) %>%
  arrange(growth12)

impactcovid <- function(data, year, month) {
  filter(.data = data, 
         year(date) == year, 
         month(date) == month) 
  }
impactcovid(data = table_fm, year = 2020, month = 3)

table_fm %>%
  filter(index == 27) %>%
  group_by(year(date)) %>%
  summarise(n = sum(n)) %>%
  View()



table_fm %>%
  distinct(index, index_libelle) %>%
  filter(index == 27)

table_fm %>% filter(month(date) == 3, year(date) >= 2019, index == 27)

table_fm %>%  
  filter(index == 27) %>%
  mutate(diff12 = rollify(.f = , window = 12))

rolling_mean <- rollify(mean, window = 5)

table_fm %>%
  filter(index == 27) %>%
  ggplot() + 
    geom_line(
      mapping = aes(x = month(date), y = n)
      ) + 
  facet_wrap(~ year(date)) + 
  coord_polar() + 
  scale_x_continuous(breaks = 1:12) + 
  theme_tufte()

table_fm %>%
  filter(index == 27) %>%
  ggplot() + 
  geom_col(
    mapping = aes(x = month(date), y = n, fill = n),
    color = "white"
  ) + 
  facet_wrap(~ year(date)) + 
  coord_polar() + 
  scale_fill_gradientn(colours = pal) + 
  scale_x_continuous(breaks = 1:12) + 
  theme_tufte()

## Line plot 
ggplot() + 
  geom_line(
    data = filter(table_fm, index == 27, year(date) == 2020), 
    mapping = aes(x = month(date), y = n), 
    color= "firebrick1"
  ) +
  geom_line(
    data = filter(table_fm, index == 27, year(date) > 2000, year(date) < 2020), 
    mapping = aes(x = month(date), y = n, group = year(date)), 
    size = .1, alpha = .5
  ) + 
  scale_y_continuous(
    limits = c(0, NA), 
    name = "Cambriolages de locaux d'habitations principales",
    labels = french_formatting) +
  scale_x_continuous(
    name = "Mois de l'année",
    breaks = 1:12
  ) + 
  labs(title = "Nombre quotidien de cambriolages de résidences principales, France métropolitaine, 2001-2020",
       subtitle = "En rouge, l'année 2020, en gris les années 2001 à 2019.",
       caption = "Source : Ministère de l'Intérieur, chiffres mensuels \nhttps://www.data.gouv.fr/fr/datasets/5617ad4dc751df6211cdbb49/\nversion du 17 avril 2020") +
  theme_tufte() 

ggsave(
  filename = "cambriolages.png",
  width=1100/120, height=700/120, dpi=120
)


## Line plot 
ggplot() + 
  geom_line(
    data = filter(table_fm, index == 28, year(date) == 2020), 
    mapping = aes(x = month(date), y = n), 
    color= "firebrick1"
  ) +
  geom_line(
    data = filter(table_fm, index == 28, year(date) > 2000, year(date) < 2020), 
    mapping = aes(x = month(date), y = n, group = year(date)), 
    size = .1, alpha = .5
  ) + 
  scale_y_continuous(
    limits = c(0, NA), 
    name = "Cambriolages de résidences secondaires",
    labels = french_formatting) +
  scale_x_continuous(
    name = "Mois de l'année",
    breaks = 1:12
  ) + 
  labs(title = "Nombre quotidien de cambriolages de résidences secondaires, France métropolitaine, 2001-2020",
       subtitle = "En rouge, l'année 2020, en gris les années 2001 à 2019.",
       caption = "Source : Ministère de l'Intérieur, chiffres mensuels \nhttps://www.data.gouv.fr/fr/datasets/5617ad4dc751df6211cdbb49/\nversion du 17 avril 2020") +
  theme_tufte() 
