library(tidyverse)

# Carreguem les dades
hotel_data <- read.csv("hotel_bookings.csv")

# Creem columna per l'estació
hotel_data <- hotel_data %>%
  mutate(
    season = case_when(
      arrival_date_month %in% c("December", "January", "February") ~ "Winter",
      arrival_date_month %in% c("March", "April", "May") ~ "Spring",
      arrival_date_month %in% c("June", "July", "August") ~ "Summer",
      TRUE ~ "Autumn"
    )
  )

# Agrupem per país i comptem els registres (files) per país
country_count <- hotel_data %>%
  group_by(country) %>%  # Agrupem per país
  summarise(total_records = n()) %>%  # Comptem el nombre de files per país
  arrange(desc(total_records))  # Ordenem per nombre de files de manera descendent

# Agafem els 10 primers països
top_10_countries <- country_count %>%
  head(10)  # Els 10 primers països amb més registres

# Filtrem les dades de hotel_data per només els 10 primers països
hotel_data_filtered <- hotel_data %>%
  filter(country %in% top_10_countries$country)

# Agrupem per país, estació i tipus d'hotel per calcular percentatge de cancel·lacions
cancel_summary <- hotel_data_filtered %>%
  group_by(hotel, country, season) %>%
  summarise(
    total_bookings = n(),
    canceled_bookings = sum(is_canceled == 1),
    cancel_rate = (canceled_bookings / total_bookings) * 100
  ) %>%
  arrange(desc(cancel_rate))

# Guardem el dataset filtrat
write.csv(cancel_summary, "cancel_summary_top_10_countries.csv", row.names = FALSE)