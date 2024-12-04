library(tidyverse)

# Carreguem les dades
hotel_data <- read.csv("hotel_bookings.csv")

# Agrupem per país i comptem els registres (files) per país
country_count <- hotel_data %>%
  group_by(country) %>%  # Agrupem per país
  summarise(total_records = n()) %>%  # Comptem el nombre de files per país
  arrange(desc(total_records))  # Ordenem per nombre de files de manera descendent

# Agafem els 10 primers països amb més reserves
top_10_countries <- country_count %>%
  head(10)  # Els 10 primers països amb més registres

# Filtrem les dades de hotel_data per només els 10 primers països
hotel_data_filtered <- hotel_data %>%
  filter(country %in% top_10_countries$country)

# Agrupem per país i tipus d'hotel per calcular percentatge de cancel·lacions (sense tenir en compte estacions)
country_summary <- hotel_data_filtered %>%
  group_by(hotel, country) %>%  # Agrupem per país i tipus d'hotel
  summarise(
    total_bookings_country = n(),
    canceled_bookings_country = sum(is_canceled == 1)
  ) %>%
  mutate(
    cancel_rate_country = (canceled_bookings_country / total_bookings_country) * 100
  )

# Guardem el nou dataset amb els totals globals per país i tipus d'hotel
write.csv(country_summary, "country_summary_totals_top_10_countries_by_hotel.csv", row.names = FALSE)