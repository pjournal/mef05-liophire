library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)

df <- read_delim("balik_hal_fiyatlari.csv", delim = ";")

df_no_outlier <- df %>%
  group_by(MAL_ADI) %>%
  mutate(FARK = AZAMI_UCRET - ASGARI_UCRET,
         IQR_FARK = IQR(FARK),
         UPPER = quantile(FARK, 0.75) + IQR_FARK * 1.5,
         LOWER = quantile(FARK, 0.25) - IQR_FARK * 1.5) %>%
  filter(FARK <= UPPER & FARK >= LOWER) %>%
  ungroup()

df_outliers <- df %>%
  group_by(MAL_ADI) %>%
  transmute(AZAMI_UCRET, ASGARI_UCRET, FARK = AZAMI_UCRET - ASGARI_UCRET,
            IQR_FARK = IQR(FARK),
            UPPER = quantile(FARK, 0.75) + IQR_FARK * 1.5,
            LOWER = quantile(FARK, 0.25) - IQR_FARK * 1.5) %>%
  filter(!(FARK <= UPPER & FARK >= LOWER))

yasak_fark <- df_no_outlier %>%
  filter(TARIH >= "2021-04-15" & TARIH <= "2021-08-31") %>%
  group_by(MAL_ADI) %>%
  summarise(FARK = max(AZAMI_UCRET - ASGARI_UCRET))
serbest_fark <- df_no_outlier %>%
  filter(!(TARIH >= "2021-04-15" & TARIH <= "2021-08-31")) %>%
  group_by(MAL_ADI) %>%
  summarise(FARK = max(AZAMI_UCRET - ASGARI_UCRET))

farklar <- serbest_fark %>%
  inner_join(yasak_fark, by = "MAL_ADI", suffix = c("_SERBEST", "_YASAK")) %>%
  mutate(DONEM_FARK = FARK_SERBEST - FARK_YASAK) %>%
  arrange(desc(DONEM_FARK))

first_n_last <- bind_rows(head(farklar), tail(farklar)) %>%
  mutate(MAL_ADI = fct_reorder(MAL_ADI, DONEM_FARK))

ggplot(first_n_last, aes(x = MAL_ADI, y = DONEM_FARK, fill = MAL_ADI)) +
  geom_col() +
  geom_text(aes(label=DONEM_FARK), size = 3) +
  coord_flip() +
  ggtitle("Change in the price differences") +
  xlab("Fish Name") + ylab("Difference(TL)") +
  theme(legend.position = "none")