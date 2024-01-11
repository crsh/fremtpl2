library("arrow")

freq_data <- read_csv_arrow("./data-raw/freMTPL2freq.csv", quote = "'") |>
  mutate(IDpol = as.character(IDpol))
write_parquet(freq_data, "./data/freMTPL2freq.parquet")

sev_data <- read_csv_arrow("./data-raw/freMTPL2sev.csv") |>
  mutate(IDpol = as.character(IDpol))
write_parquet(sev_data, "./data/freMTPL2sev.parquet")
