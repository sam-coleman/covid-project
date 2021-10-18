url_counties <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"

filename_nyt <- "./nyt_counties.csv"

## Download the data locally
curl::curl_download(
  url_counties,
  destfile = filename_nyt
)

## Loads the downloaded csv
df_covid <- read_csv(filename_nyt)
df_covid

