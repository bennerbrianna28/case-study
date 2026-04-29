data(penguins)
summary(penguins)

# get the data for the app
dir.create("neiss")
#> Warning in dir.create("neiss"): 'neiss' already exists
download <- function(name) {
  url <- "https://raw.github.com/hadley/mastering-shiny/main/neiss/"
  download.file(paste0(url, name), paste0("neiss/", name), quiet = TRUE)
}
download("injuries.tsv.gz")
download("population.tsv")
download("products.tsv")

injuries <- vroom::vroom("neiss/injuries.tsv.gz")
injuries

products <- vroom::vroom("neiss/products.tsv")
products

population <- vroom::vroom("neiss/population.tsv")
population
