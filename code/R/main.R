FORCE_CACHE <- FALSE

# Exploration: cross-check AiMark barcodes against Open Food Facts UK coverage
# Quality threshold: 60/100 (exploration). Not part of the pipeline.

# Setup ----
source(here::here("code", "R", "00-setup.R"))
source(here::here("code", "R", "01-clean_data.R"))
source(here::here("code", "R", "utils", "01-clean_data.R"))

# Load Open Food Facts + filter to UK ----
data_openfoodfacts <- extract_openfoodfacts_data(dump_path = "data/raw data/openfoodfacts/openfoodfacts-mongodbdump",
    fields = c("countries", "code", "_id", "ean", "upc"),
    db_name = "off",
    collection_name = "products",
    mongo_url = "mongodb://localhost:27018",
    verbose = TRUE)

