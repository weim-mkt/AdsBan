# Load libraries -------------------------------------------------------------
packages <- c(
    "data.table",
    "sf",
    "nngeo",
    "lwgeom",
    "foreach",
    "doSNOW",
    "dplyr",
    "lubridate",
    "stringr",
    "fst",
    "haven",
    "readxl",
    "xlsx",
    "gmodels",
    "sp",
    "geosphere",
    "fasttime",
    "parallel",
    "formattable",
    "ggplot2",
    "ggthemes",
    "fixest",
    "ggpubr",
    "modelsummary",
    "progress",
    "benchmarkme",
    "zoo",
    "geodist",
    "birk",
    "jsonlite",
    "arrow",
    "duckdb",
    "mongolite",
    "qs2"
)

if (!requireNamespace("pak", quietly = TRUE)) {
    install.packages("pak")
}

missing <- setdiff(packages, pak::pkg_status(packages)$package)

if (length(missing) > 0) {
    pak::pkg_install(missing, ask = FALSE)
}

invisible(lapply(packages, library, character.only = TRUE))
