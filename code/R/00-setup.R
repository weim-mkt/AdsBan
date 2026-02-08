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
    "birk"
)

installed <- packages %in% rownames(installed.packages())

if (any(!installed)) {
    install.packages(packages[!installed], dependencies = TRUE)
}

invisible(lapply(packages, library, character.only = TRUE))
