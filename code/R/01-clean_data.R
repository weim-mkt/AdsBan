#' Load Open Food Facts Data
#'
#' @description
#' Loads the Open Food Facts products dataset from a JSONL file.
#' Supports partial loading of the first few lines for inspection.
#'
#' @details
#' The data is expected to be in JSONL (JSON Lines) format, where each line is a valid JSON object.
#' The function uses `jsonlite` to parse the data.
#'
#' @param file_path Character string. The path to the JSONL file.
#'   Defaults to "data/raw data/openfoodfacts/openfoodfacts-products.jsonl".
#' @param n_max Numeric. The maximum number of lines to read.
#'   Defaults to `Inf` (read the entire file). Set to a small integer (e.g., 100) to preview the data.
#' @param force_cache Logical. If FALSE (default), attempts to load from cached FST file.
#'   If TRUE or cache not found, loads from raw file and saves to cache (only if n_max is Inf).
#'
#' @return A data frame (or list, depending on JSON structure consistency) containing the product data.
#'
#' @export
load_openfoodfacts_data <- function(
    file_path = "data/raw data/openfoodfacts/openfoodfacts-products.jsonl",
    n_max = Inf,
    force_cache = FALSE
) {
    cache_path <- "data/raw data/raw fst/openfoodfacts_products.fst"

    # Try loading from cache if appropriate
    if (!force_cache && file.exists(cache_path) && !is.finite(n_max)) {
        message("Loading cached Open Food Facts data...")
        return(fst::read_fst(cache_path, as.data.table = TRUE))
    }

    if (!file.exists(file_path)) {
        stop("File not found: ", file_path)
    }

    if (is.finite(n_max)) {
        # Partial load
        message(sprintf("Loading first %d lines from %s...", n_max, file_path))

        # Read raw lines
        lines <- readLines(
            file_path,
            n = n_max,
            warn = FALSE,
            encoding = "UTF-8"
        )

        # Parse using stream_in on a text connection
        # stream_in is generally more robust for NDJSON than lapply(fromJSON)
        data <- jsonlite::stream_in(textConnection(lines), verbose = FALSE)
        data.table::setDT(data)
    } else {
        # Full load
        message(sprintf("Loading all data from %s...", file_path))

        # Open file connection
        con <- file(file_path, "r", encoding = "UTF-8")
        on.exit(close(con))

        data <- jsonlite::stream_in(con, verbose = TRUE)
        data.table::setDT(data)

        # Save to cache
        message("Saving to cache...")
        if (!dir.exists(dirname(cache_path))) {
            dir.create(dirname(cache_path), recursive = TRUE)
        }
        fst::write_fst(data, cache_path)
    }

    return(data)
}

#' Load AiMark Barcode Mapping Data
#'
#' @description
#' Loads and combines all barcode mapping CSV files from the specified directory.
#' Extracts the year from the filename and adds it as a column.
#'
#' @param folder_path Character string. Path to the folder containing the CSV files.
#'   Defaults to "data/raw data/AiMark Data/UK 2021-2023/Barcode mapping".
#' @param force_cache Logical. If FALSE (default), attempts to load from cached FST file.
#'   If TRUE or cache not found, loads from raw files and saves to cache.
#'
#' @return A data.table containing combined data from all CSV files with columns:
#'   product, barcode, and year.
#'
#' @export
load_barcodes <- function(
    folder_path = "data/raw data/AiMark Data/UK 2021-2023/Barcode mapping",
    force_cache = FALSE
) {
    cache_path <- "data/raw data/raw fst/aimark_barcodes.fst"

    # Try loading from cache
    if (!force_cache && file.exists(cache_path)) {
        message("Loading cached barcode data...")
        return(fst::read_fst(cache_path, as.data.table = TRUE))
    }

    # Check if directory exists
    if (!dir.exists(folder_path)) {
        stop("Directory not found: ", folder_path)
    }

    # List all CSV files
    file_list <- list.files(
        path = folder_path,
        pattern = "\\.csv$",
        full.names = TRUE
    )

    if (length(file_list) == 0) {
        warning("No CSV files found in ", folder_path)
        return(NULL)
    }

    # Function to read a single file and add year
    read_and_process <- function(f) {
        # Extract year from filename (assuming format "UK barcodes YYYY.csv")
        file_name <- basename(f)
        year_match <- regmatches(file_name, regexpr("\\d{4}", file_name))

        if (length(year_match) == 0) {
            warning("Could not extract year from filename: ", file_name)
            year_val <- NA_integer_
        } else {
            year_val <- as.integer(year_match)
        }

        # Read CSV using data.table::fread for speed
        dt <- data.table::fread(
            f,
            colClasses = c("product" = "integer", "barcode" = "character")
        )

        # Add year column
        dt[, year := year_val]

        return(dt)
    }

    # Read all files and combine
    message(sprintf(
        "Loading %d files from %s...",
        length(file_list),
        folder_path
    ))
    all_data <- data.table::rbindlist(lapply(file_list, read_and_process))

    # Save to cache
    message("Saving to cache...")
    if (!dir.exists(dirname(cache_path))) {
        dir.create(dirname(cache_path), recursive = TRUE)
    }
    fst::write_fst(all_data, cache_path)

    return(all_data)
}

#' Load AiMark Purchase Data
#'
#' @description
#' Loads and combines purchase data CSV files from multiple year-range directories.
#' The files are expected to be named 'purchase_YYYY.csv'.
#'
#' @param base_dirs Character vector. Paths to the directories containing the purchase data.
#'   Defaults to the known UK data directories.
#' @param force_cache Logical. If FALSE (default), attempts to load from cached FST file.
#'   If TRUE or cache not found, loads from raw files and saves to cache.
#'
#' @return A data.table containing combined purchase data from all found CSV files.
#'
#' @export
load_purchase_data <- function(
    base_dirs = c(
        "data/raw data/AiMark Data/UK 2005-2010",
        "data/raw data/AiMark Data/UK 2011-2015",
        "data/raw data/AiMark Data/UK 2016-2020",
        "data/raw data/AiMark Data/UK 2021-2023"
    ),
    force_cache = FALSE
) {
    cache_path <- "data/raw data/raw fst/aimark_purchase_data.fst"

    # Try loading from cache
    if (!force_cache && file.exists(cache_path)) {
        message("Loading cached purchase data...")
        return(fst::read_fst(cache_path, as.data.table = TRUE))
    }

    # Initialize list to store file paths
    all_files <- character()

    # Collect all matching files from all directories
    for (dir in base_dirs) {
        if (dir.exists(dir)) {
            files <- list.files(
                path = dir,
                pattern = "^purchase_\\d{4}\\.csv$",
                full.names = TRUE
            )
            if (length(files) > 0) {
                all_files <- c(all_files, files)
            } else {
                warning(sprintf("No purchase files found in %s", dir))
            }
        } else {
            warning(sprintf("Directory not found: %s", dir))
        }
    }

    if (length(all_files) == 0) {
        stop(
            "No purchase data files found in any of the specified directories."
        )
    }

    # Function to read a single file
    read_file <- function(f) {
        # Extract year from filename for potential validation/debugging
        file_name <- basename(f)
        year_match <- regmatches(file_name, regexpr("\\d{4}", file_name))
        year_val <- as.integer(year_match)

        message(sprintf("Reading %s...", file_name))

        # Read CSV using data.table::fread
        # Not specifying colClasses to allow auto-detection, as purchase data format varies
        dt <- data.table::fread(f)

        return(dt)
    }

    # Read all files and combine
    message(sprintf("Loading %d purchase data files...", length(all_files)))

    # Use rbindlist with fill=TRUE to handle potential column mismatches across years
    all_data <- data.table::rbindlist(lapply(all_files, read_file), fill = TRUE)

    # Save to cache
    message("Saving to cache...")
    if (!dir.exists(dirname(cache_path))) {
        dir.create(dirname(cache_path), recursive = TRUE)
    }
    fst::write_fst(all_data, cache_path)

    return(all_data)
}
