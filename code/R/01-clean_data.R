# Load AiMark Data ----

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

# Extract Open Food Facts data ----
#' Extract fields from Open Food Facts MongoDB Dump
#'
#' @description
#' Extracts specific fields from the Open Food Facts MongoDB dump.
#' Checks if the data is already in MongoDB; if not, restores it from the dump file.
#' Requires a running MongoDB instance.
#'
#' @param dump_path Character string. Path to the MongoDB dump file.
#'   Defaults to "data/raw data/openfoodfacts/openfoodfacts-mongodbdump".
#' @param fields Character vector. Fields to extract. Defaults to c("countries", "code").
#' @param db_name Character string. Database name. Defaults to "off".
#' @param collection_name Character string. Collection name. Defaults to "products".
#' @param mongo_url Character string. MongoDB connection URL. Defaults to "mongodb://localhost".
#' @param verbose Logical. Whether to print progress.
#' @param force_cache Logical. If TRUE, ignore any existing fst cache for this
#'   field set and re-query MongoDB. Defaults to FALSE.
#'
#' @return A data.table containing the extracted fields.
#'
#' @export
extract_openfoodfacts_data <- function(
    dump_path = "data/raw data/openfoodfacts/openfoodfacts-mongodbdump",
    fields = c("countries", "code", "_id", "ean", "upc"),
    db_name = "off",
    collection_name = "products",
    mongo_url = "mongodb://localhost:27018",
    verbose = TRUE,
    force_cache = FALSE
) {
    # Cache keyed on the requested field set — different fields, different cache file
    fields_tag <- paste(sort(fields), collapse = "_") |>
        gsub("[^A-Za-z0-9]+", "_", x = _)
    cache_path <- file.path(
        "data",
        "raw data",
        "raw fst",
        sprintf("openfoodfacts_%s.fst", fields_tag)
    )

    if (!force_cache && file.exists(cache_path)) {
        if (verbose) message("Loading cached Open Food Facts data...")
        return(fst::read_fst(cache_path, as.data.table = TRUE))
    }

    # Check if file exists
    if (!file.exists(dump_path)) {
        stop("Dump file not found: ", dump_path)
    }

    # Connect to MongoDB
    if (verbose) {
        message("Connecting to MongoDB...")
    }
    con <- tryCatch(
        mongolite::mongo(
            collection = collection_name,
            db = db_name,
            url = mongo_url
        ),
        error = function(e) {
            stop(
                "Could not connect to MongoDB. Make sure it is running.\nError: ",
                e$message
            )
        }
    )

    # Check if collection is empty
    if (con$count() == 0) {
        if (verbose) {
            message("Collection is empty. Restoring from dump...")
        }

        # Construct mongorestore command
        # Note: --archive expects the file path
        cmd <- sprintf(
            "mongorestore --archive='%s' --nsInclude=%s.%s --uri='%s'",
            dump_path,
            db_name,
            collection_name,
            mongo_url
        )

        if (verbose) {
            message("Running: ", cmd)
        }
        ret <- system(cmd)

        if (ret != 0) {
            stop("mongorestore failed with exit code ", ret)
        }
    } else {
        if (verbose) message("Collection exists. Skipping restore.")
    }

    # Construct fields projection
    # mongolite expects a JSON string for fields, e.g., '{"countries": 1, "code": 1, "_id": 0}'
    fields_list <- as.list(rep(1, length(fields)))
    names(fields_list) <- fields
    # Exclude _id unless requested
    if (!"_id" %in% fields) {
        fields_list[["_id"]] <- 0
    }
    fields_json <- jsonlite::toJSON(fields_list, auto_unbox = TRUE)

    if (verbose) {
        message("Extracting fields: ", paste(fields, collapse = ", "))
    }

    # Query data
    # Use find() to get data. It returns a data.frame.
    dt <- data.table::as.data.table(con$find(fields = fields_json))

    # Save to cache for fast reloads across R sessions
    if (verbose) message("Saving to cache: ", cache_path)
    if (!dir.exists(dirname(cache_path))) {
        dir.create(dirname(cache_path), recursive = TRUE)
    }
    fst::write_fst(dt, cache_path)

    return(dt)
}
