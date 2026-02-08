#' Check Barcode Uniqueness for Products
#'
#' @description
#' Checks if each product maps to a single unique barcode across all years
#' or if it is associated with multiple barcodes.
#'
#' @param data_barcode A data.table containing 'product' and 'barcode' columns.
#'
#' @return A data.table summarizing the number of unique barcodes per product.
#'   Columns: product, n_barcodes, is_unique_mapping, barcodes_list.
#'
#' @export
check_barcode <- function(data_barcode) {
  # Ensure input is a data.table
  if (!data.table::is.data.table(data_barcode)) {
    data_barcode <- data.table::as.data.table(data_barcode)
  }
  
  # Check required columns
  required_cols <- c("product", "barcode")
  if (!all(required_cols %in% names(data_barcode))) {
    stop("Input data must contain 'product' and 'barcode' columns.")
  }
  
  # Calculate unique barcodes per product
  summary_dt <- data_barcode[, .(
    n_barcodes = data.table::uniqueN(barcode),
    barcodes_list = list(unique(barcode))
  ), by = product]
  
  # Add uniqueness flag
  summary_dt[, is_unique_mapping := n_barcodes == 1]
  
  # Print summary report
  total_products <- nrow(summary_dt)
  unique_mapping_count <- sum(summary_dt$is_unique_mapping)
  multiple_mapping_count <- total_products - unique_mapping_count
  
  message("Barcode Mapping Check Summary:")
  message(sprintf("Total Products: %d", total_products))
  message(sprintf("Products with Unique Barcode: %d (%.2f%%)", 
                  unique_mapping_count, 
                  100 * unique_mapping_count / total_products))
  message(sprintf("Products with Multiple Barcodes: %d (%.2f%%)", 
                  multiple_mapping_count, 
                  100 * multiple_mapping_count / total_products))
  
  return(summary_dt)
}

#' Check Product Code Coverage in Barcode Data
#'
#' @description
#' Checks what percentage of product codes in the purchase data are present
#' in the barcode mapping data. Reports overall coverage and coverage by year.
#'
#' @param data_purchase A data.table containing purchase data with 'product_code' and 'date_of_purchase' columns.
#' @param data_barcode A data.table containing barcode mapping data with a 'product' column.
#'
#' @return A list containing:
#'   - overall_stats: A named vector with overall counts and percentages.
#'   - yearly_stats: A data.table with coverage statistics per year.
#'   - missing_products: A vector of product codes found in purchase data but missing from barcode data.
#'
#' @export
check_product_coverage <- function(data_purchase, data_barcode) {
  # Ensure inputs are data.tables
  if (!data.table::is.data.table(data_purchase)) data_purchase <- data.table::as.data.table(data_purchase)
  if (!data.table::is.data.table(data_barcode)) data_barcode <- data.table::as.data.table(data_barcode)
  
  # Check required columns
  if (!"product_code" %in% names(data_purchase)) stop("Purchase data must have 'product_code' column.")
  if (!"product" %in% names(data_barcode)) stop("Barcode data must have 'product' column.")
  
  # Extract year from Date_of_purchase if not already present
  if (!"year" %in% names(data_purchase)) {
      if ("Date_of_purchase" %in% names(data_purchase)) {
          # Try to parse date. Assuming YYYY-MM-DD or similar standard format
          # If it's integer YYYYMMDD, handle that too
          if (is.numeric(data_purchase$Date_of_purchase)) {
             # Assuming YYYYMMDD
             data_purchase[, year := as.integer(substr(as.character(Date_of_purchase), 1, 4))]
          } else {
             data_purchase[, year := data.table::year(as.Date(Date_of_purchase))]
          }
      } else {
          warning("No 'year' or 'Date_of_purchase' column found. Yearly stats will be skipped.")
      }
  }

  barcode_products <- unique(data_barcode$product)
  
  # --- Overall Coverage ---
  purchase_products <- unique(data_purchase$product_code)
  n_purchase <- length(purchase_products)
  matched_products <- intersect(purchase_products, barcode_products)
  n_matched <- length(matched_products)
  n_missing <- n_purchase - n_matched
  missing_products <- setdiff(purchase_products, barcode_products)
  pct_covered <- if (n_purchase > 0) (n_matched / n_purchase) * 100 else 0
  
  # Print Overall Report
  message("Product Code Coverage Summary:")
  message("-------------------------------")
  message("Overall:")
  message(sprintf("  Total Unique Products: %d", n_purchase))
  message(sprintf("  Matched: %d (%.2f%%)", n_matched, pct_covered))
  message(sprintf("  Missing: %d", n_missing))
  
  # --- Yearly Coverage ---
  yearly_stats <- NULL
  if ("year" %in% names(data_purchase)) {
      message("\nCoverage by Year:")
      
      # Calculate stats by year
      yearly_stats <- data_purchase[, .(
          total_products = data.table::uniqueN(product_code),
          matched_products = length(intersect(unique(product_code), barcode_products))
      ), by = year][order(year)]
      
      yearly_stats[, `:=`(
          missing_products = total_products - matched_products,
          pct_covered = (matched_products / total_products) * 100
      )]
      
      # Print yearly stats
      print(yearly_stats)
  }
  
  return(list(
    overall_stats = c(
      total_purchase_products = n_purchase,
      matched_products = n_matched,
      missing_products = n_missing,
      pct_covered = pct_covered
    ),
    yearly_stats = yearly_stats,
    missing_products = missing_products
  ))
}
