# AiMark data

## AiMark Barcode Data

**Data Path:** `data/raw data/AiMark Data/UK 2021-2023/Barcode mapping/*.csv`

The barcode mapping data contains detailed product attributes. Below is a categorization of the available variables:

### 1. Product Identification
*   `product_code`: Unique identifier for the product.
*   `product_code_description`: Text description of the product.
*   `variant`: Specific product variant.

### 2. Brand & Manufacturer
*   `Brand`, `Original_Brand`: Brand name.
*   `Sub_brand`, `New_Sub_Brand`: Sub-brand hierarchy.
*   `manufacturer`: Name of the manufacturer.
*   `holding_company`: Parent company.
*   `distributor`: Distributor name.
*   `PL`: Private Label indicator.

### 3. Category & Hierarchy
*   **General**: `Category`, `category_name`, `sub_category`, `sub_category_name`, `sub_type`.
*   **Sector Specific**:
    *   `report_sector`, `confec_sector`
    *   `sector_of_mens_toiletries`
    *   `sector_of_soft_drinks`, `sub_sector_of_soft_drinks`, `cc_soft_drinks_sector`
    *   `total_defined_baby_market`
    *   `Coca_Cola_Ire_Sector`, `Coca_Cola_Ire_Type`
*   **Market/Retailer Specific Classifications**:
    *   `shop_category`, `shop_aisle`
    *   `lw_market`, `lw_category`, `lw_sub_category`
    *   `rst_4_trading_area`, `rst_4_market`, `rst_4_market_sector`, `rst_4_sub_market`, `rst_4_extended`
    *   `BG_Category_name`, `BG_Category_number`
    *   `Sisters_Category`

### 4. Physical Attributes
*   **Size & Packaging**: `size`, `Measurement_unit`, `pack_type`, `number_in_a_pack`, `ctb_pack_format`, `ctb_pack_group`.
*   **Format**: `prepacked_loose`, `single_serve_multi_serve`.
*   **Appearance/Shape**: `colour`, `Sisters_Bread_Roll_Shape`, `rst_cheese_cut`.

### 5. Content, Dietary & Health
*   **Health Claims**: `low_sugar_calorie_fat`, `low_salt_claim`, `level_of_fat`, `healthy_vs_standard`.
*   **Dietary**: `organic`, `free_from`, `regular_vs_diet`.
*   **Ingredients/Type**: `carbonation_indicator`, `free_non_free_range`, `meat_type_2`, `ready_meal_meat_type`.
*   **Preparation**: `cook_non_cook`, `freezer_type`.
*   **Ethical**: `fairtrade`, `fairtrade_2`.

### 6. Shopper & Marketing
*   `ctb_shopper_segment`, `ctb_shopper_sub_segment`
*   `range_grouping`
*   `private_label_tiering`
*   `fine_mass`
*   `gift_sel`
*   `kids_branded`, `adult_special`

### 7. Origin & Purchase Context
*   `country_of_origin`
*   `ireland_purchase`
*   `in_store_not_in_store`

### 8. Other Classifications
*   `rst_4_crm_type`
*   `LF02_Levbrnd`
*   `lw_type`

## AiMark Purchase Data

# Open Food Facts

## Products Data

**Data Path:** `data/raw data/openfoodfacts/openfoodfacts-products.jsonl`

The Open Food Facts dataset contains over 300 variables. Below is a categorization of the key fields:

### 1. Product Identification & Metadata
*   **Identifiers**: `_id`, `code` (barcode), `id`.
*   **Names**: `product_name`, `generic_name` (and language variants like `_fr`, `_en`).
*   **Quantity**: `quantity`, `product_quantity`, `product_quantity_unit`.
*   **Brands & Origins**: `brands`, `brands_tags`, `origins`, `origins_tags`, `manufacturing_places`, `emb_codes` (emboss codes).
*   **Categories**: `categories`, `categories_tags`, `categories_hierarchy`.
*   **Stores & Countries**: `stores`, `countries`, `countries_tags`, `purchase_places`.
*   **Metadata**: `creator`, `created_t`, `last_modified_t`, `last_modified_by`, `states`, `states_tags` (completion status).

### 2. Ingredients & Allergens
*   **Ingredients**: `ingredients_text`, `ingredients` (list of objects), `ingredients_tags`, `ingredients_analysis_tags`.
*   **Allergens & Traces**: `allergens`, `allergens_tags`, `traces`, `traces_tags`.
*   **Additives**: `additives_n`, `additives_tags`.
*   **Specific Components**: `ingredients_from_palm_oil_n`, `ingredients_that_may_be_from_palm_oil_n`, `amino_acids_tags`, `minerals_tags`, `vitamins_tags`, `nucleotides_tags`.

### 3. Nutrition Facts
*   **Data**: `nutrition_data`, `nutrition_data_per` (serving or 100g), `nutrition_data_prepared_per`.
*   **Nutrients**: `nutriments` (nested object containing values for energy, fat, sugar, proteins, salt, etc.), `nutrient_levels_tags` (low/moderate/high markers).
*   **Serving**: `serving_size`, `serving_quantity`.

### 4. Scores & Grades
*   **Nutri-Score**: `nutriscore_grade`, `nutriscore_score`, `nutriscore_tags`, `nutrition_grades`.
*   **Eco-Score**: `ecoscore_grade`, `ecoscore_score`, `ecoscore_tags`, `environmental_score`.
*   **NOVA**: `nova_group`, `nova_groups_tags` (processing level).

### 5. Packaging
*   **Details**: `packaging`, `packaging_tags`, `packaging_text`, `packagings` (array of packaging components), `packaging_materials_tags`, `packaging_recycling_tags`.

### 6. Data Quality & Debug
*   **Quality**: `data_quality_tags`, `data_quality_errors_tags`, `data_quality_warnings_tags`, `data_quality_info_tags`.
*   **Contributors**: `checkers_tags`, `correctors_tags`, `informers_tags`, `photographers_tags`, `editors_tags`.
*   **Status**: `complete`, `completeness`, `last_edit_dates_tags`.
*   **Debug**: Various `*_debug_tags` fields.

### 7. System & Internal
*   `_keywords`, `sortkey`, `max_imgid`, `last_image_t`, `lc` (language code), `interface_version`.
