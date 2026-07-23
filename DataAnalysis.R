# Exploratory Data Analysis (EDA)
# House price analysis---------

library(tidyverse)
library(scales)

base_folder <- "C:/Users/User/OneDrive/ドキュメント"

# Column names used in the Land Registry files
column_names <- c(
  "transaction_id", "price", "date", "postcode",
  "property_type", "old_new", "duration", "paon",
  "saon", "street", "locality", "town",
  "district", "county", "category", "record_status"
)

# Read the files from 2021 to 2025
house_data_2021_2025 <- map_dfr(
  2021:2025,
  function(current_year) {
    
    file_name <- file.path(
      base_folder,
      paste0("pp-", current_year, ".csv")
    )
    
    if (!file.exists(file_name)) {
      stop(paste("File not found:", file_name))
    }
    
    read_csv(
      file_name,
      col_names = column_names,
      col_types = cols(
        .default = col_character(),
        price = col_double()
      ),
      show_col_types = FALSE
    ) %>%
      mutate(
        year = current_year,
        county = str_to_upper(str_squish(county)),
        district = str_to_title(str_squish(district))
      )
  }
)

# Keep Norfolk and Suffolk records
house_data_2021_2025 <- house_data_2021_2025 %>%
  filter(
    county %in% c("NORFOLK", "SUFFOLK"),
    !is.na(price),
    price > 0,
    !is.na(district),
    district != ""
  )

# 2024 average house price for each district
district_average <- house_data_2021_2025 %>%
  filter(year == 2024) %>%
  group_by(county, district) %>%
  summarise(
    average_price = mean(price, na.rm = TRUE),
    .groups = "drop"
  )

print(district_average)

# Yearly average house price for each county
yearly_average <- house_data_2021_2025 %>%
  group_by(county, year) %>%
  summarise(
    average_price = mean(price, na.rm = TRUE),
    .groups = "drop"
  )

print(yearly_average)


# Norfolk box plot
norfolk_boxplot <- district_average %>%
  filter(county == "NORFOLK") %>%
  ggplot(
    aes(
      x = county,
      y = average_price
    )
  ) +
  geom_boxplot() +
  geom_jitter(
    width = 0.10,
    size = 2
  ) +
  scale_y_continuous(
    labels = label_currency(
      prefix = "£",
      accuracy = 1
    )
  ) +
  labs(
    title = "Average House Prices in Norfolk Districts, 2024",
    x = "Norfolk",
    y = "Average district house price"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )

print(norfolk_boxplot)


# Suffolk box plot
suffolk_boxplot <- district_average %>%
  filter(county == "SUFFOLK") %>%
  ggplot(
    aes(
      x = county,
      y = average_price
    )
  ) +
  geom_boxplot() +
  geom_jitter(
    width = 0.10,
    size = 2
  ) +
  scale_y_continuous(
    labels = label_currency(
      prefix = "£",
      accuracy = 1
    )
  ) +
  labs(
    title = "Average House Prices in Suffolk Districts, 2024",
    x = "Suffolk",
    y = "Average district house price"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )

print(suffolk_boxplot)

# Norfolk bar chart
norfolk_barchart <- district_average %>%
  filter(county == "NORFOLK") %>%
  ggplot(
    aes(
      x = reorder(district, average_price),
      y = average_price
    )
  ) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(
    labels = scales::label_currency(
      prefix = "£",
      big.mark = ",",
      accuracy = 1
    )
  ) +
  labs(
    title = "Average House Prices by District in Norfolk, 2024",
    x = "District",
    y = "Average house price"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )

print(norfolk_barchart)

ggsave(
  "norfolk_barchart.png",
  plot = norfolk_barchart,
  width = 8,
  height = 6,
  dpi = 300
)


# Suffolk bar chart
suffolk_barchart <- district_average %>%
  filter(county == "SUFFOLK") %>%
  ggplot(
    aes(
      x = reorder(district, average_price),
      y = average_price
    )
  ) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(
    labels = scales::label_currency(
      prefix = "£",
      big.mark = ",",
      accuracy = 1
    )
  ) +
  labs(
    title = "Average House Prices by District in Suffolk, 2024",
    x = "District",
    y = "Average house price"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )

print(suffolk_barchart)

ggsave(
  "suffolk_barchart.png",
  plot = suffolk_barchart,
  width = 8,
  height = 6,
  dpi = 300
)

# Norfolk line chart
norfolk_linechart <- yearly_average %>%
  filter(county == "NORFOLK") %>%
  ggplot(
    aes(
      x = year,
      y = average_price
    )
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_x_continuous(
    breaks = 2021:2025
  ) +
  scale_y_continuous(
    labels = label_currency(
      prefix = "£",
      big.mark = ",",
      accuracy = 1
    )
  ) +
  labs(
    title = "Average House Prices in Norfolk, 2021-2025",
    x = "Year",
    y = "Average house price"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )

print(norfolk_linechart)

ggsave(
  "norfolk_linechart.png",
  plot = norfolk_linechart,
  width = 8,
  height = 6,
  dpi = 300
)


# Suffolk line chart
suffolk_linechart <- yearly_average %>%
  filter(county == "SUFFOLK") %>%
  ggplot(
    aes(
      x = year,
      y = average_price
    )
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_x_continuous(
    breaks = 2021:2025
  ) +
  scale_y_continuous(
    labels = label_currency(
      prefix = "£",
      big.mark = ",",
      accuracy = 1
    )
  ) +
  labs(
    title = "Average House Prices in Suffolk, 2021-2025",
    x = "Year",
    y = "Average house price"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )

print(suffolk_linechart)

ggsave(
  "suffolk_linechart.png",
  plot = suffolk_linechart,
  width = 8,
  height = 6,
  dpi = 300
)
# Broadband speed analysis-------
# Questions 1 and 2

library(tidyverse)

base_folder <- "C:/Users/User/OneDrive/ドキュメント"

# Load broadband performance data
perf <- read.csv(
  file.path(
    base_folder,
    "201805_fixed_pc_performance_r03.csv"
  ),
  stringsAsFactors = FALSE
)

# Clean broadband speed data
perf <- perf %>%
  mutate(
    Average.download.speed..Mbit.s. = parse_number(
      as.character(
        Average.download.speed..Mbit.s.
      ),
      na = c(
        "",
        "NA",
        "N/A",
        "."
      )
    ),
    
    Maximum.download.speed..Mbit.s. = parse_number(
      as.character(
        Maximum.download.speed..Mbit.s.
      ),
      na = c(
        "",
        "NA",
        "N/A",
        "."
      )
    ),
    
    postcode.area = str_to_upper(
      str_squish(postcode.area)
    ),
    
    postcode_space = str_to_upper(
      str_squish(postcode_space)
    )
  ) %>%
  filter(
    !is.na(postcode.area),
    !is.na(postcode_space),
    postcode_space != "",
    !is.na(Average.download.speed..Mbit.s.),
    !is.na(Maximum.download.speed..Mbit.s.)
  )


# Keep Norfolk and Suffolk postcode areas
nf_sf <- perf %>%
  filter(
    postcode.area %in% c(
      "NR",
      "IP"
    )
  ) %>%
  mutate(
    County = case_when(
      postcode.area == "NR" ~ "NORFOLK",
      postcode.area == "IP" ~ "SUFFOLK"
    ),
    
    # Extract the outward postcode district
    District = sub(
      " .*",
      "",
      postcode_space
    )
  ) %>%
  filter(
    !is.na(District),
    District != ""
  )


# Question 1
# Calculate average download speed for each postcode district
district_avg <- nf_sf %>%
  group_by(
    County,
    District
  ) %>%
  summarise(
    AvgDownload = mean(
      Average.download.speed..Mbit.s.,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

print(district_avg)


# Norfolk box plot
norfolk_speed_boxplot <- district_avg %>%
  filter(
    County == "NORFOLK"
  ) %>%
  ggplot(
    aes(
      x = County,
      y = AvgDownload
    )
  ) +
  geom_boxplot() +
  geom_jitter(
    width = 0.10,
    size = 2
  ) +
  labs(
    title = "Average Download Speed by District in Norfolk",
    x = "Norfolk",
    y = "Average download speed (Mbit/s)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold"
    )
  )

print(norfolk_speed_boxplot)

ggsave(
  file.path(
    base_folder,
    "Norfolk_broadband_boxplot.png"
  ),
  plot = norfolk_speed_boxplot,
  width = 8,
  height = 6,
  dpi = 300
)


# Suffolk box plot
suffolk_speed_boxplot <- district_avg %>%
  filter(
    County == "SUFFOLK"
  ) %>%
  ggplot(
    aes(
      x = County,
      y = AvgDownload
    )
  ) +
  geom_boxplot() +
  geom_jitter(
    width = 0.10,
    size = 2
  ) +
  labs(
    title = "Average Download Speed by District in Suffolk",
    x = "Suffolk",
    y = "Average download speed (Mbit/s)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold"
    )
  )

print(suffolk_speed_boxplot)

ggsave(
  file.path(
    base_folder,
    "Suffolk_broadband_boxplot.png"
  ),
  plot = suffolk_speed_boxplot,
  width = 8,
  height = 6,
  dpi = 300
)


# Question 2
# Calculate average and maximum download speed by postcode district
district_speed <- nf_sf %>%
  group_by(
    County,
    District
  ) %>%
  summarise(
    `Average download speed` = mean(
      Average.download.speed..Mbit.s.,
      na.rm = TRUE
    ),
    
    `Maximum download speed` = mean(
      Maximum.download.speed..Mbit.s.,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(
      `Average download speed`,
      `Maximum download speed`
    ),
    names_to = "Speed_Type",
    values_to = "Download_Speed"
  )

print(district_speed)


# Norfolk bar chart
norfolk_speed_barchart <- district_speed %>%
  filter(
    County == "NORFOLK"
  ) %>%
  ggplot(
    aes(
      x = reorder(
        District,
        Download_Speed
      ),
      y = Download_Speed,
      fill = Speed_Type
    )
  ) +
  geom_col(
    position = "dodge"
  ) +
  coord_flip() +
  labs(
    title = "Average and Maximum Download Speed in Norfolk",
    x = "Postcode district",
    y = "Download speed (Mbit/s)",
    fill = "Speed type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(
      face = "bold"
    )
  )

print(norfolk_speed_barchart)

ggsave(
  file.path(
    base_folder,
    "Norfolk_broadband_barchart.png"
  ),
  plot = norfolk_speed_barchart,
  width = 9,
  height = 7,
  dpi = 300
)


# Suffolk bar chart
suffolk_speed_barchart <- district_speed %>%
  filter(
    County == "SUFFOLK"
  ) %>%
  ggplot(
    aes(
      x = reorder(
        District,
        Download_Speed
      ),
      y = Download_Speed,
      fill = Speed_Type
    )
  ) +
  geom_col(
    position = "dodge"
  ) +
  coord_flip() +
  labs(
    title = "Average and Maximum Download Speed in Suffolk",
    x = "Postcode district",
    y = "Download speed (Mbit/s)",
    fill = "Speed type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(
      face = "bold"
    )
  )

print(suffolk_speed_barchart)

ggsave(
  file.path(
    base_folder,
    "Suffolk_broadband_barchart.png"
  ),
  plot = suffolk_speed_barchart,
  width = 9,
  height = 7,
  dpi = 300
)
# Crime rate------
#ques 1
library(tidyverse)
library(lubridate)

# Select the ZIP file itself
crime_zip <- file.choose()

# Extract the ZIP
crime_folder <- tempfile("crime_data_")
dir.create(crime_folder)

unzip(
  crime_zip,
  exdir = crime_folder
)

# all CSV files inside the ZIP
all_csv_files <- list.files(
  crime_folder,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

# Keep Norfolk and Suffolk street-crime files
crime_files <- all_csv_files[
  str_detect(
    str_to_lower(all_csv_files),
    "norfolk|suffolk"
  ) &
    str_detect(
      str_to_lower(basename(all_csv_files)),
      "street\\.csv$"
    )
]

# Check files
print(length(crime_files))
print(head(crime_files))

if (length(crime_files) == 0) {
  stop("No Norfolk or Suffolk street-crime files were found.")
}

# Read and combine the crime files
crime_data <- map_dfr(
  crime_files,
  function(file) {
    read_csv(
      file,
      col_types = cols(.default = col_character()),
      show_col_types = FALSE
    ) %>%
      select(
        Month,
        `LSOA name`,
        `Crime type`
      ) %>%
      mutate(Source_File = file)
  }
) %>%
  mutate(
    Date = ymd(paste0(Month, "-01")),
    
    County = case_when(
      str_detect(
        Source_File,
        regex("norfolk", ignore_case = TRUE)
      ) ~ "NORFOLK",
      
      str_detect(
        Source_File,
        regex("suffolk", ignore_case = TRUE)
      ) ~ "SUFFOLK"
    ),
    
    District = str_remove(
      `LSOA name`,
      "\\s+[0-9]{3}[A-Z]$"
    )
  ) %>%
  filter(
    !is.na(Date),
    !is.na(County),
    !is.na(District),
    District != ""
  )

# Confirm successful import
table(crime_data$County)
range(crime_data$Date)

burglary_2024 <- crime_data %>%
  filter(
    year(Date) == 2024,
    `Crime type` == "Burglary"
  ) %>%
  count(
    County,
    District,
    Date,
    name = "Burglary_Count"
  ) %>%
  group_by(
    County,
    District
  ) %>%
  complete(
    Date = seq(
      as.Date("2024-01-01"),
      as.Date("2024-12-01"),
      by = "month"
    ),
    fill = list(Burglary_Count = 0)
  ) %>%
  ungroup()

norfolk_crime_boxplot <- burglary_2024 %>%
  filter(County == "NORFOLK") %>%
  ggplot(
    aes(
      x = reorder(District, Burglary_Count, FUN = median),
      y = Burglary_Count
    )
  ) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "Monthly Burglary by District in Norfolk, 2024",
    x = "District",
    y = "Monthly number of burglaries"
  ) +
  theme_minimal()

print(norfolk_crime_boxplot)


# Suffolk box plot
suffolk_crime_boxplot <- burglary_2024 %>%
  filter(County == "SUFFOLK") %>%
  ggplot(
    aes(
      x = reorder(District, Burglary_Count, FUN = median),
      y = Burglary_Count
    )
  ) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "Monthly Burglary by District in Suffolk, 2024",
    x = "District",
    y = "Monthly number of burglaries"
  ) +
  theme_minimal()

print(suffolk_crime_boxplot)

# Q2: Vehicle crime rate per 10,000 people – January 2024
# Load the packages
library(dplyr)
if (!requireNamespace("fmsb", quietly = TRUE)) {
  install.packages("fmsb")
}
library(fmsb)


# Folder where all the files are saved
base_folder <- "C:/Users/User/OneDrive/ドキュメント"


# Districts included in the analysis
norfolk_districts <- c(
  "Breckland",
  "Broadland",
  "Great Yarmouth",
  "King's Lynn and West Norfolk",
  "North Norfolk",
  "Norwich",
  "South Norfolk"
)

suffolk_districts <- c(
  "Babergh",
  "East Suffolk",
  "Ipswich",
  "Mid Suffolk",
  "West Suffolk"
)

district_info <- data.frame(
  District = c(
    norfolk_districts,
    suffolk_districts
  ),
  County = c(
    rep("NORFOLK", length(norfolk_districts)),
    rep("SUFFOLK", length(suffolk_districts))
  )
)

target_districts <- district_info$District


# Find all the files inside the Documents folder
all_files <- list.files(
  base_folder,
  recursive = TRUE,
  full.names = TRUE
)


# Find the population file
population_file <- all_files[
  grepl(
    "^Population2011.*\\.csv$",
    basename(all_files),
    ignore.case = TRUE
  )
][1]


# Find the crime ZIP file
crime_zip <- all_files[
  grepl(
    "^427a01f6c4d0f343ed1cb89053d0361e3caa0cd3.*\\.zip$",
    basename(all_files),
    ignore.case = TRUE
  )
][1]


# Check whether the postcode lookup CSV is already available
lookup_file <- all_files[
  grepl(
    "^NSP21CL_MAY24_UK_LU.*\\.csv$",
    basename(all_files),
    ignore.case = TRUE
  )
][1]


# Stop the code if the required files cannot be found
if (is.na(population_file)) {
  stop("Population file not found.")
}

if (is.na(crime_zip)) {
  stop("Crime ZIP file not found.")
}


# Unzip the postcode lookup if the CSV is not already available
if (is.na(lookup_file)) {
  
  lookup_zip <- all_files[
    grepl(
      "^NSP21CL_MAY24_UK_LU.*\\.zip$",
      basename(all_files),
      ignore.case = TRUE
    )
  ][1]
  
  if (is.na(lookup_zip)) {
    stop("Postcode lookup ZIP file not found.")
  }
  
  lookup_folder <- file.path(
    tempdir(),
    "postcode_lookup"
  )
  
  unlink(
    lookup_folder,
    recursive = TRUE
  )
  
  dir.create(
    lookup_folder,
    recursive = TRUE
  )
  
  unzip(
    lookup_zip,
    exdir = lookup_folder
  )
  
  lookup_csvs <- list.files(
    lookup_folder,
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  
  # Some downloads contain another ZIP file inside the main ZIP
  if (length(lookup_csvs) == 0) {
    
    nested_zips <- list.files(
      lookup_folder,
      pattern = "\\.zip$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    )
    
    for (i in seq_along(nested_zips)) {
      
      nested_folder <- file.path(
        lookup_folder,
        paste0("nested_", i)
      )
      
      dir.create(
        nested_folder,
        recursive = TRUE
      )
      
      unzip(
        nested_zips[i],
        exdir = nested_folder
      )
    }
    
    lookup_csvs <- list.files(
      lookup_folder,
      pattern = "\\.csv$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    )
  }
  
  if (length(lookup_csvs) == 0) {
    stop("No postcode lookup CSV found.")
  }
  
  selected_lookup <- lookup_csvs[
    grepl(
      "NSP21CL",
      basename(lookup_csvs),
      ignore.case = TRUE
    )
  ]
  
  if (length(selected_lookup) > 0) {
    lookup_file <- selected_lookup[1]
  } else {
    lookup_file <- lookup_csvs[1]
  }
}


# Function for changing a full postcode into a postcode sector
make_sector <- function(x) {
  
  x <- toupper(
    trimws(
      gsub("\\s+", " ", x)
    )
  )
  
  outward <- sub(
    "\\s.*$",
    "",
    x
  )
  
  inward <- sub(
    "^.*\\s",
    "",
    x
  )
  
  paste(
    outward,
    substr(inward, 1, 1)
  )
}


# Read the postcode lookup data
lu <- read.csv(
  lookup_file,
  stringsAsFactors = FALSE
)

names(lu) <- tolower(names(lu))


# Find the postcode and district columns
postcode_column <- intersect(
  c(
    "pcds",
    "pcd8",
    "pcd7",
    "pcd",
    "postcode"
  ),
  names(lu)
)[1]

district_column <- grep(
  "^lad.*nm$",
  names(lu),
  value = TRUE
)[1]

if (
  is.na(postcode_column) ||
  is.na(district_column)
) {
  stop("Postcode or district column not found in lookup.")
}


# Keep only the districts needed for this analysis
lu_target <- lu %>%
  transmute(
    Postcode = .data[[postcode_column]],
    District = .data[[district_column]]
  ) %>%
  filter(
    District %in% target_districts
  )

lu_target$Sector <- make_sector(
  lu_target$Postcode
)


# Match each postcode sector with its most common district
sector_lad <- aggregate(
  District ~ Sector,
  data = lu_target,
  FUN = function(x) {
    names(
      sort(
        table(x),
        decreasing = TRUE
      )
    )[1]
  }
)


# Read the population data
pop <- read.csv(
  population_file,
  stringsAsFactors = FALSE
)

names(pop) <- tolower(names(pop))


# Find the postcode and population columns
pop_postcode_column <- grep(
  "^postcode$|^pcds$|^pcd$",
  names(pop),
  value = TRUE
)[1]

pop_population_column <- grep(
  "^population$|usual.*resident|all.*person",
  names(pop),
  value = TRUE
)[1]

if (
  is.na(pop_postcode_column) ||
  is.na(pop_population_column)
) {
  stop("Population columns not found.")
}


# Prepare the postcode-sector population data
population_data <- pop %>%
  transmute(
    Sector = make_sector(
      .data[[pop_postcode_column]]
    ),
    Population = as.numeric(
      gsub(
        "[^0-9.-]",
        "",
        .data[[pop_population_column]]
      )
    )
  ) %>%
  filter(
    !is.na(Population)
  )


# Match population records with districts
merged_pop <- merge(
  population_data,
  sector_lad,
  by = "Sector"
)


# Calculate the total population of each district
district_pop <- merged_pop %>%
  group_by(District) %>%
  summarise(
    Population = sum(
      Population,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# Unzip the crime data
crime_folder <- file.path(
  tempdir(),
  "crime_q2"
)

unlink(
  crime_folder,
  recursive = TRUE
)

dir.create(
  crime_folder,
  recursive = TRUE
)

unzip(
  crime_zip,
  exdir = crime_folder
)


# Find all crime CSV files
crime_files <- list.files(
  crime_folder,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)


# Find the January 2024 Norfolk crime file
norfolk_file <- crime_files[
  grepl(
    "2024-01-norfolk-street\\.csv$",
    basename(crime_files),
    ignore.case = TRUE
  )
][1]


# Find the January 2024 Suffolk crime file
suffolk_file <- crime_files[
  grepl(
    "2024-01-suffolk-street\\.csv$",
    basename(crime_files),
    ignore.case = TRUE
  )
][1]

if (
  is.na(norfolk_file) ||
  is.na(suffolk_file)
) {
  stop("January 2024 crime files not found.")
}


# Read the Norfolk and Suffolk crime data
norfolk_crime <- read.csv(
  norfolk_file,
  stringsAsFactors = FALSE
)

suffolk_crime <- read.csv(
  suffolk_file,
  stringsAsFactors = FALSE
)


# Combine both counties into one dataset
crime <- rbind(
  norfolk_crime,
  suffolk_crime
)


# Get the district name from the LSOA name
crime$District <- sub(
  "\\s+[0-9]{3}[A-Z]$",
  "",
  crime$LSOA.name
)

crime$District <- gsub(
  "’",
  "'",
  crime$District
)


# Replace the older Suffolk district names
crime$District[
  crime$District %in% c(
    "Suffolk Coastal",
    "Waveney"
  )
] <- "East Suffolk"

crime$District[
  crime$District %in% c(
    "Forest Heath",
    "St Edmundsbury"
  )
] <- "West Suffolk"


# Count vehicle crimes in each district
vehicle_counts <- crime %>%
  filter(
    Crime.type == "Vehicle crime",
    District %in% target_districts
  ) %>%
  group_by(District) %>%
  summarise(
    Count = n(),
    .groups = "drop"
  )


# Combine the population and crime data
merged <- district_info %>%
  left_join(
    district_pop,
    by = "District"
  ) %>%
  left_join(
    vehicle_counts,
    by = "District"
  ) %>%
  mutate(
    Count = ifelse(
      is.na(Count),
      0,
      Count
    ),
    Rate10k = (
      Count / Population
    ) * 10000
  ) %>%
  filter(
    !is.na(Population),
    Population > 0
  )

print(merged)


# Function for creating the radar chart
create_radar <- function(
    data,
    county_name,
    title_text,
    line_colour,
    fill_colour
) {
  
  county_data <- data %>%
    filter(
      County == county_name
    ) %>%
    arrange(District)
  
  values <- county_data$Rate10k
  names(values) <- county_data$District
  
  if (length(values) < 3) {
    stop(
      paste(
        "Insufficient data for",
        county_name
      )
    )
  }
  
  maximum_value <- max(
    c(values, 1),
    na.rm = TRUE
  )
  
  radar_data <- as.data.frame(
    rbind(
      Maximum = rep(
        maximum_value * 1.1,
        length(values)
      ),
      Minimum = rep(
        0,
        length(values)
      ),
      Rate = values
    )
  )
  
  colnames(radar_data) <- names(values)
  
  radarchart(
    radar_data,
    axistype = 1,
    pcol = line_colour,
    pfcol = adjustcolor(
      fill_colour,
      alpha.f = 0.3
    ),
    plwd = 2,
    vlcex = 0.7,
    title = title_text
  )
}


# Save the Norfolk radar chart
png(
  file.path(
    base_folder,
    "Norfolk_vehicle_crime_Jan2024.png"
  ),
  width = 1400,
  height = 1100,
  res = 160
)

create_radar(
  merged,
  "NORFOLK",
  "Vehicle Crime Rate per 10,000 People - Norfolk (Jan 2024)",
  "darkred",
  "red"
)

dev.off()


# Save the Suffolk radar chart
png(
  file.path(
    base_folder,
    "Suffolk_vehicle_crime_Jan2024.png"
  ),
  width = 1400,
  height = 1100,
  res = 160
)

create_radar(
  merged,
  "SUFFOLK",
  "Vehicle Crime Rate per 10,000 People - Suffolk (Jan 2024)",
  "darkblue",
  "blue"
)

dev.off()


# Show the Norfolk chart in the RStudio Plots window
create_radar(
  merged,
  "NORFOLK",
  "Vehicle Crime Rate per 10,000 People - Norfolk (Jan 2024)",
  "darkred",
  "red"
)

# Q3: Drug offence rate from June 2023 to April 2026

library(dplyr)
library(tidyr)
library(ggplot2)

base_folder <- "C:/Users/User/OneDrive/ドキュメント"

# Districts used in the study
norfolk_districts <- c(
  "Breckland", "Broadland", "Great Yarmouth",
  "King's Lynn and West Norfolk", "North Norfolk",
  "Norwich", "South Norfolk"
)

suffolk_districts <- c(
  "Babergh", "East Suffolk", "Ipswich",
  "Mid Suffolk", "West Suffolk"
)

district_info <- data.frame(
  District = c(norfolk_districts, suffolk_districts),
  County = c(
    rep("NORFOLK", length(norfolk_districts)),
    rep("SUFFOLK", length(suffolk_districts))
  )
)

target_districts <- district_info$District

# Look for the files in the Documents folder
all_files <- list.files(
  base_folder,
  recursive = TRUE,
  full.names = TRUE
)

population_file <- all_files[
  grepl(
    "^Population2011.*\\.csv$",
    basename(all_files),
    ignore.case = TRUE
  )
][1]

crime_zip <- all_files[
  grepl(
    "^427a01f6c4d0f343ed1cb89053d0361e3caa0cd3.*\\.zip$",
    basename(all_files),
    ignore.case = TRUE
  )
][1]

lookup_file <- all_files[
  grepl(
    "(NSP21CL|PCD_OA21).*\\.csv$",
    basename(all_files),
    ignore.case = TRUE
  )
][1]

if (is.na(population_file)) {
  stop("Population file not found.")
}

if (is.na(crime_zip)) {
  stop("Crime ZIP file not found.")
}

# Unzip the postcode lookup if only the ZIP file is available
if (is.na(lookup_file)) {
  
  lookup_zip <- all_files[
    grepl(
      "(NSP21CL|PCD_OA21).*\\.zip$",
      basename(all_files),
      ignore.case = TRUE
    )
  ][1]
  
  if (is.na(lookup_zip)) {
    stop("Postcode lookup file not found.")
  }
  
  lookup_folder <- file.path(
    tempdir(),
    "postcode_lookup_q3"
  )
  
  unlink(lookup_folder, recursive = TRUE)
  dir.create(lookup_folder, recursive = TRUE)
  
  unzip(
    lookup_zip,
    exdir = lookup_folder
  )
  
  lookup_csvs <- list.files(
    lookup_folder,
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  # Check whether there is another ZIP inside
  if (length(lookup_csvs) == 0) {
    
    nested_zips <- list.files(
      lookup_folder,
      pattern = "\\.zip$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    )
    
    for (i in seq_along(nested_zips)) {
      
      nested_folder <- file.path(
        lookup_folder,
        paste0("nested_", i)
      )
      
      dir.create(nested_folder, recursive = TRUE)
      
      unzip(
        nested_zips[i],
        exdir = nested_folder
      )
    }
    
    lookup_csvs <- list.files(
      lookup_folder,
      pattern = "\\.csv$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    )
  }
  
  if (length(lookup_csvs) == 0) {
    stop("No postcode lookup CSV was found.")
  }
  
  suitable_lookup <- lookup_csvs[
    grepl(
      "NSP21CL|PCD_OA21",
      basename(lookup_csvs),
      ignore.case = TRUE
    )
  ]
  
  if (length(suitable_lookup) > 0) {
    lookup_file <- suitable_lookup[1]
  } else {
    lookup_file <- lookup_csvs[
      which.max(file.info(lookup_csvs)$size)
    ]
  }
}

# Change a full postcode into a postcode sector
make_sector <- function(x) {
  
  x <- toupper(
    trimws(
      gsub("\\s+", " ", x)
    )
  )
  
  outward <- sub("\\s.*$", "", x)
  inward <- sub("^.*\\s", "", x)
  
  paste(
    outward,
    substr(inward, 1, 1)
  )
}

# Read the postcode lookup
lu <- read.csv(
  lookup_file,
  stringsAsFactors = FALSE
)

names(lu) <- tolower(names(lu))

postcode_column <- intersect(
  c("pcds", "pcd8", "pcd7", "pcd", "postcode"),
  names(lu)
)[1]

district_column <- grep(
  "^lad.*nm$",
  names(lu),
  value = TRUE
)[1]

if (
  is.na(postcode_column) ||
  is.na(district_column)
) {
  stop("Postcode or district column not found.")
}

lu_target <- lu %>%
  transmute(
    Postcode = .data[[postcode_column]],
    District = .data[[district_column]]
  ) %>%
  filter(District %in% target_districts)

lu_target$Sector <- make_sector(
  lu_target$Postcode
)

# Use the main district found in each postcode sector
sector_lad <- aggregate(
  District ~ Sector,
  data = lu_target,
  FUN = function(x) {
    names(
      sort(
        table(x),
        decreasing = TRUE
      )
    )[1]
  }
)

# Read the population data
pop <- read.csv(
  population_file,
  stringsAsFactors = FALSE
)

names(pop) <- tolower(names(pop))

pop_postcode_column <- grep(
  "^postcode$|^pcds$|^pcd$",
  names(pop),
  value = TRUE
)[1]

pop_population_column <- grep(
  "^population$|usual.*resident|all.*person",
  names(pop),
  value = TRUE
)[1]

if (
  is.na(pop_postcode_column) ||
  is.na(pop_population_column)
) {
  stop("Population columns not found.")
}

population_data <- pop %>%
  transmute(
    Sector = make_sector(
      .data[[pop_postcode_column]]
    ),
    Population = as.numeric(
      gsub(
        "[^0-9.-]",
        "",
        .data[[pop_population_column]]
      )
    )
  ) %>%
  filter(!is.na(Population))

# Calculate the population for each district
district_population <- population_data %>%
  inner_join(
    sector_lad,
    by = "Sector"
  ) %>%
  group_by(District) %>%
  summarise(
    Population = sum(Population, na.rm = TRUE),
    .groups = "drop"
  )

# Adding the districts together to get each county's population
county_population <- district_population %>%
  inner_join(
    district_info,
    by = "District"
  ) %>%
  group_by(County) %>%
  summarise(
    Population = sum(Population, na.rm = TRUE),
    .groups = "drop"
  )

print(county_population)

# Unziping the crime data
crime_folder <- file.path(
  tempdir(),
  "crime_q3"
)

unlink(crime_folder, recursive = TRUE)
dir.create(crime_folder, recursive = TRUE)

unzip(
  crime_zip,
  exdir = crime_folder
)

# Finding Norfolk and Suffolk street-crime files
crime_files <- list.files(
  crime_folder,
  pattern = "street\\.csv$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

crime_file_info <- data.frame(
  File = crime_files,
  FileName = basename(crime_files),
  stringsAsFactors = FALSE
) %>%
  mutate(
    FileMonth = substr(FileName, 1, 7),
    County = case_when(
      grepl("norfolk", FileName, ignore.case = TRUE) ~
        "NORFOLK",
      grepl("suffolk", FileName, ignore.case = TRUE) ~
        "SUFFOLK",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(County),
    FileMonth >= "2023-06",
    FileMonth <= "2026-04"
  )

if (nrow(crime_file_info) == 0) {
  stop("No crime files were found for the selected dates.")
}

# Reading all the monthly files
crime_data <- bind_rows(
  lapply(
    seq_len(nrow(crime_file_info)),
    function(i) {
      
      current_file <- read.csv(
        crime_file_info$File[i],
        stringsAsFactors = FALSE
      )
      
      if (
        !all(
          c("Month", "Crime.type") %in%
          names(current_file)
        )
      ) {
        return(NULL)
      }
      
      current_file %>%
        select(
          Month,
          Crime.type
        ) %>%
        mutate(
          County = crime_file_info$County[i]
        )
    }
  )
)

# Count drug offences each month
monthly_drug_counts <- crime_data %>%
  filter(
    Crime.type == "Drugs"
  ) %>%
  mutate(
    Date = as.Date(
      paste0(Month, "-01")
    )
  ) %>%
  filter(
    Date >= as.Date("2023-06-01"),
    Date <= as.Date("2026-04-01")
  ) %>%
  group_by(
    Date,
    County
  ) %>%
  summarise(
    Drug_Offences = n(),
    .groups = "drop"
  )

# Including every month even when no offence was recorded
all_months <- expand_grid(
  Date = seq(
    as.Date("2023-06-01"),
    as.Date("2026-04-01"),
    by = "month"
  ),
  County = c("NORFOLK", "SUFFOLK")
)

monthly_drug_rate <- all_months %>%
  left_join(
    monthly_drug_counts,
    by = c("Date", "County")
  ) %>%
  mutate(
    Drug_Offences = replace_na(
      Drug_Offences,
      0
    )
  ) %>%
  left_join(
    county_population,
    by = "County"
  ) %>%
  mutate(
    Rate10k = (
      Drug_Offences /
        Population
    ) * 10000
  )

print(monthly_drug_rate)

# Norfolk chart
norfolk_chart <- monthly_drug_rate %>%
  filter(County == "NORFOLK") %>%
  ggplot(
    aes(
      x = Date,
      y = Rate10k
    )
  ) +
  geom_line(
    colour = "darkred",
    linewidth = 1
  ) +
  geom_point(
    colour = "darkred",
    size = 1.5
  ) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y"
  ) +
  labs(
    title = "Drug Offence Rate in Norfolk",
    subtitle = "June 2023 to April 2026",
    x = "Month",
    y = "Drug offences per 10,000 people"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

# Suffolk chart
suffolk_chart <- monthly_drug_rate %>%
  filter(County == "SUFFOLK") %>%
  ggplot(
    aes(
      x = Date,
      y = Rate10k
    )
  ) +
  geom_line(
    colour = "darkblue",
    linewidth = 1
  ) +
  geom_point(
    colour = "darkblue",
    size = 1.5
  ) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y"
  ) +
  labs(
    title = "Drug Offence Rate in Suffolk",
    subtitle = "June 2023 to April 2026",
    x = "Month",
    y = "Drug offences per 10,000 people"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

# Save both charts
ggsave(
  file.path(
    base_folder,
    "Norfolk_drug_offence_rate.png"
  ),
  plot = norfolk_chart,
  width = 12,
  height = 7,
  dpi = 300
)

ggsave(
  file.path(
    base_folder,
    "Suffolk_drug_offence_rate.png"
  ),
  plot = suffolk_chart,
  width = 12,
  height = 7,
  dpi = 300
)
print(norfolk_chart)
print(suffolk_chart)


#school------
# Q1: Attainment 8 box plots for 2021/22

library(tidyverse)
base_folder <- "C:/Users/User/OneDrive/ドキュメント"
academic_year <- "2021/22"

# Find the two files
norfolk_zip <- list.files(
  base_folder,
  pattern = "^Performancetables_114451.*\\.zip$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)[1]

suffolk_zip <- list.files(
  base_folder,
  pattern = "^Performancetables_114530.*\\.zip$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)[1]

if (is.na(norfolk_zip)) {
  stop("Norfolk ZIP file was not found.")
}

if (is.na(suffolk_zip)) {
  stop("Suffolk ZIP file was not found.")
}


# Read the KS4 file for one county
read_ks4_data <- function(zip_file, county_name) {
  
  output_folder <- file.path(
    tempdir(),
    paste0("ks4_", county_name)
  )
  
  unlink(output_folder, recursive = TRUE)
  dir.create(output_folder, recursive = TRUE)
  
  unzip(
    zip_file,
    exdir = output_folder
  )
  
  csv_files <- list.files(
    output_folder,
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  ks4_file <- csv_files[
    grepl(
      "ks4.*final\\.csv$",
      basename(csv_files),
      ignore.case = TRUE
    )
  ][1]
  
  if (is.na(ks4_file)) {
    ks4_file <- csv_files[
      grepl(
        "ks4",
        basename(csv_files),
        ignore.case = TRUE
      )
    ][1]
  }
  
  if (is.na(ks4_file)) {
    stop(paste("KS4 file was not found for", county_name))
  }
  
  school_data <- read.csv(
    ks4_file,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8-BOM"
  )
  
  # Find the Attainment 8 column
  attainment_column <- grep(
    "ATT.*8|8.*ATT",
    names(school_data),
    value = TRUE,
    ignore.case = TRUE
  )[1]
  
  # Find the town column
  town_column <- grep(
    "^TOWN$|POST.?TOWN|LOCALITY",
    names(school_data),
    value = TRUE,
    ignore.case = TRUE
  )[1]
  
  if (is.na(attainment_column)) {
    print(
      names(school_data)[
        grepl(
          "ATT|SCORE|8",
          names(school_data),
          ignore.case = TRUE
        )
      ]
    )
    
    stop(
      paste(
        "Attainment 8 column was not found for",
        county_name
      )
    )
  }
  
  if (is.na(town_column)) {
    print(
      names(school_data)[
        grepl(
          "TOWN|LOCAL|DISTRICT",
          names(school_data),
          ignore.case = TRUE
        )
      ]
    )
    
    stop(
      paste(
        "Town column was not found for",
        county_name
      )
    )
  }
  
  school_data %>%
    transmute(
      County = county_name,
      
      Town = str_to_title(
        str_squish(
          as.character(
            .data[[town_column]]
          )
        )
      ),
      
      Attainment8 = parse_number(
        as.character(
          .data[[attainment_column]]
        ),
        na = c(
          "",
          "NA",
          "SUPP",
          "NE",
          "NP",
          "."
        )
      )
    ) %>%
    filter(
      !is.na(Town),
      Town != "",
      !is.na(Attainment8),
      Attainment8 >= 0,
      Attainment8 <= 100
    )
}


# Read both counties
norfolk_data <- read_ks4_data(
  norfolk_zip,
  "Norfolk"
)

suffolk_data <- read_ks4_data(
  suffolk_zip,
  "Suffolk"
)

# Put the data together
attainment_data <- bind_rows(
  norfolk_data,
  suffolk_data
)

print(
  attainment_data %>%
    count(County, Town)
)


# Norfolk box plot
norfolk_boxplot <- attainment_data %>%
  filter(County == "Norfolk") %>%
  ggplot(
    aes(
      x = reorder(
        Town,
        Attainment8,
        median
      ),
      y = Attainment8
    )
  ) +
  geom_boxplot(
    fill = "lightblue"
  ) +
  coord_flip() +
  labs(
    title = "Average Attainment 8 Score in Norfolk",
    subtitle = paste("Academic year", academic_year),
    x = "Town",
    y = "Average Attainment 8 score"
  ) +
  theme_minimal()


# Suffolk box plot
suffolk_boxplot <- attainment_data %>%
  filter(County == "Suffolk") %>%
  ggplot(
    aes(
      x = reorder(
        Town,
        Attainment8,
        median
      ),
      y = Attainment8
    )
  ) +
  geom_boxplot(
    fill = "lightgreen"
  ) +
  coord_flip() +
  labs(
    title = "Average Attainment 8 Score in Suffolk",
    subtitle = paste("Academic year", academic_year),
    x = "Town",
    y = "Average Attainment 8 score"
  ) +
  theme_minimal()


# Save the plots
ggsave(
  file.path(
    base_folder,
    "Norfolk_Attainment8_2021_22.png"
  ),
  plot = norfolk_boxplot,
  width = 10,
  height = 8,
  dpi = 300
)

ggsave(
  file.path(
    base_folder,
    "Suffolk_Attainment8_2021_22.png"
  ),
  plot = suffolk_boxplot,
  width = 10,
  height = 8,
  dpi = 300
)


# Show the plots
print(norfolk_boxplot)
print(suffolk_boxplot)

# Average Attainment 8 score by district from 2021/22 to 2024/25

library(tidyverse)

base_folder <- "C:/Users/User/OneDrive/ドキュメント"

# Download the official district-level KS4 data
data_url <- paste0(
  "https://api.education.gov.uk/statistics/v1/data-sets/",
  "18e39901-e1a0-3b70-aeae-b3fe542fcf1c/csv"
)
ks4_data <- read_csv(
  data_url,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

# Districts in both counties
norfolk_districts <- c(
  "Breckland",
  "Broadland",
  "Great Yarmouth",
  "King's Lynn and West Norfolk",
  "North Norfolk",
  "Norwich",
  "South Norfolk"
)

suffolk_districts <- c(
  "Babergh",
  "East Suffolk",
  "Ipswich",
  "Mid Suffolk",
  "West Suffolk"
)

# Keep the total Attainment 8 results
district_attainment <- ks4_data %>%
  filter(
    geographic_level == "Local authority district",
    geography_basis == "School location",
    establishment_type_group == "All state-funded",
    breakdown_topic == "Total",
    breakdown == "Total",
    disadvantage_status == "Total",
    fsm_status == "Total",
    time_period %in% c(
      "202122",
      "202223",
      "202324",
      "202425"
    ),
    lad_name %in% c(
      norfolk_districts,
      suffolk_districts
    )
  ) %>%
  transmute(
    District = lad_name,
    
    County = case_when(
      District %in% norfolk_districts ~ "Norfolk",
      District %in% suffolk_districts ~ "Suffolk"
    ),
    
    AcademicYear = case_when(
      time_period == "202122" ~ "2021/22",
      time_period == "202223" ~ "2022/23",
      time_period == "202324" ~ "2023/24",
      time_period == "202425" ~ "2024/25"
    ),
    
    Average_Attainment8 = suppressWarnings(
      parse_number(attainment8_average)
    )
  ) %>%
  filter(
    !is.na(Average_Attainment8)
  ) %>%
  mutate(
    AcademicYear = factor(
      AcademicYear,
      levels = c(
        "2021/22",
        "2022/23",
        "2023/24",
        "2024/25"
      )
    )
  )

print(district_attainment)

# Norfolk line chart
norfolk_chart <- district_attainment %>%
  filter(County == "Norfolk") %>%
  ggplot(
    aes(
      x = AcademicYear,
      y = Average_Attainment8,
      colour = District,
      group = District
    )
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Average Attainment 8 Score by District in Norfolk",
    subtitle = "Academic years 2021/22 to 2024/25",
    x = "Academic year",
    y = "Average Attainment 8 score",
    colour = "District"
  ) +
  theme_minimal()

# Suffolk line chart
suffolk_chart <- district_attainment %>%
  filter(County == "Suffolk") %>%
  ggplot(
    aes(
      x = AcademicYear,
      y = Average_Attainment8,
      colour = District,
      group = District
    )
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Average Attainment 8 Score by District in Suffolk",
    subtitle = "Academic years 2021/22 to 2024/25",
    x = "Academic year",
    y = "Average Attainment 8 score",
    colour = "District"
  ) +
  theme_minimal()

# Save the charts
ggsave(
  file.path(
    base_folder,
    "Norfolk_Attainment8_2021_2025.png"
  ),
  plot = norfolk_chart,
  width = 11,
  height = 7,
  dpi = 300
)

ggsave(
  file.path(
    base_folder,
    "Suffolk_Attainment8_2021_2025.png"
  ),
  plot = suffolk_chart,
  width = 11,
  height = 7,
  dpi = 300
)

# Show the charts
print(norfolk_chart)
print(suffolk_chart)



# Linear models for Norfolk and Suffolk districts-----

library(tidyverse)
library(lubridate)
library(scales)

base_folder <- "C:/Users/User/OneDrive/ドキュメント"


# Check that the earlier parts of the code have been run
required_objects <- c(
  "district_average",
  "perf",
  "lu",
  "postcode_column",
  "district_column",
  "district_population",
  "district_attainment"
)

missing_objects <- required_objects[
  !vapply(required_objects, exists, logical(1))
]

if (length(missing_objects) > 0) {
  stop(
    paste(
      "Run the earlier code first. Missing objects:",
      paste(missing_objects, collapse = ", ")
    )
  )
}


# Keep district names in the same format
clean_key <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("’", "'") %>%
    str_squish() %>%
    str_to_upper() %>%
    str_remove_all("[^A-Z0-9]")
}


# Keep postcodes in the same format
clean_postcode <- function(x) {
  x %>%
    as.character() %>%
    str_to_upper() %>%
    str_remove_all("[^A-Z0-9]")
}


# Districts included in the analysis
district_list <- tibble(
  District = c(
    "Breckland",
    "Broadland",
    "Great Yarmouth",
    "King's Lynn and West Norfolk",
    "North Norfolk",
    "Norwich",
    "South Norfolk",
    "Babergh",
    "East Suffolk",
    "Ipswich",
    "Mid Suffolk",
    "West Suffolk"
  ),
  
  County = c(
    rep("Norfolk", 7),
    rep("Suffolk", 5)
  )
) %>%
  mutate(
    DistrictKey = clean_key(District)
  )


# Average house price in each district
house_lm <- district_average %>%
  transmute(
    DistrictKey = clean_key(district),
    HousePrice = as.numeric(average_price)
  ) %>%
  group_by(DistrictKey) %>%
  summarise(
    HousePrice = mean(HousePrice, na.rm = TRUE),
    .groups = "drop"
  )


# Link broadband postcodes with local authority districts
postcode_lookup_lm <- lu %>%
  transmute(
    PostcodeKey = clean_postcode(
      .data[[postcode_column]]
    ),
    
    DistrictKey = clean_key(
      .data[[district_column]]
    )
  ) %>%
  filter(
    PostcodeKey != "",
    DistrictKey != ""
  ) %>%
  distinct(
    PostcodeKey,
    .keep_all = TRUE
  )


# Average download speed in each district
speed_lm <- perf %>%
  transmute(
    PostcodeKey = clean_postcode(postcode_space),
    
    DownloadSpeed = suppressWarnings(
      as.numeric(
        Average.download.speed..Mbit.s.
      )
    )
  ) %>%
  filter(
    !is.na(DownloadSpeed)
  ) %>%
  inner_join(
    postcode_lookup_lm,
    by = "PostcodeKey"
  ) %>%
  filter(
    DistrictKey %in% district_list$DistrictKey
  ) %>%
  group_by(DistrictKey) %>%
  summarise(
    DownloadSpeed = mean(
      DownloadSpeed,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# Population of each district
population_lm <- district_population %>%
  transmute(
    DistrictKey = clean_key(District),
    Population = as.numeric(Population)
  )


# Find the crime ZIP file
crime_zip_lm <- list.files(
  base_folder,
  pattern = "^427a01f6c4d0f343ed1cb89053d0361e3caa0cd3.*\\.zip$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)[1]

if (is.na(crime_zip_lm)) {
  stop("Crime ZIP file was not found.")
}


# Open the crime data
crime_folder_lm <- file.path(
  tempdir(),
  "crime_linear_models"
)

unlink(
  crime_folder_lm,
  recursive = TRUE
)

dir.create(
  crime_folder_lm,
  recursive = TRUE
)

unzip(
  crime_zip_lm,
  exdir = crime_folder_lm
)


# Find the 2024 street-crime files
crime_files_lm <- list.files(
  crime_folder_lm,
  pattern = "street\\.csv$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

crime_files_lm <- crime_files_lm[
  str_detect(
    str_to_lower(
      basename(crime_files_lm)
    ),
    "^2024-.*(norfolk|suffolk)-street\\.csv$"
  )
]

if (length(crime_files_lm) == 0) {
  stop("No Norfolk or Suffolk 2024 crime files were found.")
}


# Read the 2024 crime records
drug_records <- map_dfr(
  crime_files_lm,
  function(file) {
    
    read_csv(
      file,
      col_types = cols(.default = col_character()),
      show_col_types = FALSE
    ) %>%
      select(
        Month,
        `LSOA name`,
        `Crime type`
      )
  }
) %>%
  mutate(
    District = str_remove(
      `LSOA name`,
      "\\s+[0-9]{3}[A-Z]$"
    ),
    
    District = str_replace_all(
      District,
      "’",
      "'"
    ),
    
    District = case_when(
      District %in% c(
        "Suffolk Coastal",
        "Waveney"
      ) ~ "East Suffolk",
      
      District %in% c(
        "Forest Heath",
        "St Edmundsbury"
      ) ~ "West Suffolk",
      
      TRUE ~ District
    ),
    
    DistrictKey = clean_key(District)
  ) %>%
  filter(
    `Crime type` == "Drugs",
    DistrictKey %in% district_list$DistrictKey
  )


# Drug-offence rate per 10,000 people
drug_lm <- drug_records %>%
  count(
    DistrictKey,
    name = "DrugOffences"
  ) %>%
  left_join(
    population_lm,
    by = "DistrictKey"
  ) %>%
  mutate(
    DrugRate = (
      DrugOffences / Population
    ) * 10000
  )


# Use 2023/24 Attainment 8 results
attainment_lm <- district_attainment %>%
  filter(
    as.character(AcademicYear) == "2023/24"
  ) %>%
  transmute(
    DistrictKey = clean_key(District),
    Attainment8 = as.numeric(
      Average_Attainment8
    )
  )


# Combine everything into one district dataset
model_data <- district_list %>%
  left_join(
    house_lm,
    by = "DistrictKey"
  ) %>%
  left_join(
    speed_lm,
    by = "DistrictKey"
  ) %>%
  left_join(
    drug_lm %>%
      select(
        DistrictKey,
        DrugRate
      ),
    by = "DistrictKey"
  ) %>%
  left_join(
    attainment_lm,
    by = "DistrictKey"
  )

print(model_data)

write_csv(
  model_data,
  file.path(
    base_folder,
    "linear_model_district_data.csv"
  )
)


# Function for creating each model and scatter plot
run_linear_model <- function(
    data,
    x_variable,
    y_variable,
    title_text,
    x_label,
    y_label,
    file_name
) {
  
  model_values <- data %>%
    select(
      District,
      County,
      all_of(
        c(
          x_variable,
          y_variable
        )
      )
    ) %>%
    drop_na()
  
  if (nrow(model_values) < 3) {
    stop(
      paste(
        "Not enough complete district data for",
        title_text
      )
    )
  }
  
  model_formula <- reformulate(
    x_variable,
    response = y_variable
  )
  
  fitted_model <- lm(
    model_formula,
    data = model_values
  )
  
  model_plot <- ggplot(
    model_values,
    aes(
      x = .data[[x_variable]],
      y = .data[[y_variable]]
    )
  ) +
    geom_point(
      aes(shape = County),
      size = 3
    ) +
    geom_smooth(
      method = "lm",
      se = TRUE
    ) +
    geom_text(
      aes(label = District),
      vjust = -0.7,
      size = 3,
      check_overlap = TRUE
    ) +
    labs(
      title = title_text,
      x = x_label,
      y = y_label,
      shape = "County"
    ) +
    theme_minimal()
  
  if (x_variable == "HousePrice") {
    model_plot <- model_plot +
      scale_x_continuous(
        labels = label_currency(
          prefix = "£",
          big.mark = ","
        )
      )
  }
  
  if (y_variable == "HousePrice") {
    model_plot <- model_plot +
      scale_y_continuous(
        labels = label_currency(
          prefix = "£",
          big.mark = ","
        )
      )
  }
  
  print(model_plot)
  print(summary(fitted_model))
  
  ggsave(
    file.path(
      base_folder,
      file_name
    ),
    plot = model_plot,
    width = 9,
    height = 7,
    dpi = 300
  )
  
  list(
    model = fitted_model,
    plot = model_plot,
    data = model_values
  )
}


# 1. House price versus download speed
model_1 <- run_linear_model(
  model_data,
  x_variable = "DownloadSpeed",
  y_variable = "HousePrice",
  title_text = "House Price versus Download Speed",
  x_label = "Average download speed (Mbit/s)",
  y_label = "Average house price",
  file_name = "LM1_house_price_download_speed.png"
)


# 2. House price versus drug-offence rate
model_2 <- run_linear_model(
  model_data,
  x_variable = "DrugRate",
  y_variable = "HousePrice",
  title_text = "House Price versus Drug-Offence Rate",
  x_label = "Drug offences per 10,000 people",
  y_label = "Average house price",
  file_name = "LM2_house_price_drug_rate.png"
)


# 3. Attainment 8 v/s house price
model_3 <- run_linear_model(
  model_data,
  x_variable = "HousePrice",
  y_variable = "Attainment8",
  title_text = "Average Attainment 8 Score versus House Price",
  x_label = "Average house price",
  y_label = "Average Attainment 8 score",
  file_name = "LM3_attainment8_house_price.png"
)


# 4. Attainment 8 v/s drug-offence rate
model_4 <- run_linear_model(
  model_data,
  x_variable = "DrugRate",
  y_variable = "Attainment8",
  title_text = "Average Attainment 8 Score versus Drug-Offence Rate",
  x_label = "Drug offences per 10,000 people",
  y_label = "Average Attainment 8 score",
  file_name = "LM4_attainment8_drug_rate.png"
)


# 5. Download speed versus drug-offence rate
model_5 <- run_linear_model(
  model_data,
  x_variable = "DrugRate",
  y_variable = "DownloadSpeed",
  title_text = "Average Download Speed versus Drug-Offence Rate",
  x_label = "Drug offences per 10,000 people",
  y_label = "Average download speed (Mbit/s)",
  file_name = "LM5_download_speed_drug_rate.png"
)


# 6. Attainment 8 versus download speed
model_6 <- run_linear_model(
  model_data,
  x_variable = "DownloadSpeed",
  y_variable = "Attainment8",
  title_text = "Average Attainment 8 Score versus Download Speed",
  x_label = "Average download speed (Mbit/s)",
  y_label = "Average Attainment 8 score",
  file_name = "LM6_attainment8_download_speed.png"
)


# Put the main model results into one table
model_list <- list(
  "House price vs download speed" = model_1$model,
  "House price vs drug rate" = model_2$model,
  "Attainment 8 vs house price" = model_3$model,
  "Attainment 8 vs drug rate" = model_4$model,
  "Download speed vs drug rate" = model_5$model,
  "Attainment 8 vs download speed" = model_6$model
)

model_summary <- imap_dfr(
  model_list,
  function(model, model_name) {
    
    coefficient_table <- summary(model)$coefficients
    
    tibble(
      Model = model_name,
      Number_of_Districts = nobs(model),
      Intercept = unname(coef(model)[1]),
      Slope = unname(coef(model)[2]),
      R_squared = summary(model)$r.squared,
      P_value = coefficient_table[2, 4]
    )
  }
)

print(model_summary)

write_csv(
  model_summary,
  file.path(
    base_folder,
    "linear_model_results.csv"
  )
)

#data cleaning------
#house price 
# Keep Norfolk and Suffolk records
house_data <- house_data %>%
  filter(county %in% c("NORFOLK", "SUFFOLK")) %>%
  filter(!is.na(price), price > 0) %>%
  mutate(
    county = str_to_upper(str_squish(county)),
    district = str_to_title(str_squish(district))
  )

#crime
crime_data <- crime_data %>%
  mutate(
    Date = ymd(paste0(Month, "-01")),
    District = str_remove(`LSOA name`, "\\s+[0-9]{3}[A-Z]$")
  ) %>%
  filter(
    !is.na(Date),
    !is.na(District),
    District != ""
  )

#postcode
clean_postcode <- function(x) {
  str_to_upper(
    str_remove_all(as.character(x), "[^A-Z0-9]")
  )
}

#school
school_data <- school_data %>%
  mutate(
    Attainment8 = parse_number(
      Attainment8,
      na = c("", "SUPP", "NE", "NP", ".")
    )
  ) %>%
  filter(!is.na(Attainment8))

#broadband speed
perf <- perf %>%
  mutate(
    AverageDownload = parse_number(
      as.character(Average.download.speed..Mbit.s.),
      na = c("", "NA", "N/A", ".")
    ),
    MaximumDownload = parse_number(
      as.character(Maximum.download.speed..Mbit.s.),
      na = c("", "NA", "N/A", ".")
    )
  ) %>%
  filter(
    !is.na(AverageDownload),
    !is.na(MaximumDownload)
  )