##function to load xrf-measurements from a folder
# load_xrf function loads CSV files from a specified folder path and combines them into one data frame.

load_xrf <- function(folder_path) {
  # Get a list of file names for all CSV files in the folder
  csv_files <- list.files(path = folder_path, pattern = ".csv", full.names = TRUE)

  # Create an empty list to store the loaded data
  data <- list()

  # Loop through each CSV file and load its data into the list
  for (file in csv_files) {
    # Load data and add it to the list with the file name as the key
    data[[file]] <- read.csv2(file)

  }

  # Function to get the number of columns in a DataFrame
  get_num_columns <- function(df) {
    ncol(df)
  }

  # Get the number of columns in each DataFrame in the list
  num_columns_list <- lapply(data, get_num_columns)

  # Display the number of columns for each DataFrame
  for (i in 1:length(num_columns_list)) {
    cat(paste("DataFrame", csv_files[i], "has", num_columns_list[[i]], "columns.\n"))
  }


  # Combine all loaded data frames into one
  Measurements <- data.frame()
  for (file in csv_files) {
    Measurements <- rbind(Measurements, data[[file]])
  }

  # Loop through each column and replace commas with periods
  for (col in colnames(Measurements)) {
    Measurements <- Measurements |>
      mutate(!!col := str_replace_all(!!sym(col), ",", "."))
  }

  # Replace "<LOD" with 0 and convert columns 1 and 14:107 to numeric
  Measurements <- Measurements |>
    mutate(across(everything(), ~ifelse(. == "<LOD", 0, .))) |>
    mutate_at(vars(1, 14:107), as.numeric)

  # Removing duplicates
  Measurements <- Measurements |>
    distinct(across(everything()))

  # Return the combined and cleaned data frame
  return(Measurements)
}


##function to get deviations
# deviation_elements function calculates the standard deviation and deviation percentage
# for elements in a dataset based on specified object and category columns.
# It also filters the data based on a threshold value if provided.

deviation_elements <- function(data, object, category, threshold = FALSE) {

  # Pivot the data to long format to work with elements as rows
  data <- data %>%
    pivot_longer(cols = Mo:Mg, names_to = "element", values_to = "value") %>%

    # Group the data by the specified category and filter out groups with only one row
    group_by({{category}}) |>
    filter(n() > 1) |>

    # Group the data by object, category, and element, then calculate standard deviation
    group_by({{object}}, {{category}}, element) |>
    summarise(
      deviation_percent = sd(value) / mean(value) * 100
    )

  # If a threshold is specified, filter the data based on the threshold
  if (threshold != FALSE) {
    data <- data |>
      filter(deviation_percent > threshold | deviation_percent < -threshold)
  }

  return(data)
}

## function to get mean without outlier
# removes outliers based on a threshold using either iqr or z-score
# and then calculates the mean based on the values that are left

remove_outliers_and_calculate_mean <- function(values, threshold = 5, method = "iqr") {
  # Check if the method is valid
  if(!method %in% c("z", "iqr")) {
    stop("Invalid method. Use 'z' for Z-score or 'iqr' for Interquartile Range.")
  }

  # Z-score method
  if(method == "z") {
    # Calculate the mean and standard deviation
    mean_value <- mean(values)
    sd_value <- sd(values)

    # Calculate the Z-scores
    z_scores <- (values - mean_value) / sd_value

    # Find values that are within the specified threshold
    non_outliers <- values[abs(z_scores) <= threshold]

  } else if(method == "iqr") {  # IQR method
    # Calculate the IQR (Interquartile Range)
    Q1 <- quantile(values, 0.25)
    Q3 <- quantile(values, 0.75)
    IQR_value <- Q3 - Q1

    # Define the lower and upper bounds for outliers
    lower_bound <- Q1 - threshold * IQR_value
    upper_bound <- Q3 + threshold * IQR_value

    # Find values that are within the IQR-based bounds
    non_outliers <- values[values >= lower_bound & values <= upper_bound]
  }

  # Calculate the mean of the non-outliers
  mean_value <- mean(non_outliers, na.rm = TRUE)

  return(mean_value)
}

remove_outliers_and_calculate_mean <- function(values, threshold = 0.75, method = "iqr") {
  # Remove NA values from the input
  values <- na.omit(values)

  # Check if the method is valid
  if(!method %in% c("z", "iqr")) {
    stop("Invalid method. Use 'z' for Z-score or 'iqr' for Interquartile Range.")
  }

  # Z-score method
  if(method == "z") {
    # Calculate the mean and standard deviation, ignoring NA values
    mean_value <- mean(values, na.rm = TRUE)
    sd_value <- sd(values, na.rm = TRUE)

    # Calculate the Z-scores
    z_scores <- (values - mean_value) / sd_value

    # Find values that are within the specified threshold
    non_outliers <- values[abs(z_scores) <= threshold]

  } else if(method == "iqr") {  # IQR method
    # Calculate the IQR (Interquartile Range), ignoring NA values
    Q1 <- quantile(values, 0.25, na.rm = TRUE)
    Q3 <- quantile(values, 0.75, na.rm = TRUE)
    IQR_value <- Q3 - Q1

    # Define the lower and upper bounds for outliers
    lower_bound <- Q1 - threshold * IQR_value
    upper_bound <- Q3 + threshold * IQR_value

    # Find values that are within the IQR-based bounds
    non_outliers <- values[values >= lower_bound & values <= upper_bound]
  }

  # Calculate the mean of the non-outliers, ignoring NA values
  mean_value <- mean(non_outliers, na.rm = TRUE)

  return(mean_value)
}

# compare_dataframes
## general function to compare to dataframes and get the values that do not match
## enter two dataframes and get every object name and the variable where the two dfs are different

compare_dataframes <- function(df1, df2) {
  # Überprüfen, ob beide DataFrames die gleiche Struktur haben
  if(!all(names(df1) == names(df2))) {
    stop("Die DataFrames haben unterschiedliche Spaltennamen.")
  }

  # Überprüfen, ob beide DataFrames die gleiche Anzahl an Zeilen haben
  if(nrow(df1) != nrow(df2)) {
    stop("Die DataFrames haben unterschiedliche Zeilenanzahl.")
  }

  # Filtern von numerischen Variablen (Spalten)
  numeric_columns <- sapply(df1, is.numeric)

  # Ergebnisse speichern
  differences <- list()

  for(col in names(df1)[numeric_columns]) {
    # Vergleichen der Werte
    unequal_indices <- which(df1[[col]] != df2[[col]])

    if(length(unequal_indices) > 0) {
      # Speichere die Zeilennamen (aus der Spalte "Object") für unterschiedliche Werte
      differences[[col]] <- df1$Object[unequal_indices]
    }
  }

  # Ergebnis anzeigen
  if(length(differences) == 0) {
    message("Alle numerischen Variablen haben identische Werte in beiden DataFrames.")
  } else {
    return(differences)
  }
}


# calculate_mean_sd_percent
## function calculates

calculate_mean_sd_percent <- function(x) {
  mean_value <- mean(x)
  sd_value <- sd(x)
  mean_percent <- (sd_value/ mean_value) * 100  # Durchschnitt in Prozent
  return(mean_percent)
}
