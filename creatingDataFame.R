# Load required libraries
library(edfReader)

# Define the directory containing EDF files
edf_dir <- "C:\\Users\\josep\\OneDrive\\Documents\\siena"  # Adjust your path if needed

# List all EDF files
edf_files <- list.files(edf_dir, pattern = "\\.edf$", full.names = TRUE, recursive = TRUE)

# Define EEG channels to extract
selected_channels <- c("EEG Fp1", "EEG F3", "EEG C3", "EEG P3", "EEG O1",
                       "EEG Fp2", "EEG F4", "EEG C4", "EEG P4", "EEG O2")

# Create an output CSV file (clear it first)
output_file <- "all_eeg_data.csv"
write.table(NULL, file = output_file, sep = ",", row.names = FALSE, col.names = FALSE)

# Function to extract EEG signals from an EDF file
extract_eeg_signals <- function(file_path, target_channels = selected_channels) {
  edf_header <- readEdfHeader(file_path)  # Read header
  
  # Get available channels
  available_channels <- edf_header$sHeaders$label
  
  # Find indices of selected channels
  signal_indices <- which(available_channels %in% target_channels)
  
  if (length(signal_indices) == 0) {
    warning(paste("No selected channels found in", file_path))
    return(NULL)
  }
  
  # Read EEG signals (bulk)
  edf_signals <- readEdfSignals(edf_header, signals = signal_indices, simplify = FALSE)
  
  # Process and write each signal to disk
  for (i in seq_along(signal_indices)) {
    channel_name <- available_channels[signal_indices[i]]
    signal_data <- edf_signals[[i]]$signal
    sampling_rate <- edf_signals[[i]]$sRate
    
    # Generate time vector
    time_vector <- seq(from = 0, length.out = length(signal_data), by = 1/sampling_rate)
    
    # Create a dataframe
    eeg_df <- data.frame(
      patient_id = basename(dirname(file_path)),
      file_name = basename(file_path),
      channel = channel_name,
      time = time_vector,
      amplitude = signal_data
    )
    
    # Append to file to avoid RAM overload
    write.table(eeg_df, file = output_file, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    
    # Free memory
    rm(eeg_df)
    gc()
  }
}

# Process all EDF files one by one (prevents crashes)
for (file in edf_files) {
  print(paste("Processing:", file))
  extract_eeg_signals(file)
}

# data is safely stored in "all_eeg_data.csv"
print("processing complete! Data saved to disk.")

