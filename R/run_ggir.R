run_ggir <- function(folders = NULL) {
  # Run GGIR analysis
  for (i in folders) {
    GGIR::GGIR(
      #------------------------
      # General parameters ----
      #------------------------

      # Directory and file info ----
      datadir = paste0("./data/gt3x/", i), # Set data file directory.
      idloc = 2, # Looks at the file name and extracts the character string
      # preceding the first occurrence of a “_” to get participant ID.

      # Device info ----
      sensor.location = "hip",
      dataFormat = "raw",

      # Computation settings ----
      mode = 1:5, # Runs all the 5 parts of the GGIR pipeline.
      overwrite = FALSE, # Do not overwrite previous milestones (default is FALSE).
      do.parallel = TRUE, # Use parallel computing.
      maxNcores = 6,

      # Output settings ----
      outputdir = paste0("./data/ggir_output"), # Set output directory.
      sep_reports = ";", # Report file will have columns.
      dec_reports = ",", # Report file will use commas for decimals.
      sep_config = ";", # Config file will have columns.
      dec_config = ",", # Config file will use commas for decimals.

      # Other settings ----
      print.filename = TRUE, # Print the file name before analysis.
      imputeTimegaps = TRUE, # Default setting. Larger than 1 sample should be imputed.

      #-----------------------
      # Part 1 parameters ----
      #-----------------------

      # Acceleration metric to be used for analysis ----
      do.enmo = TRUE,
      acc.metric = "ENMO",

      # Nonwear time detection ----
      windowsizes = c(5, 900, 3600),
      # The 1rst number sets the epoch length for aggregating data (in seconds).
      # The 2nd number sets the time interval between the starts of the time windows
      # used to detect nonwear time (in seconds).
      # The 3rd number sets the size of the time window used to detect nonwear time (in seconds).

      #-----------------------
      # Part 2 parameters ----
      #-----------------------

      # Data to be considered for analysis ----
      do.imp = FALSE, # No data imputation, as recommended when the device was not worn during the night.
      data_masking_strategy = 4, # Consider data after the first midnight only.
      max_calendar_days = 7, # Consider 7 days of analysis only.
      timewindow = "MM", # Each day is defined as the period that goes from midnight to midnight.
      includedaycrit = c(10, 10), # Minimum number of hours for a valid day (P2 + P5).
      dayborder = 0, # Hour at which days start and end (default is 0).
      qwindow = c(0, 24), # Segment(s) of the day to be analysed.

      # Compute intensity distribution metrics of interest
      qlevels = c(
        c(1320 / 1440), # Computes M120.
        c(1380 / 1440), # Computes M60.
        c(1395 / 1440), # Computes M45.
        c(1410 / 1440), # Computes M30.
        c(1420 / 1440), # Computes M20.
        c(1425 / 1440), # Computes M15.
        c(1430 / 1440), # Computes M10.
        c(1435 / 1440), # Computes M5.
        c(1438 / 1440), # Computes M2.
        c(1439 / 1440) # Computes M1.
      ),
      iglevels = TRUE, # Compute intensity gradient with default configuration
      # (i.e., iglevels = c(seq(0, 4000, by = 25), 8000)).

      #-----------------------
      # Parts 3 + 4 parameters ----
      #-----------------------

      # Sustained inactivity bout (SIB) detection settings ----
      HASIB.algo = "vanHees2015",

      # Sleep bout detection settings ----
      relyonguider = FALSE,
      HASPT.algo = c("NotWorn", "HDCZA"), # Suggested when the device should not be worn during the night.
      HASPT.ignore.invalid = NA, # Recommended when HASPT.algo = 'NotWorn".
      ignorenonwear = FALSE, # Recommended when HASPT.algo = 'NotWorn".
      includenightcrit = 10,

      #-----------------------
      # Part 5 parameters ----
      #-----------------------

      # Physical activity intensity thresolds ----
      threshold.lig = 63.3, # Hildebrand et al., 2017; DOI: 10.1111/sms.12795.
      threshold.mod = 142.6, # Hildebrand et al., 2014; DOI: DOI: 10.1249/MSS.0000000000000289.
      threshold.vig = 464.6, # Hildebrand et al., 2014; DOI: DOI: 10.1249/MSS.0000000000000289.

      # Bout features ----
      boutcriter.in = 0.9, # Default value.
      boutcriter.lig = 0.8, # Default value.
      boutcriter.mvpa = 0.8, # Default value.
      boutdur.in = c(10, 20, 30), # Default value.
      boutdur.lig = c(1, 5, 10), # Default value.
      boutdur.mvpa = c(1, 5, 10), # Default value.

      # Fragmentation metrics ----
      frag.metrics = "all",
      part5_agg2_60seconds = FALSE, # Does not get metrics averaged per minute.

      # Export raw data ----
      save_ms5rawlevels = TRUE # Export 5-s data.
    )
  }
}
