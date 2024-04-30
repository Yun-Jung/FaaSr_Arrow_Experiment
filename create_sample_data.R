create_sample_data <- function(folder, output1, output2) {
  ###################### ADD NETWORK MONITORING FUNCTION #####################################
  # Function to fetch current network statistics for all interfaces
  get_all_network_stats <- function() {
    command <- "sudo apt-get -y install iproute2"
    command <- "ip -s link"
    stats_output <- system(command, intern = TRUE)
    parse_all_network_interface_stats(stats_output)
  }
  
  # Function to parse output for all interfaces
  parse_all_network_interface_stats <- function(stats_output) {
    interfaces <- strsplit(stats_output, "(\n)")[[1]]
    interface_stats <- list()
    current_interface <- NULL
    rx_next <- FALSE
    tx_next <- FALSE
    
    for (line in interfaces) {
      if (grepl("^[0-9]+: ", line)) {  # Start of a new interface block
        if (!is.null(current_interface)) {
          interface_stats[[current_interface]] <- list(RX = rx, TX = tx)
        }
        current_interface <- gsub("^[0-9]+: ([^:]+):.*", "\\1", line)
        rx <- NULL
        tx <- NULL
        rx_next <- FALSE
        tx_next <- FALSE
      } else if (rx_next) {
        rx <- as.numeric(gsub(".* ([0-9]+) .*", "\\1", line))
        rx_next <- FALSE
      } else if (tx_next) {
        tx <- as.numeric(gsub(".* ([0-9]+) .*", "\\1", line))
        tx_next <- FALSE
      }
      if (grepl("RX: bytes", line)) {
        rx_next <- TRUE
      }
      if (grepl("TX: bytes", line)) {
        tx_next <- TRUE
      }
    }
    if (!is.null(current_interface)) {  # Add last interface
      interface_stats[[current_interface]] <- list(RX = rx, TX = tx)
    }
    return(interface_stats)
  }
  
  # Function to compute differences in network stats
  compute_network_diff <- function(before_stats, after_stats) {
    diff_stats <- list()
    for (interface in names(before_stats)) {
      rx_diff <- after_stats[[interface]]$RX - before_stats[[interface]]$RX
      tx_diff <- after_stats[[interface]]$TX - before_stats[[interface]]$TX
      diff_stats[[interface]] <- list(RX_Diff = rx_diff, TX_Diff = tx_diff)
    }
    return(diff_stats)
  }
  
  # Wrapper to run functions with network monitoring
  run_with_network_monitoring <- function(func) {
    before_stats <- get_all_network_stats()
    func()
    after_stats <- get_all_network_stats()
    
    network_diff <- compute_network_diff(before_stats, after_stats)
    network_activity_log[[length(network_activity_log) + 1]] <- network_diff
  }
  
  ###################### ADD NETWORK MONITORING FUNCTION #####################################
  # Create sample files for FaaSr demo and stores in an S3 bucket
  #
  # The function uses the default S3 bucket name, configured in the FaaSr JSON
  # folder: name of the folder where the sample data is to be stored
  # output1, output2: names of the sample files to be created 
  
  # This demo function generates two data frames, exports them as CSV files and store them into S3
  # First we create two data frames df2 and df2
  #
  df1 <- NULL
  for (e in 1:10)
    rbind(df1,data.frame(v1=e,v2=e^2,v3=e^3)) -> df1
  df2 <- NULL
  for (e in 1:10)
    rbind(df2,data.frame(v1=e,v2=2*e,v3=3*e)) -> df2

  # Now we export these data frames to CSV files df1.csv and df2.csv stored in a local directory
  #
  write.table(df1, file="df1.csv", sep=",", row.names=F, col.names=T)
  write.table(df2, file="df2.csv", sep=",", row.names=F, col.names=T)
  
  # Now, upload the these file to the S3 bucket with folder name and file name provided by user
  #
  #faasr_put_file(local_file="df1.csv", remote_folder=folder, remote_file=output1)
  #faasr_put_file(local_file="df2.csv", remote_folder=folder, remote_file=output2)
  
  run_with_network_monitoring(faasr_put_file(local_file="df1.csv", remote_folder=folder, remote_file=output1))
  run_with_network_monitoring(faasr_put_file(local_file="df2.csv", remote_folder=folder, remote_file=output2))
  
  # Print network monitoring results
  log_msg <- paste0('Network Activity Log:')
  log_msg <- paste0(network_activity_log)
  
  # Print a log message
  # 
  log_msg <- paste0('create_sample_data finished; outputs written to folder ', folder, ' in default S3 bucket')
  faasr_log(log_msg)
}	
