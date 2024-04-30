create_sample_data <- function(folder, output1, output2) {
  ###################### ADD NETWORK MONITORING FUNCTION #####################################
  # Function to fetch current network statistics using 'ifconfig'
  get_network_stats <- function() {
    system("sudo apt-get -y update", intern = TRUE)
    system("sudo apt-get install -y net-tools", intern = TRUE)
    command <- "ifconfig"
    stats_output <- system(command, intern = TRUE)
    parse_network_interface_stats(stats_output)
  }
  
  # Function to parse output from 'ifconfig'
  parse_network_interface_stats <- function(stats_output) {
    # Initialize an empty list to store stats for each interface
    interface_stats <- list()
    current_interface <- NULL
    lines <- stats_output
    
    # Loop through each line of the output
    for (i in seq_along(lines)) {
      line <- lines[i]
      # Detect the start of a new interface block
      if (grepl("^[a-zA-Z0-9]", line)) {
        if (!is.null(current_interface)) {
          interface_stats[[current_interface$name]] <- current_interface
        }
        current_interface <- list(name = gsub("(:).*", "", line), RX = 0, TX = 0)
      }
      # Extract received bytes
      if (grepl("RX packets", line)) {
        current_interface$RX <- as.numeric(gsub(".*RX bytes:([0-9]+).*", "\\1", line))
      }
      # Extract transmitted bytes
      if (grepl("TX packets", line)) {
        current_interface$TX <- as.numeric(gsub(".*TX bytes:([0-9]+).*", "\\1", line))
      }
    }
    if (!is.null(current_interface)) {
      interface_stats[[current_interface$name]] <- current_interface
    }
    return(interface_stats)
  }
  # Function to run any function with network monitoring
  run_with_network_monitoring <- function(func) {
    before_stats <- get_network_stats()
    func()
    after_stats <- get_network_stats()
    
    # Calculate differences in network traffic
    network_diff <- lapply(names(before_stats), function(name) {
      if (name %in% names(after_stats)) {
        list(
          RX = after_stats[[name]]$RX - before_stats[[name]]$RX,
          TX = after_stats[[name]]$TX - before_stats[[name]]$TX
        )
      } else {
        list(RX = NA, TX = NA)
      }
    })
    names(network_diff) <- names(before_stats)
    network_diff
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
  
  # Initialize log and run functions
  network_activity_log <- list()
  network_activity_log[[1]] <- run_with_network_monitoring(faasr_put_file(local_file="df1.csv", remote_folder=folder, remote_file=output1))
  network_activity_log[[2]] <- run_with_network_monitoring(faasr_put_file(local_file="df2.csv", remote_folder=folder, remote_file=output2))
  
  # Print network monitoring results
  log_msg <- paste0('Network Activity Log:')
  log_msg <- paste0(network_activity_log)
  
  # Print a log message
  # 
  log_msg <- paste0('create_sample_data finished; outputs written to folder ', folder, ' in default S3 bucket')
  faasr_log(log_msg)
}	
