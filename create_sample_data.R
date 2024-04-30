
create_sample_data <- function(folder, output1, output2) {
  ###################### ADD NETWORK MONITORING FUNCTION #####################################
  system('sudo apt-get update && apt-get upgrade -y')
  system('sudo apt-get install -y r-base python3 python3-dev python3-pip python3-venv')
  install.packages("reticulate")
  library(reticulate)
  print(reticulate::py_discover_config())
  use_python("/usr/bin/python3.10")
  print(reticulate::py_discover_config())
  print('test1')
  py_install("psutil")
  print('test2')
  
  psutil <- import("psutil")
  
  # Function to fetch and return formatted network statistics
  get_network_stats <- function() {
    net_io <- psutil$net_io_counters(pernic = TRUE)
    stats <- lapply(names(net_io), function(name) {
      interface_stats <- net_io[[name]]
      list(
        Interface = name,
        Bytes_Sent = interface_stats$bytes_sent,
        Bytes_Recv = interface_stats$bytes_recv,
        Packets_Sent = interface_stats$packets_sent,
        Packets_Recv = interface_stats$packets_recv
      )
    })
    names(stats) <- names(net_io)
    return(stats)
  }
  
  # Function to run any given function with network monitoring
  run_with_network_monitoring <- function(func) {
    before_stats <- get_network_stats()
    func()  # Run the passed function
    after_stats <- get_network_stats()
    
    # Calculate and return the differences in network traffic
    network_diff <- lapply(names(before_stats), function(name) {
      if (name %in% names(after_stats)) {
        before <- before_stats[[name]]
        after <- after_stats[[name]]
        list(
          Interface = name,
          Bytes_Sent_Diff = after$Bytes_Sent - before$Bytes_Sent,
          Bytes_Recv_Diff = after$Bytes_Recv - before$Bytes_Recv,
          Packets_Sent_Diff = after$Packets_Sent - before$Packets_Sent,
          Packets_Recv_Diff = after$Packets_Recv - before$Packets_Recv
        )
      }
    })
    return(network_diff)
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
  
  # Running functions with network monitoring
  log_msg <- paste0('Monitoring Function One:')
  result_one <- run_with_network_monitoring(faasr_put_file(local_file="df1.csv", remote_folder=folder, remote_file=output1))
  log_msg <- paste0(result_one)
  
  log_msg <- paste0('Monitoring Function One:')
  result_two <- run_with_network_monitoring(faasr_put_file(local_file="df2.csv", remote_folder=folder, remote_file=output2))
  log_msg <- paste0(result_two)
  
  
  # Print a log message
  # 
  log_msg <- paste0('create_sample_data finished; outputs written to folder ', folder, ' in default S3 bucket')
  faasr_log(log_msg)
}	
