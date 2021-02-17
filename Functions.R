smart_checking <- function(df1, df2, classtime){
  outcomes <- list()
  is_error1 <- "NO"
  is_error2 <- "NO"
  outcomes$error_no1 <- ""
  outcomes$error_no2 <- ""
  
  if (nrow(df1)==0){
    outcomes$error_no1 <- c("  Your student roster file is empty  ")
    is_error1 <- "YES"
  }
  
  if (nrow(df2)==0){
    outcomes$error_no2 <- c("  Your zoom usage report file is empty  ")
    is_error2 <- "YES"
  }
  
  if ((is_error1=="NO")&(is_error2=="NO")){
    Name_line <- which(df1[,1]=="Name")
    
    if (length(Name_line)==1){
      colnames(df1) <- df1[Name_line, ]
      df1 <- df1[-c(1:Name_line),]
    }

    df1 <- na.omit(df1)
    
    allnames <- strsplit(df1$Name, ", " )
    df1$last_name <- NA
    df1$first_name <- NA
    df1$whole_name <- NA
    for (i in 1:nrow(df1)){
      df1$last_name[i] <- toupper(allnames[[i]][1])
      df1$first_name[i] <- toupper(allnames[[i]][2])
      df1$whole_name[i] <- paste(df1$first_name[i], df1$last_name[i])
    }
    
    colnames(df2) <- c("Name", "Email", "Join", "Leave", "Duration", "Guest")
    df2$Name <- toupper(df2$Name)
    
    # group data by user names, so we can tally the total time a participant stayed in class
    # this is because a person may join zoom multiple times due to internet issues
    df3 <- df2 %>% group_by(Name) %>% summarise(total_time = sum(Duration))
    
    # remove (SHE/HER) and (HE/HIS) in names
    df3$Name <- str_replace(df3$Name, "\\s*\\([SHE/HER\\)]+\\)", "")
    df3$Name <- str_replace(df3$Name, "\\s*\\([HE/HIS\\)]+\\)", "")
    
    check_in <- df1$whole_name %in% df3$Name
    
    outcomes$leave_early <- as.data.frame(cbind(c(df3$Name[which(df3$total_time < classtime)]), 
                                                c(df3$total_time[which(df3$total_time < classtime)])))
    
    colnames(outcomes$leave_early) <- c("Name", "Time_Stay")
    
    outcomes$Cannot_find_their_names <- setdiff(df3$Name, df1$whole_name)
    outcomes$message <- paste("Names cannot be found in the student roster: \n", paste(outcomes$Cannot_find_their_names, sep="", collapse = ", "))
    outcomes$file <- data.frame(Name=df1$Name)
    outcomes$file$Present <- ifelse(check_in==TRUE, 1, 0)
    return(outcomes)
  }
  
  if ((is_error1=="YES")|(is_error2=="YES")){
    outcomes <- list()
    outcomes$error_no1 <- error_no1
    outcomes$error_no2 <- error_no2
    outcomes$leave_early <- data.frame(Name=NA, Time_Stay=NA)
    outcomes$Cannot_find_their_names <- ""
    outcomes$message <- ""
    outcomes$file <- data.frame(Name=NA, Present=NA)
    return(outcomes)
  }
}
