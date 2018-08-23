
file_path<-"~/aki/inst/create_initial_cohort.sql"

parse_sql<-function(file_path){
  con<-file(file_path,"r")
  sql.string <- ""
  
  while (TRUE){
    line <- readLines(con, n = 1)
    
    if (length(line)==0){
      break
    }
    
    line<-gsub("\\t", " ", line)
    
    if(grepl("--",line) == TRUE){
      line <- paste(sub("--","/*",line),"*/")
    }
    
    sql.string <- paste(sql.string, line)
  }
  
  close(con)
  return(sql.string)
}