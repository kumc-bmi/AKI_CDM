## parse Oracle sql lines


parse_sql<-function(file_path){
  #read file
  con<-file(file_path,"r")
  #initialize string
  sql.string <- ""
  
  while (TRUE){
    #parse the first line
    line <- readLines(con, n = 1)
    #check for endings
    if (length(line)==0) break
    #check for overhead comments
    if(grepl("^(/\\*)",line)) next
    #remove the first line
    line<-gsub("\\t", " ", line)
    #translate comment symbol '--'
    if(grepl("--",line) == TRUE){
      line <- paste(sub("--","/*",line),"*/")
    }
    #attach new line
    sql.string <- paste(sql.string, line)
  }
  close(con)
  return(sql.string)
}