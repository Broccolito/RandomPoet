rm(list = ls())

library("rjson")
library("stringr")

get_directory = function(){
  args <- commandArgs(trailingOnly = FALSE)
  file <- "--file="
  rstudio <- "RStudio"
  
  match <- grep(rstudio, args)
  if(length(match) > 0){
    return(dirname(rstudioapi::getSourceEditorContext()$path))
  }else{
    match <- grep(file, args)
    if (length(match) > 0) {
      return(dirname(normalizePath(sub(file, "", args[match]))))
    }else{
      return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
    }
  }
}

wd = get_directory()
json_wd = paste0(wd, "/json")
setwd(json_wd)

filelist = c(list.files(pattern = "poet.tang"), list.files(pattern = "poet.song"))
filelist = paste0("json/", filelist)

setwd(wd)

# Load Poets
poet_list = vector()
for(f in filelist){
  cat(paste0("\n-------------  Loading ", f, "  -------------\n"))
  d = fromJSON(file = f)
  l = length(d)
  for(i in 1:l){
    poet_list = c(poet_list, d[[l]]$paragraphs)
  }
}

#Load Strains
strain_list = vector()
for(f in filelist){
  cat(paste0("\n-------------  Loading ", f, "  -------------\n"))
  d = fromJSON(file = f)
  l = length(d)
  for(i in 1:l){
    strain_list = c(strain_list, d[[l]]$strains)
  }
}

save(poet_list, file = "poet.rds")
save(strain_list, file = "strain.rds")

cat(paste0("\n-------------  Loading Successful -------------\n"))

