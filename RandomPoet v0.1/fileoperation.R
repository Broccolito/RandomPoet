#This is the script for better management of file directory

#This function will help reset the working directory to desktop
desktop = function(){
  setwd("C:/Users/lenovo/Desktop")
}

#This function will help get into the directory specified
getinto = function(filename){
  if(!dir.exists(filename)){
    print("The file is not found...")
    stop("Wrong directory...")
  }else{
    setwd(paste(getwd(),"/",filename,sep = ""))
    print("The new directory is:")
    getwd()
  }
}

#This function will go back to the previous directory
getback = function(){
  wd = strsplit(getwd(),"/")
  wd = wd[[1]]
  if(length(wd)>1){
    newwd = wd[1:length(wd)-1]
    temwd = ""
    for(i in 1:length(newwd)){
      temwd = paste(temwd,newwd[i],sep = "/")
    }
    temwd = substring(temwd,2,nchar(temwd))
    setwd(temwd)
  }else{
    print("The current directory is already the mother directory...")
    warning("Unable to get back to previous directory...")
  }
  print("The current directory is:")
  getwd()
}

#This function will create a massive number of folders with homogeous naming
bat.create = function(filename,number){
  for(i in 1:number){
    dir.create(paste(filename,i,sep = "_"))
  }
}

#This function will either delate all the files in the directory or delate the files with certain extentions
bat.delate = function(type, x){
  fl = list.files()
  if(type == "all"){
    removedfile = ""
    for(i in 1:length(fl)){
      removedfile = c(removedfile,fl[i])
      file.remove(fl[i])
    }
  }else if(type == "ext"){
    removedfile = ""
    for(i in 1:length(fl)){
      temfl = strsplit(fl[i],"[.]")
      temfl = temfl[[1]]
      if(temfl[length(temfl)] == x){
        removedfile = c(removedfile,fl[i])
        file.remove(fl[i])
      }
    }
  }
  if(length(removedfile) > 1){
    print("The files that are removed are listed: ")
    print(removedfile[2:length(removedfile)])
  }
}

#This function will return all the unique file formats in the directory
unique.type = function(){
  fl = list.files()
  ftype = vector()
  for(i in 1:length(fl)){
    temfl = unlist(strsplit(fl[i],"[.]"))
    if(length(temfl) > 1){
      ftype[i] = temfl[length(temfl)]
    }
  }
  ftype = unique(ftype[!is.na(ftype)])
  return(ftype)
}


