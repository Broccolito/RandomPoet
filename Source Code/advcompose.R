library(parallel)

if(!exists("desktop")){
  source("fileoperation.r")
}

if(!exists("create.sent")){
  source("compose.r")
}

poem.withyun = function(char = 5,sent = 4){
  if(any(round(sent/2,1) != round(sent/2,0),sent <= 0)){
    return("Yun can only be achieved when positive even sentences are operated.")
  }else{
    if(!exists("getloop")){
      getloop = function(char = 5){
        if(!exists("getyun.lastchar")){
          getyun.lastchar = function(sentence){
            last.char = unlist(strsplit(sentence,""))
            last.char = last.char[length(last.char)]
            yun = getyun(last.char)
            return(yun)
          }
        }
        first.sent = create.sent(char)
        yun = getyun.lastchar(first.sent)
        second.sent = create.sent(char)
        cat("Generating loops with Yun.")
        starcount = "."
        while(getyun.lastchar(second.sent) != yun){
          second.sent = create.sent(char)
          cat(starcount)
        }
        loop = paste(first.sent,",",second.sent,".",sep = "")
        cat("\n")
        return(loop)
      }
    }
    yunpoem = ""
    for(i in 1:sent/2){
      yunpoem = paste(yunpoem,getloop(char = char))
    }
   return(yunpoem)
  }
}