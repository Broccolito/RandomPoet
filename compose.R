setwd("C:\\Users\\lenovo\\Desktop\\RandomPoet")

library("pinyin")

if(!exists("desktop")){
  source("fileoperation.r")
}

if(!exists("dict.total")){
  source("makedict.r")
}

getpinyin = function(Char){
  return(pinyin(mychar = Char,method = "toneless",sep = " "))
}
getyun = function(char){
  charpinyin = getpinyin(Char = char)
  charpinyin = unlist(strsplit(charpinyin,""))
  for(i in 1:length(charpinyin)){
    if(any(charpinyin[i] == c("a","e","i","o","u"))){
      temyun = charpinyin[i:length(charpinyin)];break
    }
  }
  yun = ""
  for(i in 1:length(temyun)){
    yun = paste(yun,temyun[i],sep = "")
  }
  return(yun)
}

wordbutverb = function(){
  selection = c(FALSE,TRUE)
  choice = sample(selection,1)
  if(choice){
    newput = sample(dict.noun,1)
  }else if(!choice){
    newput = sample(dict.adj,1)
  }
  return(newput)
}

create.sent = function(n = 5){
  again = TRUE
  while(again){
    verb = sample(dict.verb,1)
    nadj = wordbutverb()
    while(nchar(nadj) + nchar(verb) > n){
      nadj = wordbutverb()
    }
    if(nchar(nadj) + nchar(verb) == n){
      temsent = c(nadj,verb)
      again = FALSE
    }else{
      temsent = c(nadj,verb)
      again = TRUE
    }
  }
  temsent = sample(temsent,length(temsent),replace = FALSE)
  sent = ""
  for(i in 1:length(temsent)){
    sent = paste(sent,temsent[i],sep = "")
  }
  return(sent)
}

print.rand.poem = function(char = 5,sent = 4){
  for(i in 1:sent){
    print(create.sent(char))
  }
}




