database = "È«ÌÆÊ«.txt"
setwd("C:\\Users\\lenovo\\Desktop\\RandomPoet")

if(!exists("desktop")){
  source("fileoperation.r")
}

if(!all(exists("wktag"),exists("wk"),exists("cl"))){
  library(jiebaR)
  wk = jiebaR::worker()
  wktag = jiebaR::worker(type = "tag")
}

gettag = function(string){
  while(!all(exists("wktag"),exists("wk"))){
    library(jiebaR)
    wk = jiebaR::worker()
    wktag = jiebaR::worker(type = "tag")
  }
  if(length(wk[string]) == 1){
    return(names(wktag[string]))
  }else{
    return("x")
  }
}

mkdict.total = function(content){
  dict = data.frame(word = wk[content], tag = names(wktag[content]))
  return(dict)
}

mkdict.n = function(dict){
  if(dim(dict)[2] == 2){
    dict.n = vector()
    n = 1
    for(i in 1:dim(dict)[1]){
      tag = dict[i,2]
      if(any(tag == c("r",
                      "n",
                      "x",
                      "ns",
                      "s",
                      "i",
                      "b",
                      "uv",
                      "nz",
                      "nrt"
      ))){
        dict.n[n] = as.character(dict[i,1])
        n = n + 1
      }
    }
    dict.n = gsub("\\d","",dict.n)
    dict.n = dict.n[!is.na(dict.n)]
    return(dict.n)
  }else{
    return(FALSE)
  }
}

mkdict.v = function(dict){
  if(dim(dict)[2] == 2){
    dict.n = vector()
    n = 1
    for(i in 1:dim(dict)[1]){
      tag = dict[i,2]
      if(any(tag == c("v",
                      "x",
                      "d",
                      "p",
                      "c",
                      "zg"
      ))){
        dict.n[n] = as.character(dict[i,1])
        n = n + 1
      }
    }
    dict.n = gsub("\\d","",dict.n)
    dict.n = dict.n[!is.na(dict.n)]
    return(dict.n)
  }else{
    return(FALSE)
  }
}

mkdict.a = function(dict){
  if(dim(dict)[2] == 2){
    dict.n = vector()
    n = 1
    for(i in 1:dim(dict)[1]){
      tag = dict[i,2]
      if(any(tag == c("a",
                      "t",
                      "n"
      ))){
        dict.n[n] = as.character(dict[i,1])
        n = n + 1
      }
    }
    dict.n = gsub("\\d","",dict.n)
    dict.n = dict.n[!is.na(dict.n)]
    return(dict.n)
  }else{
    return(FALSE)
  }
}

content = readChar(database, nchars = file.info(database)$size)
content = gsub("\\d","",content) #Get rid of the numbers in the content

dict.total = mkdict.total(content = content); rm(content)
dict.noun = mkdict.n(dict.total)
dict.verb = mkdict.v(dict.total)
dict.adj = mkdict.a(dict.total)

dict.noun.one = dict.noun[nchar(dict.noun) == 1]
dict.noun.two = dict.noun[nchar(dict.noun) == 2]
dict.noun.three = dict.noun[nchar(dict.noun) == 3]
dict.noun.four = dict.noun[nchar(dict.noun) == 4]

dict.verb.one = dict.verb[nchar(dict.verb) == 1]
dict.verb.two = dict.verb[nchar(dict.verb) == 2]
dict.verb.three = dict.verb[nchar(dict.verb) == 3]
dict.verb.four = dict.verb[nchar(dict.verb) == 4]

dict.adj.one = dict.adj[nchar(dict.adj) == 1]
dict.adj.two = dict.adj[nchar(dict.adj) == 2]
dict.adj.three = dict.adj[nchar(dict.adj) == 3]
dict.adj.four = dict.adj[nchar(dict.adj) == 4]

scanned.noun = length(dict.noun)
scanned.noun.unique = length(unique(dict.noun))
scanned.verb = length(dict.verb)
scanned.verb.unique = length(unique(dict.verb))
scanned.adj = length(dict.adj)
scanned.adj.unique = length(unique(dict.adj))
print(paste(scanned.noun,
            "nouns are scanned, within which",
            scanned.noun.unique, "nouns are unique nouns.",
            sep = " "));rm(scanned.noun,scanned.noun.unique)
print(paste(scanned.verb,
            "verbs are scanned, within which",
            scanned.verb.unique, "verbss are unique verbs.",
            sep = " "));rm(scanned.verb,scanned.verb.unique)
print(paste(scanned.adj,
            "adjectives are scanned, within which",
            scanned.adj.unique, "adjectives are unique adjectives.",
            sep = " "));rm(scanned.adj,scanned.adj.unique)