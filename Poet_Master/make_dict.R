library("jiebaR")
library("jiebaRD")

load(file = "poet.rds")
load(file = "strain.rds")

wkt = worker("tag")
wk = worker()

dict = cbind(names(wkt[poet_list]), wk[poet_list])
dict = unique(dict)

dict_v = unique(dict[dict[,1] == "v",])
dict_a = unique(dict[dict[,1] == "a",])
dict_n = unique(dict[dict[,1] == "n" | dict[,1] == "ns",])
