#Jan 7, 2023

#Sister species table
getwd()
library(foreach)
#install.packages("phytools")
library(phytools)
library(maps)
library(phangorn)
library(phylotools)
library(ips)
library(dplyr)
library(tidyr)
library(stringr)
#Read fasta files of placed co1 on the bb tree
R_co1_20_1=read.fasta(file = "CO1_20_1S.fasta")
R_co1_20_2=read.fasta(file = "CO1_20_2S.fasta")
R_co1_20_3=read.fasta(file = "CO1_20_3S.fasta")
R_co1_20_4=read.fasta(file = "CO1_20_4S.fasta")
R_co1_20_5=read.fasta(file = "CO1_20_5S.fasta")
R_co1_20_6=read.fasta(file = "CO1_20_6S.fasta")
R_co1_20_7=read.fasta(file = "CO1_20_7S.fasta")
R_co1_20_8=read.fasta(file = "CO1_20_8S.fasta")
R_co1_20_9=read.fasta(file = "CO1_20_9S.fasta")
R_co1_20_10=read.fasta(file = "CO1_20_10S.fasta")
R_co1_40_1=read.fasta(file = "CO1_40_1S.fasta")
R_co1_40_2=read.fasta(file = "CO1_40_2S.fasta")
R_co1_40_3=read.fasta(file = "CO1_40_3S.fasta")
R_co1_40_4=read.fasta(file = "CO1_40_4S.fasta")
R_co1_40_5=read.fasta(file = "CO1_40_5S.fasta")
R_co1_40_6=read.fasta(file = "CO1_40_6S.fasta")
R_co1_40_7=read.fasta(file = "CO1_40_7S.fasta")
R_co1_40_8=read.fasta(file = "CO1_40_8S.fasta")
R_co1_40_9=read.fasta(file = "CO1_40_9S.fasta")
R_co1_40_10=read.fasta(file = "CO1_40_10S.fasta")
R_co1_60_1=read.fasta(file = "CO1_60_1S.fasta")
R_co1_60_2=read.fasta(file = "CO1_60_2S.fasta")
R_co1_60_3=read.fasta(file = "CO1_60_3S.fasta")
R_co1_60_4=read.fasta(file = "CO1_60_4S.fasta")
R_co1_60_5=read.fasta(file = "CO1_60_5S.fasta")
R_co1_60_6=read.fasta(file = "CO1_60_6S.fasta")
R_co1_60_7=read.fasta(file = "CO1_60_7S.fasta")
R_co1_60_8=read.fasta(file = "CO1_60_8S.fasta")
R_co1_60_9=read.fasta(file = "CO1_60_9S.fasta")
R_co1_60_10=read.fasta(file = "CO1_60_10S.fasta")
R_co1_80_1=read.fasta(file = "CO1_80_1S.fasta")
R_co1_80_2=read.fasta(file = "CO1_80_2S.fasta")
R_co1_80_3=read.fasta(file = "CO1_80_3S.fasta")
R_co1_80_4=read.fasta(file = "CO1_80_4S.fasta")
R_co1_80_5=read.fasta(file = "CO1_80_5S.fasta")
R_co1_80_6=read.fasta(file = "CO1_80_6S.fasta")
R_co1_80_7=read.fasta(file = "CO1_80_7S.fasta")
R_co1_80_8=read.fasta(file = "CO1_80_8S.fasta")
R_co1_80_9=read.fasta(file = "CO1_80_9S.fasta")
R_co1_80_10=read.fasta(file = "CO1_80_10S.fasta")
R_co1_1_1=read.fasta(file = "CO1_99_1S.fasta")
R_co1_1_2=read.fasta(file = "CO1_99_2S.fasta")
R_co1_1_3=read.fasta(file = "CO1_99_3S.fasta")
R_co1_1_4=read.fasta(file = "CO1_99_4S.fasta")
R_co1_1_5=read.fasta(file = "CO1_99_5S.fasta")
R_co1_1_6=read.fasta(file = "CO1_99_6S.fasta")
R_co1_1_7=read.fasta(file = "CO1_99_7S.fasta")
R_co1_1_8=read.fasta(file = "CO1_99_8S.fasta")
R_co1_1_9=read.fasta(file = "CO1_99_9S.fasta")
R_co1_1_10=read.fasta(file = "CO1_99_10S.fasta")

#read the refernce tree
ReferenceTree <- read.tree("RAxML_bestTree.FR100_new")
#Read the 40 trees generated using Random samples
R20_1_tree<-read.tree("20_1_new")
R20_2_tree<-read.tree("20_2_new")
R20_3_tree<-read.tree("20_3_new")
R20_4_tree<-read.tree("20_4_new")
R20_5_tree<-read.tree("20_5_new")
R20_6_tree<-read.tree("20_6_new")
R20_7_tree<-read.tree("20_7_new")
R20_8_tree<-read.tree("20_8_new")
R20_9_tree<-read.tree("20_9_new")
R20_10_tree<-read.tree("20_10_new")
R40_1_tree<-read.tree("40_1_new")
R40_2_tree<-read.tree("40_2_new")
R40_3_tree<-read.tree("40_3_new")
R40_4_tree<-read.tree("40_4_new")
R40_5_tree<-read.tree("40_5_new")
R40_6_tree<-read.tree("40_6_new")
R40_7_tree<-read.tree("40_7_new")
R40_8_tree<-read.tree("40_8_new")
R40_9_tree<-read.tree("40_9_new")
R40_10_tree<-read.tree("40_10_new")
R60_1_tree<-read.tree("60_1_new")
R60_2_tree<-read.tree("60_2_new")
R60_3_tree<-read.tree("60_3_new")
R60_4_tree<-read.tree("60_4_new")
R60_5_tree<-read.tree("60_5_new")
R60_6_tree<-read.tree("60_6_new")
R60_7_tree<-read.tree("60_7_new")
R60_8_tree<-read.tree("60_8_new")
R60_9_tree<-read.tree("60_9_new")
R60_10_tree<-read.tree("60_10_new")
R80_1_tree<-read.tree("80_1_new")
R80_2_tree<-read.tree("80_2_new")
R80_3_tree<-read.tree("80_3_new")
R80_4_tree<-read.tree("80_4_new")
R80_5_tree<-read.tree("80_5_new")
R80_6_tree<-read.tree("80_6_new")
R80_7_tree<-read.tree("80_7_new")
R80_8_tree<-read.tree("80_8_new")
R80_9_tree<-read.tree("80_9_new")
R80_10_tree<-read.tree("80_10_new")
R99_1_tree<-read.tree("99_1_new")
R99_2_tree<-read.tree("99_2_new")
R99_3_tree<-read.tree("99_3_new")
R99_4_tree<-read.tree("99_4_new")
R99_5_tree<-read.tree("99_5_new")
R99_6_tree<-read.tree("99_6_new")
R99_7_tree<-read.tree("99_7_new")
R99_8_tree<-read.tree("99_8_new")
R99_9_tree<-read.tree("99_9_new")
R99_10_tree<-read.tree("99_10_new")

#------
R_co1_20_1_species <- R_co1_20_1$seq.name
R_co1_20_2_species <- R_co1_20_2$seq.name
R_co1_20_3_species <- R_co1_20_3$seq.name
R_co1_20_4_species <- R_co1_20_4$seq.name
R_co1_20_5_species <- R_co1_20_5$seq.name
R_co1_20_6_species <- R_co1_20_6$seq.name
R_co1_20_7_species <- R_co1_20_7$seq.name
R_co1_20_8_species <- R_co1_20_8$seq.name
R_co1_20_9_species <- R_co1_20_9$seq.name
R_co1_20_10_species <- R_co1_20_10$seq.name
R_co1_40_1_species <- R_co1_40_1$seq.name
R_co1_40_2_species <- R_co1_40_2$seq.name
R_co1_40_3_species <- R_co1_40_3$seq.name
R_co1_40_4_species <- R_co1_40_4$seq.name
R_co1_40_5_species <- R_co1_40_5$seq.name
R_co1_40_6_species <- R_co1_40_6$seq.name
R_co1_40_7_species <- R_co1_40_7$seq.name
R_co1_40_8_species <- R_co1_40_8$seq.name
R_co1_40_9_species <- R_co1_40_9$seq.name
R_co1_40_10_species <- R_co1_40_10$seq.name
R_co1_60_1_species <- R_co1_60_1$seq.name
R_co1_60_2_species <- R_co1_60_2$seq.name
R_co1_60_3_species <- R_co1_60_3$seq.name
R_co1_60_4_species <- R_co1_60_4$seq.name
R_co1_60_5_species <- R_co1_60_5$seq.name
R_co1_60_6_species <- R_co1_60_6$seq.name
R_co1_60_7_species <- R_co1_60_7$seq.name
R_co1_60_8_species <- R_co1_60_8$seq.name
R_co1_60_9_species <- R_co1_60_9$seq.name
R_co1_60_10_species <- R_co1_60_10$seq.name
R_co1_80_1_species <- R_co1_80_1$seq.name
R_co1_80_2_species <- R_co1_80_2$seq.name
R_co1_80_3_species <- R_co1_80_3$seq.name
R_co1_80_4_species <- R_co1_80_4$seq.name
R_co1_80_5_species <- R_co1_80_5$seq.name
R_co1_80_6_species <- R_co1_80_6$seq.name
R_co1_80_7_species <- R_co1_80_7$seq.name
R_co1_80_8_species <- R_co1_80_8$seq.name
R_co1_80_9_species <- R_co1_80_9$seq.name
R_co1_80_10_species <- R_co1_80_10$seq.name
R_co1_1_1_species <- R_co1_1_1$seq.name
R_co1_1_2_species <- R_co1_1_2$seq.name
R_co1_1_3_species <- R_co1_1_3$seq.name
R_co1_1_4_species <- R_co1_1_4$seq.name
R_co1_1_5_species <- R_co1_1_5$seq.name
R_co1_1_6_species <- R_co1_1_6$seq.name
R_co1_1_7_species <- R_co1_1_7$seq.name
R_co1_1_8_species <- R_co1_1_8$seq.name
R_co1_1_9_species <- R_co1_1_9$seq.name
R_co1_1_10_species <- R_co1_1_10$seq.name

#To remove single species in genus groups
Ref_Tree <- as.list(ReferenceTree)
Ref_Tree_Tips <- Ref_Tree$tip.label
class(Ref_Tree_Tips)
Ref_Tree_Tips <- as.data.frame(Ref_Tree_Tips)
df_ReferenceTree<-data.frame(str_extract(Ref_Tree_Tips$Ref_Tree_Tips , "[^_]+"))
colnames(df_ReferenceTree) <- c("genus")
names(df_ReferenceTree)
#Get ununique species by removing single species
#Ref_Tree_distinct <- distinct(df_ReferenceTree)
rm(df_RederenceTree)
genus_more_1<- df_ReferenceTree %>% 
  group_by(genus) %>% 
  filter(n()>=2)
#Get distinct names
Ref_Tree_distinct <- distinct(genus_more_1)#766 obs.

RefTree_R_co1_20_1_List <- foreach(i=1:length(R_co1_20_1_species)) %do% sister(ReferenceTree,R_co1_20_1_species[i],type="terminal",label=T)
R_80_co1_20_1_List <- foreach(i=1:length(R_co1_20_1_species)) %do% sister(R80_1_tree,R_co1_20_1_species[i],type="terminal",label=T)

#df_RefTree_R_co1_20_1_List <- data.frame(RefTree_R_co1_20_1_List)#diff size column error
#When converting a list to a dataframe there's an error called different size column error.
#To solve that I used one liner with plyr
#t to transpose mt dataframe for a better analysis
df_RefTree_R_co1_20_1_List <- t(plyr::ldply(RefTree_R_co1_20_1_List, rbind))#315 rows
df_R_80_co1_20_1_List <- t(plyr::ldply(R_80_co1_20_1_List, rbind))#315 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_1<-data.frame(str_extract(df_RefTree_R_co1_20_1_List , "[^_]+"))
df_R_80_co1_20_1<-data.frame(str_extract(df_R_80_co1_20_1_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_1) <- "genus"
colnames(df_R_80_co1_20_1) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_1_new <- data.frame(split(df_RefTree_R_co1_20_1,rep(1:828,each=315)))
class(df_RefTree_R_co1_20_1_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_1_new <- data.frame(split(df_R_80_co1_20_1,rep(1:828,each=315)))
class(df_R_80_co1_20_1_new)
df_RefTree_R_co1_20_1_new[is.na(df_RefTree_R_co1_20_1_new)] <- ""
df_RefTree_R_co1_20_1_new_t <- data.frame(t(df_RefTree_R_co1_20_1_new))
is.na(df_RefTree_R_co1_20_1_new_t)
df_R_80_co1_20_1_new[is.na(df_R_80_co1_20_1_new)] <- ""
df_R_80_co1_20_1_new_t<- data.frame(t(df_R_80_co1_20_1_new))
#uniq_R_80_co1_20_1_new <- apply(df_R_80_co1_20_1_new_t[], 1, unique)
#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_1_new<- apply(df_RefTree_R_co1_20_1_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_1_new<- apply(df_R_80_co1_20_1_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_1_new)

similar <- uniq_RefTree_R_co1_20_1_new [(uniq_RefTree_R_co1_20_1_new %in% uniq_R_80_co1_20_1_new )] #707
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_1_new [!(uniq_RefTree_R_co1_20_1_new %in% uniq_R_80_co1_20_1_new )] #121

# df_R_80_co1_20_1_new_t_t <- data.frame(t(df_R_80_co1_20_1_new_t))
# uniq1_df_R_80_co1_20_1_new_t_t <- unique(df_R_80_co1_20_1_new_t_t)
# uniq_df_R_80_co1_20_1_new_t <- unique(df_R_80_co1_20_1_new_t)
# comparedf(df_RefTree_R_co1_20_1_new_t,df_R_80_co1_20_1_new_t)
# comparedf(df_RefTree_R_co1_20_1_new,df_R_80_co1_20_1_new)
# class(df_RefTree_R_co1_20_1_new_t)

# hh <- df_RefTree_R_co1_20_1_new[1:5,]
# ii <- df_R_80_co1_20_1_new[1:5,]
# library(janitor)
# compare_df_cols(hh,ii,return = "match")
# library(diffdf)
# diffdf(hh,ii)
# #-------Example
# 
# df1 <- data.frame(id = paste0("person", 1:3),
#                   a = c("a", "b", "c"),
#                   b = c(1, 3, 4),
#                   c = c("f", "e", "d"),
#                   row.names = paste0("rn", 1:3),
#                   stringsAsFactors = FALSE)
# df2 <- data.frame(id = paste0("person", 3:1),
#                   a = c("c", "b", "a"),
#                   b = c(1, 3, 4),
#                   d = paste0("rn", 1:3),
#                   row.names = paste0("rn", c(1,3,2)),
#                   stringsAsFactors = FALSE)
# comparedf(df1,df2)
# summary(comparedf(df1,df2))
# #-----
# library(arsenal)
# comparedf(df_RefTree_R_co1_20_1_new,df_R_80_co1_20_1_new)
# #----
# 
# ibrary(dplyr)
# dfff <- df_RefTree_R_co1_20_1_new %>%
#   mutate(across(everything(), ~ replace(.x, is.na(.x), "")))
# dff <- as.list(dfff)
# unique1 <- unique(df_RefTree_R_co1_20_1_new$genus)
# 
# df_RefTree_R_co1_20_1_new  <- as.list(df_R_80_co1_20_1_new)
# df_R_80_co1_20_1_new <- as.list(df_R_80_co1_20_1_new)

# 


# df_RefTree_R_co1_20_1 %>% drop_na("genus")
# max.length <- max(sapply(RefTree_R_co1_20_1_List , length))
# ## Add NA values to list elements
# db <- lapply(RefTree_R_co1_20_1_List , function(v) { c(v, rep(NA, max.length-length(v)))})
# ## Rbind
# db <- data.frame(do.call(rbind, db))
# max.length <- max(sapply(R_80_co1_20_1_List , length))
# ## Add NA values to list elements
# db_80_20_1 <- lapply(R_80_co1_20_1_List, function(v) { c(v, rep(NA, max.length-length(v)))})
# ## Rbind
# db_80_20_1  <- data.frame(do.call(rbind, db_80_20_1 ))
# 
# db1 <- sub("\\_.*", "", db)
# 
# RefTree_R_co1_20_1 <- unlist(RefTree_R_co1_20_1_List)
# sub("(.*)\\_.*","\\1",RefTree_R_co1_20_1_List)
# lapply(X = RefTree_R_co1_20_1_List, sub("(.*)\\_.*","\\1",RefTree_R_co1_20_1_List))
# sub("\\_.*", "", RefTree_R_co1_20_1_List)
# Filter(function(x) sub("(.*)\\_.*","\\1", x), RefTree_R_co1_20_1_List)
# RefTree_R_co1_20_1_List[1]
# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_20_1_List [(RefTree_R_co1_20_1_List %in% R_80_co1_20_1_List )] #401
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_20_1_List [!(RefTree_R_co1_20_1_List %in% R_80_co1_20_1_List)] #503

RefTree_R_co1_20_2_List <- foreach(i=1:length(R_co1_20_2_species)) %do% sister(ReferenceTree,R_co1_20_2_species[i],type="terminal",label=T)
R_80_co1_20_2_List <- foreach(i=1:length(R_co1_20_2_species)) %do% sister(R80_2_tree,R_co1_20_2_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_2_List <- t(plyr::ldply(RefTree_R_co1_20_2_List, rbind))#4519 rows
df_R_80_co1_20_2_List <- t(plyr::ldply(R_80_co1_20_2_List, rbind))#315 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_2<-data.frame(str_extract(df_RefTree_R_co1_20_2_List , "[^_]+"))
df_R_80_co1_20_2<-data.frame(str_extract(df_R_80_co1_20_2_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_2) <- "genus"
colnames(df_R_80_co1_20_2) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_2_new <- data.frame(split(df_RefTree_R_co1_20_2,rep(1:828,each=4519)))
class(df_RefTree_R_co1_20_2_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_2_new <- data.frame(split(df_R_80_co1_20_2,rep(1:828,each=315)))
class(df_R_80_co1_20_2_new)
df_RefTree_R_co1_20_2_new[is.na(df_RefTree_R_co1_20_2_new)] <- ""
df_RefTree_R_co1_20_2_new_t <- data.frame(t(df_RefTree_R_co1_20_2_new))
is.na(df_RefTree_R_co1_20_2_new_t)
df_R_80_co1_20_2_new[is.na(df_R_80_co1_20_2_new)] <- ""
df_R_80_co1_20_2_new_t<- data.frame(t(df_R_80_co1_20_2_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_2_new<- apply(df_RefTree_R_co1_20_2_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_2_new<- apply(df_R_80_co1_20_2_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_2_new)

similar <- uniq_RefTree_R_co1_20_2_new [(uniq_RefTree_R_co1_20_2_new %in% uniq_R_80_co1_20_2_new )] #701
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_2_new [!(uniq_RefTree_R_co1_20_2_new %in% uniq_R_80_co1_20_2_new )] #127


# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_20_2_List [(RefTree_R_co1_20_2_List %in% R_80_co1_20_2_List )] #393
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_20_2_List [!(RefTree_R_co1_20_2_List %in% R_80_co1_20_2_List)] #511

RefTree_R_co1_20_3_List <- foreach(i=1:length(R_co1_20_3_species)) %do% sister(ReferenceTree,R_co1_20_3_species[i],type="terminal",label=T)
R_80_co1_20_3_List <- foreach(i=1:length(R_co1_20_3_species)) %do% sister(R80_3_tree,R_co1_20_3_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_3_List <- t(plyr::ldply(RefTree_R_co1_20_3_List, rbind))#315 rows
df_R_80_co1_20_3_List <- t(plyr::ldply(R_80_co1_20_3_List, rbind))#315 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_3<-data.frame(str_extract(df_RefTree_R_co1_20_3_List , "[^_]+"))
df_R_80_co1_20_3<-data.frame(str_extract(df_R_80_co1_20_3_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_3) <- "genus"
colnames(df_R_80_co1_20_3) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_3_new <- data.frame(split(df_RefTree_R_co1_20_3,rep(1:828,each=315)))
class(df_RefTree_R_co1_20_3_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_3_new <- data.frame(split(df_R_80_co1_20_3,rep(1:828,each=315)))
class(df_R_80_co1_20_3_new)
df_RefTree_R_co1_20_3_new[is.na(df_RefTree_R_co1_20_3_new)] <- ""
df_RefTree_R_co1_20_3_new_t <- data.frame(t(df_RefTree_R_co1_20_3_new))
is.na(df_RefTree_R_co1_20_3_new_t)
df_R_80_co1_20_3_new[is.na(df_R_80_co1_20_3_new)] <- ""
df_R_80_co1_20_3_new_t<- data.frame(t(df_R_80_co1_20_3_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_3_new<- apply(df_RefTree_R_co1_20_3_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_3_new<- apply(df_R_80_co1_20_3_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_3_new)

similar <- uniq_RefTree_R_co1_20_3_new [(uniq_RefTree_R_co1_20_3_new %in% uniq_R_80_co1_20_3_new )] #718
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_3_new [!(uniq_RefTree_R_co1_20_3_new %in% uniq_R_80_co1_20_3_new )] #110

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_20_3_List [(RefTree_R_co1_20_3_List %in% R_80_co1_20_3_List )] #358
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_20_3_List [!(RefTree_R_co1_20_3_List %in% R_80_co1_20_3_List)] #546

RefTree_R_co1_20_4_List <- foreach(i=1:length(R_co1_20_4_species)) %do% sister(ReferenceTree,R_co1_20_4_species[i],type="terminal",label=T)
R_80_co1_20_4_List <- foreach(i=1:length(R_co1_20_4_species)) %do% sister(R80_4_tree,R_co1_20_4_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_4_List <- t(plyr::ldply(RefTree_R_co1_20_4_List, rbind))#4519 rows
df_R_80_co1_20_4_List <- t(plyr::ldply(R_80_co1_20_4_List, rbind))#199 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_4<-data.frame(str_extract(df_RefTree_R_co1_20_4_List , "[^_]+"))
df_R_80_co1_20_4<-data.frame(str_extract(df_R_80_co1_20_4_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_4) <- "genus"
colnames(df_R_80_co1_20_4) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_4_new <- data.frame(split(df_RefTree_R_co1_20_4,rep(1:828,each=4519)))
class(df_RefTree_R_co1_20_4_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_4_new <- data.frame(split(df_R_80_co1_20_4,rep(1:828,each=199)))
class(df_R_80_co1_20_4_new)
df_RefTree_R_co1_20_4_new[is.na(df_RefTree_R_co1_20_4_new)] <- ""
df_RefTree_R_co1_20_4_new_t <- data.frame(t(df_RefTree_R_co1_20_4_new))
is.na(df_RefTree_R_co1_20_4_new_t)
df_R_80_co1_20_4_new[is.na(df_R_80_co1_20_4_new)] <- ""
df_R_80_co1_20_4_new_t<- data.frame(t(df_R_80_co1_20_4_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_4_new<- apply(df_RefTree_R_co1_20_4_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_4_new<- apply(df_R_80_co1_20_4_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_4_new)

similar <- uniq_RefTree_R_co1_20_4_new [(uniq_RefTree_R_co1_20_4_new %in% uniq_R_80_co1_20_4_new )] #710
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_4_new [!(uniq_RefTree_R_co1_20_4_new %in% uniq_R_80_co1_20_4_new )] #118

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_20_4_List [(RefTree_R_co1_20_4_List %in% R_80_co1_20_4_List )] #364
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_20_4_List [!(RefTree_R_co1_20_4_List %in% R_80_co1_20_4_List)] #540

RefTree_R_co1_20_5_List <- foreach(i=1:length(R_co1_20_5_species)) %do% sister(ReferenceTree,R_co1_20_5_species[i],type="terminal",label=T)
R_80_co1_20_5_List <- foreach(i=1:length(R_co1_20_5_species)) %do% sister(R80_5_tree,R_co1_20_5_species[i],type="terminal",label=T)
df_RefTree_R_co1_20_5_List <- t(plyr::ldply(RefTree_R_co1_20_5_List, rbind))#4519 rows
df_R_80_co1_20_5_List <- t(plyr::ldply(R_80_co1_20_5_List, rbind))#130 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_5<-data.frame(str_extract(df_RefTree_R_co1_20_5_List , "[^_]+"))
df_R_80_co1_20_5<-data.frame(str_extract(df_R_80_co1_20_5_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_5) <- "genus"
colnames(df_R_80_co1_20_5) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_5_new <- data.frame(split(df_RefTree_R_co1_20_5,rep(1:828,each=4519)))
class(df_RefTree_R_co1_20_5_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_5_new <- data.frame(split(df_R_80_co1_20_5,rep(1:828,each=130)))
class(df_R_80_co1_20_5_new)
df_RefTree_R_co1_20_5_new[is.na(df_RefTree_R_co1_20_5_new)] <- ""
df_RefTree_R_co1_20_5_new_t <- data.frame(t(df_RefTree_R_co1_20_5_new))
is.na(df_RefTree_R_co1_20_5_new_t)
df_R_80_co1_20_5_new[is.na(df_R_80_co1_20_5_new)] <- ""
df_R_80_co1_20_5_new_t<- data.frame(t(df_R_80_co1_20_5_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_5_new<- apply(df_RefTree_R_co1_20_5_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_5_new<- apply(df_R_80_co1_20_5_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_5_new)

similar <- uniq_RefTree_R_co1_20_5_new [(uniq_RefTree_R_co1_20_5_new %in% uniq_R_80_co1_20_5_new )] #705
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_5_new [!(uniq_RefTree_R_co1_20_5_new %in% uniq_R_80_co1_20_5_new )] #123

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_20_5_List [(RefTree_R_co1_20_5_List %in% R_80_co1_20_5_List )] #382
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_20_5_List [!(RefTree_R_co1_20_5_List %in% R_80_co1_20_5_List)] #522

RefTree_R_co1_20_6_List <- foreach(i=1:length(R_co1_20_6_species)) %do% sister(ReferenceTree,R_co1_20_6_species[i],type="terminal",label=T)
R_80_co1_20_6_List <- foreach(i=1:length(R_co1_20_6_species)) %do% sister(R80_6_tree,R_co1_20_6_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_6_List <- t(plyr::ldply(RefTree_R_co1_20_6_List, rbind))#64 rows
df_R_80_co1_20_6_List <- t(plyr::ldply(R_80_co1_20_6_List, rbind))#64 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_6<-data.frame(str_extract(df_RefTree_R_co1_20_6_List , "[^_]+"))
df_R_80_co1_20_6<-data.frame(str_extract(df_R_80_co1_20_6_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_6) <- "genus"
colnames(df_R_80_co1_20_6) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_6_new <- data.frame(split(df_RefTree_R_co1_20_6,rep(1:828,each=64)))
class(df_RefTree_R_co1_20_6_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_6_new <- data.frame(split(df_R_80_co1_20_6,rep(1:828,each=64)))
class(df_R_80_co1_20_6_new)
df_RefTree_R_co1_20_6_new[is.na(df_RefTree_R_co1_20_6_new)] <- ""
df_RefTree_R_co1_20_6_new_t <- data.frame(t(df_RefTree_R_co1_20_6_new))
is.na(df_RefTree_R_co1_20_6_new_t)
df_R_80_co1_20_6_new[is.na(df_R_80_co1_20_6_new)] <- ""
df_R_80_co1_20_6_new_t<- data.frame(t(df_R_80_co1_20_6_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_6_new<- apply(df_RefTree_R_co1_20_6_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_6_new<- apply(df_R_80_co1_20_6_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_6_new)

similar <- uniq_RefTree_R_co1_20_6_new [(uniq_RefTree_R_co1_20_6_new %in% uniq_R_80_co1_20_6_new )] #710
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_6_new [!(uniq_RefTree_R_co1_20_6_new %in% uniq_R_80_co1_20_6_new )] #118

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_20_6_List [(RefTree_R_co1_20_6_List %in% R_80_co1_20_6_List )] #337
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_20_6_List [!(RefTree_R_co1_20_6_List %in% R_80_co1_20_6_List)] #567

RefTree_R_co1_20_7_List <- foreach(i=1:length(R_co1_20_7_species)) %do% sister(ReferenceTree,R_co1_20_7_species[i],type="terminal",label=T)
R_80_co1_20_7_List <- foreach(i=1:length(R_co1_20_7_species)) %do% sister(R80_7_tree,R_co1_20_7_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_7_List <- t(plyr::ldply(RefTree_R_co1_20_7_List, rbind))#65 rows
df_R_80_co1_20_7_List <- t(plyr::ldply(R_80_co1_20_7_List, rbind))#89 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_7<-data.frame(str_extract(df_RefTree_R_co1_20_7_List , "[^_]+"))
df_R_80_co1_20_7<-data.frame(str_extract(df_R_80_co1_20_7_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_7) <- "genus"
colnames(df_R_80_co1_20_7) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_7_new <- data.frame(split(df_RefTree_R_co1_20_7,rep(1:828,each=65)))
class(df_RefTree_R_co1_20_7_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_7_new <- data.frame(split(df_R_80_co1_20_7,rep(1:828,each=89)))
class(df_R_80_co1_20_7_new)
df_RefTree_R_co1_20_7_new[is.na(df_RefTree_R_co1_20_7_new)] <- ""
df_RefTree_R_co1_20_7_new_t <- data.frame(t(df_RefTree_R_co1_20_7_new))
is.na(df_RefTree_R_co1_20_7_new_t)
df_R_80_co1_20_7_new[is.na(df_R_80_co1_20_7_new)] <- ""
df_R_80_co1_20_7_new_t<- data.frame(t(df_R_80_co1_20_7_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_7_new<- apply(df_RefTree_R_co1_20_7_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_7_new<- apply(df_R_80_co1_20_7_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_7_new)

similar <- uniq_RefTree_R_co1_20_7_new [(uniq_RefTree_R_co1_20_7_new %in% uniq_R_80_co1_20_7_new )] #730
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_7_new [!(uniq_RefTree_R_co1_20_7_new %in% uniq_R_80_co1_20_7_new )] #98

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_20_7_List [(RefTree_R_co1_20_7_List %in% R_80_co1_20_7_List )] #384
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_20_7_List [!(RefTree_R_co1_20_7_List %in% R_80_co1_20_7_List)] #520

RefTree_R_co1_20_8_List <- foreach(i=1:length(R_co1_20_8_species)) %do% sister(ReferenceTree,R_co1_20_8_species[i],type="terminal",label=T)
R_80_co1_20_8_List <- foreach(i=1:length(R_co1_20_8_species)) %do% sister(R80_8_tree,R_co1_20_8_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_8_List <- t(plyr::ldply(RefTree_R_co1_20_8_List, rbind))#99 rows
df_R_80_co1_20_8_List <- t(plyr::ldply(R_80_co1_20_8_List, rbind))#118 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_8<-data.frame(str_extract(df_RefTree_R_co1_20_8_List , "[^_]+"))
df_R_80_co1_20_8<-data.frame(str_extract(df_R_80_co1_20_8_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_8) <- "genus"
colnames(df_R_80_co1_20_8) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_8_new <- data.frame(split(df_RefTree_R_co1_20_8,rep(1:828,each=99)))
class(df_RefTree_R_co1_20_8_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_8_new <- data.frame(split(df_R_80_co1_20_8,rep(1:828,each=118)))
class(df_R_80_co1_20_8_new)
df_RefTree_R_co1_20_8_new[is.na(df_RefTree_R_co1_20_8_new)] <- ""
df_RefTree_R_co1_20_8_new_t <- data.frame(t(df_RefTree_R_co1_20_8_new))
is.na(df_RefTree_R_co1_20_8_new_t)
df_R_80_co1_20_8_new[is.na(df_R_80_co1_20_8_new)] <- ""
df_R_80_co1_20_8_new_t<- data.frame(t(df_R_80_co1_20_8_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_8_new<- apply(df_RefTree_R_co1_20_8_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_8_new<- apply(df_R_80_co1_20_8_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_8_new)

similar <- uniq_RefTree_R_co1_20_8_new [(uniq_RefTree_R_co1_20_8_new %in% uniq_R_80_co1_20_8_new )] #726
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_8_new [!(uniq_RefTree_R_co1_20_8_new %in% uniq_R_80_co1_20_8_new )] #102

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_20_8_List [(RefTree_R_co1_20_8_List %in% R_80_co1_20_8_List )] #389
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_20_8_List [!(RefTree_R_co1_20_8_List %in% R_80_co1_20_8_List)] #515

RefTree_R_co1_20_9_List <- foreach(i=1:length(R_co1_20_9_species)) %do% sister(ReferenceTree,R_co1_20_9_species[i],type="terminal",label=T)
R_80_co1_20_9_List <- foreach(i=1:length(R_co1_20_9_species)) %do% sister(R80_9_tree,R_co1_20_9_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_9_List <- t(plyr::ldply(RefTree_R_co1_20_9_List, rbind))#4519 rows
df_R_80_co1_20_9_List <- t(plyr::ldply(R_80_co1_20_9_List, rbind))#315 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_9<-data.frame(str_extract(df_RefTree_R_co1_20_9_List , "[^_]+"))
df_R_80_co1_20_9<-data.frame(str_extract(df_R_80_co1_20_9_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_9) <- "genus"
colnames(df_R_80_co1_20_9) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_9_new <- data.frame(split(df_RefTree_R_co1_20_9,rep(1:828,each=4519)))
class(df_RefTree_R_co1_20_9_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_9_new <- data.frame(split(df_R_80_co1_20_9,rep(1:828,each=315)))
class(df_R_80_co1_20_9_new)
df_RefTree_R_co1_20_9_new[is.na(df_RefTree_R_co1_20_9_new)] <- ""
df_RefTree_R_co1_20_9_new_t <- data.frame(t(df_RefTree_R_co1_20_9_new))
is.na(df_RefTree_R_co1_20_9_new_t)
df_R_80_co1_20_9_new[is.na(df_R_80_co1_20_9_new)] <- ""
df_R_80_co1_20_9_new_t<- data.frame(t(df_R_80_co1_20_9_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_9_new<- apply(df_RefTree_R_co1_20_9_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_9_new<- apply(df_R_80_co1_20_9_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_9_new)

similar <- uniq_RefTree_R_co1_20_9_new [(uniq_RefTree_R_co1_20_9_new %in% uniq_R_80_co1_20_9_new )] #720
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_9_new [!(uniq_RefTree_R_co1_20_9_new %in% uniq_R_80_co1_20_9_new )] #108
# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_20_9_List [(RefTree_R_co1_20_9_List %in% R_80_co1_20_9_List )] #394
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_20_9_List [!(RefTree_R_co1_20_9_List %in% R_80_co1_20_9_List)] #510

RefTree_R_co1_20_10_List <- foreach(i=1:length(R_co1_20_10_species)) %do% sister(ReferenceTree,R_co1_20_10_species[i],type="terminal",label=T)
R_80_co1_20_10_List <- foreach(i=1:length(R_co1_20_10_species)) %do% sister(R80_10_tree,R_co1_20_10_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_10_List <- t(plyr::ldply(RefTree_R_co1_20_10_List, rbind))#99 rows
df_R_80_co1_20_10_List <- t(plyr::ldply(R_80_co1_20_10_List, rbind))#119 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_10<-data.frame(str_extract(df_RefTree_R_co1_20_10_List , "[^_]+"))
df_R_80_co1_20_10<-data.frame(str_extract(df_R_80_co1_20_10_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_10) <- "genus"
colnames(df_R_80_co1_20_10) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_10_new <- data.frame(split(df_RefTree_R_co1_20_10,rep(1:828,each=99)))
class(df_RefTree_R_co1_20_10_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_10_new <- data.frame(split(df_R_80_co1_20_10,rep(1:828,each=119)))
class(df_R_80_co1_20_10_new)
df_RefTree_R_co1_20_10_new[is.na(df_RefTree_R_co1_20_10_new)] <- ""
df_RefTree_R_co1_20_10_new_t <- data.frame(t(df_RefTree_R_co1_20_10_new))
is.na(df_RefTree_R_co1_20_10_new_t)
df_R_80_co1_20_10_new[is.na(df_R_80_co1_20_10_new)] <- ""
df_R_80_co1_20_10_new_t<- data.frame(t(df_R_80_co1_20_10_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_10_new<- apply(df_RefTree_R_co1_20_10_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_10_new<- apply(df_R_80_co1_20_10_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_10_new)

similar <- uniq_RefTree_R_co1_20_10_new [(uniq_RefTree_R_co1_20_10_new %in% uniq_R_80_co1_20_10_new )] #721
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_10_new [!(uniq_RefTree_R_co1_20_10_new %in% uniq_R_80_co1_20_10_new )] #107

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_20_10_List [(RefTree_R_co1_20_10_List %in% R_80_co1_20_10_List )] #367
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_20_10_List [!(RefTree_R_co1_20_10_List %in% R_80_co1_20_10_List)] #537

#bb 60% tree when placed 40% co1

#To get easy coding, I rename RefTree_R_co1_40_1_List as RefTree_R_co1_20_1_List******
#R_60_co1_40_1_List as R_80_co1_20_1_List*******

RefTree_R_co1_20_1_List <- foreach(i=1:length(R_co1_40_1_species)) %do% sister(ReferenceTree,R_co1_40_1_species[i],type="terminal",label=T)
R_80_co1_20_1_List <- foreach(i=1:length(R_co1_40_1_species)) %do% sister(R60_1_tree,R_co1_40_1_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_1_List <- t(plyr::ldply(RefTree_R_co1_20_1_List, rbind))#315 rows
df_R_80_co1_20_1_List <- t(plyr::ldply(R_80_co1_20_1_List, rbind))#316 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_1<-data.frame(str_extract(df_RefTree_R_co1_20_1_List , "[^_]+"))
df_R_80_co1_20_1<-data.frame(str_extract(df_R_80_co1_20_1_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_1) <- "genus"
colnames(df_R_80_co1_20_1) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_1_new <- data.frame(split(df_RefTree_R_co1_20_1,rep(1:1661,each=315)))
class(df_RefTree_R_co1_20_1_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_1_new <- data.frame(split(df_R_80_co1_20_1,rep(1:1661,each=316)))
class(df_R_80_co1_20_1_new)
df_RefTree_R_co1_20_1_new[is.na(df_RefTree_R_co1_20_1_new)] <- ""
df_RefTree_R_co1_20_1_new_t <- data.frame(t(df_RefTree_R_co1_20_1_new))
is.na(df_RefTree_R_co1_20_1_new_t)
df_R_80_co1_20_1_new[is.na(df_R_80_co1_20_1_new)] <- ""
df_R_80_co1_20_1_new_t<- data.frame(t(df_R_80_co1_20_1_new))
#uniq_R_80_co1_20_1_new <- apply(df_R_80_co1_20_1_new_t[], 1, unique)
#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_1_new<- apply(df_RefTree_R_co1_20_1_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_1_new<- apply(df_R_80_co1_20_1_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_1_new)

similar <- uniq_RefTree_R_co1_20_1_new [(uniq_RefTree_R_co1_20_1_new %in% uniq_R_80_co1_20_1_new )] #1353
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_1_new [!(uniq_RefTree_R_co1_20_1_new %in% uniq_R_80_co1_20_1_new )] #308
# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_40_1_List [(RefTree_R_co1_40_1_List %in% R_60_co1_40_1_List )] #627
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_40_1_List [!(RefTree_R_co1_40_1_List %in% R_60_co1_40_1_List )] #1181

RefTree_R_co1_20_2_List <- foreach(i=1:length(R_co1_40_2_species)) %do% sister(ReferenceTree,R_co1_40_2_species[i],type="terminal",label=T)
R_80_co1_20_2_List <- foreach(i=1:length(R_co1_40_2_species)) %do% sister(R60_2_tree,R_co1_40_2_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_2_List <- t(plyr::ldply(RefTree_R_co1_20_2_List, rbind))#4519 rows
df_R_80_co1_20_2_List <- t(plyr::ldply(R_80_co1_20_2_List, rbind))#314 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_2<-data.frame(str_extract(df_RefTree_R_co1_20_2_List , "[^_]+"))
df_R_80_co1_20_2<-data.frame(str_extract(df_R_80_co1_20_2_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_2) <- "genus"
colnames(df_R_80_co1_20_2) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_2_new <- data.frame(split(df_RefTree_R_co1_20_2,rep(1:1661,each=4519)))
class(df_RefTree_R_co1_20_2_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_2_new <- data.frame(split(df_R_80_co1_20_2,rep(1:1661,each=314)))
class(df_R_80_co1_20_2_new)
df_RefTree_R_co1_20_2_new[is.na(df_RefTree_R_co1_20_2_new)] <- ""
df_RefTree_R_co1_20_2_new_t <- data.frame(t(df_RefTree_R_co1_20_2_new))
is.na(df_RefTree_R_co1_20_2_new_t)
df_R_80_co1_20_2_new[is.na(df_R_80_co1_20_2_new)] <- ""
df_R_80_co1_20_2_new_t<- data.frame(t(df_R_80_co1_20_2_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_2_new<- apply(df_RefTree_R_co1_20_2_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_2_new<- apply(df_R_80_co1_20_2_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_2_new)

similar <- uniq_RefTree_R_co1_20_2_new [(uniq_RefTree_R_co1_20_2_new %in% uniq_R_80_co1_20_2_new )] #1356
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_2_new [!(uniq_RefTree_R_co1_20_2_new %in% uniq_R_80_co1_20_2_new )] #305

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_40_2_List [(RefTree_R_co1_40_2_List %in% R_60_co1_40_2_List )] #582
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_40_2_List [!(RefTree_R_co1_40_2_List %in% R_60_co1_40_2_List )] #1226

RefTree_R_co1_20_3_List <- foreach(i=1:length(R_co1_40_3_species)) %do% sister(ReferenceTree,R_co1_40_3_species[i],type="terminal",label=T)
R_80_co1_20_3_List <- foreach(i=1:length(R_co1_40_3_species)) %do% sister(R60_3_tree,R_co1_40_3_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_3_List <- t(plyr::ldply(RefTree_R_co1_20_3_List, rbind))#315 rows
df_R_80_co1_20_3_List <- t(plyr::ldply(R_80_co1_20_3_List, rbind))#315 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_3<-data.frame(str_extract(df_RefTree_R_co1_20_3_List , "[^_]+"))
df_R_80_co1_20_3<-data.frame(str_extract(df_R_80_co1_20_3_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_3) <- "genus"
colnames(df_R_80_co1_20_3) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_3_new <- data.frame(split(df_RefTree_R_co1_20_3,rep(1:1661,each=315)))
class(df_RefTree_R_co1_20_3_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_3_new <- data.frame(split(df_R_80_co1_20_3,rep(1:1661,each=315)))
class(df_R_80_co1_20_3_new)
df_RefTree_R_co1_20_3_new[is.na(df_RefTree_R_co1_20_3_new)] <- ""
df_RefTree_R_co1_20_3_new_t <- data.frame(t(df_RefTree_R_co1_20_3_new))
is.na(df_RefTree_R_co1_20_3_new_t)
df_R_80_co1_20_3_new[is.na(df_R_80_co1_20_3_new)] <- ""
df_R_80_co1_20_3_new_t<- data.frame(t(df_R_80_co1_20_3_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_3_new<- apply(df_RefTree_R_co1_20_3_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_3_new<- apply(df_R_80_co1_20_3_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_3_new)

similar <- uniq_RefTree_R_co1_20_3_new [(uniq_RefTree_R_co1_20_3_new %in% uniq_R_80_co1_20_3_new )] #1355
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_3_new [!(uniq_RefTree_R_co1_20_3_new %in% uniq_R_80_co1_20_3_new )] #306


# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_40_3_List [(RefTree_R_co1_40_3_List %in% R_60_co1_40_3_List )] #573
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_40_3_List [!(RefTree_R_co1_40_3_List %in% R_60_co1_40_3_List )] #1235

RefTree_R_co1_20_4_List <- foreach(i=1:length(R_co1_40_4_species)) %do% sister(ReferenceTree,R_co1_40_4_species[i],type="terminal",label=T)
R_80_co1_20_4_List <- foreach(i=1:length(R_co1_40_4_species)) %do% sister(R60_4_tree,R_co1_40_4_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_4_List <- t(plyr::ldply(RefTree_R_co1_20_4_List, rbind))#4519 rows
df_R_80_co1_20_4_List <- t(plyr::ldply(R_80_co1_20_4_List, rbind))#4519 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_4<-data.frame(str_extract(df_RefTree_R_co1_20_4_List , "[^_]+"))
df_R_80_co1_20_4<-data.frame(str_extract(df_R_80_co1_20_4_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_4) <- "genus"
colnames(df_R_80_co1_20_4) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_4_new <- data.frame(split(df_RefTree_R_co1_20_4,rep(1:1661,each=4519)))
class(df_RefTree_R_co1_20_4_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_4_new <- data.frame(split(df_R_80_co1_20_4,rep(1:1661,each=4519)))
class(df_R_80_co1_20_4_new)
df_RefTree_R_co1_20_4_new[is.na(df_RefTree_R_co1_20_4_new)] <- ""
df_RefTree_R_co1_20_4_new_t <- data.frame(t(df_RefTree_R_co1_20_4_new))
is.na(df_RefTree_R_co1_20_4_new_t)
df_R_80_co1_20_4_new[is.na(df_R_80_co1_20_4_new)] <- ""
df_R_80_co1_20_4_new_t<- data.frame(t(df_R_80_co1_20_4_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_4_new<- apply(df_RefTree_R_co1_20_4_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_4_new<- apply(df_R_80_co1_20_4_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_4_new)

similar <- uniq_RefTree_R_co1_20_4_new [(uniq_RefTree_R_co1_20_4_new %in% uniq_R_80_co1_20_4_new )] #1367
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_4_new [!(uniq_RefTree_R_co1_20_4_new %in% uniq_R_80_co1_20_4_new )] #294

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_40_4_List [(RefTree_R_co1_40_4_List %in% R_60_co1_40_4_List )] #588
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_40_4_List [!(RefTree_R_co1_40_4_List %in% R_60_co1_40_4_List )] #1220

RefTree_R_co1_20_5_List <- foreach(i=1:length(R_co1_40_5_species)) %do% sister(ReferenceTree,R_co1_40_5_species[i],type="terminal",label=T)
R_80_co1_20_5_List <- foreach(i=1:length(R_co1_40_5_species)) %do% sister(R60_5_tree,R_co1_40_5_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_5_List <- t(plyr::ldply(RefTree_R_co1_20_5_List, rbind))#4519 rows
df_R_80_co1_20_5_List <- t(plyr::ldply(R_80_co1_20_5_List, rbind))#315 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_5<-data.frame(str_extract(df_RefTree_R_co1_20_5_List , "[^_]+"))
df_R_80_co1_20_5<-data.frame(str_extract(df_R_80_co1_20_5_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_5) <- "genus"
colnames(df_R_80_co1_20_5) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_5_new <- data.frame(split(df_RefTree_R_co1_20_5,rep(1:1661,each=4519)))
class(df_RefTree_R_co1_20_5_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_5_new <- data.frame(split(df_R_80_co1_20_5,rep(1:1661,each=315)))
class(df_R_80_co1_20_5_new)
df_RefTree_R_co1_20_5_new[is.na(df_RefTree_R_co1_20_5_new)] <- ""
df_RefTree_R_co1_20_5_new_t <- data.frame(t(df_RefTree_R_co1_20_5_new))
is.na(df_RefTree_R_co1_20_5_new_t)
df_R_80_co1_20_5_new[is.na(df_R_80_co1_20_5_new)] <- ""
df_R_80_co1_20_5_new_t<- data.frame(t(df_R_80_co1_20_5_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_5_new<- apply(df_RefTree_R_co1_20_5_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_5_new<- apply(df_R_80_co1_20_5_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_5_new)

similar <- uniq_RefTree_R_co1_20_5_new [(uniq_RefTree_R_co1_20_5_new %in% uniq_R_80_co1_20_5_new )] #1369
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_5_new [!(uniq_RefTree_R_co1_20_5_new %in% uniq_R_80_co1_20_5_new )] #292


# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_40_5_List [(RefTree_R_co1_40_5_List %in% R_60_co1_40_5_List )] #578
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_40_5_List [!(RefTree_R_co1_40_5_List %in% R_60_co1_40_5_List )] #1230

RefTree_R_co1_20_6_List <- foreach(i=1:length(R_co1_40_6_species)) %do% sister(ReferenceTree,R_co1_40_6_species[i],type="terminal",label=T)
R_80_co1_20_6_List <- foreach(i=1:length(R_co1_40_6_species)) %do% sister(R60_6_tree,R_co1_40_6_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_6_List <- t(plyr::ldply(RefTree_R_co1_20_6_List, rbind))#4519 rows
df_R_80_co1_20_6_List <- t(plyr::ldply(R_80_co1_20_6_List, rbind))#139 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_6<-data.frame(str_extract(df_RefTree_R_co1_20_6_List , "[^_]+"))
df_R_80_co1_20_6<-data.frame(str_extract(df_R_80_co1_20_6_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_6) <- "genus"
colnames(df_R_80_co1_20_6) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_6_new <- data.frame(split(df_RefTree_R_co1_20_6,rep(1:1661,each=4519)))
class(df_RefTree_R_co1_20_6_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_6_new <- data.frame(split(df_R_80_co1_20_6,rep(1:1661,each=139)))
class(df_R_80_co1_20_6_new)
df_RefTree_R_co1_20_6_new[is.na(df_RefTree_R_co1_20_6_new)] <- ""
df_RefTree_R_co1_20_6_new_t <- data.frame(t(df_RefTree_R_co1_20_6_new))
is.na(df_RefTree_R_co1_20_6_new_t)
df_R_80_co1_20_6_new[is.na(df_R_80_co1_20_6_new)] <- ""
df_R_80_co1_20_6_new_t<- data.frame(t(df_R_80_co1_20_6_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_6_new<- apply(df_RefTree_R_co1_20_6_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_6_new<- apply(df_R_80_co1_20_6_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_6_new)

similar <- uniq_RefTree_R_co1_20_6_new [(uniq_RefTree_R_co1_20_6_new %in% uniq_R_80_co1_20_6_new )] #1366
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_6_new [!(uniq_RefTree_R_co1_20_6_new %in% uniq_R_80_co1_20_6_new )] #295

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_40_6_List [(RefTree_R_co1_40_6_List %in% R_60_co1_40_6_List )] #575
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_40_6_List [!(RefTree_R_co1_40_6_List %in% R_60_co1_40_6_List )] #1233

RefTree_R_co1_20_7_List <- foreach(i=1:length(R_co1_40_7_species)) %do% sister(ReferenceTree,R_co1_40_7_species[i],type="terminal",label=T)
R_80_co1_20_7_List <- foreach(i=1:length(R_co1_40_7_species)) %do% sister(R60_7_tree,R_co1_40_7_species[i],type="terminal",label=T)


df_RefTree_R_co1_20_7_List <- t(plyr::ldply(RefTree_R_co1_20_7_List, rbind))#315 rows
df_R_80_co1_20_7_List <- t(plyr::ldply(R_80_co1_20_7_List, rbind))#315 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_7<-data.frame(str_extract(df_RefTree_R_co1_20_7_List , "[^_]+"))
df_R_80_co1_20_7<-data.frame(str_extract(df_R_80_co1_20_7_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_7) <- "genus"
colnames(df_R_80_co1_20_7) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_7_new <- data.frame(split(df_RefTree_R_co1_20_7,rep(1:1661,each=315)))
class(df_RefTree_R_co1_20_7_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_7_new <- data.frame(split(df_R_80_co1_20_7,rep(1:1661,each=315)))
class(df_R_80_co1_20_7_new)
df_RefTree_R_co1_20_7_new[is.na(df_RefTree_R_co1_20_7_new)] <- ""
df_RefTree_R_co1_20_7_new_t <- data.frame(t(df_RefTree_R_co1_20_7_new))
is.na(df_RefTree_R_co1_20_7_new_t)
df_R_80_co1_20_7_new[is.na(df_R_80_co1_20_7_new)] <- ""
df_R_80_co1_20_7_new_t<- data.frame(t(df_R_80_co1_20_7_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_7_new<- apply(df_RefTree_R_co1_20_7_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_7_new<- apply(df_R_80_co1_20_7_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_7_new)

similar <- uniq_RefTree_R_co1_20_7_new [(uniq_RefTree_R_co1_20_7_new %in% uniq_R_80_co1_20_7_new )] #1376
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_7_new [!(uniq_RefTree_R_co1_20_7_new %in% uniq_R_80_co1_20_7_new )] #285


# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_40_7_List [(RefTree_R_co1_40_7_List %in% R_60_co1_40_7_List )] #606
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_40_7_List [!(RefTree_R_co1_40_7_List %in% R_60_co1_40_7_List )] #1202

RefTree_R_co1_20_8_List <- foreach(i=1:length(R_co1_40_8_species)) %do% sister(ReferenceTree,R_co1_40_8_species[i],type="terminal",label=T)
R_80_co1_20_8_List <- foreach(i=1:length(R_co1_40_8_species)) %do% sister(R60_8_tree,R_co1_40_8_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_8_List <- t(plyr::ldply(RefTree_R_co1_20_8_List, rbind))#99 rows
df_R_80_co1_20_8_List <- t(plyr::ldply(R_80_co1_20_8_List, rbind))#590 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_8<-data.frame(str_extract(df_RefTree_R_co1_20_8_List , "[^_]+"))
df_R_80_co1_20_8<-data.frame(str_extract(df_R_80_co1_20_8_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_8) <- "genus"
colnames(df_R_80_co1_20_8) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_8_new <- data.frame(split(df_RefTree_R_co1_20_8,rep(1:1661,each=99)))
class(df_RefTree_R_co1_20_8_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_8_new <- data.frame(split(df_R_80_co1_20_8,rep(1:1661,each=590)))
class(df_R_80_co1_20_8_new)
df_RefTree_R_co1_20_8_new[is.na(df_RefTree_R_co1_20_8_new)] <- ""
df_RefTree_R_co1_20_8_new_t <- data.frame(t(df_RefTree_R_co1_20_8_new))
is.na(df_RefTree_R_co1_20_8_new_t)
df_R_80_co1_20_8_new[is.na(df_R_80_co1_20_8_new)] <- ""
df_R_80_co1_20_8_new_t<- data.frame(t(df_R_80_co1_20_8_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_8_new<- apply(df_RefTree_R_co1_20_8_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_8_new<- apply(df_R_80_co1_20_8_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_8_new)

similar <- uniq_RefTree_R_co1_20_8_new [(uniq_RefTree_R_co1_20_8_new %in% uniq_R_80_co1_20_8_new )] #1366
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_8_new [!(uniq_RefTree_R_co1_20_8_new %in% uniq_R_80_co1_20_8_new )] #295

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_40_8_List [(RefTree_R_co1_40_8_List %in% R_60_co1_40_8_List )] #610
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_40_8_List [!(RefTree_R_co1_40_8_List %in% R_60_co1_40_8_List )] #1198

RefTree_R_co1_20_9_List <- foreach(i=1:length(R_co1_40_9_species)) %do% sister(ReferenceTree,R_co1_40_9_species[i],type="terminal",label=T)
R_80_co1_20_9_List <- foreach(i=1:length(R_co1_40_9_species)) %do% sister(R60_9_tree,R_co1_40_9_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_9_List <- t(plyr::ldply(RefTree_R_co1_20_9_List, rbind))#4519 rows
df_R_80_co1_20_9_List <- t(plyr::ldply(R_80_co1_20_9_List, rbind))#315 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_9<-data.frame(str_extract(df_RefTree_R_co1_20_9_List , "[^_]+"))
df_R_80_co1_20_9<-data.frame(str_extract(df_R_80_co1_20_9_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_9) <- "genus"
colnames(df_R_80_co1_20_9) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_9_new <- data.frame(split(df_RefTree_R_co1_20_9,rep(1:1661,each=4519)))
class(df_RefTree_R_co1_20_9_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_9_new <- data.frame(split(df_R_80_co1_20_9,rep(1:1661,each=315)))
class(df_R_80_co1_20_9_new)
df_RefTree_R_co1_20_9_new[is.na(df_RefTree_R_co1_20_9_new)] <- ""
df_RefTree_R_co1_20_9_new_t <- data.frame(t(df_RefTree_R_co1_20_9_new))
is.na(df_RefTree_R_co1_20_9_new_t)
df_R_80_co1_20_9_new[is.na(df_R_80_co1_20_9_new)] <- ""
df_R_80_co1_20_9_new_t<- data.frame(t(df_R_80_co1_20_9_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_9_new<- apply(df_RefTree_R_co1_20_9_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_9_new<- apply(df_R_80_co1_20_9_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_9_new)

similar <- uniq_RefTree_R_co1_20_9_new [(uniq_RefTree_R_co1_20_9_new %in% uniq_R_80_co1_20_9_new )] #1373
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_9_new [!(uniq_RefTree_R_co1_20_9_new %in% uniq_R_80_co1_20_9_new )] #288

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_40_9_List [(RefTree_R_co1_40_9_List %in% R_60_co1_40_9_List )] #575
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_40_9_List [!(RefTree_R_co1_40_9_List %in% R_60_co1_40_9_List )] #1233

RefTree_R_co1_20_10_List <- foreach(i=1:length(R_co1_40_10_species)) %do% sister(ReferenceTree,R_co1_40_10_species[i],type="terminal",label=T)
R_80_co1_20_10_List <- foreach(i=1:length(R_co1_40_10_species)) %do% sister(R60_10_tree,R_co1_40_10_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_10_List <- t(plyr::ldply(RefTree_R_co1_20_10_List, rbind))#4519 rows
df_R_80_co1_20_10_List <- t(plyr::ldply(R_80_co1_20_10_List, rbind))#70 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_10<-data.frame(str_extract(df_RefTree_R_co1_20_10_List , "[^_]+"))
df_R_80_co1_20_10<-data.frame(str_extract(df_R_80_co1_20_10_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_10) <- "genus"
colnames(df_R_80_co1_20_10) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_10_new <- data.frame(split(df_RefTree_R_co1_20_10,rep(1:1661,each=4519)))
class(df_RefTree_R_co1_20_10_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_10_new <- data.frame(split(df_R_80_co1_20_10,rep(1:1661,each=70)))
class(df_R_80_co1_20_10_new)
df_RefTree_R_co1_20_10_new[is.na(df_RefTree_R_co1_20_10_new)] <- ""
df_RefTree_R_co1_20_10_new_t <- data.frame(t(df_RefTree_R_co1_20_10_new))
is.na(df_RefTree_R_co1_20_10_new_t)
df_R_80_co1_20_10_new[is.na(df_R_80_co1_20_10_new)] <- ""
df_R_80_co1_20_10_new_t<- data.frame(t(df_R_80_co1_20_10_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_10_new<- apply(df_RefTree_R_co1_20_10_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_10_new<- apply(df_R_80_co1_20_10_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_10_new)

similar <- uniq_RefTree_R_co1_20_10_new [(uniq_RefTree_R_co1_20_10_new %in% uniq_R_80_co1_20_10_new )] #1353
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_10_new [!(uniq_RefTree_R_co1_20_10_new %in% uniq_R_80_co1_20_10_new )] #308

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_40_10_List [(RefTree_R_co1_40_10_List %in% R_60_co1_40_10_List )] #587
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_40_10_List [!(RefTree_R_co1_40_10_List %in% R_60_co1_40_10_List )] #1221

#RefTree_biasco1List[sapply(names(RefTree_biasco1List), function(x) !identical(RefTree_biasco1List[[x]], biasco1List[[x]]))] 

#bb 40% tree when placed 60% co1
#To get easy coding, I rename RefTree_R_co1_60_1_List as RefTree_R_co1_20_1_List******
#R_40_co1_60_1_List as R_80_co1_20_1_List*******
RefTree_R_co1_20_1_List <- foreach(i=1:length(R_co1_60_1_species)) %do% sister(ReferenceTree,R_co1_60_1_species[i],type="terminal",label=T)
R_80_co1_20_1_List <- foreach(i=1:length(R_co1_60_1_species)) %do% sister(R40_1_tree,R_co1_60_1_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_1_List <- t(plyr::ldply(RefTree_R_co1_20_1_List, rbind))#315 rows
df_R_80_co1_20_1_List <- t(plyr::ldply(R_80_co1_20_1_List, rbind))#201 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_1<-data.frame(str_extract(df_RefTree_R_co1_20_1_List , "[^_]+"))
df_R_80_co1_20_1<-data.frame(str_extract(df_R_80_co1_20_1_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_1) <- "genus"
colnames(df_R_80_co1_20_1) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_1_new <- data.frame(split(df_RefTree_R_co1_20_1,rep(1:2471,each=315)))
class(df_RefTree_R_co1_20_1_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_1_new <- data.frame(split(df_R_80_co1_20_1,rep(1:2471,each=201)))
class(df_R_80_co1_20_1_new)
df_RefTree_R_co1_20_1_new[is.na(df_RefTree_R_co1_20_1_new)] <- ""
df_RefTree_R_co1_20_1_new_t <- data.frame(t(df_RefTree_R_co1_20_1_new))
is.na(df_RefTree_R_co1_20_1_new_t)
df_R_80_co1_20_1_new[is.na(df_R_80_co1_20_1_new)] <- ""
df_R_80_co1_20_1_new_t<- data.frame(t(df_R_80_co1_20_1_new))
#uniq_R_80_co1_20_1_new <- apply(df_R_80_co1_20_1_new_t[], 1, unique)
#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_1_new<- apply(df_RefTree_R_co1_20_1_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_1_new<- apply(df_R_80_co1_20_1_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_1_new)

similar <- uniq_RefTree_R_co1_20_1_new [(uniq_RefTree_R_co1_20_1_new %in% uniq_R_80_co1_20_1_new )] #1846
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_1_new [!(uniq_RefTree_R_co1_20_1_new %in% uniq_R_80_co1_20_1_new )] #625

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_60_1_List [(RefTree_R_co1_60_1_List %in% R_40_co1_60_1_List )] #585
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_60_1_List [!(RefTree_R_co1_60_1_List %in% R_40_co1_60_1_List )] #2127

RefTree_R_co1_20_2_List <- foreach(i=1:length(R_co1_60_2_species)) %do% sister(ReferenceTree,R_co1_60_2_species[i],type="terminal",label=T)
R_80_co1_20_2_List <- foreach(i=1:length(R_co1_60_2_species)) %do% sister(R40_2_tree,R_co1_60_2_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_2_List <- t(plyr::ldply(RefTree_R_co1_20_2_List, rbind))#4519 rows
df_R_80_co1_20_2_List <- t(plyr::ldply(R_80_co1_20_2_List, rbind))#316 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_2<-data.frame(str_extract(df_RefTree_R_co1_20_2_List , "[^_]+"))
df_R_80_co1_20_2<-data.frame(str_extract(df_R_80_co1_20_2_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_2) <- "genus"
colnames(df_R_80_co1_20_2) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_2_new <- data.frame(split(df_RefTree_R_co1_20_2,rep(1:2471,each=4519)))
class(df_RefTree_R_co1_20_2_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_2_new <- data.frame(split(df_R_80_co1_20_2,rep(1:2471,each=316)))
class(df_R_80_co1_20_2_new)
df_RefTree_R_co1_20_2_new[is.na(df_RefTree_R_co1_20_2_new)] <- ""
df_RefTree_R_co1_20_2_new_t <- data.frame(t(df_RefTree_R_co1_20_2_new))
is.na(df_RefTree_R_co1_20_2_new_t)
df_R_80_co1_20_2_new[is.na(df_R_80_co1_20_2_new)] <- ""
df_R_80_co1_20_2_new_t<- data.frame(t(df_R_80_co1_20_2_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_2_new<- apply(df_RefTree_R_co1_20_2_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_2_new<- apply(df_R_80_co1_20_2_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_2_new)

similar <- uniq_RefTree_R_co1_20_2_new [(uniq_RefTree_R_co1_20_2_new %in% uniq_R_80_co1_20_2_new )] #1848
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_2_new [!(uniq_RefTree_R_co1_20_2_new %in% uniq_R_80_co1_20_2_new )] #623

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_60_2_List [(RefTree_R_co1_60_2_List %in% R_40_co1_60_2_List )] #543
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_60_2_List [!(RefTree_R_co1_60_2_List %in% R_40_co1_60_2_List )] #2169


RefTree_R_co1_20_3_List <- foreach(i=1:length(R_co1_60_3_species)) %do% sister(ReferenceTree,R_co1_60_3_species[i],type="terminal",label=T)
R_80_co1_20_3_List <- foreach(i=1:length(R_co1_60_3_species)) %do% sister(R40_3_tree,R_co1_60_3_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_3_List <- t(plyr::ldply(RefTree_R_co1_20_3_List, rbind))#4519 rows
df_R_80_co1_20_3_List <- t(plyr::ldply(R_80_co1_20_3_List, rbind))#589 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_3<-data.frame(str_extract(df_RefTree_R_co1_20_3_List , "[^_]+"))
df_R_80_co1_20_3<-data.frame(str_extract(df_R_80_co1_20_3_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_3) <- "genus"
colnames(df_R_80_co1_20_3) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_3_new <- data.frame(split(df_RefTree_R_co1_20_3,rep(1:2471,each=4519)))
class(df_RefTree_R_co1_20_3_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_3_new <- data.frame(split(df_R_80_co1_20_3,rep(1:2471,each=589)))
class(df_R_80_co1_20_3_new)
df_RefTree_R_co1_20_3_new[is.na(df_RefTree_R_co1_20_3_new)] <- ""
df_RefTree_R_co1_20_3_new_t <- data.frame(t(df_RefTree_R_co1_20_3_new))
is.na(df_RefTree_R_co1_20_3_new_t)
df_R_80_co1_20_3_new[is.na(df_R_80_co1_20_3_new)] <- ""
df_R_80_co1_20_3_new_t<- data.frame(t(df_R_80_co1_20_3_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_3_new<- apply(df_RefTree_R_co1_20_3_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_3_new<- apply(df_R_80_co1_20_3_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_3_new)

similar <- uniq_RefTree_R_co1_20_3_new [(uniq_RefTree_R_co1_20_3_new %in% uniq_R_80_co1_20_3_new )] #1872
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_3_new [!(uniq_RefTree_R_co1_20_3_new %in% uniq_R_80_co1_20_3_new )] #599

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_60_3_List [(RefTree_R_co1_60_3_List %in% R_40_co1_60_3_List )] #592
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_60_3_List [!(RefTree_R_co1_60_3_List %in% R_40_co1_60_3_List )] #2120

RefTree_R_co1_20_4_List <- foreach(i=1:length(R_co1_60_4_species)) %do% sister(ReferenceTree,R_co1_60_4_species[i],type="terminal",label=T)
R_80_co1_20_4_List <- foreach(i=1:length(R_co1_60_4_species)) %do% sister(R40_4_tree,R_co1_60_4_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_4_List <- t(plyr::ldply(RefTree_R_co1_20_4_List, rbind))#4519 rows
df_R_80_co1_20_4_List <- t(plyr::ldply(R_80_co1_20_4_List, rbind))#610 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_4<-data.frame(str_extract(df_RefTree_R_co1_20_4_List , "[^_]+"))
df_R_80_co1_20_4<-data.frame(str_extract(df_R_80_co1_20_4_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_4) <- "genus"
colnames(df_R_80_co1_20_4) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_4_new <- data.frame(split(df_RefTree_R_co1_20_4,rep(1:2471,each=4519)))
class(df_RefTree_R_co1_20_4_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_4_new <- data.frame(split(df_R_80_co1_20_4,rep(1:2471,each=610)))
class(df_R_80_co1_20_4_new)
df_RefTree_R_co1_20_4_new[is.na(df_RefTree_R_co1_20_4_new)] <- ""
df_RefTree_R_co1_20_4_new_t <- data.frame(t(df_RefTree_R_co1_20_4_new))
is.na(df_RefTree_R_co1_20_4_new_t)
df_R_80_co1_20_4_new[is.na(df_R_80_co1_20_4_new)] <- ""
df_R_80_co1_20_4_new_t<- data.frame(t(df_R_80_co1_20_4_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_4_new<- apply(df_RefTree_R_co1_20_4_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_4_new<- apply(df_R_80_co1_20_4_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_4_new)

similar <- uniq_RefTree_R_co1_20_4_new [(uniq_RefTree_R_co1_20_4_new %in% uniq_R_80_co1_20_4_new )] #1884
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_4_new [!(uniq_RefTree_R_co1_20_4_new %in% uniq_R_80_co1_20_4_new )] #587

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_60_4_List [(RefTree_R_co1_60_4_List %in% R_40_co1_60_4_List )] #600
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_60_4_List [!(RefTree_R_co1_60_4_List %in% R_40_co1_60_4_List )] #2212

RefTree_R_co1_20_5_List <- foreach(i=1:length(R_co1_60_5_species)) %do% sister(ReferenceTree,R_co1_60_5_species[i],type="terminal",label=T)
R_80_co1_20_5_List <- foreach(i=1:length(R_co1_60_5_species)) %do% sister(R40_5_tree,R_co1_60_5_species[i],type="terminal",label=T)


df_RefTree_R_co1_20_5_List <- t(plyr::ldply(RefTree_R_co1_20_5_List, rbind))#4519 rows
df_R_80_co1_20_5_List <- t(plyr::ldply(R_80_co1_20_5_List, rbind))#608 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_5<-data.frame(str_extract(df_RefTree_R_co1_20_5_List , "[^_]+"))
df_R_80_co1_20_5<-data.frame(str_extract(df_R_80_co1_20_5_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_5) <- "genus"
colnames(df_R_80_co1_20_5) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_5_new <- data.frame(split(df_RefTree_R_co1_20_5,rep(1:2471,each=4519)))
class(df_RefTree_R_co1_20_5_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_5_new <- data.frame(split(df_R_80_co1_20_5,rep(1:2471,each=608)))
class(df_R_80_co1_20_5_new)
df_RefTree_R_co1_20_5_new[is.na(df_RefTree_R_co1_20_5_new)] <- ""
df_RefTree_R_co1_20_5_new_t <- data.frame(t(df_RefTree_R_co1_20_5_new))
is.na(df_RefTree_R_co1_20_5_new_t)
df_R_80_co1_20_5_new[is.na(df_R_80_co1_20_5_new)] <- ""
df_R_80_co1_20_5_new_t<- data.frame(t(df_R_80_co1_20_5_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_5_new<- apply(df_RefTree_R_co1_20_5_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_5_new<- apply(df_R_80_co1_20_5_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_5_new)

similar <- uniq_RefTree_R_co1_20_5_new [(uniq_RefTree_R_co1_20_5_new %in% uniq_R_80_co1_20_5_new )] #1892
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_5_new [!(uniq_RefTree_R_co1_20_5_new %in% uniq_R_80_co1_20_5_new )] #579

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_60_5_List [(RefTree_R_co1_60_5_List %in% R_40_co1_60_5_List )] #569
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_60_5_List [!(RefTree_R_co1_60_5_List %in% R_40_co1_60_5_List )] #2143

RefTree_R_co1_20_6_List <- foreach(i=1:length(R_co1_60_6_species)) %do% sister(ReferenceTree,R_co1_60_6_species[i],type="terminal",label=T)
R_80_co1_20_6_List <- foreach(i=1:length(R_co1_60_6_species)) %do% sister(R40_6_tree,R_co1_60_6_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_6_List <- t(plyr::ldply(RefTree_R_co1_20_6_List, rbind))#4519 rows
df_R_80_co1_20_6_List <- t(plyr::ldply(R_80_co1_20_6_List, rbind))#316 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_6<-data.frame(str_extract(df_RefTree_R_co1_20_6_List , "[^_]+"))
df_R_80_co1_20_6<-data.frame(str_extract(df_R_80_co1_20_6_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_6) <- "genus"
colnames(df_R_80_co1_20_6) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_6_new <- data.frame(split(df_RefTree_R_co1_20_6,rep(1:2471,each=4519)))
class(df_RefTree_R_co1_20_6_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_6_new <- data.frame(split(df_R_80_co1_20_6,rep(1:2471,each=316)))
class(df_R_80_co1_20_6_new)
df_RefTree_R_co1_20_6_new[is.na(df_RefTree_R_co1_20_6_new)] <- ""
df_RefTree_R_co1_20_6_new_t <- data.frame(t(df_RefTree_R_co1_20_6_new))
is.na(df_RefTree_R_co1_20_6_new_t)
df_R_80_co1_20_6_new[is.na(df_R_80_co1_20_6_new)] <- ""
df_R_80_co1_20_6_new_t<- data.frame(t(df_R_80_co1_20_6_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_6_new<- apply(df_RefTree_R_co1_20_6_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_6_new<- apply(df_R_80_co1_20_6_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_6_new)

similar <- uniq_RefTree_R_co1_20_6_new [(uniq_RefTree_R_co1_20_6_new %in% uniq_R_80_co1_20_6_new )] #1819
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_6_new [!(uniq_RefTree_R_co1_20_6_new %in% uniq_R_80_co1_20_6_new )] #652

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_60_6_List [(RefTree_R_co1_60_6_List %in% R_40_co1_60_6_List )] #567
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_60_6_List [!(RefTree_R_co1_60_6_List %in% R_40_co1_60_6_List )] #2145

RefTree_R_co1_20_7_List <- foreach(i=1:length(R_co1_60_7_species)) %do% sister(ReferenceTree,R_co1_60_7_species[i],type="terminal",label=T)
R_80_co1_20_7_List <- foreach(i=1:length(R_co1_60_7_species)) %do% sister(R40_7_tree,R_co1_60_7_species[i],type="terminal",label=T)


df_RefTree_R_co1_20_7_List <- t(plyr::ldply(RefTree_R_co1_20_7_List, rbind))#315 rows
df_R_80_co1_20_7_List <- t(plyr::ldply(R_80_co1_20_7_List, rbind))#316 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_7<-data.frame(str_extract(df_RefTree_R_co1_20_7_List , "[^_]+"))
df_R_80_co1_20_7<-data.frame(str_extract(df_R_80_co1_20_7_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_7) <- "genus"
colnames(df_R_80_co1_20_7) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_7_new <- data.frame(split(df_RefTree_R_co1_20_7,rep(1:2471,each=315)))
class(df_RefTree_R_co1_20_7_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_7_new <- data.frame(split(df_R_80_co1_20_7,rep(1:2471,each=316)))
class(df_R_80_co1_20_7_new)
df_RefTree_R_co1_20_7_new[is.na(df_RefTree_R_co1_20_7_new)] <- ""
df_RefTree_R_co1_20_7_new_t <- data.frame(t(df_RefTree_R_co1_20_7_new))
is.na(df_RefTree_R_co1_20_7_new_t)
df_R_80_co1_20_7_new[is.na(df_R_80_co1_20_7_new)] <- ""
df_R_80_co1_20_7_new_t<- data.frame(t(df_R_80_co1_20_7_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_7_new<- apply(df_RefTree_R_co1_20_7_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_7_new<- apply(df_R_80_co1_20_7_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_7_new)

similar <- uniq_RefTree_R_co1_20_7_new [(uniq_RefTree_R_co1_20_7_new %in% uniq_R_80_co1_20_7_new )] #1883
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_7_new [!(uniq_RefTree_R_co1_20_7_new %in% uniq_R_80_co1_20_7_new )] #588

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_60_7_List [(RefTree_R_co1_60_7_List %in% R_40_co1_60_7_List )] #584
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_60_7_List [!(RefTree_R_co1_60_7_List %in% R_40_co1_60_7_List )] #2128

RefTree_R_co1_20_8_List <- foreach(i=1:length(R_co1_60_8_species)) %do% sister(ReferenceTree,R_co1_60_8_species[i],type="terminal",label=T)
R_80_co1_20_8_List <- foreach(i=1:length(R_co1_60_8_species)) %do% sister(R40_8_tree,R_co1_60_8_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_8_List <- t(plyr::ldply(RefTree_R_co1_20_8_List, rbind))#4519 rows
df_R_80_co1_20_8_List <- t(plyr::ldply(R_80_co1_20_8_List, rbind))#340 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_8<-data.frame(str_extract(df_RefTree_R_co1_20_8_List , "[^_]+"))
df_R_80_co1_20_8<-data.frame(str_extract(df_R_80_co1_20_8_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_8) <- "genus"
colnames(df_R_80_co1_20_8) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_8_new <- data.frame(split(df_RefTree_R_co1_20_8,rep(1:2471,each=4519)))
class(df_RefTree_R_co1_20_8_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_8_new <- data.frame(split(df_R_80_co1_20_8,rep(1:2471,each=340)))
class(df_R_80_co1_20_8_new)
df_RefTree_R_co1_20_8_new[is.na(df_RefTree_R_co1_20_8_new)] <- ""
df_RefTree_R_co1_20_8_new_t <- data.frame(t(df_RefTree_R_co1_20_8_new))
is.na(df_RefTree_R_co1_20_8_new_t)
df_R_80_co1_20_8_new[is.na(df_R_80_co1_20_8_new)] <- ""
df_R_80_co1_20_8_new_t<- data.frame(t(df_R_80_co1_20_8_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_8_new<- apply(df_RefTree_R_co1_20_8_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_8_new<- apply(df_R_80_co1_20_8_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_8_new)

similar <- uniq_RefTree_R_co1_20_8_new [(uniq_RefTree_R_co1_20_8_new %in% uniq_R_80_co1_20_8_new )] #1861
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_8_new [!(uniq_RefTree_R_co1_20_8_new %in% uniq_R_80_co1_20_8_new )] #610

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_60_8_List [(RefTree_R_co1_60_8_List %in% R_40_co1_60_8_List )] #591
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_60_8_List [!(RefTree_R_co1_60_8_List %in% R_40_co1_60_8_List )] #2121

RefTree_R_co1_20_9_List <- foreach(i=1:length(R_co1_60_9_species)) %do% sister(ReferenceTree,R_co1_60_9_species[i],type="terminal",label=T)
R_80_co1_20_9_List <- foreach(i=1:length(R_co1_60_9_species)) %do% sister(R40_9_tree,R_co1_60_9_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_9_List <- t(plyr::ldply(RefTree_R_co1_20_9_List, rbind))#4519 rows
df_R_80_co1_20_9_List <- t(plyr::ldply(R_80_co1_20_9_List, rbind))#315 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_9<-data.frame(str_extract(df_RefTree_R_co1_20_9_List , "[^_]+"))
df_R_80_co1_20_9<-data.frame(str_extract(df_R_80_co1_20_9_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_9) <- "genus"
colnames(df_R_80_co1_20_9) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_9_new <- data.frame(split(df_RefTree_R_co1_20_9,rep(1:2471,each=4519)))
class(df_RefTree_R_co1_20_9_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_9_new <- data.frame(split(df_R_80_co1_20_9,rep(1:2471,each=315)))
class(df_R_80_co1_20_9_new)
df_RefTree_R_co1_20_9_new[is.na(df_RefTree_R_co1_20_9_new)] <- ""
df_RefTree_R_co1_20_9_new_t <- data.frame(t(df_RefTree_R_co1_20_9_new))
is.na(df_RefTree_R_co1_20_9_new_t)
df_R_80_co1_20_9_new[is.na(df_R_80_co1_20_9_new)] <- ""
df_R_80_co1_20_9_new_t<- data.frame(t(df_R_80_co1_20_9_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_9_new<- apply(df_RefTree_R_co1_20_9_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_9_new<- apply(df_R_80_co1_20_9_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_9_new)

similar <- uniq_RefTree_R_co1_20_9_new [(uniq_RefTree_R_co1_20_9_new %in% uniq_R_80_co1_20_9_new )] #1926
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_9_new [!(uniq_RefTree_R_co1_20_9_new %in% uniq_R_80_co1_20_9_new )] #545


# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_60_9_List [(RefTree_R_co1_60_9_List %in% R_40_co1_60_9_List )] #553
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_60_9_List [!(RefTree_R_co1_60_9_List %in% R_40_co1_60_9_List )] #2159

RefTree_R_co1_20_10_List <- foreach(i=1:length(R_co1_60_10_species)) %do% sister(ReferenceTree,R_co1_60_10_species[i],type="terminal",label=T)
R_80_co1_20_10_List <- foreach(i=1:length(R_co1_60_10_species)) %do% sister(R40_10_tree,R_co1_60_10_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_10_List <- t(plyr::ldply(RefTree_R_co1_20_10_List, rbind))#4519 rows
df_R_80_co1_20_10_List <- t(plyr::ldply(R_80_co1_20_10_List, rbind))#200 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_10<-data.frame(str_extract(df_RefTree_R_co1_20_10_List , "[^_]+"))
df_R_80_co1_20_10<-data.frame(str_extract(df_R_80_co1_20_10_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_10) <- "genus"
colnames(df_R_80_co1_20_10) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_10_new <- data.frame(split(df_RefTree_R_co1_20_10,rep(1:2471,each=4519)))
class(df_RefTree_R_co1_20_10_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_10_new <- data.frame(split(df_R_80_co1_20_10,rep(1:2471,each=200)))
class(df_R_80_co1_20_10_new)
df_RefTree_R_co1_20_10_new[is.na(df_RefTree_R_co1_20_10_new)] <- ""
df_RefTree_R_co1_20_10_new_t <- data.frame(t(df_RefTree_R_co1_20_10_new))
is.na(df_RefTree_R_co1_20_10_new_t)
df_R_80_co1_20_10_new[is.na(df_R_80_co1_20_10_new)] <- ""
df_R_80_co1_20_10_new_t<- data.frame(t(df_R_80_co1_20_10_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_10_new<- apply(df_RefTree_R_co1_20_10_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_10_new<- apply(df_R_80_co1_20_10_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_10_new)

similar <- uniq_RefTree_R_co1_20_10_new [(uniq_RefTree_R_co1_20_10_new %in% uniq_R_80_co1_20_10_new )] #1841
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_10_new [!(uniq_RefTree_R_co1_20_10_new %in% uniq_R_80_co1_20_10_new )] #630


# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_60_10_List [(RefTree_R_co1_60_10_List %in% R_40_co1_60_10_List )] #603
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_60_10_List [!(RefTree_R_co1_60_10_List %in% R_40_co1_60_10_List )] #2109


#bb 20% tree when placed 80% co1
#To get easy coding, I rename RefTree_R_co1_80_1_List as RefTree_R_co1_20_1_List******
#R_20_co1_80_1_List as R_80_co1_20_1_List*******
RefTree_R_co1_20_1_List <- foreach(i=1:length(R_co1_80_1_species)) %do% sister(ReferenceTree,R_co1_80_1_species[i],type="terminal",label=T)
R_80_co1_20_1_List <- foreach(i=1:length(R_co1_80_1_species)) %do% sister(R20_1_tree,R_co1_80_1_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_1_List <- t(plyr::ldply(RefTree_R_co1_20_1_List, rbind))#4519 rows
df_R_80_co1_20_1_List <- t(plyr::ldply(R_80_co1_20_1_List, rbind))#692 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_1<-data.frame(str_extract(df_RefTree_R_co1_20_1_List , "[^_]+"))
df_R_80_co1_20_1<-data.frame(str_extract(df_R_80_co1_20_1_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_1) <- "genus"
colnames(df_R_80_co1_20_1) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_1_new <- data.frame(split(df_RefTree_R_co1_20_1,rep(1:3285,each=4519)))
class(df_RefTree_R_co1_20_1_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_1_new <- data.frame(split(df_R_80_co1_20_1,rep(1:3285,each=692)))
class(df_R_80_co1_20_1_new)
df_RefTree_R_co1_20_1_new[is.na(df_RefTree_R_co1_20_1_new)] <- ""
df_RefTree_R_co1_20_1_new_t <- data.frame(t(df_RefTree_R_co1_20_1_new))
is.na(df_RefTree_R_co1_20_1_new_t)
df_R_80_co1_20_1_new[is.na(df_R_80_co1_20_1_new)] <- ""
df_R_80_co1_20_1_new_t<- data.frame(t(df_R_80_co1_20_1_new))
#uniq_R_80_co1_20_1_new <- apply(df_R_80_co1_20_1_new_t[], 1, unique)
#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_1_new<- apply(df_RefTree_R_co1_20_1_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_1_new<- apply(df_R_80_co1_20_1_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_1_new)

similar <- uniq_RefTree_R_co1_20_1_new [(uniq_RefTree_R_co1_20_1_new %in% uniq_R_80_co1_20_1_new )] #1889
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_1_new [!(uniq_RefTree_R_co1_20_1_new %in% uniq_R_80_co1_20_1_new )] #1396

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_80_1_List [(RefTree_R_co1_80_1_List %in% R_20_co1_80_1_List )] #299
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_80_1_List [!(RefTree_R_co1_80_1_List %in% R_20_co1_80_1_List )] #3317

RefTree_R_co1_20_2_List <- foreach(i=1:length(R_co1_80_2_species)) %do% sister(ReferenceTree,R_co1_80_2_species[i],type="terminal",label=T)
R_80_co1_20_2_List <- foreach(i=1:length(R_co1_80_2_species)) %do% sister(R20_2_tree,R_co1_80_2_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_2_List <- t(plyr::ldply(RefTree_R_co1_20_2_List, rbind))#4519 rows
df_R_80_co1_20_2_List <- t(plyr::ldply(R_80_co1_20_2_List, rbind))#234 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_2<-data.frame(str_extract(df_RefTree_R_co1_20_2_List , "[^_]+"))
df_R_80_co1_20_2<-data.frame(str_extract(df_R_80_co1_20_2_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_2) <- "genus"
colnames(df_R_80_co1_20_2) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_2_new <- data.frame(split(df_RefTree_R_co1_20_2,rep(1:3285,each=4519)))
class(df_RefTree_R_co1_20_2_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_2_new <- data.frame(split(df_R_80_co1_20_2,rep(1:3285,each=234)))
class(df_R_80_co1_20_2_new)
df_RefTree_R_co1_20_2_new[is.na(df_RefTree_R_co1_20_2_new)] <- ""
df_RefTree_R_co1_20_2_new_t <- data.frame(t(df_RefTree_R_co1_20_2_new))
is.na(df_RefTree_R_co1_20_2_new_t)
df_R_80_co1_20_2_new[is.na(df_R_80_co1_20_2_new)] <- ""
df_R_80_co1_20_2_new_t<- data.frame(t(df_R_80_co1_20_2_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_2_new<- apply(df_RefTree_R_co1_20_2_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_2_new<- apply(df_R_80_co1_20_2_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_2_new)

similar <- uniq_RefTree_R_co1_20_2_new [(uniq_RefTree_R_co1_20_2_new %in% uniq_R_80_co1_20_2_new )] #1887
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_2_new [!(uniq_RefTree_R_co1_20_2_new %in% uniq_R_80_co1_20_2_new )] #1398

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_80_2_List [(RefTree_R_co1_80_2_List %in% R_20_co1_80_2_List )] #290
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_80_2_List [!(RefTree_R_co1_80_2_List %in% R_20_co1_80_2_List )] #3326

RefTree_R_co1_20_3_List <- foreach(i=1:length(R_co1_80_3_species)) %do% sister(ReferenceTree,R_co1_80_3_species[i],type="terminal",label=T)
R_80_co1_20_3_List <- foreach(i=1:length(R_co1_80_3_species)) %do% sister(R20_3_tree,R_co1_80_3_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_3_List <- t(plyr::ldply(RefTree_R_co1_20_3_List, rbind))#4519 rows
df_R_80_co1_20_3_List <- t(plyr::ldply(R_80_co1_20_3_List, rbind))#312 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_3<-data.frame(str_extract(df_RefTree_R_co1_20_3_List , "[^_]+"))
df_R_80_co1_20_3<-data.frame(str_extract(df_R_80_co1_20_3_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_3) <- "genus"
colnames(df_R_80_co1_20_3) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_3_new <- data.frame(split(df_RefTree_R_co1_20_3,rep(1:3285,each=4519)))
class(df_RefTree_R_co1_20_3_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_3_new <- data.frame(split(df_R_80_co1_20_3,rep(1:3285,each=312)))
class(df_R_80_co1_20_3_new)
df_RefTree_R_co1_20_3_new[is.na(df_RefTree_R_co1_20_3_new)] <- ""
df_RefTree_R_co1_20_3_new_t <- data.frame(t(df_RefTree_R_co1_20_3_new))
is.na(df_RefTree_R_co1_20_3_new_t)
df_R_80_co1_20_3_new[is.na(df_R_80_co1_20_3_new)] <- ""
df_R_80_co1_20_3_new_t<- data.frame(t(df_R_80_co1_20_3_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_3_new<- apply(df_RefTree_R_co1_20_3_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_3_new<- apply(df_R_80_co1_20_3_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_3_new)

similar <- uniq_RefTree_R_co1_20_3_new [(uniq_RefTree_R_co1_20_3_new %in% uniq_R_80_co1_20_3_new )] #1907
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_3_new [!(uniq_RefTree_R_co1_20_3_new %in% uniq_R_80_co1_20_3_new )] #1378

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_80_3_List [(RefTree_R_co1_80_3_List %in% R_20_co1_80_3_List )] #325
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_80_3_List [!(RefTree_R_co1_80_3_List %in% R_20_co1_80_3_List )] #3291

RefTree_R_co1_20_4_List <- foreach(i=1:length(R_co1_80_4_species)) %do% sister(ReferenceTree,R_co1_80_4_species[i],type="terminal",label=T)
R_80_co1_20_4_List <- foreach(i=1:length(R_co1_80_4_species)) %do% sister(R20_4_tree,R_co1_80_4_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_4_List <- t(plyr::ldply(RefTree_R_co1_20_4_List, rbind))#4519 rows
df_R_80_co1_20_4_List <- t(plyr::ldply(R_80_co1_20_4_List, rbind))#640 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_4<-data.frame(str_extract(df_RefTree_R_co1_20_4_List , "[^_]+"))
df_R_80_co1_20_4<-data.frame(str_extract(df_R_80_co1_20_4_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_4) <- "genus"
colnames(df_R_80_co1_20_4) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_4_new <- data.frame(split(df_RefTree_R_co1_20_4,rep(1:3285,each=4519)))
class(df_RefTree_R_co1_20_4_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_4_new <- data.frame(split(df_R_80_co1_20_4,rep(1:3285,each=640)))
class(df_R_80_co1_20_4_new)
df_RefTree_R_co1_20_4_new[is.na(df_RefTree_R_co1_20_4_new)] <- ""
df_RefTree_R_co1_20_4_new_t <- data.frame(t(df_RefTree_R_co1_20_4_new))
is.na(df_RefTree_R_co1_20_4_new_t)
df_R_80_co1_20_4_new[is.na(df_R_80_co1_20_4_new)] <- ""
df_R_80_co1_20_4_new_t<- data.frame(t(df_R_80_co1_20_4_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_4_new<- apply(df_RefTree_R_co1_20_4_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_4_new<- apply(df_R_80_co1_20_4_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_4_new)

similar <- uniq_RefTree_R_co1_20_4_new [(uniq_RefTree_R_co1_20_4_new %in% uniq_R_80_co1_20_4_new )] #1912
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_4_new [!(uniq_RefTree_R_co1_20_4_new %in% uniq_R_80_co1_20_4_new )] #1373

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_80_4_List [(RefTree_R_co1_80_4_List %in% R_20_co1_80_4_List )] #335
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_80_4_List [!(RefTree_R_co1_80_4_List %in% R_20_co1_80_4_List )] #3281

RefTree_R_co1_20_5_List <- foreach(i=1:length(R_co1_80_5_species)) %do% sister(ReferenceTree,R_co1_80_5_species[i],type="terminal",label=T)
R_80_co1_20_5_List <- foreach(i=1:length(R_co1_80_5_species)) %do% sister(R20_5_tree,R_co1_80_5_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_5_List <- t(plyr::ldply(RefTree_R_co1_20_5_List, rbind))#4519 rows
df_R_80_co1_20_5_List <- t(plyr::ldply(R_80_co1_20_5_List, rbind))#343 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_5<-data.frame(str_extract(df_RefTree_R_co1_20_5_List , "[^_]+"))
df_R_80_co1_20_5<-data.frame(str_extract(df_R_80_co1_20_5_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_5) <- "genus"
colnames(df_R_80_co1_20_5) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_5_new <- data.frame(split(df_RefTree_R_co1_20_5,rep(1:3285,each=4519)))
class(df_RefTree_R_co1_20_5_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_5_new <- data.frame(split(df_R_80_co1_20_5,rep(1:3285,each=343)))
class(df_R_80_co1_20_5_new)
df_RefTree_R_co1_20_5_new[is.na(df_RefTree_R_co1_20_5_new)] <- ""
df_RefTree_R_co1_20_5_new_t <- data.frame(t(df_RefTree_R_co1_20_5_new))
is.na(df_RefTree_R_co1_20_5_new_t)
df_R_80_co1_20_5_new[is.na(df_R_80_co1_20_5_new)] <- ""
df_R_80_co1_20_5_new_t<- data.frame(t(df_R_80_co1_20_5_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_5_new<- apply(df_RefTree_R_co1_20_5_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_5_new<- apply(df_R_80_co1_20_5_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_5_new)

similar <- uniq_RefTree_R_co1_20_5_new [(uniq_RefTree_R_co1_20_5_new %in% uniq_R_80_co1_20_5_new )] #1905
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_5_new [!(uniq_RefTree_R_co1_20_5_new %in% uniq_R_80_co1_20_5_new )] #1380

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_80_5_List [(RefTree_R_co1_80_5_List %in% R_20_co1_80_5_List )] #344
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_80_5_List [!(RefTree_R_co1_80_5_List %in% R_20_co1_80_5_List )] #3272

RefTree_R_co1_20_6_List <- foreach(i=1:length(R_co1_80_6_species)) %do% sister(ReferenceTree,R_co1_80_6_species[i],type="terminal",label=T)
R_80_co1_20_6_List <- foreach(i=1:length(R_co1_80_6_species)) %do% sister(R20_6_tree,R_co1_80_6_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_6_List <- t(plyr::ldply(RefTree_R_co1_20_6_List, rbind))#4519 rows
df_R_80_co1_20_6_List <- t(plyr::ldply(R_80_co1_20_6_List, rbind))#3994 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_6<-data.frame(str_extract(df_RefTree_R_co1_20_6_List , "[^_]+"))
df_R_80_co1_20_6<-data.frame(str_extract(df_R_80_co1_20_6_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_6) <- "genus"
colnames(df_R_80_co1_20_6) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_6_new <- data.frame(split(df_RefTree_R_co1_20_6,rep(1:3285,each=4519)))
class(df_RefTree_R_co1_20_6_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_6_new <- data.frame(split(df_R_80_co1_20_6,rep(1:3285,each=3994)))
class(df_R_80_co1_20_6_new)
df_RefTree_R_co1_20_6_new[is.na(df_RefTree_R_co1_20_6_new)] <- ""
df_RefTree_R_co1_20_6_new_t <- data.frame(t(df_RefTree_R_co1_20_6_new))
is.na(df_RefTree_R_co1_20_6_new_t)
df_R_80_co1_20_6_new[is.na(df_R_80_co1_20_6_new)] <- ""
df_R_80_co1_20_6_new_t<- data.frame(t(df_R_80_co1_20_6_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_6_new<- apply(df_RefTree_R_co1_20_6_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_6_new<- apply(df_R_80_co1_20_6_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_6_new)

similar <- uniq_RefTree_R_co1_20_6_new [(uniq_RefTree_R_co1_20_6_new %in% uniq_R_80_co1_20_6_new )] #1861
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_6_new [!(uniq_RefTree_R_co1_20_6_new %in% uniq_R_80_co1_20_6_new )] #1424

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_80_6_List [(RefTree_R_co1_80_6_List %in% R_20_co1_80_6_List )] #260
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_80_6_List [!(RefTree_R_co1_80_6_List %in% R_20_co1_80_6_List )] #3356

RefTree_R_co1_20_7_List <- foreach(i=1:length(R_co1_80_7_species)) %do% sister(ReferenceTree,R_co1_80_7_species[i],type="terminal",label=T)
R_80_co1_20_7_List <- foreach(i=1:length(R_co1_80_7_species)) %do% sister(R20_7_tree,R_co1_80_7_species[i],type="terminal",label=T)


df_RefTree_R_co1_20_7_List <- t(plyr::ldply(RefTree_R_co1_20_7_List, rbind))#4519 rows
df_R_80_co1_20_7_List <- t(plyr::ldply(R_80_co1_20_7_List, rbind))#313 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_7<-data.frame(str_extract(df_RefTree_R_co1_20_7_List , "[^_]+"))
df_R_80_co1_20_7<-data.frame(str_extract(df_R_80_co1_20_7_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_7) <- "genus"
colnames(df_R_80_co1_20_7) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_7_new <- data.frame(split(df_RefTree_R_co1_20_7,rep(1:3285,each=4519)))
class(df_RefTree_R_co1_20_7_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_7_new <- data.frame(split(df_R_80_co1_20_7,rep(1:3285,each=313)))
class(df_R_80_co1_20_7_new)
df_RefTree_R_co1_20_7_new[is.na(df_RefTree_R_co1_20_7_new)] <- ""
df_RefTree_R_co1_20_7_new_t <- data.frame(t(df_RefTree_R_co1_20_7_new))
is.na(df_RefTree_R_co1_20_7_new_t)
df_R_80_co1_20_7_new[is.na(df_R_80_co1_20_7_new)] <- ""
df_R_80_co1_20_7_new_t<- data.frame(t(df_R_80_co1_20_7_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_7_new<- apply(df_RefTree_R_co1_20_7_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_7_new<- apply(df_R_80_co1_20_7_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_7_new)

similar <- uniq_RefTree_R_co1_20_7_new [(uniq_RefTree_R_co1_20_7_new %in% uniq_R_80_co1_20_7_new )] #1910
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_7_new [!(uniq_RefTree_R_co1_20_7_new %in% uniq_R_80_co1_20_7_new )] #1375

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_80_7_List [(RefTree_R_co1_80_7_List %in% R_20_co1_80_7_List )] #307
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_80_7_List [!(RefTree_R_co1_80_7_List %in% R_20_co1_80_7_List )] #3309

RefTree_R_co1_20_8_List <- foreach(i=1:length(R_co1_80_8_species)) %do% sister(ReferenceTree,R_co1_80_8_species[i],type="terminal",label=T)
R_80_co1_20_8_List <- foreach(i=1:length(R_co1_80_8_species)) %do% sister(R20_8_tree,R_co1_80_8_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_8_List <- t(plyr::ldply(RefTree_R_co1_20_8_List, rbind))#4519 rows
df_R_80_co1_20_8_List <- t(plyr::ldply(R_80_co1_20_8_List, rbind))#689 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_8<-data.frame(str_extract(df_RefTree_R_co1_20_8_List , "[^_]+"))
df_R_80_co1_20_8<-data.frame(str_extract(df_R_80_co1_20_8_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_8) <- "genus"
colnames(df_R_80_co1_20_8) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_8_new <- data.frame(split(df_RefTree_R_co1_20_8,rep(1:3285,each=4519)))
class(df_RefTree_R_co1_20_8_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_8_new <- data.frame(split(df_R_80_co1_20_8,rep(1:3285,each=689)))
class(df_R_80_co1_20_8_new)
df_RefTree_R_co1_20_8_new[is.na(df_RefTree_R_co1_20_8_new)] <- ""
df_RefTree_R_co1_20_8_new_t <- data.frame(t(df_RefTree_R_co1_20_8_new))
is.na(df_RefTree_R_co1_20_8_new_t)
df_R_80_co1_20_8_new[is.na(df_R_80_co1_20_8_new)] <- ""
df_R_80_co1_20_8_new_t<- data.frame(t(df_R_80_co1_20_8_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_8_new<- apply(df_RefTree_R_co1_20_8_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_8_new<- apply(df_R_80_co1_20_8_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_8_new)

similar <- uniq_RefTree_R_co1_20_8_new [(uniq_RefTree_R_co1_20_8_new %in% uniq_R_80_co1_20_8_new )] #1886
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_8_new [!(uniq_RefTree_R_co1_20_8_new %in% uniq_R_80_co1_20_8_new )] #1399

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_80_8_List [(RefTree_R_co1_80_8_List %in% R_20_co1_80_8_List )] #316
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_80_8_List [!(RefTree_R_co1_80_8_List %in% R_20_co1_80_8_List )] #3300

RefTree_R_co1_20_9_List <- foreach(i=1:length(R_co1_80_9_species)) %do% sister(ReferenceTree,R_co1_80_9_species[i],type="terminal",label=T)
R_80_co1_20_9_List <- foreach(i=1:length(R_co1_80_9_species)) %do% sister(R20_9_tree,R_co1_80_9_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_9_List <- t(plyr::ldply(RefTree_R_co1_20_9_List, rbind))#4519 rows
df_R_80_co1_20_9_List <- t(plyr::ldply(R_80_co1_20_9_List, rbind))#589 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_9<-data.frame(str_extract(df_RefTree_R_co1_20_9_List , "[^_]+"))
df_R_80_co1_20_9<-data.frame(str_extract(df_R_80_co1_20_9_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_9) <- "genus"
colnames(df_R_80_co1_20_9) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_9_new <- data.frame(split(df_RefTree_R_co1_20_9,rep(1:3285,each=4519)))
class(df_RefTree_R_co1_20_9_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_9_new <- data.frame(split(df_R_80_co1_20_9,rep(1:3285,each=589)))
class(df_R_80_co1_20_9_new)
df_RefTree_R_co1_20_9_new[is.na(df_RefTree_R_co1_20_9_new)] <- ""
df_RefTree_R_co1_20_9_new_t <- data.frame(t(df_RefTree_R_co1_20_9_new))
is.na(df_RefTree_R_co1_20_9_new_t)
df_R_80_co1_20_9_new[is.na(df_R_80_co1_20_9_new)] <- ""
df_R_80_co1_20_9_new_t<- data.frame(t(df_R_80_co1_20_9_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_9_new<- apply(df_RefTree_R_co1_20_9_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_9_new<- apply(df_R_80_co1_20_9_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_9_new)

similar <- uniq_RefTree_R_co1_20_9_new [(uniq_RefTree_R_co1_20_9_new %in% uniq_R_80_co1_20_9_new )] #1975
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_9_new [!(uniq_RefTree_R_co1_20_9_new %in% uniq_R_80_co1_20_9_new )] #1310


# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_80_9_List [(RefTree_R_co1_80_9_List %in% R_20_co1_80_9_List )] #312
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_80_9_List [!(RefTree_R_co1_80_9_List %in% R_20_co1_80_9_List )] #3304

RefTree_R_co1_20_10_List <- foreach(i=1:length(R_co1_80_10_species)) %do% sister(ReferenceTree,R_co1_80_10_species[i],type="terminal",label=T)
R_80_co1_20_10_List <- foreach(i=1:length(R_co1_80_10_species)) %do% sister(R20_10_tree,R_co1_80_10_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_10_List <- t(plyr::ldply(RefTree_R_co1_20_10_List, rbind))#4519 rows
df_R_80_co1_20_10_List <- t(plyr::ldply(R_80_co1_20_10_List, rbind))#235 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_10<-data.frame(str_extract(df_RefTree_R_co1_20_10_List , "[^_]+"))
df_R_80_co1_20_10<-data.frame(str_extract(df_R_80_co1_20_10_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_10) <- "genus"
colnames(df_R_80_co1_20_10) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_10_new <- data.frame(split(df_RefTree_R_co1_20_10,rep(1:3285,each=4519)))
class(df_RefTree_R_co1_20_10_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_10_new <- data.frame(split(df_R_80_co1_20_10,rep(1:3285,each=235)))
class(df_R_80_co1_20_10_new)
df_RefTree_R_co1_20_10_new[is.na(df_RefTree_R_co1_20_10_new)] <- ""
df_RefTree_R_co1_20_10_new_t <- data.frame(t(df_RefTree_R_co1_20_10_new))
is.na(df_RefTree_R_co1_20_10_new_t)
df_R_80_co1_20_10_new[is.na(df_R_80_co1_20_10_new)] <- ""
df_R_80_co1_20_10_new_t<- data.frame(t(df_R_80_co1_20_10_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_10_new<- apply(df_RefTree_R_co1_20_10_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_10_new<- apply(df_R_80_co1_20_10_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_10_new)

similar <- uniq_RefTree_R_co1_20_10_new [(uniq_RefTree_R_co1_20_10_new %in% uniq_R_80_co1_20_10_new )] #1923
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_10_new [!(uniq_RefTree_R_co1_20_10_new %in% uniq_R_80_co1_20_10_new )] #1362


# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_80_10_List [(RefTree_R_co1_80_10_List %in% R_20_co1_80_10_List )] #335
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_80_10_List [!(RefTree_R_co1_80_10_List %in% R_20_co1_80_10_List )] #3281
#---------------------


#bb 99% tree when placed 1% co1
#To get easy coding, I rename RefTree_R_co1_1_1_List as RefTree_R_co1_20_1_List******
#R_99_co1_1_1_List as R_80_co1_20_1_List*******
RefTree_R_co1_20_1_List <- foreach(i=1:length(R_co1_1_1_species)) %do% sister(ReferenceTree,R_co1_1_1_species[i],type="terminal",label=T)
R_80_co1_20_1_List <- foreach(i=1:length(R_co1_1_1_species)) %do% sister(R99_1_tree,R_co1_1_1_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_1_List <- t(plyr::ldply(RefTree_R_co1_20_1_List, rbind))#15 rows
df_R_80_co1_20_1_List <- t(plyr::ldply(R_80_co1_20_1_List, rbind))#15 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_1<-data.frame(str_extract(df_RefTree_R_co1_20_1_List , "[^_]+"))
df_R_80_co1_20_1<-data.frame(str_extract(df_R_80_co1_20_1_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_1) <- "genus"
colnames(df_R_80_co1_20_1) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_1_new <- data.frame(split(df_RefTree_R_co1_20_1,rep(1:27,each=15)))
class(df_RefTree_R_co1_20_1_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_1_new <- data.frame(split(df_R_80_co1_20_1,rep(1:27,each=15)))
class(df_R_80_co1_20_1_new)
df_RefTree_R_co1_20_1_new[is.na(df_RefTree_R_co1_20_1_new)] <- ""
df_RefTree_R_co1_20_1_new_t <- data.frame(t(df_RefTree_R_co1_20_1_new))
is.na(df_RefTree_R_co1_20_1_new_t)
df_R_80_co1_20_1_new[is.na(df_R_80_co1_20_1_new)] <- ""
df_R_80_co1_20_1_new_t<- data.frame(t(df_R_80_co1_20_1_new))
#uniq_R_80_co1_20_1_new <- apply(df_R_80_co1_20_1_new_t[], 1, unique)
#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_1_new<- apply(df_RefTree_R_co1_20_1_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_1_new<- apply(df_R_80_co1_20_1_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_1_new)

similar <- uniq_RefTree_R_co1_20_1_new [(uniq_RefTree_R_co1_20_1_new %in% uniq_R_80_co1_20_1_new )] #26
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_1_new [!(uniq_RefTree_R_co1_20_1_new %in% uniq_R_80_co1_20_1_new )] #1

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_1_1_List [(RefTree_R_co1_1_1_List %in% R_99_co1_1_1_List )] #24
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_1_1_List [!(RefTree_R_co1_1_1_List %in% R_99_co1_1_1_List)] #21

RefTree_R_co1_20_2_List <- foreach(i=1:length(R_co1_1_2_species)) %do% sister(ReferenceTree,R_co1_1_2_species[i],type="terminal",label=T)
R_80_co1_20_2_List <- foreach(i=1:length(R_co1_1_2_species)) %do% sister(R99_2_tree,R_co1_1_2_species[i],type="terminal",label=T)


df_RefTree_R_co1_20_2_List <- t(plyr::ldply(RefTree_R_co1_20_2_List, rbind))#17 rows
df_R_80_co1_20_2_List <- t(plyr::ldply(R_80_co1_20_2_List, rbind))#17 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_2<-data.frame(str_extract(df_RefTree_R_co1_20_2_List , "[^_]+"))
df_R_80_co1_20_2<-data.frame(str_extract(df_R_80_co1_20_2_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_2) <- "genus"
colnames(df_R_80_co1_20_2) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_2_new <- data.frame(split(df_RefTree_R_co1_20_2,rep(1:27,each=17)))
class(df_RefTree_R_co1_20_2_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_2_new <- data.frame(split(df_R_80_co1_20_2,rep(1:27,each=17)))
class(df_R_80_co1_20_2_new)
df_RefTree_R_co1_20_2_new[is.na(df_RefTree_R_co1_20_2_new)] <- ""
df_RefTree_R_co1_20_2_new_t <- data.frame(t(df_RefTree_R_co1_20_2_new))
is.na(df_RefTree_R_co1_20_2_new_t)
df_R_80_co1_20_2_new[is.na(df_R_80_co1_20_2_new)] <- ""
df_R_80_co1_20_2_new_t<- data.frame(t(df_R_80_co1_20_2_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_2_new<- apply(df_RefTree_R_co1_20_2_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_2_new<- apply(df_R_80_co1_20_2_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_2_new)

similar <- uniq_RefTree_R_co1_20_2_new [(uniq_RefTree_R_co1_20_2_new %in% uniq_R_80_co1_20_2_new )] #26
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_2_new [!(uniq_RefTree_R_co1_20_2_new %in% uniq_R_80_co1_20_2_new )] #1

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_1_2_List [(RefTree_R_co1_1_2_List %in% R_99_co1_1_2_List )] #22
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_1_2_List [!(RefTree_R_co1_1_2_List %in% R_99_co1_1_2_List)] #23

RefTree_R_co1_20_3_List <- foreach(i=1:length(R_co1_1_3_species)) %do% sister(ReferenceTree,R_co1_1_3_species[i],type="terminal",label=T)
R_80_co1_20_3_List <- foreach(i=1:length(R_co1_1_3_species)) %do% sister(R99_3_tree,R_co1_1_3_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_3_List <- t(plyr::ldply(RefTree_R_co1_20_3_List, rbind))#10 rows
df_R_80_co1_20_3_List <- t(plyr::ldply(R_80_co1_20_3_List, rbind))#10 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_3<-data.frame(str_extract(df_RefTree_R_co1_20_3_List , "[^_]+"))
df_R_80_co1_20_3<-data.frame(str_extract(df_R_80_co1_20_3_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_3) <- "genus"
colnames(df_R_80_co1_20_3) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_3_new <- data.frame(split(df_RefTree_R_co1_20_3,rep(1:27,each=10)))
class(df_RefTree_R_co1_20_3_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_3_new <- data.frame(split(df_R_80_co1_20_3,rep(1:27,each=10)))
class(df_R_80_co1_20_3_new)
df_RefTree_R_co1_20_3_new[is.na(df_RefTree_R_co1_20_3_new)] <- ""
df_RefTree_R_co1_20_3_new_t <- data.frame(t(df_RefTree_R_co1_20_3_new))
is.na(df_RefTree_R_co1_20_3_new_t)
df_R_80_co1_20_3_new[is.na(df_R_80_co1_20_3_new)] <- ""
df_R_80_co1_20_3_new_t<- data.frame(t(df_R_80_co1_20_3_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_3_new<- apply(df_RefTree_R_co1_20_3_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_3_new<- apply(df_R_80_co1_20_3_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_3_new)

similar <- uniq_RefTree_R_co1_20_3_new [(uniq_RefTree_R_co1_20_3_new %in% uniq_R_80_co1_20_3_new )] #23
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_3_new [!(uniq_RefTree_R_co1_20_3_new %in% uniq_R_80_co1_20_3_new )] #4

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_1_3_List [(RefTree_R_co1_1_3_List %in% R_99_co1_1_3_List )] #23
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_1_3_List [!(RefTree_R_co1_1_3_List %in% R_99_co1_1_3_List)] #22

RefTree_R_co1_20_4_List <- foreach(i=1:length(R_co1_1_4_species)) %do% sister(ReferenceTree,R_co1_1_4_species[i],type="terminal",label=T)
R_80_co1_20_4_List <- foreach(i=1:length(R_co1_1_4_species)) %do% sister(R99_4_tree,R_co1_1_4_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_4_List <- t(plyr::ldply(RefTree_R_co1_20_4_List, rbind))#7 rows
df_R_80_co1_20_4_List <- t(plyr::ldply(R_80_co1_20_4_List, rbind))#7 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_4<-data.frame(str_extract(df_RefTree_R_co1_20_4_List , "[^_]+"))
df_R_80_co1_20_4<-data.frame(str_extract(df_R_80_co1_20_4_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_4) <- "genus"
colnames(df_R_80_co1_20_4) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_4_new <- data.frame(split(df_RefTree_R_co1_20_4,rep(1:27,each=7)))
class(df_RefTree_R_co1_20_4_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_4_new <- data.frame(split(df_R_80_co1_20_4,rep(1:27,each=7)))
class(df_R_80_co1_20_4_new)
df_RefTree_R_co1_20_4_new[is.na(df_RefTree_R_co1_20_4_new)] <- ""
df_RefTree_R_co1_20_4_new_t <- data.frame(t(df_RefTree_R_co1_20_4_new))
is.na(df_RefTree_R_co1_20_4_new_t)
df_R_80_co1_20_4_new[is.na(df_R_80_co1_20_4_new)] <- ""
df_R_80_co1_20_4_new_t<- data.frame(t(df_R_80_co1_20_4_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_4_new<- apply(df_RefTree_R_co1_20_4_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_4_new<- apply(df_R_80_co1_20_4_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_4_new)

similar <- uniq_RefTree_R_co1_20_4_new [(uniq_RefTree_R_co1_20_4_new %in% uniq_R_80_co1_20_4_new )] #26
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_4_new [!(uniq_RefTree_R_co1_20_4_new %in% uniq_R_80_co1_20_4_new )] #1

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_1_4_List [(RefTree_R_co1_1_4_List %in% R_99_co1_1_4_List )] #21
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_1_4_List [!(RefTree_R_co1_1_4_List %in% R_99_co1_1_4_List)] #24

RefTree_R_co1_20_5_List <- foreach(i=1:length(R_co1_1_5_species)) %do% sister(ReferenceTree,R_co1_1_5_species[i],type="terminal",label=T)
R_80_co1_20_5_List <- foreach(i=1:length(R_co1_1_5_species)) %do% sister(R99_5_tree,R_co1_1_5_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_5_List <- t(plyr::ldply(RefTree_R_co1_20_5_List, rbind))#10 rows
df_R_80_co1_20_5_List <- t(plyr::ldply(R_80_co1_20_5_List, rbind))#10 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_5<-data.frame(str_extract(df_RefTree_R_co1_20_5_List , "[^_]+"))
df_R_80_co1_20_5<-data.frame(str_extract(df_R_80_co1_20_5_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_5) <- "genus"
colnames(df_R_80_co1_20_5) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_5_new <- data.frame(split(df_RefTree_R_co1_20_5,rep(1:27,each=10)))
class(df_RefTree_R_co1_20_5_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_5_new <- data.frame(split(df_R_80_co1_20_5,rep(1:27,each=10)))
class(df_R_80_co1_20_5_new)
df_RefTree_R_co1_20_5_new[is.na(df_RefTree_R_co1_20_5_new)] <- ""
df_RefTree_R_co1_20_5_new_t <- data.frame(t(df_RefTree_R_co1_20_5_new))
is.na(df_RefTree_R_co1_20_5_new_t)
df_R_80_co1_20_5_new[is.na(df_R_80_co1_20_5_new)] <- ""
df_R_80_co1_20_5_new_t<- data.frame(t(df_R_80_co1_20_5_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_5_new<- apply(df_RefTree_R_co1_20_5_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_5_new<- apply(df_R_80_co1_20_5_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_5_new)

similar <- uniq_RefTree_R_co1_20_5_new [(uniq_RefTree_R_co1_20_5_new %in% uniq_R_80_co1_20_5_new )] #26
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_5_new [!(uniq_RefTree_R_co1_20_5_new %in% uniq_R_80_co1_20_5_new )] #1

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_1_5_List [(RefTree_R_co1_1_5_List %in% R_99_co1_1_5_List )] #25
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_1_5_List [!(RefTree_R_co1_1_5_List %in% R_99_co1_1_5_List)] #20

RefTree_R_co1_20_6_List <- foreach(i=1:length(R_co1_1_6_species)) %do% sister(ReferenceTree,R_co1_1_6_species[i],type="terminal",label=T)
R_80_co1_20_6_List <- foreach(i=1:length(R_co1_1_6_species)) %do% sister(R99_6_tree,R_co1_1_6_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_6_List <- t(plyr::ldply(RefTree_R_co1_20_6_List, rbind))#42 rows
df_R_80_co1_20_6_List <- t(plyr::ldply(R_80_co1_20_6_List, rbind))#7 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_6<-data.frame(str_extract(df_RefTree_R_co1_20_6_List , "[^_]+"))
df_R_80_co1_20_6<-data.frame(str_extract(df_R_80_co1_20_6_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_6) <- "genus"
colnames(df_R_80_co1_20_6) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_6_new <- data.frame(split(df_RefTree_R_co1_20_6,rep(1:27,each=42)))
class(df_RefTree_R_co1_20_6_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_6_new <- data.frame(split(df_R_80_co1_20_6,rep(1:27,each=7)))
class(df_R_80_co1_20_6_new)
df_RefTree_R_co1_20_6_new[is.na(df_RefTree_R_co1_20_6_new)] <- ""
df_RefTree_R_co1_20_6_new_t <- data.frame(t(df_RefTree_R_co1_20_6_new))
is.na(df_RefTree_R_co1_20_6_new_t)
df_R_80_co1_20_6_new[is.na(df_R_80_co1_20_6_new)] <- ""
df_R_80_co1_20_6_new_t<- data.frame(t(df_R_80_co1_20_6_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_6_new<- apply(df_RefTree_R_co1_20_6_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_6_new<- apply(df_R_80_co1_20_6_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_6_new)

similar <- uniq_RefTree_R_co1_20_6_new [(uniq_RefTree_R_co1_20_6_new %in% uniq_R_80_co1_20_6_new )] #24
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_6_new [!(uniq_RefTree_R_co1_20_6_new %in% uniq_R_80_co1_20_6_new )] #3

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_1_6_List [(RefTree_R_co1_1_6_List %in% R_99_co1_1_6_List )] #31
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_1_6_List [!(RefTree_R_co1_1_6_List %in% R_99_co1_1_6_List)] #14

RefTree_R_co1_20_7_List <- foreach(i=1:length(R_co1_1_7_species)) %do% sister(ReferenceTree,R_co1_1_7_species[i],type="terminal",label=T)
R_80_co1_20_7_List <- foreach(i=1:length(R_co1_1_7_species)) %do% sister(R99_7_tree,R_co1_1_7_species[i],type="terminal",label=T)


df_RefTree_R_co1_20_7_List <- t(plyr::ldply(RefTree_R_co1_20_7_List, rbind))#5 rows
df_R_80_co1_20_7_List <- t(plyr::ldply(R_80_co1_20_7_List, rbind))#5 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_7<-data.frame(str_extract(df_RefTree_R_co1_20_7_List , "[^_]+"))
df_R_80_co1_20_7<-data.frame(str_extract(df_R_80_co1_20_7_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_7) <- "genus"
colnames(df_R_80_co1_20_7) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_7_new <- data.frame(split(df_RefTree_R_co1_20_7,rep(1:27,each=5)))
class(df_RefTree_R_co1_20_7_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_7_new <- data.frame(split(df_R_80_co1_20_7,rep(1:27,each=5)))
class(df_R_80_co1_20_7_new)
df_RefTree_R_co1_20_7_new[is.na(df_RefTree_R_co1_20_7_new)] <- ""
df_RefTree_R_co1_20_7_new_t <- data.frame(t(df_RefTree_R_co1_20_7_new))
is.na(df_RefTree_R_co1_20_7_new_t)
df_R_80_co1_20_7_new[is.na(df_R_80_co1_20_7_new)] <- ""
df_R_80_co1_20_7_new_t<- data.frame(t(df_R_80_co1_20_7_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_7_new<- apply(df_RefTree_R_co1_20_7_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_7_new<- apply(df_R_80_co1_20_7_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_7_new)

similar <- uniq_RefTree_R_co1_20_7_new [(uniq_RefTree_R_co1_20_7_new %in% uniq_R_80_co1_20_7_new )] #25
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_7_new [!(uniq_RefTree_R_co1_20_7_new %in% uniq_R_80_co1_20_7_new )] #2

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_1_7_List [(RefTree_R_co1_1_7_List %in% R_99_co1_1_7_List )] #32
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_1_7_List [!(RefTree_R_co1_1_7_List %in% R_99_co1_1_7_List)] #13

RefTree_R_co1_20_8_List <- foreach(i=1:length(R_co1_1_8_species)) %do% sister(ReferenceTree,R_co1_1_8_species[i],type="terminal",label=T)
R_80_co1_20_8_List <- foreach(i=1:length(R_co1_1_8_species)) %do% sister(R99_8_tree,R_co1_1_8_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_8_List <- t(plyr::ldply(RefTree_R_co1_20_8_List, rbind))#25 rows
df_R_80_co1_20_8_List <- t(plyr::ldply(R_80_co1_20_8_List, rbind))#46 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_8<-data.frame(str_extract(df_RefTree_R_co1_20_8_List , "[^_]+"))
df_R_80_co1_20_8<-data.frame(str_extract(df_R_80_co1_20_8_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_8) <- "genus"
colnames(df_R_80_co1_20_8) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_8_new <- data.frame(split(df_RefTree_R_co1_20_8,rep(1:27,each=25)))
class(df_RefTree_R_co1_20_8_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_8_new <- data.frame(split(df_R_80_co1_20_8,rep(1:27,each=46)))
class(df_R_80_co1_20_8_new)
df_RefTree_R_co1_20_8_new[is.na(df_RefTree_R_co1_20_8_new)] <- ""
df_RefTree_R_co1_20_8_new_t <- data.frame(t(df_RefTree_R_co1_20_8_new))
is.na(df_RefTree_R_co1_20_8_new_t)
df_R_80_co1_20_8_new[is.na(df_R_80_co1_20_8_new)] <- ""
df_R_80_co1_20_8_new_t<- data.frame(t(df_R_80_co1_20_8_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_8_new<- apply(df_RefTree_R_co1_20_8_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_8_new<- apply(df_R_80_co1_20_8_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_8_new)

similar <- uniq_RefTree_R_co1_20_8_new [(uniq_RefTree_R_co1_20_8_new %in% uniq_R_80_co1_20_8_new )] #22
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_8_new [!(uniq_RefTree_R_co1_20_8_new %in% uniq_R_80_co1_20_8_new )] #5

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_1_8_List [(RefTree_R_co1_1_8_List %in% R_99_co1_1_8_List )] #21
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_1_8_List [!(RefTree_R_co1_1_8_List %in% R_99_co1_1_8_List)] #24

RefTree_R_co1_20_9_List <- foreach(i=1:length(R_co1_1_9_species)) %do% sister(ReferenceTree,R_co1_1_9_species[i],type="terminal",label=T)
R_80_co1_20_9_List <- foreach(i=1:length(R_co1_1_9_species)) %do% sister(R99_9_tree,R_co1_1_9_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_9_List <- t(plyr::ldply(RefTree_R_co1_20_9_List, rbind))#7 rows
df_R_80_co1_20_9_List <- t(plyr::ldply(R_80_co1_20_9_List, rbind))#7 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_9<-data.frame(str_extract(df_RefTree_R_co1_20_9_List , "[^_]+"))
df_R_80_co1_20_9<-data.frame(str_extract(df_R_80_co1_20_9_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_9) <- "genus"
colnames(df_R_80_co1_20_9) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_9_new <- data.frame(split(df_RefTree_R_co1_20_9,rep(1:27,each=7)))
class(df_RefTree_R_co1_20_9_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_9_new <- data.frame(split(df_R_80_co1_20_9,rep(1:27,each=7)))
class(df_R_80_co1_20_9_new)
df_RefTree_R_co1_20_9_new[is.na(df_RefTree_R_co1_20_9_new)] <- ""
df_RefTree_R_co1_20_9_new_t <- data.frame(t(df_RefTree_R_co1_20_9_new))
is.na(df_RefTree_R_co1_20_9_new_t)
df_R_80_co1_20_9_new[is.na(df_R_80_co1_20_9_new)] <- ""
df_R_80_co1_20_9_new_t<- data.frame(t(df_R_80_co1_20_9_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_9_new<- apply(df_RefTree_R_co1_20_9_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_9_new<- apply(df_R_80_co1_20_9_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_9_new)

similar <- uniq_RefTree_R_co1_20_9_new [(uniq_RefTree_R_co1_20_9_new %in% uniq_R_80_co1_20_9_new )] #25
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_9_new [!(uniq_RefTree_R_co1_20_9_new %in% uniq_R_80_co1_20_9_new )] #2


# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_1_9_List [(RefTree_R_co1_1_9_List %in% R_99_co1_1_9_List )] #24
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_1_9_List [!(RefTree_R_co1_1_9_List %in% R_99_co1_1_9_List)] #21


RefTree_R_co1_20_10_List <- foreach(i=1:length(R_co1_1_10_species)) %do% sister(ReferenceTree,R_co1_1_10_species[i],type="terminal",label=T)
R_80_co1_20_10_List <- foreach(i=1:length(R_co1_1_10_species)) %do% sister(R99_10_tree,R_co1_1_10_species[i],type="terminal",label=T)


df_RefTree_R_co1_20_10_List <- t(plyr::ldply(RefTree_R_co1_20_10_List, rbind))#42 rows
df_R_80_co1_20_10_List <- t(plyr::ldply(R_80_co1_20_10_List, rbind))#20 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_10<-data.frame(str_extract(df_RefTree_R_co1_20_10_List , "[^_]+"))
df_R_80_co1_20_10<-data.frame(str_extract(df_R_80_co1_20_10_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_10) <- "genus"
colnames(df_R_80_co1_20_10) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_10_new <- data.frame(split(df_RefTree_R_co1_20_10,rep(1:27,each=42)))
class(df_RefTree_R_co1_20_10_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_10_new <- data.frame(split(df_R_80_co1_20_10,rep(1:27,each=20)))
class(df_R_80_co1_20_10_new)
df_RefTree_R_co1_20_10_new[is.na(df_RefTree_R_co1_20_10_new)] <- ""
df_RefTree_R_co1_20_10_new_t <- data.frame(t(df_RefTree_R_co1_20_10_new))
is.na(df_RefTree_R_co1_20_10_new_t)
df_R_80_co1_20_10_new[is.na(df_R_80_co1_20_10_new)] <- ""
df_R_80_co1_20_10_new_t<- data.frame(t(df_R_80_co1_20_10_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_10_new<- apply(df_RefTree_R_co1_20_10_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_10_new<- apply(df_R_80_co1_20_10_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_10_new)

similar <- uniq_RefTree_R_co1_20_10_new [(uniq_RefTree_R_co1_20_10_new %in% uniq_R_80_co1_20_10_new )] #23
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_10_new [!(uniq_RefTree_R_co1_20_10_new %in% uniq_R_80_co1_20_10_new )] #4
