#I want to compare a lists of list (In my code this is named as "Similar") with another name list and keep the lists of list that includes names in the name list (In my code this name list is Ref_Tree_distinct with 766 objects)
#Finally, I want to have the count of lists of list in "Similar" including two or more genes
#Note that this lists of list is from a tree tip labels (So, the lists have different lengths with species names)
#I want genus name not the species name in the downstream analysis.
#Hence, generated the following code

#Read fasta files of placed co1 on the bb tree
R_co1_20_1=read.fasta(file = "CO1_20_1.fasta")
#read the refernce tree
ReferenceTree <- read.tree("RAxML_bestTree.FR100_new")
#read the backbone tree
R80_1_tree<-read.tree("EPA80_1R_new.nwk")

R_co1_20_1_species <- R_co1_20_1$seq.name

#To remove single species in genus groups
Ref_Tree <- as.list(ReferenceTree)
Ref_Tree_Tips <- Ref_Tree$tip.label
class(Ref_Tree_Tips)
Ref_Tree_Tips <- as.data.frame(Ref_Tree_Tips)
df_ReferenceTree<-data.frame(str_extract(Ref_Tree_Tips$Ref_Tree_Tips , "[^_]+"))
colnames(df_ReferenceTree) <- c("genus")
names(df_ReferenceTree)
#Get ununique species by removing single species
genus_more_1<- df_ReferenceTree %>% 
  group_by(genus) %>% 
  filter(n()>=2)
#Get distinct names
Ref_Tree_distinct <- distinct(genus_more_1)#766 obs.



#Sister species of placement sequences in reference tree
RefTree_R_co1_20_1_List <- foreach(i=1:length(R_co1_20_1_species)) %do% sister(ReferenceTree,R_co1_20_1_species[i],type="terminal",label=T)
#Sister species for placement sequeneces(20%) after placing on bb tree(80% bb tree)
R_80_co1_20_1_List <- foreach(i=1:length(R_co1_20_1_species)) %do% sister(R80_1_tree,R_co1_20_1_species[i],type="terminal",label=T)

#I have species name list, but I want just genus name. As this is a list of list I couldn't find a better way to extract genus name from the species name in the different size list of lists
#If you have a simple soultion I am happy to see that as well. Thank you!
#df_RefTree_R_co1_20_1_List <- data.frame(RefTree_R_co1_20_1_List)#diff size column error
#When converting a list to a dataframe there's an error called different size column error.
#To solve that I used one liner with plyr
#t to transpose mt dataframe for a better analysis
df_RefTree_R_co1_20_1_List <- t(plyr::ldply(RefTree_R_co1_20_1_List, rbind))#98 rows
df_R_80_co1_20_1_List <- t(plyr::ldply(R_80_co1_20_1_List, rbind))#3997 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_1<-data.frame(str_extract(df_RefTree_R_co1_20_1_List , "[^_]+"))
df_R_80_co1_20_1<-data.frame(str_extract(df_R_80_co1_20_1_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_1) <- "genus"
colnames(df_R_80_co1_20_1) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
#I had to use this step as I need all genus name in the same list as they were before.
df_RefTree_R_co1_20_1_new <- data.frame(split(df_RefTree_R_co1_20_1,rep(1:904,each=98)))
class(df_RefTree_R_co1_20_1_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_1_new <- data.frame(split(df_R_80_co1_20_1,rep(1:904,each=3997)))
class(df_R_80_co1_20_1_new)
#NA with ""
df_RefTree_R_co1_20_1_new[is.na(df_RefTree_R_co1_20_1_new)] <- ""
df_RefTree_R_co1_20_1_new_t <- data.frame(t(df_RefTree_R_co1_20_1_new))
#Just checking
#is.na(df_RefTree_R_co1_20_1_new_t)
df_R_80_co1_20_1_new[is.na(df_R_80_co1_20_1_new)] <- ""
df_R_80_co1_20_1_new_t<- data.frame(t(df_R_80_co1_20_1_new))
#uniq_R_80_co1_20_1_new <- apply(df_R_80_co1_20_1_new_t[], 1, unique)
#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_1_new<- apply(df_RefTree_R_co1_20_1_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_1_new<- apply(df_R_80_co1_20_1_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_1_new)

#As I want to check the similar tip labels in reference tree and the placment tree  (bb after placing co1 genes), I used the following step
similar <- uniq_RefTree_R_co1_20_1_new [(uniq_RefTree_R_co1_20_1_new %in% uniq_R_80_co1_20_1_new )] #672
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_1_new [!(uniq_RefTree_R_co1_20_1_new %in% uniq_R_80_co1_20_1_new )] #232

#The issue is the list name "similar" has single species for some genus names. I want to remove the single species genus from this "similar" list
#At the very begining I found genus names with two or more species
#I want to compare the "similar" list of lists with the genus name list (Ref_Tree_distinct, this hass 766 objects)
#genus name list is in a dataframe, but feel free to change it to another data type
#So final result should include the similar lists of list without single genes (with two or more gene names)
