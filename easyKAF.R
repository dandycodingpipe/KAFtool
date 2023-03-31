# easyKAF function #

# this script summarizes the entire KAF workflow of information retrieval, NLP, and ARM



# package dependencies:
library(tidyverse)
library(tidytext)
library(tm)
library(arules)
library(arulesViz)

# my script dependencies:
source("Information_Retrieval.R")
source("NLP.R")
source("Apriori_ARM_tool.R")
source("MeSH_Classification.R")
source("Visualization.R")

easyKAF <- function(venv, lang_model){

# what do you want to search?

print("((complement system pathway) OR (complement system proteins)) AND (pathology OR pathologies) AND 1980:2023[dp]")
print("dog hip dysplasia")
print("human DNA polymerase AND (damage or repair)")
print("(Crohns disease OR Crohn's disease) AND (therapy OR treatment OR prognosis)")
print(myQuery <- "(muscle synergy OR muscle coordination)")
print("the complement system pathway and diseases")
print("(gunpowder OR propellant) AND toxicity")
print("microfabrication chip AND membrane encapsulation")


#commence KAF worfklow

queryString <- toupper(toString(readline("these are some examples above. write whatever you want and see what biologists know about the subject!")))

databaseString <- toupper(toString(readline(paste("U.S PubMed or Europe PMC as a database? (EU runs better!)", '\n', "EU (1) or US (2):"))))

 if(databaseString == "1"){
            databaseString = "pmc"} else {databaseString = "pubmed"}
      # 1. retrieving
      
            
                  source("Information_Retrieval.R")

                  retrieved <- info_retrieval(query = queryString, how_many_articles = 1000,  database = databaseString)
      
      # 2. parsing
                  
                  source("NLP.R")
                  preProcessed <- Text_Parser(retrieved, venv = venv, lang_model =  lang_model, reduced_search = 0.2)
                  
      # 3. mining
                  source("Apriori_ARM_tool.R")
                  rules <- ARM(preProcessed, 0.01, 0.75, 0.005)
                  #filt_rules <- which(small_rules$lift <= 2)
                  #rules <- rules[-filt_rules,]
 
                  return(rules)     
}

      
