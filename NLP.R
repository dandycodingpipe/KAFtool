# Natural Language Processing #

# dependencies:

      library(tidyverse)

#1) reducing text size
      Abstract_Trimmer <- function(raw_data, fraction){
            
            # Removes abstracts which contain "NA" instead of text
            NAs <- which(is.na(raw_data$abstract))
            #for whatever reason there is a glitch that removes every datum if no NAs are present
                  if(is_empty(NAs)==TRUE) {
                        print('No corrupted abstracts in retrieval...')
                  } else {
                        raw_data <- raw_data[-NAs,]
                  }
            
            # Calculates the amount of characters to remove based on text size and removes a fraction
                  for(i in 1:length(raw_data$abstract)) {
            
                        total <- nchar(raw_data$abstract[i])
                        new_length <- total - round((fraction*total), digits = 0)
                        new_length <- as.integer(new_length)
                  
                        raw_data$abstract[i] <- str_trunc(raw_data$abstract[i], new_length, 'left')
            }
            return(raw_data)
      }#end abstract trimmer

#2) one-and-done parser
      Text_Parser <- function(data, venv_path, lang_model, reduced_search){
            
            library(spacyr)
            #applying step #1
            Abstract_Trimmer(data, reduced_search)
            print(paste("1) search Reduction. removing",reduced_search*100,"% of abstract head..."))
            
            #spacy environment creation and text parsing
            spacy_initialize(model = lang_model, virtualenv = venv_path )
            print("2) spaCy initialized! parsing the abstracts...")
            parsed_text <- spacy_parse(data$abstract, dependency = TRUE)
            
            #3) negation modifier filtering
            parsed_text <- rm_negation(parsed_text)
            print('3) sentences containing negation modifiers removed...')
            
            print('4) filtering Parts-of-Speech...')
            #4) POS filtering and lemma extraction
            NOUN_POS <- which(parsed_text$pos == "NOUN")
            VERB_POS <- which(parsed_text$pos == "VERB")
            ADJ_POS <- which(parsed_text$pos == "ADJ")
            
            MASTER_POS <- c(NOUN_POS,VERB_POS,ADJ_POS)
            parsed_text <- parsed_text[MASTER_POS,]
            print(paste("filtered ",length(parsed_text$lemma),"potentially relevant lemma."))
           
            
            spacy_finalize()
            print('natural language processing complete!')
            
            return(parsed_text)
      }
      
#3) negation removal function
      rm_negation <- function(Abstract_Parse){
            
            #3) Identifying Coordinates of Negative Modifiers
            
            Copy_Abstract_Parse <- Abstract_Parse
            Negation_Coord <- which(Copy_Abstract_Parse$dep_rel == "neg")
            print(paste("there are", length(Negation_Coord), "negations in the abstracts..."))
            
            #4) Function for Isolating Sentences Containing Negation Modifiers 
            
            master_token_list <- c()
            print(paste("retrieving negation sentences..."))
            for(neg in 1:length(Negation_Coord)){
                  
                  #identifying text_id and sentence_id corresponding to the negation coordinate
                  text_id <- Copy_Abstract_Parse$doc_id[Negation_Coord[neg]]
                  
                  sentence_id <- Copy_Abstract_Parse$sentence_id[Negation_Coord[neg]]
                  
                  #retrieving the coordinates for each token in the sentence containing negation
                  text_id_coordinates <- which(Copy_Abstract_Parse$doc_id == text_id)
                  
                  sentence_id_coordinates <- which(Copy_Abstract_Parse$sentence_id[text_id_coordinates] == sentence_id)
                  
                  token_ids <- text_id_coordinates[sentence_id_coordinates]
                  
                  master_token_list <- c(master_token_list, token_ids)
            }
            print(paste("there are", length(master_token_list), "tokens associatied with negations in the abstracts. Removing..."))
            #Removing Sentences Containing Negations
            
            Copy_Abstract_Parse <- Copy_Abstract_Parse[-master_token_list,]
            return(Copy_Abstract_Parse)
      }