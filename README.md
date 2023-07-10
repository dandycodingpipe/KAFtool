# Keyword Association Finder

The Keyword Association Finder, or KAF, helps you delve deeper into new or familiar biotechnology topics by data mining several abstracts relating to your typical database query.

## Description

This tool mines anywhere from 500 to 20,000 articles that relate to a PubMed or Europe PMC query and retrieves the most signficant keyword relationships that exist within the corpus. The results can be visualized according to biomedical subjects such as anatomical keywords, diseases, chemicals & drugs, or analytical and therapeutic techniques.

## Getting Started

### Dependencies
* Windows, Mac, or Linux 
* R Version (4.2.2 or latest version)
* Python virtual environment with spacy and a spacy english-language model (e.g en_core_web_sm or en_core_web_trf)
* R studio (Optional)


### Installing
```
devtools::install_github('dandycodingpipe/KAFtool')
library(KAFtool)
library(tidyverse)
library(httr)
library(jsonlite)
```
* Before anything, make sure you have R and python installed on your computer.
* Install pip and venv through your command line interface (command prompt on Windows/terminal on Mac)

Windows
```
python -m venv /path/to/directory
path\to\venv\Scripts\activate.bat
pip install spacy
python -m spacy download en_core_web_sm
```

Mac
```
virtualenv path/to/venv
source path/to/venv/bin/activate
pip install -U pip setuptools wheel
pip install -U spacy
python -m spacy download en_core_web_sm
```
Remember the path you define your virtual environment in because it is required in the program.

Also, if you are done using the tool it is recommended to deactivate and remove the virtual environment from your computer.

## Quick Execution (faster results)

This provides the quickest and most reliable ruleset production methodology. (Default parameters of 1000 articles, min_supp = 0.01, min_conf = 0.50, and min_p = 0.001)

```
source("easyKAF.R)
rules <- easyKAF(venv = your_venv, lang_model = your_model)
```
## Manual program execution (more control)

The software is currently not available as a package so it must be called through Github. It is simplified into a 5 functions whose results must be passed onto each other.
* Calling the scripts
```
source("Information_Retrieval.R")
source("NLP.R")
source("Apriori_ARM_tool.R")
source("MeSH_Classification.R")
source("Visualization.R")
```
### Information Retrieval
This is perhaps the most important step. You can make your query essentially any medical or biotechnological query you'd like, but remember that a well-made query can significantly improve the raw data KAF retrieves, therefore the results of the text mining.
See these references for how to manage the unique syntax of each database to optimise your search queries.
[PubMed Query Help/Syntax](https://pubmed.ncbi.nlm.nih.gov/help/) or
[Europe PMC Query Syntax](https://europepmc.org/searchsyntax)
```
myQuery <- "human DNA polymerase AND (damage or repair)"
retrieved_info <- info_retrieval(myQuery, 1000, "pmc")
```
### Natural Language Processing (NLP)
```
NLP_info <- Text_Parser(retrieved_info, venv_path = "your//environment//path", 
            lang_model = "en_core_web_sm", 0.2)
```
the package that KAF uses to initiate communication between R and Python (spacyr) can be sensitive to how the environment path is defined. You may need to try different combinations of single or double forward and backward slashes for declaring the environment path. If the venv is not found, quit R, reinitiate it, and try a new environment path.

### Association-rules Mining (ARM)
```
keyword_info <- ARM(NLP_info, min_supp = 0.01, min_conf = 0.75, min_p = 0.005)
```
### Classification & Visualization
```

classified_keywords <- MeSH_finalizer(keyword_info)

results <- ruleViewer(classified_keywords, "raw", "bme")
```


## Help

Do not retrieve more than 1500 articles for PubMed searches, Europe PMC is better optimized and can retrieve up to 20,000. Be conscious of your hardware limitations as this software can create significant computational load with parameters that call many articles or mine large rulesets.

## Authors

Contributors names and contact info

ex. Christian A. Hernandez-Fajardo
ex. [Linkedin](https://www.linkedin.com/in/christianalejandro/)

## Version History

* 0.2 (coming soon)
    * KAF is now defined as an R package that can be retrieved through devtools::github!
    * KAF now integrates new features like rule-set deduplication and AOP-wiki classification for AOP discovery
    * See [commit change]() or See [release history]()
* 0.1
    * Initial Release

## Disclaimer

It is not the intention of KAF to provide specific medical advice but rather to provide users with information to better understand health, diagnosed disorders, and advances in medicine and biotechnology. Results from KAF are not medical advice and you should consult with a qualified physician for diagnosis and for answers to your personal questions.

## Licensing and Copyrighting

This software sources data from the National Library of Medecine's PubMed.
   
Europe PMC Articles in the open access subset are still protected by copyright, but are made available under a Creative Commons license, or similar, that generally allows more liberal redistribution and reuse than a traditional copyrighted work. 

## Acknowledgments

This tool was developed for the [Systox group](https://systox.u-paris-sciences.fr/) (Unit T3S) at the Université Paris Cité as a means of exploring new methods for leveraging scientific research for the discovery of adverse outcome pathways.

Complementary tool:

* [AOP-helpFinder](http://aop-helpfinder.u-paris-sciences.fr/)
