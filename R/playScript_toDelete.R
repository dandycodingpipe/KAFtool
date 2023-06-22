library(devtools)
document()

# Install package
devtools::install_github('dandycodingpipe/KAFtool')
library(KAFtool)
remove.packages("KAFtool")

# Generate association-rules data
rules <- easyKAF(venv = "C:/Users/Chris/OneDrive/2023/Systox/venvJune19", lang_model = "en_core_web_lg")
clean_rules <- MeSH_finalizer(rules, c("-"))
viz <- ruleViewer(clean_rules, "bar", "bme" )
prepared_rules <- Rule_Concatenator(rules)

# Generate AOP referencing data
library(XML)
xml <- "aop-wiki-xml-2023-04-01(1).xml"
doc <- AOP_XML_children_organizer(xml)
