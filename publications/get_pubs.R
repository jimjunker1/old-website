library(tidyverse)
library(scholar)
library(RefManageR)

# ++++  Helper functions ++++ #
'%ni%' <- Negate('%in%')

get_complete_coauthors <- function (id, pubid, sleep = 3){
  httr::set_config(httr::user_agent("jrjunker@mtu.edu; +https://jimjunker.com"))
  auths = ""
  url_template = "http://scholar.google.com/citations?view_op=view_citation&citation_for_view=%s:%s"
  url = sprintf(url_template, id, pubid)
  Sys.sleep(sleep)
  url1 <- xml2::read_html(url)
  auths = as.character(rvest::html_node(url1, ".gsc_vcd_value") %>%
                         rvest::html_text())
  return(auths)
}

convert_complete_author <- function(complete_coauthor_string){

  x <- complete_coauthor_string
  author_lists <- lapply(strsplit(x, ","), stringr::str_trim)
  author_lists <- sapply(author_lists, strsplit, " ")
  author_persons <- lapply(author_lists, function(name){
    if(length(name) == 2){
      person(given = substr(name[1], start = 1, stop = 1), family = name[2], middle = NULL)
    } else if(length(name) == 3){
      person(given = paste0(substr(name[1],start =1, stop = 1),substr(name[2], start = 1, stop = 1)), family = name[3])
    } else{
      person(given = paste0(substr(name[1:(length(name)-1)], start = 1, stop = 1)), family = length(name))
    }})
 return(as.personList(author_persons))
}

author_exchange <- function(x,y){
  x_flat <- unlist(x)
  x_flat$author <- y
  z <- RelistBibEntry(x_flat)
  return(z)
}
# ++++++++++++++++++++++++++ #

# pull pubs list from google scholar & filter out non-journal articles
# Scholar doesn't allow long author lists so must pull from article page
# in the future figure out a better way to do this #
# maybe Zotero ????
pubs <- readRDS(file = "./data/pubs.rds")
pubs <- scholar::get_publications("5pxjNGYAAAAJ") %>%
  slice(-(n()-3):-n()) %>%
  dplyr::mutate(author = author %>%
                  as.character %>%
                  stringr::str_trim(),
                journal = journal,
                first_author = case_when(stringr::str_starts(author, "JR Junker") ~ TRUE,
                                         TRUE ~ FALSE)) %>%
  filter(title %ni% c("Freshwater processing of terrestrial dissolved organic matter: What governs lability?","RIVERINE DISSOLVED ORGANIC MATTER DECOMPOSITION AND DYNAMICS",
                      "Trophic basis of invertebrate production in a Northern Rockies stream with recent willow recovery",
                      "The effects of temperature on stream ecosystem structure, secondary production, and food web dynamics")) %>%
  dplyr::arrange(desc(year))

## convert the author to full author list for CV
full_authors = pubs %>% dplyr::select(pubid) %>% purrr::map(~scholar::get_complete_authors(id = "5pxjNGYAAAAJ", pubid = .x, delay = 0.4 ))
saveRDS(., file = "./data/full_authors.rds")
full_authors = readRDS(file = "./data/full_authors.rds") %>% flatten


pubs_mod <- pubs %>% dplyr::mutate(author = full_authors) %>%
  mutate(author = gsub("\\.","", author),
         author = enc2native(author),
         author = gsub(fixed("<ed>"), "i", author, useBytes = TRUE),
         author = gsub("Jón", "J", author, useBytes = TRUE),
         author = gsub("Gís", "G", author, useBytes = TRUE))#;pubs[,'author']
# saveRDS(pubs, file = "./data/pubs.rds")

# create the bib object for publication page/widget
## create and filter the bib from Google Scholar
jrj.bib <- RefManageR::ReadGS(scholar.id = "5pxjNGYAAAAJ", sort.by.date = TRUE, check.entries = 'warn') %>%
  rlist::list.filter(names(.) %ni% c("junker2011trophic","smith2015riverine","dandrilli2016freshwater", "junker2019effects"))
# WriteBib(jrj.bib, file = "./working_scripts/pubs.bib", biblatex = FALSE)
# jrj.bib2 <- RefManageR::ReadZotero(user = "5288391", .params = list(collection = "My Library", key = "1o3VUn2pbhJsLZn4aWRYMovx"), delete.file = FALSE)
## use full_author list to convert the full author list to person objects

full_authors_list <- lapply(full_authors, convert_complete_author)

# now exchange full_authors_list for each publication in bib
debugonce(author_exchange)
jrj.bib = mapply(author_exchange, x = jrj.bib, y = full_authors_list)
jrj.bib <- lapply(jrj.bib, RelistBibEntry)
# write the bib file
debugonce(WriteBib)
RefManageR::WriteBib(jrj.bib, file = "./data/pubs.bib")

# in terminal run
# reticulate::py_run_file(file = "./working_scripts/pubs_update.py")
# Ctrl + Alt + Enter
academic import --bibtex content/publication/pubs.bib --overwrite

# a = scholar::get_complete_authors(id = "q3E1S9MAAAAJ", pubs[1,"pubid"])
# b = scholar::get_complete_authors(id = "q3E1S9MAAAAJ", pubs[2,"pubid"])
# c = scholar::get_complete_authors(id = "q3E1S9MAAAAJ", pubs[3,"pubid"])
# d = scholar::get_complete_authors(id = "q3E1S9MAAAAJ", pubs[4,"pubid"])
# e = scholar::get_complete_authors(id = "q3E1S9MAAAAJ", pubs[5,"pubid"])
# f = scholar::get_complete_authors(id = "q3E1S9MAAAAJ", pubs[6,"pubid"])
# g = scholar::get_complete_authors(id = "q3E1S9MAAAAJ", pubs[7,"pubid"])
# full_authors = c(a,b,c,d,e,f,g)
