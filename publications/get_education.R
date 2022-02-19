library(rorcid)
library(tidyverse)
edu <- do.call("rbind", rorcid::orcid_educations("0000-0001-9713-2330")$`0000-0001-9713-2330`$`affiliation-group`$summaries)
saveRDS(edu, "./data/edu.rds")
