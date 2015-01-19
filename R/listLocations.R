listLocations <-
function(){

locationUrl=html(paste("http://www.nhm.ac.uk/research-curation/scientific-resources/taxonomy-systematics/host-parasites/database/location.jsp?location=&paragroup=&showparasites=on&paraspecies=&fmhostgenus=Contains&fmparagenus=Starts+with&showrefs=on&fmsubgroup=Starts+with&groupby=parasite&pstatus=&showhosts=on&hostspecies=&hostgenus=&paragenus=&fmparaspecies=Starts+with&subgroup=&fmhostspecies=Contains&hstate=&getlocation=select"))

locations <- locationUrl %>% 
html_nodes("option") %>%
html_text()

    loc1=gsub('\r\n ', '', locations)
loc2=gsub('  ', '', loc1)
return(loc2)
}
