findHost <-
function(genus, species, location=''){

hpUrl=html(paste("http://www.nhm.ac.uk/research-curation/scientific-resources/taxonomy-systematics/host-parasites/database/results.jsp?paragroup=&fmsubgroup=Starts+with&subgroup=&fmparagenus=Starts+with&paragenus=&fmparaspecies=Starts+with&paraspecies=&fmhostgenus=Contains&hostgenus=",genus,"&fmhostspecies=Contains&hostspecies=", species,"&location=",location,"&hstate=&pstatus=&showparasites=on&showhosts=on&showrefs=on&groupby=parasite&search=Search", sep=''))

names <- hpUrl %>% 
html_nodes(".searchlink")%>%
html_text()

hpList = matrix(names, ncol=2, byrow=TRUE)
colnames(hpList)=c('Parasite', 'Host')
return(hpList)

}
