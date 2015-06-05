test_query <- function() {
    ah = AnnotationHub()
    q1 <- query(ah, c("GTF", "Ensembl", "Homo sapiens"))
    checkTrue("AH7558" %in% names(q1)) 
    nm <- c("title", "dataprovider", "species", "taxonomyid", "genome", 
        "description", "tags", "rdataclass", "sourceurl", "sourcetype")
    checkEquals(nm, names(mcols(q1)))
}
