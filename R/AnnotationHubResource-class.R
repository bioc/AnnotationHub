setClass("AnnotationHubResource", representation(hub="AnnotationHub"))

setMethod("show", "AnnotationHubResource",
    function(object)
{
    cat("class:", class(object), "\n")
})

setGeneric(".get1", function(x, ...) {
    stopifnot(is(x, "AnnotationHubResource") && length(x) == 1L)
    standardGeneric(".get1")
})

setMethod(".get1", "AnnotationHubResource",
    function(x, ...)
{
    msg <- sprintf("no '.get1' method defined for object
        of class %s, consider defining your own.",
        sQuote(class(x)))
    stop(paste(strwrap(msg), collapse="\n"))
})

##
## implementations
##

.require <-
    function(pkg)
{
    if (length(grep(sprintf("package:%s", pkg), search())) != 0L)
        return()
    message("require(", dQuote(pkg), ")")
    tryCatch({
        suppressPackageStartupMessages({
            require(pkg, quietly=TRUE, character.only=TRUE)
        })
    }, error=function(err) {
        msg <- sprintf("require(%s) failed: %s", dQuote(pkg),
                       conditionMessage(err))
        stop(msg)
    })
}

## FaFile

setClass("FaFileResource", contains="AnnotationHubResource")

setMethod(".get1", "FaFileResource",
    function(x, ...)
{
    fa <- cache(.hub(x))
    Rsamtools::FaFile(file=fa[1],index=fa[2])
})

## Rda

setClass("RdaResource", contains="AnnotationHubResource")

setMethod(".get1", "RdaResource",
    function(x, ...)
{
    get(load(cache(.hub(x))))
})

setClass("data.frameResource", contains="RdaResource")

setClass("GRangesResource", contains="RdaResource")

setMethod(".get1", "GRangesResource",
    function(x, ...)
{
    .require("GenomicRanges")
    callNextMethod(x, ...)
})

setClass("VCFResource", contains="RdaResource")

setMethod(".get1", "VCFResource",
    function(x, ...)
{
    .require("VariantAnnotation")
    callNextMethod(x, ...)
})

## UCSC chain file
setClass("ChainFileResource", contains="AnnotationHubResource")

## trace(AnnotationHub:::.get1, tracer=browser, signature ="ChainFileResource")
setMethod(".get1", "ChainFileResource",
    function(x, ...)
{
    chain <- cache(.hub(x))
    tf <- .gunzip(chain, tempfile())
    tf <- rtracklayer::import.chain(tf)
    tf[GenomeInfoDb::sortSeqlevels(names(tf))]
})

setClass("TwoBitFileResource", contains="AnnotationHubResource")

setMethod(".get1", "TwoBitFileResource",
    function(x, ...)      
{
    bit <- cache(.hub(x))
    rtracklayer::TwoBitFile(bit)
})

setClass("GTFFileResource", contains="AnnotationHubResource")

setMethod(".get1", "GTFFileResource",
    function(x, ...)
{
    .require("rtracklayer")
    er <- cache(.hub(x))
    rtracklayer::import(er, format="gtf")
})

setClass("BigWigFileResource", contains="AnnotationHubResource")

setMethod(".get1", "BigWigFileResource",
    function(x, ...)
{
    .require("rtracklayer")
    er <- cache(.hub(x))
    rtracklayer::BigWigFile(er)  
})

setClass("dbSNPVCFFileResource", contains="AnnotationHubResource")

setMethod(".get1", "dbSNPVCFFileResource",
    function(x, ...)      
{
    .require("VariantAnnotation")
    er <- cache(.hub(x))
    VariantAnnotation::VcfFile(file=er[1],index=er[2])      
})
## SQLiteFile

setClass("SQLiteFileResource", contains="AnnotationHubResource") 

setMethod(".get1", "SQLiteFileResource",
    function(x, ...)
{
    AnnotationDbi::loadDb(cache(.hub(x)))
})

## GRASP2 SQLiteFile

setClass("GRASPResource", contains="SQLiteFileResource")

setMethod(".get1", "GRASPResource",
    function(x, ...)
{
    RSQLite::dbConnect(RSQLite::SQLite(), cache(.hub(x)),
        flags=RSQLite::SQLITE_RO)
})

setClass("ZipResource", contains="AnnotationHubResource")

setMethod(".get1", "ZipResource",
    function(x, filenames, ...)
{
    zip <- cache(.hub(x))
    for (fl in filenames)
        unzip(zip, fl, exdir=tempdir())
    file.path(tempdir(), filenames)
})

setClass("ChEAResource", contains="ZipResource")

setMethod(".get1", "ChEAResource",
    function(x, ...)
{
    fl <- callNextMethod(x, filenames="chea-background.csv")
    read.csv(fl, header=FALSE, stringsAsFactors=FALSE, 
        col.names=c("s.no","TranscriptionFactor", "TranscriptionFactor-PubmedID", 
        "TranscriptionFactorTarget", "PubmedID", "Experiment", "CellType",
        "Species","DateAdded"))
}) 

setClass("BioPaxResource", contains="RdaResource")

setMethod(".get1", "BioPaxResource",
    function(x, ...)
{
    .require("rBiopaxParser")
    callNextMethod(x, ...)
})
 
setClass("PazarResource", contains="AnnotationHubResource")

setMethod(".get1", "PazarResource",
    function(x, ...)
{
    .require("GenomicRanges")
    er <- cache(.hub(x))
    dat <- read.delim(er, header=FALSE, stringsAsFactors=FALSE,
        col.names=c("PazarTFID","EnsemblTFAccession", "TFName", "PazarGeneID",
        "EnsemblGeneAccession", "Chr", "GeneStart", "GeneEnd", "Species", 
	"ProjectName","PMID", "AnalysisMethod"))
    dat <- dat[, -12]  # collumn contains only NA
    tryCatch({
        dat <- makeGRangesFromDataFrame(dat, keep.extra.columns=TRUE)
    }, error=function(err){
    })
    dat
})
 

setClass("CSVtoGrangesResource", contains="AnnotationHubResource")

setMethod(".get1", "CSVtoGrangesResource",
   function(x, ...)
{
    .require("GenomicRanges")
    er <- cache(.hub(x))
    dat <- read.csv(er, header=TRUE, stringsAsFactors=FALSE)
    dat <- dat[,!(names(dat) %in% "width")]
    makeGRangesFromDataFrame(dat, keep.extra.columns=TRUE)
})

setClass("ExpressionSetResource", contains="RdaResource")

setMethod(".get1", "ExpressionSetResource",
    function(x, ...)
{
    .require("Biobase")
    callNextMethod(x, ...)
})

setClass("EpichmmModelsResource", contains="AnnotationHubResource")

setMethod(".get1", "EpichmmModelsResource",
    function(x, ...)
{
    .require("rtracklayer")
    yy <- .hub(x)
    gr <- rtracklayer::import(cache(yy), format="bed", genome="hg19")
    gr <- .mapAbbr2FullName(gr)
    .tidyGRanges(x, gr)
 
})

## helper function which changes 'chr10:100011323-100011459<-1' to gr!
.makeGrFromCharacterString <- function(data) {
    nms = sub("<-*1", "", rownames(data))  
    lst = strsplit(nms, "[:-]")                 
    v = function(x, i) vapply(x, "[[", "character", i)
    gr = GenomicRanges::GRanges(v(lst, 1), 
       IRanges::IRanges(as.integer(v(lst, 2)), as.integer(v(lst, 3))))
    mcols(gr) = data[,1:2]
    gr
}

## this data is got from :
## chromhmmSegmentations/ChmmModels/coreMarks/jointModel/final/labelmap_15_coreMarks.tab
## chromhmmSegmentations/ChmmModels/coreMarks/jointModel/final/colormap_15_coreMarks.tab
## the bed file has abbr - and these 3 columns are helpful for collaborators.
.mapAbbr2FullName <- function(gr) {
    map <- as.data.frame(matrix(c(
        "1_TssA", "Active TSS", "Red", rgb(255,0,0, maxColorValue=255),
        "2_TssAFlnk", "Flanking Active TSS", "Orange Red", rgb(255,69,0, 
             maxColorValue=255),
        "3_TxFlnk", "Transcr. at gene 5' and 3'", "LimeGreen", rgb(50,205,50, 
             maxColorValue=255),
        "4_Tx", "Strong transcription", "Green", rgb(0,128,0, 
             maxColorValue=255),
        "5_TxWk", "Weak transcription", "DarkGreen", rgb(0,100,0, 
             maxColorValue=255),
        "6_EnhG", "Genic enhancers", "GreenYellow", rgb(194,225,5, 
             maxColorValue=255),
        "7_Enh", "Enhancers", "Yellow", rgb(255,255,0, 
             maxColorValue=255),
        "8_ZNF/Rpts", "ZNF genes & repeats", "Medium Aquamarine", 
             rgb(102,205,170, maxColorValue=255),
        "9_Het", "Heterochromatin", "PaleTurquoise", rgb(138,145,208, 
             maxColorValue=255),
        "10_TssBiv", "Bivalent/Poised TSS", "IndianRed", 
             rgb(205,92,92, maxColorValue=255),
        "11_BivFlnk", "Flanking Bivalent TSS/Enh", "DarkSalmon", 
             rgb(233,150,122, maxColorValue=255),
        "12_EnhBiv", "Bivalent Enhancer", "DarkKhaki", 
             rgb(189,183,107, maxColorValue=255),
        "13_ReprPC", "Repressed PolyComb", "Silver", 
             rgb(128,128,128, maxColorValue=255),
        "14_ReprPCWk", "Weak Repressed PolyComb", "Gainsboro", 
             rgb(192,192,192, maxColorValue=255),
        "15_Quies", "Quiescent/Low", "White", rgb(255,255,255, maxColorValue=255)), 
             byrow=TRUE, nrow=15), stringsAsFactors=FALSE)
    colnames(map) <- c("abbr", "name", "color_name", "color_code")

    ##perform the mapping
    toMatch <- mcols(gr)$name
    newdf <- map[match(toMatch, map$abbr),]
    mcols(gr) <- newdf
    gr
}
