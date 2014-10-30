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
    .require("Rsamtools")
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

setMethod(".get1", "ChainFileResource",
    function(x, ...)
{
    chain <- cache(.hub(x))
    .require("rtracklayer")
    rtracklayer::import.chain(chain)
})

## SQLiteFile

setClass("SQLiteFileResource", contains="AnnotationHubResource")

setMethod(".get1", "SQLiteFileResource",
    function(x, ...)
{
    .require("AnnotationDbi")
    AnnotationDbi::loadDb(cache(.hub(x)))
})