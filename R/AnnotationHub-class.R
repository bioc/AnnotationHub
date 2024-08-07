### =========================================================================
### AnnotationHub objects
### -------------------------------------------------------------------------
###

setClass("AnnotationHub", contains="Hub")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

## Add code to check : https://annotationhub.bioconductor.org/metadata/highest_id
## And if not, delete the DB so it will be re-downloaded...
AnnotationHub <-
    function(..., hub=getAnnotationHubOption("URL"),
             cache=getAnnotationHubOption("CACHE"),
             proxy=getAnnotationHubOption("PROXY"),
             localHub=getAnnotationHubOption("LOCAL"),
             ask=getAnnotationHubOption("ASK"))
{

    if ((cache == R_user_dir("AnnotationHub", which="cache")) && (Sys.getenv("ANNOTATION_HUB_CACHE")=="")){
        olddefault = rappdirs::user_cache_dir(appname="AnnotationHub")
        if (dir.exists(olddefault) && (length(list.files(olddefault)) != 0)){
            stop(msg=paste0("DEFUNCT: As of AnnotationHub (>2.23.2), default caching location has changed.\n",
                 "  Problematic cache: ", path.expand(olddefault), "\n",
                 "  See https://bioconductor.org/packages/devel/bioc/vignettes/AnnotationHub/inst/doc/TroubleshootingTheHubs.html#default-caching-location-update\n"))
            cache = olddefault
        }
    }
    if (!is.null(proxy)) {
        message("Assuming valid proxy connection through '",
                ifelse(is(proxy,"request"),
                       paste(unlist(proxy), collapse=":"),
                       proxy),
                "'",
                "\n If you experience connection issues consider ",
                "using 'localHub=TRUE'")
    } else if (!localHub) {
        connect <- suppressWarnings(tryCatch({
            readBin(hub, n=1L, what="raw")
            TRUE
        }, error = function(...){
            FALSE
        }))
        if (!connect){
            message("Cannot connect to AnnotationHub server, using 'localHub=TRUE' instead")
            localHub <- FALSE
        }
    }
    if (localHub) {
        message("Using 'localHub=TRUE'\n",
                "  If offline, please also see BiocManager vignette section on offline use")
    }
    .Hub("AnnotationHub", hub, cache, proxy, localHub, ask, ...)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### cache method
###

setMethod("cache", "AnnotationHub",
    function(x, ..., force=FALSE, verbose=FALSE)
{
    callNextMethod(
        x,
        cache.root="AnnotationHub",
        cache.fun=setAnnotationHubOption,
        proxy=getAnnotationHubOption("PROXY"),
        max.downloads=getAnnotationHubOption("MAX_DOWNLOADS"),
        force=force,
        verbose=verbose
    )
})
