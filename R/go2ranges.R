
#' given a GO term, obtain a GRanges on the basis of EnsDb and GO.db
#' @import GO.db
#' @importFrom AnnotationDbi mapIds
#' @importFrom ensembldb genes
#' @param term character(1) should be GO term
#' @param ens_build character(1) either "v75" or "v79"
#' @examples
#' r = go2ranges()
#' head(r)
#' @export
go2ranges = function(term="oxidoreductase activity", ens_build="v79") {
  id = AnnotationDbi::mapIds(GO.db::GO.db, keys=term, keytype="TERM",  column="GOID")
  stopifnot(length(id)==1)
  ensids = AnnotationDbi::select(org.Hs.eg.db::org.Hs.eg.db, keytype="GO", key=id,
    columns="ENSEMBL")
  if (ens_build == "v79") g = ensembldb::genes(EnsDb.Hsapiens.v79::EnsDb.Hsapiens.v79)
  else if (ens_build == "v75") g = ensembldb::genes(EnsDb.Hsapiens.v75::EnsDb.Hsapiens.v75)
  else stop("ens_build must be v75 or v79") # 81?
  ok = intersect(na.omit(ensids$ENSEMBL), names(g))
  if (length(ok) < 1) stop("no ranges obtained")
  g[ok]
}

