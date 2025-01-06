concept_converter <- function (data, verbose = TRUE, pb = if (verbose) openalexR:::oa_progress(length(data)) else NULL) 
{
  concept_process <- tibble::tribble(~type, ~field, "identical", 
                                     "id", "identical", "display_name", "identical", "wikidata", 
                                     "identical", "level", "identical", "description", "identical", 
                                     "image_url", "identical", "image_thumbnail_url", "identical", 
                                     "works_count", "identical", "cited_by_count", "identical", 
                                     "works_api_url", "identical", "score", "rbind_df", 
                                     "counts_by_year", "rbind_df", "ancestors", "rbind_df", 
                                     "related_concepts", "flat", "ids")
  n <- length(data)
  list_df <- vector(mode = "list", length = n)
  for (i in seq.int(n)) {
    if (verbose) 
      pb$tick()
    item <- data[[i]]
    fields <- concept_process[concept_process$field %in% 
                                names(item), ]
    sim_fields <- mapply(function(x, y) openalexR:::subs_na(item[[x]], 
                                                type = y), fields$field, fields$type, SIMPLIFY = FALSE)
    intern_fields <- NULL
    if (!is.null(item$international)) {
      intern_fields <- lapply(item$international[c("display_name", 
                                                   "description")], subs_na, type = "flat")
      names(intern_fields) <- paste(names(intern_fields), 
                                    "international", sep = "_")
    }
    list_df[[i]] <- c(sim_fields, intern_fields)
  }
  col_order <- c("id", "display_name", "display_name_international", 
                 "description", "description_international", "wikidata", 
                 "level", "ids", "image_url", "image_thumbnail_url", "ancestors", 
                 "related_concepts", "score", "works_count", 
                 "cited_by_count", "counts_by_year", "works_api_url")
  out_df <- openalexR:::rbind_oa_ls(list_df)
  out_df[, intersect(col_order, names(out_df))]
}
