for (item in main_df["Time of maximum wind gust"]) {
  print(parse_time(item, "%H:%M"))
}

new_time <- lapply(X = main_df["Time of maximum wind gust"], FUN = parse_time)
main_df["Time of maximum wind gust"] <- tibble(new_time["Time of maximum wind gust"])
################################################################
lol <- list(
  structure(
    list(
      pair = c("BoneMarrow", "Pulmonary"),
      genes = "PRR11"),
    .Names = c("pair", "genes")
  ),
  structure(
    list(
      pair = c("BoneMarrow", "Umbilical"),
      genes = "GNB2L1"),
    .Names = c("pair", "genes")),
  structure(
    list(
      pair = c("Pulmonary", "Umbilical"),
      genes = "ATP1B1"),
    .Names = c("pair", "genes")))

tibble(
  pair = map(lol, "pair"),
  genes_vec = map_chr(lol, "genes")
) %>%
  mutate(
    pair1 = map_chr(pair, 1),
    pair2 = map_chr(pair, 2)
  ) %>%
  select(pair1, pair2, genes_vec)