## Setting up clustering methods

library(ggdendro)
library(proxy)
library(stats)
library(DescTools)

vote_dist <- function(c1, c2, min_overlap = 10) {
  mask <- !is.na(c1) & !is.na(c2)
  if (sum(mask) < min_overlap) {
    return(1) # too few votes (one year should have at least 10 RES regardless)
  }
  # simple 1 – pct_agreement:
  1 - mean(c1[mask] == c2[mask])
}

get_dist_mat <- function(cvt){ # Getting dist. matricies. Takes a bit for larger datasets but idk a more efficient fn than proxy::dist
  c_vt <- t(cvt) |>
    as.data.frame() |>
    mutate(across(everything(), ~ factor(.)))
  rownames(c_vt) <- colnames(cvt)
  na_rows <- apply(c_vt, 1, \(x) sum(!is.na(x)))
  c_vt <- c_vt[na_rows != 0,]
  d_mt <- proxy::dist(x = c_vt,
                      method = function(c1, c2) vote_dist(c1, c2, min_overlap = 10))
  n_na <- which(rowSums(!is.na(as.matrix(d_mt))) > 1) 
  as.dist(as.matrix(d_mt)[n_na, n_na])
}

hclustering <- function(dist_mat, groups){ # Separate from above so I can export the GA dist. matricies (and it won't take an hour to run)
  hcst <- hclust(dist_mat, method = "ward.D2")
  csts <- cutree(hcst, k = groups)
  data.frame(country = names(csts), cluster = csts) |> arrange(cluster)
}

merger <- function(to_merge, begin, end) { # Merges w/ mgwreg
  mgwreg |> filter(between(year, begin, end)) |>
    group_by(country_text_id) |>
    summarize(v2x_polyarchy = mean(v2x_polyarchy, na.rm = TRUE),
              v2x_regime_amb = round(mean(v2x_regime_amb, na.rm = TRUE)),
              diff_polyarchy = mean(diff_polyarchy, na.rm = TRUE),
              backslided = any(backslided, na.rm = TRUE),
              regime = names(which.max(table(regime))),
              bve = ifelse(any(bve == "backslided"), "backslided", bve)) |>
    merge(to_merge, by.x = "country_text_id", by.y = "country") |>
    as_tibble()
}

merger_ga <- function(to_merge, begin, end) { # Merges w/ mgwreg; changes diff_polyarchy to be more sparse
  mgwreg |>
    filter(between(year, begin, end)) |>
    group_by(country_text_id) |>
    summarize(v2x_polyarchy = mean(v2x_polyarchy, na.rm = TRUE),
              v2x_regime_amb = round(mean(v2x_regime_amb, na.rm = TRUE)),
              diff_polyarchy = mean(diff_polyarchy, na.rm = TRUE),
              backslided = any(backslided, na.rm = TRUE),
              regime = names(which.max(table(regime))),
              bve = ifelse(any(bve == "backslided"), "backslided", bve)) |>
    merge(to_merge, by.x = "country_text_id", by.y = "country") |>
    mutate(diff_polyarchy = asinh(diff_polyarchy * 100)) # scaling
}

color_scheme_graph <- c("#999999", "#0072B2", "#D55E00", "#56B4E9", "#009E73", "#000000", "#CC7987", "#800080", "#F0E442")

plotting <- function(df) {
  ggplot(df, aes(x = diff_polyarchy, y = v2x_polyarchy, color = factor(cluster))) +
    geom_point(size = 2) +
    labs(color = "Cluster") +
    scale_color_manual(values = color_scheme_graph) +
    stat_ellipse(geom='polygon',
                 aes(fill=cluster),
                 alpha = 0.06,
                 show.legend = FALSE,
                 level = 0.8) +
    xlab("Average Difference in Electoral Democracy Score") +
    ylab("Average Electoral Democracy Score")
}
