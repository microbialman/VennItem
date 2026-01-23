#' @import ggplot2
#' @import ggforce
#' @importFrom stringr str_pad
#' @importFrom scales hue_pal
#' @importFrom utils combn
#' @importFrom stats setNames
#' @importFrom utils globalVariables


# Required packages
# install.packages(c("ggplot2", "ggforce", "stringr"))
library(ggplot2)
library(ggforce)
library(stringr)
library(scales)

# Helper: format a character vector into n columns using monospaced alignment
format_columns <- function(items, ncol = 1, col_sep = "   ") {
  items <- as.character(items)
  if (length(items) == 0) return("")

  ncol <- max(1, as.integer(ncol))
  nrow <- ceiling(length(items) / ncol)

  # Fill a matrix by column to form columnar layout
  mat <- matrix("", nrow = nrow, ncol = ncol)
  mat[seq_along(items)] <- items

  # Pad each column to the same width so text aligns in a mono font
  col_widths <- apply(mat, 2, function(col) max(nchar(col), na.rm = TRUE))
  for (j in seq_len(ncol)) {
    mat[, j] <- str_pad(mat[, j], width = col_widths[j], side = "right")
  }

  # Collapse rows with separator between columns
  rows <- apply(mat, 1, function(r) paste0(r, collapse = col_sep))
  paste0(rows, collapse = "\n")
}

# Compute region items (e.g., "A", "A & B", "A & B & C") from named list of sets
compute_region_items <- function(sets, sort_items = FALSE) {
  stopifnot(is.list(sets), !is.null(names(sets)), all(nzchar(names(sets))))
  set_names <- names(sets)

  # Universe of items
  universe <- unique(unlist(sets, use.names = FALSE))

  # Which sets each item belongs to
  item_membership <- lapply(universe, function(x) set_names[vapply(sets, function(s) x %in% s, logical(1))])
  names(item_membership) <- universe

  # Helper to build a region key in the SAME order as set_names
  ordered_key <- function(nms) {
    ord <- order(match(nms, set_names))
    paste(nms[ord], collapse = " & ")
  }

  # Build all possible non-empty combinations as keys, preserving order
  combos <- unlist(lapply(seq_along(set_names), function(k) {
    combn(set_names, k, simplify = FALSE)
  }), recursive = FALSE)
  keys <- vapply(combos, ordered_key, character(1))
  region_items <- setNames(rep(list(character(0)), length(keys)), keys)

  # Assign each item to its ordered key
  for (it in universe) {
    present <- item_membership[[it]]
    if (length(present) > 0) {
      key <- ordered_key(present)
      region_items[[key]] <- c(region_items[[key]], it)
    }
  }


  if (isTRUE(sort_items)) {
    region_items <- lapply(region_items, function(v) sort(unique(v)))
  }


  region_items
}


# Default circle layout and label positions for 2 or 3 sets
default_layout <- function(set_names) {
  k <- length(set_names)
  if (k == 2) {
    # Circle centers and radius
    circles <- data.frame(
      set = set_names,
      x0 = c(-0.65,  0.65),
      y0 = c( 0,  0),
      r  = c( 1,  1)
    )
    # Label positions (heuristic but readable)
    label_pos <- rbind(
      data.frame(region = set_names[1], x = -1, y =  0.0),
      data.frame(region = set_names[2], x =  1, y =  0.0),
      data.frame(region = paste(set_names, collapse = " & "), x = 0.0, y = 0.0)
    )
    list(circles = circles, label_pos = label_pos)
  } else if (k == 3) {
    # Equilateral-ish triangle layout
    circles <- data.frame(
      set = set_names,
      x0 = c(-0.8,  0.8,  0.0),
      y0 = c( 0.0,  0.0,  1.1),
      r  = c( 1.2,  1.2,  1.2)
    )
    nm <- set_names
    label_pos <- rbind(
      # Singleton regions
      data.frame(region = nm[1], x = -1.5, y =  0.00),
      data.frame(region = nm[2], x =  1.5, y =  0.00),
      data.frame(region = nm[3], x =  0.00, y =  1.7),
      # Pairwise intersections (excluding the third)
      data.frame(region = paste(nm[c(1,2)], collapse = " & "), x = 0.00,  y = -0.3),
      data.frame(region = paste(nm[c(1,3)], collapse = " & "), x = -0.70, y =  0.65),
      data.frame(region = paste(nm[c(2,3)], collapse = " & "), x =  0.70, y =  0.65),
      # Triple intersection
      data.frame(region = paste(nm, collapse = " & "),          x =  0.00,  y =  0.30)
    )
    list(circles = circles, label_pos = label_pos)
  } else {
    stop("This function supports 2 or 3 sets. For 4+ sets, consider an UpSet plot (e.g., UpSetR or ComplexUpset).")
  }
}

# Compute positions for external set labels just outside each circle
compute_set_label_positions <- function(circles, set_label_angles = NULL, set_label_nudge = 0.25) {
  # circles must have columns: set, x0, y0, r
  stopifnot(all(c("set", "x0", "y0", "r") %in% names(circles)))
  k <- nrow(circles)
  sets <- circles$set

  # Default angles (degrees) for 2 or 3 sets
  default_angles <- if (k == 2) {
    # Left circle 135, Right circle 45
    c(135, 45)
  } else if (k == 3) {
    # Left 225, Right 315, Top 90
    c(225, 315, 90)
  } else {
    stop("External set labels: supported only for 2 or 3 sets.")
  }

  if (is.null(set_label_angles)) {
    set_label_angles <- default_angles
  }
  if (length(set_label_angles) != k) {
    stop(sprintf("set_label_angles must have length %d (one angle per set).", k))
  }

  # Convert degrees to radians
  theta <- set_label_angles * pi / 180
  # Place labels at radius * (1 + nudge) along angle
  rr <- circles$r * (1 + set_label_nudge)

  label_x <- circles$x0 + rr * cos(theta)
  label_y <- circles$y0 + rr * sin(theta)

  data.frame(
    set = sets,
    label = sets,
    x = label_x,
    y = label_y,
    stringsAsFactors = FALSE
  )
}


#' Create Venn Diagram with Items
#'
#' @description Draws a 2- or 3-set Venn diagram with items listed in regions.
#' @param sets Named list of character vectors.
#' @param ncol_items Number of columns for items drawn within each section.
#' @param max_items_per_region Show top n items only. Set to 0 for classic set size Venn.
#' @param fill_alpha Fill opacity.
#' @param outline_size Circle stroke size.
#' @param palette Vector of custom fill colours.
#' @param text_size Size of item text.
#' @param font_family Font of item text, monospace recommended.
#' @param title Plot title.
#' @param legend Show fill legend.
#' @param sort_items Sort items alphabetically within sections.
#' @param show_set_labels Label circles with set names.
#' @param set_label_size Size of set names.
#' @param set_label_family Font of set names.
#' @param set_label_nudge Distance of set names from circle.
#' @param set_label_angles Angle of set name from center of plot.
#' @return A ggplot object.
#' @examples
#' sets <- list(A = c("apple","banana"), B = c("banana","kiwi"), C = c("banana","kiwi","apple","pear"))
#' vennItem(sets)
#' big_sets <- list(A = c("apple","banana","durian","lychee","grapes","pear","melon"),
#' B = c("banana","kiwi","satsuma","orange","lemon","lime"),
#' C = c("banana","kiwi","apple","pear","coconut"))
#' vennItem(big_sets, ncol_items = 2)
#' @export
vennItem <- function(
    sets,
    ncol_items = 1,              # columns per region for item text
    max_items_per_region = Inf,  # cap items shown per region; will append (+N more)
    fill_alpha = 0.25,
    outline_size = 0.8,
    palette = NULL,             # vector of colors, length = #sets
    text_size = 3.5,            # ggplot2 text size
    font_family = "mono",       # monospaced font for column alignment
    title = NULL,
    legend = "none",
    sort_items = TRUE,           #sort items in each section
    show_set_labels = TRUE,
    set_label_size = text_size*1.5,
    set_label_family = font_family,
    set_label_nudge = 0.25,
    set_label_angles = NULL
) {
  stopifnot(is.list(sets), !is.null(names(sets)), all(nzchar(names(sets))))
  set_names <- names(sets)
  k <- length(set_names)
  if (!(k %in% c(2, 3))) {
    stop("This function supports only 2 or 3 sets.")
  }

  # Compute region items
  reg_items <- compute_region_items(sets, sort_items)

  # Layout
  layout <- default_layout(set_names)
  circles <- layout$circles
  if (is.null(palette)) {
    palette <- scales::hue_pal()(length(set_names))
  }
  circles$fill <- palette
  circles$colour <- palette

  # External set labels (positions just outside each circle)
  set_label_df <- compute_set_label_positions(
    circles = circles,
    set_label_angles = set_label_angles,
    set_label_nudge = set_label_nudge
  )

  # Label positions
  label_pos <- layout$label_pos

  # Build label strings per region (formatted into columns)
  regions <- label_pos$region
  label_df <- data.frame(region = regions, label = "", stringsAsFactors = FALSE)

  label_df$label <- vapply(regions, function(rk) {
    items <- reg_items[[rk]]
    if (length(items) == 0) return("")
    if (is.finite(max_items_per_region) && length(items) > max_items_per_region && max_items_per_region != 0) {
      shown <- items[seq_len(max_items_per_region)]
      extra <- length(items) - max_items_per_region
      paste0(format_columns(shown, ncol = ncol_items), "\n... (+", extra, " more)")
    }else if(max_items_per_region == 0){
      as.character(length(items))
    } else {
      format_columns(items, ncol = ncol_items)
    }
  }, character(1))

  # Combine positions with labels
  label_df <- merge(label_pos, label_df, by = "region", all.x = TRUE)

  # Plot
  p <- ggplot() +
    ggforce::geom_circle(
      data = circles,
      aes(x0 = x0, y0 = y0, r = r, fill = set, color = set),
      alpha = fill_alpha, linewidth = outline_size
    ) +
    geom_text(
      data = label_df,
      aes(x = x, y = y, label = label),
      family = font_family, size = text_size, lineheight = 0.95
    ) +
    coord_equal() +
    theme_void() +
    theme(
      legend.position = if (legend == "none") "none" else "top",
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5, margin = margin(b = 6))
    )

  # External set names in the same color as circles
  if (isTRUE(show_set_labels)) {
    p <- p +
      geom_text(
        data = set_label_df,
        aes(x = x, y = y, label = label, color = set),
        family = set_label_family,
        size = set_label_size,
        fontface = "bold"
      )
  }

  #title
  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }
  p
}
