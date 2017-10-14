#' Create a world map emphasizing one country.
#'
#' @description
#' This function creates a `ggplot2` plot showing a map of the world on the
#' right panel and an inset showing a country or countries on the left panel.
#'
#' @details
#' The maps data are obtained from the `maps` package.
#'
#' @param emphasize Character vector with country names as found in the "world"
#'   database in the `maps` package.
#' @return A ggplot object.
#' @export
#' @examples
#' \dontrun{
#' # Load the package
#' library(package = "insetmap")
#'
#' # Prepare the map
#' inset1 <- inset_world(emphasize = c("Guatemala", "Costa Rica"))
#' inset1
#'
#' # Modify a map
#'
#' # Load ggplot2 package
#' library(package = "ggplot2")
#'
#' # Modify the produced plot
#' inset2 <- inset1 +
#'   scale_fill_manual(values = c("#ffaaaa", "blue"))
#' inset2
#' }
inset_world <- function(emphasize, ...){

  # Prepare map data
  world_data <- st_as_sf(maps::map('world', plot = FALSE, fill = TRUE))


  # Get data to center
  emphasized_data <- filter(world_data, ID %in% emphasize)




  #----------------------------------------------------------------------------*
  # Build crs ----
  #----------------------------------------------------------------------------*


  centroid <- emphasized_data %>%
    summarize(ID = paste(ID, collapse = ", ")) %>%
    st_centroid() %>%
    st_coordinates() %>%
    as_tibble() %>%
    unlist()

  crs <- paste0(
    "+proj=ortho +y_0=0 +lon_0=", centroid["X"],
    " +lat_0=", centroid["Y"],
    " +ellps=WGS84 +no_defs"
  )




  #----------------------------------------------------------------------------*
  # Correct projections ----
  #----------------------------------------------------------------------------*

  world_data <- st_transform(world_data, crs = crs)

  emphasized_data <- st_transform(emphasized_data, crs = crs)




  #----------------------------------------------------------------------------*
  # Prepare bounding box for inset ----
  #----------------------------------------------------------------------------*

  # Get bounding box
  emph_bbox <- st_bbox(emphasized_data)


  # Get limits approximating ratio
  inset_coords <- emph_bbox %>%
    # Tidy bbox
    unclass %>%
    as.data.frame() %>%
    tibble::rownames_to_column("var") %>%
    set_names(c("var", "coord")) %>%
    separate(col = var, into = c("var", "p"), sep = "m") %>%
    # For each dimension
    group_by(var) %>%
    summarize(
      # Get the length of the bbox sides and grow by a proportion
      length = diff( sort( coord ) ) * 1.05,
      # And get the location
      center = mean( coord )
    ) %>%
    # Update both dimensions' length to form a square
    mutate(
      length = max(length),
      min = center - length/2,
      max = center + length/2
    ) %>%
    # Get into bbox structure
    select(var, min, max) %>%
    gather(key = dim, value = coord, min, max) %>%
    unite(col = dim, var, dim, sep = "") %>%
    spread(key = dim, value = coord) %>%
    unlist()


  # Update into bbox object
  inset_box <- emph_bbox
  inset_box[1:4] <- inset_coords[names(inset_box)]


  # Build inset box as a geometry
  inset_box <- inset_box %>%
    st_as_sfc %>%
    data_frame(
      geometry = .
    ) %>%
    st_as_sf()


  # Prepare context fitting the inset box
  inset_context <- world_data %>%
    filter(! ID %in% emphasize) %>%
    st_make_valid() %>%
    st_intersection(inset_box) %>%
    mutate(ID = "context")




  #----------------------------------------------------------------------------*
  # Update datasets ----
  #----------------------------------------------------------------------------*

  # Update world map data to include inset
  world_emph_data <- world_data %>%
    rbind(
      filter(., ID %in% emphasize) %>%
        mutate(
          ID = "keep-world"
        )
    ) %>%
    rbind( inset_context ) %>%
    mutate(
      facet = ifelse(
        test = ID %in% c(emphasize, "context"),
        yes = paste(emphasize, collapse = ", "),
        no = "World"
      ),
      facet = factor(
        facet,
        levels = c(paste(emphasize, collapse = ", "), "World"),
        ordered = TRUE
      )
    )




  #----------------------------------------------------------------------------*
  # Prepare plot ----
  #----------------------------------------------------------------------------*
  world_inset_plot <- world_emph_data %>%
    ggplot() +
    # Draw map
    geom_sf(
      aes(
        fill = ID == "keep-world",
        color = ID == "keep-world",
        alpha = ID == "context"
      ),
      show.legend = FALSE
    ) +
    # Draw inset box
    geom_sf(
      data = inset_box,
      fill = NA, color = "red", linetype = "dashed"
    ) +
    # Facet the inset
    facet_wrap( ~ facet, scales = "free" ) +
    # Customize appearance
    scale_fill_manual(values = c("grey90", "grey30")) +
    scale_color_manual(values = c("grey70", "grey30")) +
    scale_alpha_manual(values = c(1, 0.3)) +
    theme_bw()

  return(world_inset_plot)
}
