#' bodymap_plot
#'
#' @param df Data frame formatted using bodymap_data function.
#' @param sex String value indicating sex of image to plot against. Choices are "female" or "male"
#'
#' @return ggplot of lesion locations
#' @export
#'
#' @examples
#' df <- bodymap_df(example_patient_dataset)
#' bodymap_plot(df, "female")
bodymap_plot <- function(df, sex) {
  # Verify data frame ---------------------------------------------------------------------------
  sex <- tolower(sex)

  checkmate::assert_data_frame(df,
    all.missing = FALSE,
    min.cols = 7
  )
  checkmate::assert_names(colnames(df, do.NULL = FALSE),
    must.include = c(
      "lesion_date",
      "lookup_name",
      "site_description",
      "lesion_type", "x", "y", "label_text"
    ), .var.name = "df"
  )
  checkmate::assert_choice(sex, c("female", "male"))



  # Prepare df ----------------------------------------------------------------------------------

  df_prim <- df %>%
    dplyr::filter(lesion_type == "Primary")

  df_met <- df %>%
    dplyr::filter(lesion_type == "Metastatic")

  n_breaks_prim <- dplyr::n_distinct(df_prim$lesion_date)
  n_breaks_met <- dplyr::n_distinct(df_met$lesion_date)

  breaks_prim <- df_prim %>%
    dplyr::select(lesion_date) %>%
    dplyr::distinct() %>%
    dplyr::arrange(lesion_date) %>%
    dplyr::mutate(lesion_date = as.Date(lesion_date))

  breaks_met <- df_met %>%
    dplyr::select(lesion_date) %>%
    dplyr::distinct() %>%
    dplyr::arrange(lesion_date) %>%
    dplyr::mutate(lesion_date = as.Date(lesion_date))

  rm(df_met)
  rm(df_prim)

  path_file <-
    ifelse(
      sex == "female",
      system.file("extdata", "biorender_female.png", package = "smmartbodymap"),
      system.file("extdata", "biorender_male.png", package = "smmartbodymap")
    )
  body_map <- png::readPNG(path_file)

  # Visualization Settings ----------------------------------------------------------------------

  pt_size <- 4
  legend_title_prim <- ifelse(n_breaks_prim > 1, "Primary Sites", "Primary Site")
  legend_title_met <- dplyr::case_match(
    n_breaks_met,
    0 ~ "No Metastatic Sites",
    1 ~ "Metastatic Site",
    .default = "Metastatic Sites"
  )

  pos_jitter <- ggplot2::position_jitter(width = 0.1, height = 0, seed = 1)
  shape_vals <- c("Primary" = 23, "Metastatic" = 21)


  # Plot ----------------------------------------------------------------------------------------

  v_plot <- ggplot2::ggplot(
    data = df,
    aes(
      x = x,
      y = y,
      shape = lesion_type,
      label = stringr::str_wrap(label_text, 25)
    )
  ) +
    ggplot2::xlim(0, 100) +
    ggplot2::ylim(0, 100) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_blank()
    ) +
    ggplot2::theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    ) +
    ggplot2::annotation_raster(
      body_map,
      ymin = 0,
      xmin = 0,
      xmax = 100,
      ymax = 100
    ) +
    ggplot2::scale_shape_manual(values = shape_vals) + # set shapes by type
    ggplot2::guides(shape = "none") + # Hide legend for shapes by type
    ggplot2::geom_point(
      position = pos_jitter,
      size = pt_size,
      alpha = 0 # create invisible plot of all points for repelling labels
    ) +
    ggrepel::geom_label_repel(
      fontface = "bold",
      size = 3,
      position = pos_jitter,
      min.segment.length = .1,
      max.overlaps = Inf,
      force = 2,
      box.padding = .75
    ) +
    suppressWarnings(ggplot2::geom_point(
      data = ~ subset(., lesion_type == "Primary"),
      aes(
        prim = as.Date(lesion_date) # create ae for scale_listed function
      ),
      size = pt_size,
      shape = 23,
      color = "black"
    ), classes = "warning") +
    suppressWarnings(ggplot2::geom_point(
      data = ~ subset(., lesion_type == "Metastatic"),
      aes(
        mets = as.Date(lesion_date) # create ae for scale_listed function
      ),
      size = pt_size,
      shape = 21,
      color = "black",
      position = pos_jitter,
      alpha = .8
    ), classes = "warning") +
    ggh4x::scale_listed( # color scales settings
      list(
        viridis::scale_fill_viridis( # primary lesion settings
          guide = "legend",
          transform = "date",
          breaks = breaks_prim$lesion_date,
          name = legend_title_prim,
          discrete = FALSE,
          option = "magma",
          begin = .15,
          end = .75,
          aesthetics = "prim"
        ),
        viridis::scale_fill_viridis( # metastatic lesion settings
          guide = "legend",
          transform = "date",
          breaks = breaks_met$lesion_date,
          name = legend_title_met,
          discrete = FALSE,
          option = "turbo",
          direction = -1,
          begin = .1,
          end = .9,
          aesthetics = "mets"
        )
      ),
      replaces = c("fill", "fill")
    ) +
    ggplot2::theme( # legend settings
      legend.position = "right",
      legend.title = element_text(
        face = "bold",
        size = rel(.9)
      ),
      legend.text = element_text(size = rel(.8)),
      legend.key.height = unit(14, "pt")
    )
  v_plot
  return(v_plot)
}
