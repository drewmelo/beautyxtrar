#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# No arquivo custom_theme.R

library(ggplot2)

# Tema principal
theme_xtra <- function (base_size = 12, base_family = "Roboto Condensed") {
  if (requireNamespace("extrafont", quietly = TRUE)) {
    extrafont::loadfonts(device = "win", quiet = TRUE)
  }
  half_line <- base_size/2
  theme(
    line = element_line(color = "black", linewidth = .5,
                        linetype = 1, lineend = "butt"),
    rect = element_rect(fill = "white", color = "black",
                        linewidth = .5, linetype = 1),
    text = element_text(family = base_family, face = "plain",
                        color = "black", size = base_size,
                        lineheight = .9, hjust = .5, vjust = .5,
                        angle = 0, margin = margin(), debug = FALSE),
    axis.line = NULL,
    axis.line.x = element_line(color = "gray70", linewidth = 0.8),
    axis.line.y = element_blank(),
    axis.text = element_text(size = base_size),
    axis.text.x = element_text(margin = margin(t = .8 * half_line/2),
                               vjust = 1, color = "gray30"),
    axis.text.x.top = element_text(margin = margin(b = .8 * half_line/2),
                                   vjust = 0),
    axis.text.y = element_text(margin = margin(r = .8 * half_line/2),
                               hjust = 1, colour = "gray30"),
    axis.text.y.right = element_text(margin = margin(l = .8 * half_line/2),
                                     hjust = 0),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color = "gray30", linewidth = .7),
    axis.ticks.length = unit(half_line / 1.5, "pt"),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.title.x = element_text(margin = margin(t = half_line),
                                vjust = 1, size = base_size * 1.11,
                                face = "plain", color = "gray40"),
    axis.title.x.top = element_text(margin = margin(b = half_line),
                                    vjust = 0),
    axis.title.y = element_text(angle = 90, vjust = 1,
                                margin = margin(r = half_line),
                                size = base_size * 1.11, color = "gray40"),
    axis.title.y.right = element_text(angle = -90, vjust = 0,
                                      margin = margin(l = half_line)),
    legend.background = element_rect(color = NA),
    legend.spacing = unit(.4, "cm"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.margin = margin(.2, .2, .2, .2, "cm"),
    legend.key = element_rect(fill = "white", color = "white"),
    legend.key.size = unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(size = rel(1), color = "gray30"),
    legend.text.align = NULL,
    legend.title = element_text(hjust = 0, size = rel(1), color = "gray40"),
    legend.title.align = NULL,
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    legend.box.margin = margin(0, 0, 0, 0, "cm"),
    legend.box.background = element_blank(),
    legend.box.spacing = unit(.4, "cm"),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = .4),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(base_size, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,
    strip.background = element_blank(),
    strip.text = element_text(size = base_size, color = "gray30", face = "plain", hjust = 0.5),
    strip.text.x = element_text(margin = margin(t = half_line,
                                                b = half_line)),
    strip.text.y = element_text(angle = -90,
                                margin = margin(l = half_line,
                                                r = half_line)),
    strip.text.y.left = element_text(angle = 90),
    strip.placement = "inside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.switch.pad.grid = unit(0.1, "cm"),
    strip.switch.pad.wrap = unit(0.1, "cm"),
    plot.background = element_rect(color = NA),
    plot.title = element_text(size = base_size * 1.8, hjust = .5,
                              vjust = 1, face = "bold",
                              margin = margin(b = half_line * 1.2)),
    plot.title.position = "panel",
    plot.subtitle = element_text(size = base_size * 1.3,
                                 hjust = .5, vjust = 1,
                                 margin = margin(b = half_line * .9)),
    plot.caption = element_text(size = rel(0.9), hjust = .5, vjust = 1,
                                margin = margin(t = half_line * .9)),
    plot.caption.position = "panel",
    plot.tag = element_text(size = rel(1.2), hjust = .5, vjust = .5),
    plot.tag.position = "topleft",
    plot.margin = margin(rep(base_size, 4)),
    complete = TRUE
  ) %+replace% theme(
    legend.position = 'top',
    legend.margin = margin(b = -8)
  )
}

# Tema principal - dark
dark_theme_xtra <- function (base_size = 12, base_family = "Roboto Condensed") {
  if (requireNamespace("extrafont", quietly = TRUE)) {
    extrafont::loadfonts(device = "win", quiet = TRUE)
  }
  half_line <- base_size/2
  theme(
    line = element_line(color = "#DDE6ED", linewidth = .5,
                        linetype = 1, lineend = "butt"),
    rect = element_rect(fill = "#27374D", color = "#DDE6ED",
                        linewidth = .5, linetype = 1),
    text = element_text(family = base_family, face = "plain",
                        color = "#DDE6ED", size = base_size,
                        lineheight = .9, hjust = .5, vjust = .5,
                        angle = 0, margin = margin(), debug = FALSE),
    axis.line = element_blank(),
    axis.line.x = NULL,
    axis.line.y = NULL,
    axis.text = element_text(size = base_size, color = "#DDE6ED"),
    axis.text.x = element_text(margin = margin(t = .8 * half_line/2),
                               vjust = 1),
    axis.text.x.top = element_text(margin = margin(b = .8 * half_line/2),
                                   vjust = 0),
    axis.text.y = element_text(margin = margin(r = .8 * half_line/2),
                               hjust = 1),
    axis.text.y.right = element_text(margin = margin(l = .8 * half_line/2),
                                     hjust = 0),
    axis.ticks = element_line(color = "#DDE6ED", linewidth = .7),
    axis.ticks.length = unit(half_line / 1.5, "pt"),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.title.x = element_text(margin = margin(t = half_line),
                                vjust = 1, size = base_size * 1.11,
                                face = "plain", color = "#9DB2BF"),
    axis.title.x.top = element_text(margin = margin(b = half_line),
                                    vjust = 0),
    axis.title.y = element_text(angle = 90, vjust = 1,
                                margin = margin(r = half_line),
                                size = base_size * 1.11, color = "#9DB2BF"),
    axis.title.y.right = element_text(angle = -90, vjust = 0,
                                      margin = margin(l = half_line)),
    legend.background = element_rect(color = NA),
    legend.spacing = unit(.4, "cm"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.margin = margin(.2, .2, .2, .2, "cm"),
    legend.key = element_rect(fill = "#27374D", color = "#27374D"),
    legend.key.size = unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(size = rel(1)),
    legend.text.align = NULL,
    legend.title = element_text(hjust = 0, size = rel(1)),
    legend.title.align = NULL,
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    legend.box.margin = margin(0, 0, 0, 0, "cm"),
    legend.box.background = element_blank(),
    legend.box.spacing = unit(.4, "cm"),
    panel.background = element_blank(),
    panel.border = element_rect(color = "#DDE6ED",
                                fill = NA, linewidth = .7),
    panel.grid.major = element_line(color = "#526D82", linewidth = .5),
    panel.grid.minor = element_line(color = "#526D82", linewidth = .25,
                                    linetype = "dashed"),
    panel.spacing = unit(base_size, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,
    strip.background = element_rect(fill = "#27374D", color = "#9DB2BF"),
    strip.text = element_text(color = "#DDE6ED", size = base_size),
    strip.text.x = element_text(margin = margin(t = half_line,
                                                b = half_line)),
    strip.text.y = element_text(angle = -90,
                                margin = margin(l = half_line,
                                                r = half_line)),
    strip.text.y.left = element_text(angle = 90),
    strip.placement = "inside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.switch.pad.grid = unit(0.1, "cm"),
    strip.switch.pad.wrap = unit(0.1, "cm"),
    plot.background = element_rect(color = NA),
    plot.title = element_text(size = base_size * 1.8, hjust = .5,
                              vjust = 1, face = "bold",
                              margin = margin(b = half_line * 1.2)),
    plot.title.position = "panel",
    plot.subtitle = element_text(size = base_size * 1.3,
                                 hjust = .5, vjust = 1,
                                 margin = margin(b = half_line * .9)),
    plot.caption = element_text(size = rel(0.9), hjust = 0, vjust = 1,
                                margin = margin(t = half_line * .9)),
    plot.caption.position = "panel",
    plot.tag = element_text(size = rel(1.2), hjust = .5, vjust = .5),
    plot.tag.position = "topleft",
    plot.margin = margin(rep(base_size, 4)),
    complete = TRUE
  ) %+replace% theme(
    legend.position = 'top',
    legend.margin = margin(b = -8)
  )
}

# Tema academico
theme_academic <- function (base_size = 12, base_family = "Times New Roman") {
  if (requireNamespace("extrafont", quietly = TRUE)) {
    extrafont::loadfonts(device = "win", quiet = TRUE)
  }
  half_line <- base_size/2
  theme(
    line = element_line(color = "black", size = .7, linetype = 1, lineend = "butt"),
    rect = element_rect(fill = "white", color = "black", size = 0.5, linetype = 1),
    text = element_text(
      family = base_family,
      face = "plain",
      color = "gray30",
      size = base_size,
      lineheight = 0.9,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      margin = margin(),
      debug = FALSE
    ),  # Adicionado
    axis.text = element_text(size = base_size, color = "gray30"),
    axis.ticks = element_line(color = "gray70", linewidth = 0.5),
    axis.ticks.length = unit(5, "pt"),
    axis.title = element_text(size = base_size * 1.11, color = "gray40"),
    legend.background = element_rect(color = NA),
    legend.spacing = unit(0.4, "cm"),
    legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
    legend.key = element_rect(fill = "white", color = "white"),
    legend.text = element_text(size = rel(1), color = "gray30"),
    legend.title = element_text(hjust = 0, size = rel(1), color = "gray40"),
    panel.background = element_blank(),
    panel.border = element_rect(color = "gray30", fill = NA, size = 0.7),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(base_size, "pt"),
    strip.background = element_rect(fill = "white", color = "gray30"),
    strip.text = element_text(color = "black", size = base_size),
    plot.background = element_rect(color = NA),
    plot.title = element_text(size = base_size * 1.8, hjust = 0, vjust = 1, face = "plain", margin = margin(b = half_line * 1.2),
                              colour = 'black'),
    plot.subtitle = element_text(size = base_size * 1.3, hjust = 0, vjust = 2, margin = margin(b = half_line * 0.9,),
                                 colour = 'gray20'),
    plot.caption = element_text(size = rel(0.9), hjust = .5, vjust = 1, margin = margin(t = half_line * 0.9)),
    plot.tag = element_text(size = rel(1.2), hjust = 0.5, vjust = 0.5),
    plot.margin = margin(rep(base_size, 4)),
    complete = TRUE
  ) %+replace% theme(
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line.x = element_line(color = "gray70", linewidth = 0.4),
    axis.line.y = element_line(color = "gray80", linewidth = 0.4),
    axis.line.x.top = NULL,  # Remove a linha superior do eixo x
    axis.line.y.right = NULL,  # Remove a linha direita do eixo y
    legend.position = 'top',
    legend.margin = margin(b = -8)
  )
}

#' Tema customizado para gráficos
#' @import extrafont
#'
#' @param base_size Tamanho base do texto.
#' @param base_family Família da fonte base.
#' @return
#' @export
#'
#'
