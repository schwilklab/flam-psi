## ggplot_theme.R
##
## Theme options for graphs to be sourced in. Provides four themes, two for
## publications and two for presentations. Math for font sizes might need to be
## checked because font sizes interact with width and height in ggsave call.

## Here are the sizes in cm for ggsave calls:

# sizes for 1col and 2col wide figures for publications. Change according to
# journal's specifications.
col2 <- 18.4 # cm # according to http://www.amjbot.org/site/misc/ifora.xhtml#Figures/Illustrations
col1 <- 8.9 # cm
col15 <- 13.0 # cm  For 1.5 col width
ppi <- 300 # for raster formats

## Dimensions for presentation output for full slide figures (eg LaTeX Beamer
## or powerpoint).
beamer_width <- 10 # cm
beamer_height <- 7 # cm


library(ggplot2)
library(gridExtra)
library(scales)
library(extrafont)
#font_import(pattern="Arial") # call once for local installation. Provides
#loadfonts() function
loadfonts()

# constants and overall settings
schwilkcolors <- c("#EC4E15", "#EE722E", "#D68D18", "#AF5F42", "#E3C477", 
                   "#A9B678", "#8F7955", "#4A4C4F", "#2C2A1D")
lighter_colors <- c("#F28B6B", "#F4A46E", "#E6B94C", "#CFA78D", "#F0E0A0")
schwilkcolors_extended <- c(schwilkcolors, lighter_colors)

# schwilkcolors <- c("#D68D18", "#836B43", "#A0AE6A", "#437683", "#18B0D6")
# schwilkcolors <- c("#D68D18", "#836B43", "#A0AE6A", "#362908", "#EC4E15")  # ?

## The ggplot theme for all figures.
bestfit <- geom_smooth(method="lm",se = FALSE, color = "black", linewidth=1.5)
textsize <- 10
smsize <- textsize-2
pt2mm <- 0.35146
smsize.mm <- smsize*pt2mm
fontfamily <- "Arial"

stat_sum_single <- function(fun, geom="point", ...) {
  stat_summary(fun.y=fun, geom=geom, size = 3, ...)
}

pubtheme <- theme_grey() +
  theme(axis.title.y = element_text(family=fontfamily,
                                    size = textsize, angle = 90, vjust=0.3),
        axis.title.x = element_text(family=fontfamily, size = textsize, vjust=-0.3),
        axis.ticks = element_line(colour = "black"),
        panel.background = element_rect(linewidth = 1.6, fill = NA),
        panel.border = element_rect(linewidth = 1.6, fill=NA),
        axis.text.x  = element_text(family=fontfamily, size=smsize, color="black"),
        axis.text.y  = element_text(family=fontfamily, size=smsize, color = "black"),
        ## strip.text.x = element_text(family=fontfamily, size = axissz, face="italic"),
        ## strip.text.y = element_text(family=fontfamily, size = axissz, face="italic"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_text(family=fontfamily, size=smsize),
        legend.text = element_text(family=fontfamily, size=smsize),
        legend.key = element_rect(fill="transparent"),
        legend.spacing.y = NULL,
        legend.margin=margin(c(1,1,1,1)),
        legend.key.height = unit(smsize, "pt"),
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        panel.grid.minor = element_line(colour = "grey95", size = 0.5),
        #    panel.grid.minor = element_blank(),
        #    panel.grid.major = element_blank(),
        strip.background = element_rect(fill = "grey80", colour = "grey50")      
        )

pubtheme.nogridlines <- pubtheme +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())



# presentation theme. Meant for LaTeX Beamer output with a graph height of 7
# cm. New text sizes aimed at screens rather than page:
prestxsz <- 12
pressmsz <- 10
axissz <- 8
prestheme   <- pubtheme +
  theme(axis.title.y = element_text(size = prestxsz),
        axis.title.x = element_text(size = prestxsz),
        axis.text.x  = element_text(size=axissz),
        axis.text.y  = element_text(size=axissz),
        strip.text.x = element_text(size = pressmsz),#, face="italic"),
        strip.text.y = element_text(size = pressmsz),#, face="italic"),
        #   strip.background = element_blank(),
        legend.title = element_text(size=pressmsz),
        legend.text = element_text(size=pressmsz))

prestheme.nogridlines <- prestheme +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          strip.background = element_blank())


