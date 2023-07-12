####################################################
##### Load the libraries

library(data.table)
library(ggplot2)
library(tidytuesdayR)
library(ggridges)
library(hrbrthemes)
library(patchwork)

###################################################
##### Get the data

tuesdata <- tidytuesdayR::tt_load('2023-07-11')
tuesdata <- tidytuesdayR::tt_load(2023, week = 28)

global_temps <- tuesdata$global_temps
nh_temps <- tuesdata$nh_temps
sh_temps <- tuesdata$sh_temps
zonann_temps <- tuesdata$zonann_temps

###################################################
##### Format the data
setDT(global_temps)
setDT(nh_temps)
setDT(sh_temps)

# Keeping only meteorological seasons to performs comparisons accross hemispheres

global_temps <- global_temps[,.(Year,DJF, MAM, JJA, SON)]
global_temps[,Hemisphere:="Global"]

nh_temps <- nh_temps[,.(Year,DJF, MAM, JJA, SON)]
nh_temps[,Hemisphere:="North"]

sh_temps <- sh_temps[,.(Year,DJF, MAM, JJA, SON)]
sh_temps[,Hemisphere:="South"]

all_temps <- rbindlist(
    list(global_temps,
         nh_temps,
         sh_temps)
)

all_temps_long <- data.table::melt(all_temps,
                                   id.vars=c("Year", "Hemisphere"),
                                   value.name = "Temp",
                                   variable.name = "Season")
                                   
all_temps_long[,SeasonTitle := ""]
all_temps_long[Season=="SON",SeasonTitle := "September, October, November"]
all_temps_long[Season=="JJA",SeasonTitle := "June, July & August"]
all_temps_long[Season=="MAM",SeasonTitle := "March, April & May"]
all_temps_long[Season=="DJF",SeasonTitle := "December, January & February"]

all_temps_long[,SeasonTitle := factor(SeasonTitle, 
                                     levels=c("December, January & February", 
                                              "March, April & May", 
                                              "June, July & August", 
                                              "September, October, November"))]
###################################################
##### Plot the data by meteorological seasons

paletteHemispheres <- rcartocolor::carto_pal(3, "Vivid")
names(paletteHemispheres) <- c("South", "North", "Global")


ggplot(all_temps_long, 
       aes(x=Year, y=Temp, colour=Hemisphere)) +
    annotate("rect", xmin=1951, xmax=1980, ymin=-Inf, ymax=Inf, alpha=0.3, fill="#AAAAAA", colour=NA) +
    annotate("text", x=1951, y=-.8, label="Reference periode for the mean", size=2.9, hjust=0, colour="#AAAAAA") +
    geom_hline(yintercept = 0, col="#444444") +
    geom_line(linewidth=1) + 
    xlim(1950,2023) +
    scale_y_continuous(labels = ~ ifelse(.x>0, paste0("-", .x, "°C"), paste0(.x, "°C")),
                       limits=c(-0.8,max(all_temps_long[,Temp], na.rm=T))) +
    scale_colour_manual(values=paletteHemispheres) +
    facet_wrap2(vars(SeasonTitle), nrow = 2, strip=strip) +
    labs(x="Year",
         title="Evolution of temperatures by meteorological season & by hemisphere",
         y="Deviations from the corresponding 1951-1980 mean",
         caption="Source: GISTEMP Team, 2023: GISS Surface Temperature Analysis (GISTEMP), version 4. NASA Goddard Institute for Space Studies.") +
    hrbrthemes::theme_ipsum(
        base_size=10.5, 
        plot_title_size = 15, 
        subtitle_size = 10, 
        caption_size = 8,
        plot_margin = margin(5, 5, 5, 5)) +
    theme(plot.caption = element_text(color="#888888"),
          axis.title.y = element_text(hjust=0.5),
          panel.background =  element_rect(fill="#FFFFFF", colour="#FFFFFF"),
          strip.text = element_text(size=12, colour="#000000"),
          legend.position = "top",
          legend.box.margin = margin(0,0,0,0),
          legend.margin = margin(0,0,0,0),
          legend.key.height = unit(2,'pt'),
          panel.grid.major.y = element_line(colour="#AAAAAA", linewidth=0.8))

###################################################
##### Plot the data by lattitude

setDT(zonann_temps)
zonann_temps_long <- data.table::melt(
    zonann_temps[,.(Year, Glob, `64N-90N`, `44N-64N`, `24N-44N`, `EQU-24N`, 
                    `24S-EQU`, `44S-24S`, `64S-44S`, `90S-64S`)],
    id.vars=c("Year", "Glob"),
    variable.name = "Lattitude",
    value.name = "Temp"
)
paletteLattitude <-
    colorRampPalette(c(paletteHemispheres[1],paletteHemispheres[3],paletteHemispheres[2]))(nlevels(zonann_temps_long$Lattitude))
names(paletteLattitude) <- levels(zonann_temps_long$Lattitude)

ggplot(zonann_temps_long, 
       aes(x=Year, y=Temp, colour=Lattitude)) +
    annotate("rect", xmin=1951, xmax=1980, ymin=-Inf, ymax=Inf, alpha=0.3, fill="#AAAAAA", colour=NA) +
    annotate("text", x=1951, y=-1.5, label="Reference periode for the mean", size=2.9, hjust=0, vjust=1, colour="#AAAAAA") +
    geom_line() +
    scale_colour_manual(values=paletteLattitude) +
    geom_hline(yintercept = 0, col="#444444") +
    geom_line(linewidth=1) + 
    scale_y_continuous(labels = ~ ifelse(.x>0, paste0("-", .x, "°C"), paste0(.x, "°C")),
                       limits=c(-1.5,3.5)) +
    xlim(1950,2023) +
    labs(x="Year",
         title="Evolution of temperatures by lattitude",
         y="Deviations from the corresponding 1951-1980 mean",
         caption="Source: GISTEMP Team, 2023: GISS Surface Temperature Analysis (GISTEMP), version 4. NASA Goddard Institute for Space Studies.") +
    hrbrthemes::theme_ipsum(
        base_size=10.5, 
        plot_title_size = 15, 
        subtitle_size = 10, 
        caption_size = 8,
        plot_margin = margin(5, 5, 5, 5)) +
    theme(plot.caption = element_text(color="#888888"),
          axis.title.y = element_text(hjust=0.5),
          panel.background =  element_rect(fill="#FFFFFF", colour="#FFFFFF"),
          strip.text = element_text(size=12, colour="#000000"),
          legend.position = "right",
          legend.box.margin = margin(0,0,0,0),
          legend.margin = margin(0,0,0,0),
          legend.key.height = unit(0.5,'cm'),
          panel.grid.major.y = element_line(colour="#AAAAAA", linewidth=0.8)) +
    guides(colour = guide_legend(override.aes=list(linewidth=3,linetype=1)))



