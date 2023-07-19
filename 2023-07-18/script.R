####################################################
##### Load the libraries

library(data.table)
library(ggplot2)
library(tidytuesdayR)
library(ggridges)
library(hrbrthemes)
library(patchwork)
library(rcartocolor)
library(ggtext)
library(cowplot)

###################################################
##### Get the data

tuesdata <- tidytuesdayR::tt_load('2023-07-18')
detectors <- tuesdata$detectors
rm(tuesdata)


setDT(detectors)

# variable 	    class 	    description
# kind 	        character 	Whether the essay was written by a "Human" or "AI".
# .pred_AI 	    double 	    The class probability from the GPT detector that the inputted text was written by AI.
# .pred_class 	character 	The uncalibrated class prediction, encoded as if_else(.pred_AI > .5, "AI", "Human")
# detector 	    character 	The name of the detector used to generate the predictions.
# native 	    character 	For essays written by humans, whether the essay was written by a native English writer or not. These categorizations are coarse; values of "Yes" may actually be written by people who do not write with English natively. NA indicates that the text was not written by a human.
# name 	        character 	A label for the experiment that the predictions were generated from.
# model 	    character 	For essays that were written by AI, the name of the model that generated the essay.
# document_id 	double 	    A unique identifier for the supplied essay. Some essays were supplied to multiple detectors. Note that some essays are AI-revised derivatives of others.
# prompt 	    character 	For essays that were written by AI, a descriptor for the form of "prompt engineering" passed to the model.

levelsDetectors <- detectors[order(detectors$sumPredByDetector),unique(detector)]
levelsName <- detectors[order(detectors$sumPredByName),unique(name)]

detectors[,pctRightByDetectorKind := sum(.pred_class == kind)/.N, by=.(detector, kind)]
detectors[,pctRightByKind := sum(.pred_class == kind)/.N, by=.(kind)]

nbKinds <- table(detectors[detector=="ZeroGPT"]$kind)
labelKinds <- paste(nbKinds, "predicted", names(nbKinds), "models")
names(labelKinds) <- names(nbKinds)

p1 <- ggplot(detectors, aes(y="All detectors", x=.pred_AI, fill=pctRightByKind)) 
p2 <- ggplot(detectors, aes(y=factor(detector, levels=levelsDetectors), x=.pred_AI, fill=pctRightByDetectorKind))

fct_add_template <- function(plot){
    plot +   
        geom_density_ridges(
            stat = "binline", bins = 10, scale = 0.95,
            draw_baseline = FALSE
        ) +
        facet_grid(~kind, scales = "free_x", labeller=labeller(kind=labelKinds)) +
        scale_fill_gradient2(low="#762A83", mid="#F7F7F7", high = "#1B7837", midpoint = 0.5, limits=c(0,1),
                             labels = scales::label_percent()) +
        hrbrthemes::theme_ipsum() +
        scale_x_continuous(limits = c(-0.1, 1.1), breaks = c(0, 0.5, 1), labels=c("<span>&#8592; Human</span>", "", "<span>AI &#8594;</span>")) +
        scale_y_discrete(expand = c(0, 0)) +
        labs(title = NULL,
             x = "Prediction",
             y = NULL,
             fill = "Well detected") +
        theme(axis.text.y = element_text(vjust = .05),
              axis.text.x = element_markdown(),
              axis.title.x = element_text(hjust=0.5, size = 12),
              plot.title = element_text(size=14, face="bold"),
              plot.margin = unit(c(0.1,0.1,0.1,0.1), units="cm"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.key.height = unit(2, 'cm'))
}

p1full <- fct_add_template(p1) +
    labs(title=NULL) +
    theme(axis.text.y = element_text(face = "bold"),
          strip.text = element_text(hjust=0.5, face="bold"),
          plot.background = element_rect(fill="#eaeac8", linewidth = 0))

p2full <- fct_add_template(p2) +
    labs(title="By detector") +
    theme(strip.text = element_blank(),
          plot.margin = unit(c(0.5,0.1,0.1,0.1), units = "cm"))

plegend <- cowplot::get_legend(p1full + theme(legend.box.margin = margin(l = 2, unit="cm")))
    

#### Assemple the 3 plots
design <- "
  1111113
  2222223
  2222223
  2222223
  2222223
  2222223
  2222223
  2222223
"
p1full + theme(legend.position = "none") + p2full + theme(legend.position = "none") + 
    plegend + #theme(plot.margin = unit(c(0)))+
    plot_layout(design=design, guides = "collect") +
    plot_annotation(
        title = 'Does AI models detect correctly AI vs. Human models ?',
        caption = "Source: GPT Detectors Are Biased Against Non-Native English Writers.\nWeixin Liang, Mert Yuksekgonul, Yining Mao, Eric Wu, James Zou. arXiv: 2304.02819",
        theme=theme(plot.title = element_text(size = 20, face="bold"))
    )

