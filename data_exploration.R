# load data

#read in required packages
require(readxl)
require(tidyverse)
library(sf)
library(mapview)
library(ggplot2)

#add back in arial font
library(showtext)
font_add("Arial", "/Library/Fonts/Arial.ttf")  # Use the actual file path
showtext_auto()

full_dataset <- read.csv("~/Desktop/doctorate/hfi_threshold/full_dataset_w_new_hfp.csv")
