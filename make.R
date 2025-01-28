

########################################################################################################################################
# read and clean bruvs data to yield weights from lengths
########################################################################################################################################

#Beware that the 1_read_clean_data.R script will yield slightly different weights than the ones used in the manuscript. This is due to an update in the fishbase database
#If you want to use the weight values used in the manuscript - skip the "1_read_clean_data.R" step and run 2_explore_data.R and subsequent script below
#If you want to generate your own weights from scratch, then run "1_read_clean_data.R", then in 2_explore_data.R replace load("1_read_clean_pelagic_old.RData") load("1_read_clean_benthic_old.RData") load("1_read_clean_pelagic_benthic_old.RData")
#with load("1_read_clean_pelagic.RData") load("1_read_clean_benthic.RData") load("1_read_clean_pelagic_benthic.RData")

source("1_read_clean_data.R")

#input: Pelagic compilation archive 2020_05_26.xlsx, All expeditions 2021_09_16 FIN.xlsx, BenthicFL.xlsx, BenthicMaxN.xlsx, BenthicMeta.xlsx,
#output: 1_read_clean_pelagic_benthic.RData, 1_read_clean_benthic.RData, 1_read_clean_pelagic.RData, merged_fl_pelagic_meta.csv, merged_fl_benthic_meta.csv


########################################################################################################################################
# explore bruvs data
########################################################################################################################################

source("2_explore_data.R")

#input: if using your own produced weights, use 1_read_clean_pelagic.RData, 1_read_clean_benthic.RData, 1_read_clean_pelagic_benthic.RData,
#       if using the weight values used in the manuscript, use 1_read_clean_pelagic_old.RData, 1_read_clean_benthic_old.RData, 1_read_clean_pelagic_benthic_old.RData,
# PelagicBenthicMetaSum.xlsx,vlcsnap-2022-12-21-09h20m25s642.png, Sphyrna_mokarran_c.png, RAW19P_103_Isurus_oxyrinchus.jpg, vlcsnap-2021-10-22-04h02m24s551.png, Malpelo Thunnus albacares.png, juvenile jacks  and scads - Carangidae sp.png, Caranx latus b.png, PCI1_064_CaesioChrysozona.jpg, PCI1_213_LutjanusBohar_Schooling.jpg, FR07 Rigg 1 Inntian G. morhua.jpg, Sharks and cod.PNG, kyphosus.png,cross_hatch.png, Screenshot (9).png,Cockburn_Sound_CSN20_088_Galeocerdo_cuvier.png", Bermuda_BDA2307_025_Clepticus_parrae-14cm copy.png, fish_fig_2.jpeg
#output: fig_1_bruvs_screengrab.jpg, fig_2_sample.jpeg, Extended_data_length_weight.jpeg, fig_3_lat_lcd.jpeg, Extended_Data_Fig_lat_lcd_lon.jpeg


########################################################################################################################################
# make response variables
########################################################################################################################################

source("3_response_variables.R")

#input: benthicdata_150622.rds, pelagicdata_020622.rds
#if you want to reproduce the analysis with weights from the manuscript download merged_fl_pelagic_meta.csv and merged_fl_benthic_meta.csv from the Zenoto link https://zenodo.org/records/14698456 and place into 'outputs' 'pelagic or 'benthic' subfolder

#output: size_response_pelagic.txt, size_response_benthic.txt, size_response_envar_pelagic_clean.txt, size_response_envar_benthic_clean.txt, pelagic_benthic_response_envar_clean.txt



########################################################################################################################################
# modelling
########################################################################################################################################

source("4_modelling.R")

#input: pelagic_benthic_response_envar_clean.txt,
#output: fig_S2_response_new.jpeg, Extended_data_envar_range.jpeg, all_models_marg_pred.txt, All_model_bruvs_prot_logTTM.jpeg
#All_model_bruvs_prot_logTTM.jpeg (Fig 4 in manuscript)