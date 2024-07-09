# CCMH color palettes

# Packages
library(palettes)

# Institutional color palettes
CCMH_institutional_palettes <- palettes::pal_palette(
  PennState_main = c("#001E44",
                     "#1E407C"),
  PennState_secondary = c("#96BEE6",
                          "#262626",
                          "#A2AAAD",
                          "#314D64",
                          "#4A7729",
                          "#FFFFFF",
                          "#6A3028",
                          "#B88965",
                          "#AC8DCE",
                          "#BC204B",
                          "#009CDE",
                          "#3EA39E",
                          "#FFD100",
                          "#E98300",
                          "#F2665E",
                          "#491D70",
                          "#008755",
                          "#99CC00",
                          "#000321"),
  Big10 = c("#0088CE",
            "#000000",
            "#939598"),
  AUCCCD = c("#023222",
             "#066645",
             "#75c347"))

plot(CCMH_institutional_palettes)

usethis::use_data(CCMH_institutional_palettes,
                  overwrite = TRUE)

# Categorical color palettes
CCMH_categorical_palettes <- palettes::pal_palette(
  summer_category5 = c("#264653",
                       "#2A9D8F",
                       "#E9C46A",
                       "#F4A261",
                       "#E76F51"),
  vibrant_category5 = c("#EF476F",
                        "#FFD166",
                        "#06D6A0",
                        "#118AB2",
                        "#073B4C"),
  retro_category4 = c("#E07A5F",
                      "#3D405B",
                      "#81B29A",
                      "#F2CC8F"),
  earth_category4 = c("#283618",
                      "#606C38",
                      "#DDA15E",
                      "#BC6C25"),
  muted_category5 = c("#50514f",
                      "#f25f5c",
                      "#ffe066",
                      "#70c1b3",
                      "#247ba0"),
  lightearthed_category4 = c("#FAEF7C",
                             "#E3CCB2",
                             "#E26274",
                             "#78589F"),
  loud_category4 = c("#22235F",
                     "#7A4D9F",
                     "#EB68A0",
                     "#A8DACD"),
  rustic_category3 = c("#473D24",
                       "#B37544",
                       "#B19083"),
  autumn_category3 = c("#CB8658",
                       "#AF4B2B",
                       "#552C22"),
  tolbright_category7 = c("#4477AA",
                          "#66CCEE",
                          "#228833",
                          "#CCBB44",
                          "#EE6677",
                          "#AA3377",
                          "#BBBBBB"),
  tolvibrant_category7 = c("#0077BB",
                           "#33BBEE",
                           "#009988",
                           "#EE7733",
                           "#CC3311",
                           "#EE3377",
                           "#BBBBBB"),
  tolmuted_category10 = c("#332288",
                          "#88CCEE",
                          "#44AA99",
                          "#117733",
                          "#999933",
                          "#DDCC77",
                          "#CC6677",
                          "#882255",
                          "#AA4499",
                          "#DDDDDD"))

plot(CCMH_categorical_palettes)

usethis::use_data(CCMH_categorical_palettes,
                  overwrite = TRUE)

# Gradient color palettes
CCMH_gradient_palettes <- palettes::pal_palette(
  green_gradient4 = c("#A3B18A",
                      "#588157",
                      "#3A5A40",
                      "#344E41"),
  pink_gradient4 = c("#FFA5AB",
                     "#da627d",
                     "#a53860",
                     "#450920"),
  blue_gradient9 = c("#CAF0F8",
                     "#ADE8F4",
                     "#90E0EF",
                     "#48CAE4",
                     "#00B4D8",
                     "#0096C7",
                     "#0077B6",
                     "#023E8A",
                     "#03045E"),
  blue_gradient5 = c("#b3cde0",
                     "#6497b1",
                     "#005b96",
                     "#03396c",
                     "#011f4b"),
  blue_gradient3 = c("#CADCFC",
                     "#8AB6F9",
                     "#00246B"),
  coral_gradient5 = c("#FFC5A6",
                      "#FDAC98",
                      "#DC8E90",
                      "#A97882",
                      "#58545F"),
  teal_gradient4 = c("#B8DDDC",
                     "#82C4BE",
                     "#419E98",
                     "#007872"),
  purple_gradient5 = c("#C7AFF7",
                       "#A68CEE",
                       "#8569E4",
                       "#6B39BC",
                       "#510993"),
  brown_gradient3 = c("#473025",
                      "#AC8061",
                      "#E2C7BB"))

plot(CCMH_gradient_palettes)

usethis::use_data(CCMH_gradient_palettes,
                  overwrite = TRUE)

# Diverging color palettes
CCMH_diverging_palettes <- palettes::pal_palette(
  tol_bluered_diverging11 = c("#364B9A",
                              "#4A7BB7",
                              "#6EA6CD",
                              "#98CAE1",
                              "#C2E4EF",
                              "#EAECCC",
                              "#FEDA8B",
                              "#FDB366",
                              "#F67E4B",
                              "#DD3D2D",
                              "#A50026"),
  tol_bluered_diverging9 = c("#2166AC",
                             "#4393C3",
                             "#92C5DE",
                             "#D1E5F0",
                             "#F7F7F7",
                             "#FDDBC7",
                             "#F4A582",
                             "#D6604D",
                             "#B2182B"),
  tol_purplegreen_diverging9 = c("#762A83",
                                 "#9970AB",
                                 "#C2A5CF",
                                 "#E7D4E8",
                                 "#F7F7F7",
                                 "#D9F0D3",
                                 "#ACD39E",
                                 "#5AAE61",
                                 "#1B7837"))

plot(CCMH_diverging_palettes)

usethis::use_data(CCMH_diverging_palettes,
                  overwrite = TRUE)

# All palettes
CCMH_palettes <- c(CCMH_institutional_palettes,
                   CCMH_categorical_palettes,
                   CCMH_gradient_palettes,
                   CCMH_diverging_palettes)

plot(CCMH_palettes)

usethis::use_data(CCMH_palettes,
                  overwrite = TRUE)
