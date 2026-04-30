library(Gmisc, quietly = TRUE)
library(glue)
library(htmlTable)
library(grid)
library(magrittr)


grid.newpage()
# 548 participants responded to our survey (clicked- on the link)
# 305 participants indicated whether they had a recent or not recent bedbug infestation
# 138 said they had a bedbug infestation more than 1 year ago
# 95 with less than 30% missing values 
# 
# 167 said that had a recent bedbug infestation
# 144 with less than 30% missing values 
# 138 with more than 75% completion on the IES-R, Ie with an average stress score 
# 121 with fully completed WEMWBS 
TxtGp <- getOption("boxGrobTxt", default = gpar(fontsize = 16))
Respondants <- boxGrob(glue("Respondants",
                           "n = {pop}",
                           pop = txtInt(548),
                           .sep = "\n"),
                           txt_gp=TxtGp
                       )
Ever_BB <- boxGrob(glue("Ever had a bedbug infestation",
                         "n = {pop}",
                         pop = txtInt(305),
                         .sep = "\n"),
                   txt_gp=TxtGp)
More_1y <- boxGrob(glue("Had an infestation more than 1 year ago",
                         "n = {incl}",
                         incl = txtInt(138),
                         .sep = "\n"),
                   txt_gp=TxtGp)
Less_1y <- boxGrob(glue("Had an infestation less than 1 year ago",
                      "n = {recr}",
                      recr = txtInt(167),
                      .sep = "\n"),
                   txt_gp=TxtGp)

Less_30_NA <- boxGrob(glue("Less than 30% missing values",
                         "n = {recr}",
                        recr= 144,
                         .sep = "\n"),
                      txt_gp=TxtGp)
IES_R <- boxGrob(glue("More than 75% completion of the IES-R",
                         "n = {recr}",
                         recr= 138,
                         .sep = "\n"),
                 txt_gp=TxtGp)
WEMWBS <- boxGrob(glue("Fully completed WEMWBS",
                         "n = {recr}",
                         recr= 121,
                         .sep = "\n"),
                  txt_gp=TxtGp)

grid.newpage()
vert <- spreadVertical(Respondants,
                       Ever_BB=Ever_BB,
                       Less_1y=Less_1y,
                       Less_30_NA=Less_30_NA,
                       grps=IES_R)
grps <- alignVertical(reference = vert$grps,
                      IES_R, WEMWBS) %>%
  spreadHorizontal()
vert$grps <- NULL

More_1y <- moveBox(More_1y,
                    x = .8,
                    y = coords(vert$Less_1y)$top + distance(vert$Ever_BB, 
                                                             vert$Less_1y, 
                                                             half = TRUE, 
                                                             center = TRUE))

for (i in 1:(length(vert) - 1)) {
  connectGrob(vert[[i]], vert[[i + 1]], type = "vert") %>%
    print
}
connectGrob(vert$Less_30_NA, grps[[1]], type = "N")
connectGrob(vert$Less_30_NA, grps[[2]], type = "N")
connectGrob(vert$Ever_BB, More_1y, type = "L")

vert
grps
More_1y
