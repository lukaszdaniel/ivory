#  File src/library/utils/R/windows/choose.files.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

Filters <-
structure(c(gettext("R or S files (*.R,*.q,*.ssc,*.S)", domain = "R-utils"),
            gettext("Enhanced metafiles (*.emf)", domain = "R-utils"),
            gettext("Postscript files (*.ps)", domain = "R-utils"),
            gettext("PDF files (*.pdf)", domain = "R-utils"),
            gettext("Png files (*.png)", domain = "R-utils"),
            gettext("Windows bitmap files (*.bmp)", domain = "R-utils"),
            gettext("Jpeg files (*.jpeg,*.jpg)", domain = "R-utils"),
            gettext("Text files (*.txt)", domain = "R-utils"),
            gettext("R images (*.RData,*.rda)", domain = "R-utils"),
            gettext("Zip files (*.zip)", domain = "R-utils"),
            gettext("All files (*.*)", domain = "R-utils"),

            "*.R;*.q;*.ssc;*.S", "*.emf", "*.ps", "*.pdf", "*.png", "*.bmp",
            "*.jpeg;*.jpg", "*.txt", "*.RData;*.rda", "*.zip", "*.*"),
       .Dim = c(11L, 2L),
       .Dimnames = list(c("R", "emf", "ps","pdf", "png",
                          "bmp", "jpeg", "txt", "RData", "zip", "All"),
                        NULL))

choose.files <- function(default = '', caption = gettext("Select files", domain = "R-utils"), multi = TRUE,
                         filters = Filters, index = nrow(Filters) )
    .Call(C_chooseFiles, default, caption, multi, filters, index)

choose.dir <- function(default = '', caption = gettext("Select folder", domain = "R-utils"))
    .Call(C_chooseDir, default, caption)
