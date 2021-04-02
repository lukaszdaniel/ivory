#  R : A Computer Language for Statistical Data Analysis
#  Copyright (C) 2016 and onwards the Rho Project Authors.
#
#  Rho is not part of the R project, and bugs and other issues should
#  not be reported via r-bugs or other R project channels; instead refer
#  to the Rho website.
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
#  https://www.R-project.org/Licenses/

# Builds a data frame with current allocation statistics.
.allocstats <- function() {
    stats <- data.frame(.Call(.C_allocstats, PACKAGE="base"))
    if(ncol(stats) == 3L) {
    colnames(stats) <- c("size", "alloc", "free")
    stats$live <- (stats$alloc - stats$free) * stats$size # Live bytes.
    stats$percent <- as.numeric(100 * stats$live / sum(stats$live))
    stats
    } else cat("allocation statistics are not enabled.\n");
}
