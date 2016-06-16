# AusplotR

AusplotR aims to give an early preview of data that was collected using the
official [AusPlot](http://www.ausplots.org/) data collection software before 
submitting the data to TERN / Aekos.

AusplotR lets the user upload one or multiple SQLite .db files,
merges the data into several tables (resolving lookups and parent-child table relationships),
maps the data (allowing to zoom in on one or all sites), and finally lets the
user download the data in CSV format for subsequent inspection.

### Workflow

* Load application: shows empty map
* Upload one or several files: loads each file, merges data, displays transects,
  displays data tables, displays an example ordination, prepares merged data for download
* Select one or "all" sites from site picker: filters data on map, in tables,
  and in ordination plot to only the selected site

Note: the download always includes all data, independent of selection.

# For developers
AusplotR was inspired by the 
[RShiny Gallery SuperZIP example](http://shiny.rstudio.com/gallery/superzip-example.html), 
but also hopes to serve as working example for some tricky issues.

### Pivoting species observations into abundance profiles
Using dplyr, species records (one record per one individuum of one species) are 

* cast into tbl_df,
* grouped by combinations of species (fieldName) and site or transect (plotName 
  or plotName and transectId),
* tallied (sum of each species per group),
* pivoted (spread) to get one column of tallied count (n) per species (fieldName),
* ungrouped to lift invisible locks on the dataframe,
* joined with site details (pl_simple).

```{R}
    sp <- tbl_df(sr) %>%
      group_by(plotName, fieldName) %>%
      tally(sort=T) %>%
      spread(fieldName, n) %>%
      ungroup() %>%
      left_join(pl_simple, by="plotName")
    # sp[is.na(sp)] <- 0
```
Subsequent analysis might require NAs to be nulled, which assumes that observations
are comprehensive, and lack of recorded presence means true absence.

### Merging a list of lists of dataframes at the inner level
Data read from an Ausplot .db file is loaded as a list of dataframes, simplified:

```
one_data <- list(
  species_records = data.frame(...),
  basal_wedge = data.frame(...),
  etc.
)

all_data <- list(one_data=one_data, ...)
```

Merging a nested list of dataframes (`all_data`) requires to merge the second 
nesting level (e.g. `species_records`) across all first levels (e.g. `one_data`).

Using functional programming and unholy amounts of coffee, loading and merging 
multiple files was implemented in `global.R`:

* `get_one_data` reads one .db file, given a filename and datapath (local file 
  path when run as script, or temp file path when run from Rshiny fileinput), 
  into a list of dataframes. It encapsulates the business logic of the data model.
* `get_data` firstly `mapply`s `get_one_data` to each uploaded file, and returns
  the result as list of lists of dataframes - using `SIMPLIFY=F` to preserve
  the resulting nested lists.
* `combine_df` `bind_row`s the dataframes by name at the second nesting level
  across the first nesting level.
* `combine_df` is `lapply`d over all dataframe names (`species_records` etc.),
  which removes list object names.
* After restoring the list object names, the merged list of dataframes has the
  same structure as the ouput of `get_one_data`.

# License
[![license](https://i.creativecommons.org/l/by-sa/4.0/88x31.png)](http://creativecommons.org/licenses/by-sa/4.0/)
AusplotR is not affiliated with [AusPlot](http://www.ausplots.org/).

AusplotR hopes to be useful, without any guarantee or liability for any damage arising from its use.
