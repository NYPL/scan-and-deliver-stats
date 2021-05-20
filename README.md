
# Scan and Deliver statistics compilation

This is the code which compiles/processes the Scan and Deliver
raw stats into a series of data tables which are consumed by,
among other things, the NYPL research dashboard.

### software dependencies

Assumes a fairly recent version of R, and the following R packages
(as of 2021-05-08)

- `colorout`
- `data.table` (version 1.14.1 or above)
- `magrittr`
- `stringr`
- `lubridate`
- `ggplot2`
- `assertr`
- `libbib` (any version 1.6.2 or above)

Developed on Debian GNU/Linux 10

### data dependencies

There are two data dependencies:

The first is the raw Scan and Deliver data.
The link to the google spreadsheet that contains the raw data
can [be found here.]( https://lair.nypl.org/-/departments/library-sites-and-services/research-libraries/scan-and-deliver-staff-resources)
Use the link labeled "Sierra EDD Metrics Report".
Export that as a TSV and place it in the `data` folder.
Make sure you name this file
`lair-scan-and-deliver-yYYY-MM-DD.dat` where `YYYY-MM-DD` is the
ISO 8601 date of the export.


The second is the compiled research dataset.
The project that generates this data
[can be found here.](https://github.com/NYPL/sierra-shadow-dataset)
The compiled file can be accessd on google drive. Just ask for a link!

Make sure you modify the global variable `SHADOW_DATA_LOCATION` in
`compile-scan-and-deliver.R` to the directory that contains the
research data set. The filename of the data set should be
something like `sierra-research-healed-joined-YYYY-MM-DD.dat.gz`
where `YYYY-MM-DD` is an ISO 8601 date.

### how to run it

Just run `./compile-scan-and-deliver.R`

Make sure...
- `compile-scan-and-deliver.R` has execute permission (`chmod +x ./compile-scan-and-deliver.R`)
- your current working directory is the one that contains `compile-scan-and-deliver`

