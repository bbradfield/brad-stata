# brad-stata
Stata packages developed by RTI's bbradfield for Stata 15+

## Packages
| Package Name | Version | Date     | Description                                           |
|:-------------|:-------:|:--------:|:------------------------------------------------------|
| bradsuite    | N/A     | 11/16/17 | Installs all the following brad commands at once      |
| bradbook     | 1.0.3   | 03/13/18 | Creates a cleaner codebook for export to Word         |
| bradmean_old | 1.3.8   | 02/16/18 | Computes multiple independent means in a single table |
| bradmean     | 1.8.1   | 10/05/21 | Computes multiple independent means in a single table |
| braddes      | 0.2.0   | 03/25/21 | Describe data in a single-line per variable format    |

## Install
To install from Github, copy and paste the following code:
```
. net install <package>, from(https://raw.github.com/bbradfield/brad-stata/master/) replace
```
To install manually, download the .ado and .sthlp files and place them in the following directory:
```
~/ado/plus/b/
```
To update when connected to the internet on Stata, use:
```
adoupdate <package>, update
OR
adoupdate, update
```

## On Naming

BRADsuite was originally developed as a series of internal tools for RTI research and development. Its name connotes its purposes and origins for use in business research and development (BRAD).
