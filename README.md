# brad-stata
Stata packages developed by bbradfield for Stata 15+

## Packages
| Package Name | Version | Date     | Description                                           |
|:-------------|:-------:|:--------:|:------------------------------------------------------|
| bradsuite    | N/A     | 11/16/17 | Installs all the following brad commands at once      |
| bradbook     | 1.0.3   | 03/13/18 | Creates a cleaner codebook for export to Word         |
| bradmean_old | 1.3.8   | 02/16/18 | Computes multiple independent means in a single table |
| bradmean     | 1.7.7   | 04/20/21 | Computes multiple independent means in a single table |
| braddes      | 0.2.0   | 03/25/21 | Describe data in a single-line per variable format    |

In the short term, bradmean_old will still be available in case of issues. Bradmean (new) has been tested over 7000 times for errors.

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

I am not creative.
