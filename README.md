# brad-stata
Stata packages developed by bbradfield for Stata14+

## Packages
| Package Name | Version | Date     | Description                                           |
|:-------------|:-------:|:--------:|:------------------------------------------------------|
| bradsuite    | N/A     | 11/16/17 | Installs all the following brad commands at once      |
| bradbook     | 1.0.1   | 01/05/18 | Creates a cleaner codebook for export to Word         |
| bradmean     | 1.3.6   | 01/12/18 | Computes multiple independent means in a single table |


## Install
To install from Github, copy and paste the following code:
```
. net install <package>, from(https://raw.github.com/bbradfield/brad-stata/master/) replace
```
To install manually, download the .ado and .sthlp files and place them in the following directory:
```
~/ado/plus/b/
```
To update when connected to the internet, use:
```
adoupdate <package>, update
OR
adoupdate, update
```

## On Naming

I am not creative.
