# Female Inventors: A Gender Gap to More Innovation in Switzerland 
This repository contains data and code for analysis focusing on the the share of female patent inventors and Science, Technology, Engineering or Mathematics (so-called STEM) graduates in Switzerland and across OECD countries. The full article can be accessed <a href = https://innoscape.de/female_share/en/female_inventors.html target = “_blank”>*here*</a>.

Women are strongly underrepresenteted among patent inventors. At the beginning of the 1980s the rate of female inventors has been very low in almost all countries. But it was especially low for Switzerland where less than two out of 100 inventors of Swiss patents published at the USPTO were women. Female representation in Switzerland has improved somewhat over the last 40 years, but in 2018 still only 9 out of 100 Swiss patent inventors were women.

One reason for this low female participation is that not so many women graduate in STEM fields which is generally a precondition to become an inventor. However, our analysis shows that most countries are also not very successfull in motivating female STEM graduates to pursue inventor careers. Taken together, Switzerland and all other German-speaking countries are amongst the most poorly performing countries: They have rather low female STEM graduate shares and only few of these female STEM graduates ultimately become patent inventors. As the Swiss economy requires constantly new technical innovations, new ideas and improved processes to remain one of the leading innovation hubs in the world, making good use of the skill potential of its workforce is key for Switzerland’s future prosperity. Thus, our analysis calls for increased efforts by policy-makers, academia and business leaders to increase women participation in inventiv activities.

## Structure of the repository

### Data
This folder contains the file `data_processing.R`, the script to generate the data used in this analysis. Raw data on patents can be accessed at the USPTO and data on STEM graduates is available at the OECD.

### Report
This folder features several scrips that jointly produce the interactive graphs presented in this analysis. The main file to generate the full report of this analysis is `female_inventors.Rmd`, which builds on packages and libraries such as `knitr`, `rmarkdown`, `ggplot2`, `shiny`, `shinyWidgets` and `plotly.js`.

### Example
![example_plot](https://raw.githubusercontent.com/cieb-unibas/female_inventors/main/Data/example_plot.png)





This report is part of <a href = http://innoscape.ch/ target = “_blank”>*Innoscape*</a>, an applied research project of the <a href = https://cieb.unibas.ch target = “_blank”>Center for International Economics and Business | CIEB</a> at the University of Basel. With *Innoscape*, we analyze how well prepared the Swiss economy is for the transformations of the 21st century. We provide economic insights on how Switzerland can successfully master these challenges and discuss how the country could remain a global innovation leader in the future.

