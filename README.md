# Abandoned Wells in New York State

This is the Github repository for our article on abandoned wells in New York State. The contents of this repo have been updated and reanalyzed as of March 25, 2022. Contents include: 

[NYS Wells 1860 - Present.ipynb](https://github.com/ilenapeng/wells/blob/4bc3152fdc93d7a553f0d062d034dae2d2e084cc/NYS%20Wells%201860%20-%20Present.ipynb) & [Orphaned wells in NYS.ipynb](https://github.com/ilenapeng/wells/blob/4bc3152fdc93d7a553f0d062d034dae2d2e084cc/Orphaned%20wells%20in%20NYS.ipynb): The script for our data analysis

[graphics](https://github.com/ilenapeng/wells/tree/main/graphics): Charts included in the article and the R script for generating them

The data for this repo was last downloaded on 3/25/22. All CSVs and data folders have been labeled with '32522' to reflect the download date. 

[data_3_25_22](https://github.com/ilenapeng/wells/tree/main/data_32522): Raw data for this analysis from the New York State Department of Environmental Conservation, Division of Mineral Resources via Open Data NY

* [data_raw_32522](https://github.com/ilenapeng/wells/tree/main/data_32522/data_raw_3_25_22)

    * [wells_1860_32522.csv](https://github.com/ilenapeng/wells/blob/4bc3152fdc93d7a553f0d062d034dae2d2e084cc/data_32522/data_raw_32522/wells_1860_32522.csv) — [Oil, Gas, & Other Regulated Wells: Beginning 1860](https://data.ny.gov/Energy-Environment/Oil-Gas-Other-Regulated-Wells-Beginning-1860/szye-wmt3). Is updated daily, our download is from March 25, 2022. 
    * [orphaned_wells_32522.csv](https://github.com/ilenapeng/wells/blob/4bc3152fdc93d7a553f0d062d034dae2d2e084cc/data_32522/data_raw_32522/orphaned_wells_32522.csvv) —  [Orphaned Wells](https://data.ny.gov/Energy-Environment/Abandoned-Wells/vgue-bamz). Is updated annually, documents abandoned and unplugged oil & gas wells. On the date of download, this data was last updated on Dec. 16, 2021.

* [data_processed_3_25_22](https://github.com/ilenapeng/wells/tree/main/data_32522/data_processed_32522): CSVs we generated from the raw data
    * [timespan_32522.csv](https://github.com/ilenapeng/wells/blob/4bc3152fdc93d7a553f0d062d034dae2d2e084cc/data_32522/data_processed_32522/timespan_32522.csv) (NYS Wells 1860 - Present.ipynb) — Used to generate violin plot, time between the dates a well was completed and plugged
    * [timespan_summ_32522.csv](https://github.com/ilenapeng/wells/blob/4bc3152fdc93d7a553f0d062d034dae2d2e084cc/data_32522/data_processed_32522/timespan_summ_32522.csv) (NYS Wells 1860 - Present.ipynb) — Summary statistics by county, for time between the dates a well was completed and plugged
    * [plug_abdn_32522.csv](https://github.com/ilenapeng/wells/blob/4bc3152fdc93d7a553f0d062d034dae2d2e084cc/data_32522/data_processed_32522/plug_abdn_32522.csv) (NYS Wells 1860 - Present.ipynb) — Number of abandoned and plugged wells in each county
    * [active_32522.csv](https://github.com/ilenapeng/wells/blob/4bc3152fdc93d7a553f0d062d034dae2d2e084cc/data_32522/data_processed_32522/active_32522.csv) (NYS Wells 1860 - Present.ipynb) — Number of active wells in each county
    * [unplug_orphaned_32522.csv](https://github.com/ilenapeng/wells/blob/4bc3152fdc93d7a553f0d062d034dae2d2e084cc/data_32522/data_processed_32522/unplug_orphaned_32522.csv) (Orphaned wells in NYS.ipynb) — Number of abandoned and unplugged wells in each county