# FYP

Results from cleaning raw data from GBIF and iDigBio using bdc package

Steps followed protocol here: https://brunobrr.github.io/bdc/index.html

bdc uses Taxadb and CoordinateCleaner to clean taxonomic and geographic data respectively. For taxadb, GBIF database v22.12 was used

Codes, cleaning reports and intermediate/resulting databases included

In summary, 4124/30435 records appear useful. 

Main problems I think are: 
1) 12983 / 30435 records have NA in coordinate columns -- removed otherwise rejected by CoordinateCleaner
2) 3788 / 17452 records flagged and removed by bdc cleaning
3) 9540 / 13664 records labelled 'Heteroptera' or 'Miridae' with no lower generic/specific classification or NA and hence not useful

Have not included the CoL data Prof Lim provided because I haven't figured out how to efficiently split the 'col.area' column into countries/provinces/county etc, as some include multiple countries/localities.
