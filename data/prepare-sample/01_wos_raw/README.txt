# Information about how these data files were obtained

## Files: wos_2012.csv, wos_2018.csv, wos_2024.csv

Person who obtained the data: Tom Hardwicke 

NOTE: this data file cannot be publicly shared because it contains large portions of a proprietary database.

Instructions: 

1. On 22nd April 2025, we used the Web of Science API tool (https://github.com/clarivate/wos-excel-converter) to search databases included in the Web of Science Core Collection, specifically, the Social Sciences Citation Index (SSCI), the Science Citation Index Expanded (SCIE), and the Emerging Sources Citation Index (ESCI). We also ran the searches using the Web of Science advanced search tool (https://www.webofscience.com/wos/woscc/advanced-search) to double check the same number of records were returned.

2. An affiliation (with University of Melbourne) and an account was required to use the API tool and advanced search tool. 
 
3. We conducted three searches, one for each publication year (2012, 2018, and 2024). For each search, we searched for records published in the publication year that had been categorised as belonging to the field of clinical psychology, had a document type classification of 'article', and were published in English. 

The 2012 search 

The specific search strings and records returned were:

Search one:
“WC=psychology, clinical and PY=2012 and DT=article and LA=english”
Returned 7,120 records. 

Search two:
“WC=psychology, clinical and PY=2018 and DT=article and LA=english”
Returned 8,591 records.

Search three:
“WC=psychology, clinical and PY=2024 and DT=article and LA=english”
Returned 14,600 records.

Note that the search field tag Year Published (PY) includes a search of both Published Early Access Year and Final Publication Year fields. Thus some items may be included in the output that were early access before the publication year, but finally published in the publication year, and some items that were early access in the publication year, but finally published in later years (see https://doi.org/10.1007/s11192-020-03697-x)

4. The records were downloaded using the API tool.
