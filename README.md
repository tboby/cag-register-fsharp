# CAG Unofficial Registry

This project contains the source for the CAG registry browser found at https://cag.boby.uk.

This registry's data is sourced as follows:

1. The latest Research and Non-Research excel documents are downloaded from the [official site](https://www.hra.nhs.uk/planning-and-improving-research/application-summaries/confidentiality-advisory-group-registers/).
   1. The first sheet is processed, and an application created for each row
   2. Every following sheet is processed, assuming a consistent format, and merged with the matching application ID
2. All minutes (main, sub-committee, and precedent set) are located from the [official pages](https://www.hra.nhs.uk/about-us/committees-and-services/confidentiality-advisory-group/cag-group-meetings-and-minutes/).
   1. Any new minutes are downloaded
   2. Each PDF is scanned for "nn/CAG/nnnn", and a note made of each location
   3. Each application has a link added for each location it was referenced in minutes, with a link to the source minutes PDF.

The data is (at time of writing) updated daily.

