
# Development todo list

* [ ] Currently I rely on the order of the columns
* [ ] Remove stats which are always NA
* [ ] When checking METa (annex_validate_sheet_metaXxxx)
      some columns must be numeric; check for numeric? Not yet implemented
* [ ] Allow for custom user functions when calling annex_stats?

# annex 0.2-0

Under development since January 2, 2023; intended to be the next release
after the initial testing phase December 2022 to February 2023.

* Renaming 'Pollutant' consistently to 'Variable' (issue #2)
* Renaming sheet 'Meta-Period' to 'Meta Season' (issue #2)
* Automatically filling 'Measurement Location' in META-Room (issue #3)
* Automatically filling 'Variable Name' in META-Variable (issue #4)
* Adjusting some (internal) function names (Pollutants -> Variables; Period -> Season)
    given the adjustments updates listed above.



# annex 0.1

Summary of major fixes and features during the development of the
package in its `0.1-X` state.

* New infrastructure with custom S3 class with a series of handy methods
* Introducing a `config` object for easy control
* Moving project to <https://github.com/IEA-EBC-Annex86/annex/>
* Adding first version of `annex_write_stats()`; no overwrite mode for now
* Added method to reshape `annex_stats` objects from long to wirde form and vice versa
* `annex_write_stats` now accepts long and wide formats
* Added logo (draft)
* Added `annex_validate` to validate an XLSX output file; likely missing some checks.
* Resolving a series of notes/warnings during build check
* Properly adding licenses (GPL-2 + GPL-3)
* Updating documentation and readme; adding process schematic.
* Removing unused R scripts (version 0 of the annex package).
* Removing rows in `STAT` when writing the file where N == NAs; no data available at all.
* Rounding all stats to 4 digits max.
* Adjusting documentation according to GR (`0.0-14`)
* Added a list of contributors; added ORCID for GR.

# Annex 0.0-9000

* Reto is just playing around.
