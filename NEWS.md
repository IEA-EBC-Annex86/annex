

# annex 0.2-12

* `annex_stats()` now also includes annual statistics for the whole day
    (`tod = "all"`).
* `annex_stats()` also includes overall stats (over the entire data set,
    all years, all `tod`). If data is only available within one specific
    year, this will be identical to the annual `tod = "all"` stats
    (duplicated data but added for convenience).
* Adjusted tinytests for new behaviour (extended stats).
* `annex_validate()` gives more details when quality violations (boundary violations)
    are found; now shows `user-study-home-room-year-month-tod` in addition to
    the XLSX row number for easier interpretability.

# annex 0.2-11

* Added experimental function for plotting objects of class `annex_stats` as
  returned by `annex_stats()`.
* Issue fixed; `annex()` now appends the user defined time zone to `datetime`
  which is later used in `annex_stats()`. Before the fix the `tod` was correct,
  however, the `quality_from`/`quality_to` was incorrect.
* Note: The version used to process user 008.

# annex 0.2-10

* Fixing a bug; `interval_*` and `Nestim` now properly set to `NA` (missing)
    if `N - NAs <= 1`.
* Added new comment column to template sheet META-Home (non-mandatory field) 
* `write_annex_stats()` adds the package version used to process the data as well
    as the _R_ version string to the INFO sheet.

# annex 0.2-9

* Added an additional check in `annex_prepare()` to ensure that all data columns
  are numeric. If one of the variables provided is not numeric but only contains
  missing values (e.g., logical NAs) the column is converted automatically
  (to `NA_real_`). Else (not numeric or not all missing) an error will be thrown.
* New feature: `annex()` has a new argument `duplicate.action` which allows
    the user to specify how he would like to handle duplicated observations
    (observations for the same date and time for a specific room). See
    `?annex` section 'Duplicates' for details.
* `annex_validate(..., quiet = FALSE)` is now quiet.
* Adding new function `annex_read_stats()` which allows to import data
    from one or multiple XLSX files written by `annex_write_stats()` (beta).
* Removing ventilation type 'Exhaust air' added in 0.2-8
* `annex_stats()` will return `NA`s for 'Mean' and 'Sd' in case the number of
    valid observations (`N - NAs`) are less than 30.
* `annex_stats()` will return `NA`s for `interval_*` if `N - NAs == 1` (we have
    exactely one valid observation).
* `annex_validate()` validates for the two conditions above.
* `annex_validate()` now also throws an error if `Sd < 0` (see condition above).
* Series of additional tests added.

# annex 0.2-8

* Added additional ventilation type in definitions (Exhaust air)

# annex 0.2-7

* Fixed a **serious issue** with how the intervals (how often measurements are
    reported) which also affects the estimate of how many observations we
    estimate (we think should be there).
* Added a new variable `Flow` (air flow) and it's conversions; defaults to
    `l/s` (liters per second) allowing values (via annex config) for
    `m3/h` (quibic meters per hour) and `cfm` (suare feet per minute).

# annex 0.2-6

* An issue with [`openxlsx`](https://github.com/ycphs/openxlsx/pull/409#event-8839793513)
  has been fixed, when using `openxlsx >= 4.2.5.9001` annex shoul work
  as expected for those not using MS Excel.
* In the mean time one small issue with `openxlsx` has been adressed (for those
  opening/modifying their `.xlsx` files with openoffice). See
  [issue 9](https://github.com/IEA-EBC-Annex86/annex/issues/9) for details.
* Modified the variable definition. Minor renaming, modified/added lower and
  upper bounds as well as allowed units. In addition ...
* ... a series of unit conversion functions has been added.
* Updated the vignettes (documentation) to the new code base.
* Updated and extended the tinytests for the new release; mainly for unit
  conversions.

# annex 0.2-4

Release candidate for testing in February 2023 with some major changes
based on the discussions in the meeting early February.

* The config object now must contain a "unit" variable. Some of the allowed
   variables (see `annex_variable_definition()`) have a set of pre-defined
   units which must be used. `annex_prepare()` internally converts all the
   data to the 'annex standard units'. E.g., the user can provide temperature
   data in Kelvin or Farenheit, `annex_prepare()` will always convert them to
   degrees Celsius. This is currently implemented for "T" (temperature; deg C),
   "RH" (relative humidity; percent) and "Pressure" (hPa).
* Auto-fill "META-Variable" for those variables where the annex package enforces
   the unit (e.g., "T" always in degrees Celsius; "RH" always in percent).
* Removed "META-Season" sheet from XLSX file.
* Changed aggregation. Originally the aggregation has been done on a quarterly
    level including all years. The new version aggregates by year + month.
* Allowing rooms to be numbered for differenciation by adding up to two digits.
    E.g. BED, BED1, BED2, BED66 (same for all room types).
* New functions `annex_variable_definition()` and `annex_room_definition()`
    which return information about allowed variables/room labels amongst other
    information.
* Added data set containing all ISO3166 alpha-3 (ISO3) country codes used for validationn.
    Convenience function `annex_countries()` which returns `data("ISO3166")`
* Added quality flag to stats; percentage of observations falling below a
   certain threshold or exceed a threshold. These bounds are defined in the
   "Definitions" sheet in the XLSX file and can be accessed calling
   `annex_variable_definition()`.
* Additional output when validating the XLSX file; improved readability.
* Added interval (measurement interval) information to stats; time in seconds to the
    previous observation recorded in the data set. Calculated on a varaible level; if no
    value is present (missing value) this will be considered to 'not have been mesured'
    which leads to longer interval times. Currently reporting five number summary plus
* Both XLSX sheets 'META-Room' and 'META-Variable' have a column for optional information
    about the measurement location (e.g., next to window, center of room, and any other
    important/helpful information) or variable (optional meta data).
* Additional tinytests for support functions.
* Validation: Decent update on the validation to show more granular errors and warnings.
    Additional checks for specific varaibles to ensure the content follows the expected
    format. Added conditional tests on variables (some require additional information).



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
