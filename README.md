The backend for SnoutCounter, a poll aggregation, electoral modeling, and other statistical modeling suite. Currently contains backend code and data for presidential approval polling averages, both general and issue-specific, and the generic ballot for the 2026 midterms.

### Directory Map

- `R/` - R code for polling averages.

- `averages/` - Day-by-day data for polling averages.

- `data/` - Polling data, from which models draw from.

- `ratings/` - Ratings sourced from the [Silver Bulletin](https://www.natesilver.net/p/pollster-ratings-silver-bulletin).

- `transformed_tables/` - Simplified polling data, for use in the polls display tables.

- `export.ipynb` - Exports all data for use on the SnoutCounter site.
