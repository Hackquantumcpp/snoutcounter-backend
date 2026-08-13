### Data Acknowledgement

Historical polling datasets from [538](https://web.archive.org/web/20250131211134/https://projects.fivethirtyeight.com/polls/). Historical generic ballot polling datasets for cycles prior to 2018 (i.e. those in `historical_gb/`) derived from [RealClearPolitics](https://www.realclearpolling.com/historical-polling-archive). Polling datasets for current cycle are collected and maintained by myself, with general presidential approval, generic ballot, and 2026 California gubernatorial primary data before January 4, 2026 being sourced from Mary Radcliffe's dataset of polls (before it was merged into [FiftyPlusOne](https://fiftyplusone.news/)) and [The New York Times](https://www.nytimes.com/interactive/polls/donald-trump-approval-rating-polls.html).

### Notes

- The `israel_iran_conflict` issue has been renamed to `twelve_day_war` for ease in retroactive analysis, and to distinguish polling on the Twelve-Day War from polling on current or future conflicts involving Iran and Israel.

- The `ballroom` issue has been renamed to `white_house_renovations`.

### Directory Map

- `fte/` - Polling datasets from 538

- `historical_gb/` - Generic ballot polling datasets derived from RCP for cycles prior to 2018

- `generic_ballot_polls.csv` - Polls measuring generic ballot for current cycle (2026).

- `president_approval_polls.csv` - Polls measuring presidential approval for current president (Donald Trump, second term).

- `president_issue_approval_polls.csv` - Polls measuring presidential approval on specific issues for current president (Donald Trump, second term).

- `governor_primary_ca.csv` - Polls measuring electoral intent for 2026 California gubernatorial primary.
