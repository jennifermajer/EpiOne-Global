# Data Requirements (Full)

Minimum fields (line list)
- `datevisit`: visit date (YYYY-MM-DD preferred; multiple formats accepted)
- `morbidity`: disease/condition label (free text)
- `orgunit`: facility name or code

Recommended
- Demographics: `sex`, `age` or `age_group`
- Geography: `admin1`, `admin2` (align with provided shapefiles)

CSV examples
- See `data/data_dictionary.csv` for field definitions and notes.

DHIS2 tips
- Export event/tracker line lists; include org unit names and IDs when possible.
- Keep raw morbidity labels; taxonomy handles mapping.

Aggregate data
- Experimental; supported with minor reshaping and column mapping.

