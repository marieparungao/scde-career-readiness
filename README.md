# scde-career-readiness
ECON Scholars research on career-readiness pathways across South Carolina public high schools, 2022–2026

## Data construction notes

### Coverage and dataset structure
The current master dataset covers 2021–22, 2022–23, and
2023–24. It contains 775 school-year observations and
45 variables, with no duplicate school-year records.

This differs from the four-year period originally proposed.

### Headcount snapshot timing
The 2021–22 and 2022–23 school-characteristics files use
135-day enrollment snapshots. The available 2023–24 file
uses a 180-day snapshot.

The variable headcount_day records this difference.
We should obtain the 2023–24 135-day file if possible.
Until then, comparisons may reflect differences in
snapshot timing as well as changes in student composition.

### Academic achievement measure
academic_achievement_points_pct is calculated as:

100 × achievement points earned / achievement points available

It measures the share of available academic achievement
indicator points earned, not the percentage of students
proficient. Schools labeled "Not Rated" retain missing scores.

Normalizing the points does not establish comparability
across accountability systems. Changes in indicator
definitions and scoring rules still need review.

### Disability composition proxy
disability_share_attendance_pct is calculated as:

100 × NTotal_DI / NTotal_ALL

Both counts come from the chronic absenteeism reporting
population. This is a proxy for school disability
composition, rather than a verified disability percentage
of total school enrollment.

Suppressed counts are retained as missing, not zero.

### Geographic classification
School locale classifications come from the historical
NCES EDGE geographic files for each corresponding academic
year. Same-year CCD school directories connect SC school
IDs to NCES school IDs.

Palmetto Unified (5209001) has no matching directory record
in the files used and retains a missing locale in all
three years.

### Missing data
All readiness records are retained in the master dataset.
Missing source values and unmatched records remain missing.

Current missing-value counts:
- Cohort size and each readiness outcome: 16
- Enrollment and constructed headcount characteristics: 6
- Academic achievement measure: 59
- Chronic absenteeism: 10
- Disability composition proxy: 21
- Geographic locale: 3

These counts overlap and should not be added together to
calculate the number of incomplete observations.

### Remaining review
- Investigate the unmatched records.
- Verify source missing-value and suppression codes.
- Review academic achievement comparability across years.
- Resolve or document the headcount snapshot difference.
- Review whether school IDs changed across years before
  estimating school fixed-effects models.