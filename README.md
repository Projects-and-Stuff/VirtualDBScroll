
###A set of Lazarus visual data-aware components for displaying and scrolling through (potentially) very large DataSets with unknown record-sizes

---

Most data-aware components fall into two variants:

1. Components that display a single field of a single record, and can easily perform any applicable scrollbar calculations
2. Components that display multiple fields of multiple records in a grid. These components display each record in rows of equal height. They only display the records currently in the record buffer, so extremely large datasets can be scrolled. Since every row is the same height, and a constant number of records is displayed at all times, the scrollbar calculations for these components are also trivial.

I was developing a software application for logbooks, and found that I could either use a data grid, in which case displaying records that had significant variation in length would be cumbersome and unaesthetic, or write the records to a plain memo component which would eventually result in memory issues as the number of records increased.

In order to get around these constraints, my goal was to create a set of components that meet the following requirements:

- Scroll through datasets of up to millions of records without knowing beforehand how large each record is
- Display each record in its entirety, with hundreds or thousands of lines allowed per record
- Allow programmers to determine how the data is formatted in the display
 
I am still currently in the process of developing and improving these components, but I wecome everyone to view, use, or contribute toward this project.

---

This project will eventually contain:

* VirtualDBScrollMemo
* VirtualDBScrollRichMemo
* VirtualDBScrollSynEdit
