# Nebuliser Particle Distribution Analysis Scripts
&copy; 2025 Sophia Davis

All code is free to use and modify as you'd like.

This Repository Contains:
- AutoIt Script for Automating Data Record Editing and Exporting
- MATLAB Classes and Scripts
- R Scripts


## 1. AutoIt Script: Spraytec Software Record Editing and Exporting Automation on Windows
### Notes:
All in all, the Automation script I wrote took about 47 minutes to export 120 files, which is around 23.5 seconds per file. I intentionally wanted it to go slow so that it wouldn't mess things up. When I was doing this manually yesterday evening, it took me over twice that amount of time (~7:52 pm - 9:36 pm, or 104 minutes) to edit and export that many records, not including all the false starts and mess-ups (just based on the Modified dates and times on the files that remain). The only place where this script briefly needed during the first run of Exp20-Water was at the 111th file, as it missed the export button so I helped it that time. I think it's fair to say that this script helps cut the processing time in half, and could potentially be more accurate than me exporting everything manually, since I may have made some mistakes due to fatigue.

104 minutes of manual processing ---> 47 minutes of leaving my laptop and letting the script run.

## Old Notes from 2023
UML Diagram Sketch Showing Plans for Refactoring:

![UML Class Sketch](uml_class_sketch.jpg)

[CFD Analysis Article for Reference](https://www.researchgate.net/figure/Magnitude-of-the-normalized-velocity-with-respect-to-the-maximum-local-velocity-at_fig13_318643936)
