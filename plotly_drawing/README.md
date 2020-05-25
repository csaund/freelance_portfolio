## Interactive Plotly Drawing
- :white_check_mark: Be able to interactively draw lines on plotly subplots with various custom inputs (color, xstart/end, ystart/end, width).
- :white_check_mark: Draw lines on multiple subplots at once
- :white_check_mark: Move lines around on subplot (until re-render)

Stretch:
- :white_check_mark: Use positions of line to populate and display new data table with derived value (% diff)
- :white_check_mark: Add lines by clicking on data points and creating input/output
- :white_check_mark: Save these lines to a dataframe that can be accessed later.
- :white_check_mark: Load lines from a dataframe that can be pre-drawn on the plotly object. 

To run: 
- Download and navigate into /plotly_drawing folder (where app.R lives)
    - make sure `w.ws.RData` or your own data lives in that folder as well! 
- Run from either inside R Studio clicking `Run App` button, or using Shiny command in R runtime.
    - to run from other R runtime, navigate to THIS directory (where the *directory* `/plotly_drawing/` lives.
    - use `runApp("plotly_drawing")` to launch the shiny app. 
    - Note: you must have the `shiny` package installed to do this. To install, run `install.packages("shiny")`

Possible additions:
- Upon moving lines, recalculate position and update that line in df
- Automatically calculate y values from clicks
