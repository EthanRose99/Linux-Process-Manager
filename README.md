## Acknowledgments
Grok (xAI) assisted with this project by:
1. Creating a GUI layout with 1/5 left and 4/5 right panels.
2. Fixing compilation errors (E0502 borrowing, Roslyn::Button typo).
3. Adding sortable CPU/memory columns and process grouping.
4. Enlarging the Show/Hide Background Processes button.
5. Ensuring chrome was classified as a foreground process.
6. Configuring table headers and "No processes found" placeholder.
7. Implementing background process filtering with toggle.
8. Removing debug print statements.
9. Combing CLI and GUI features to act in same window

/*******************************************************************************
*Operating systems: Linux task manager
*
*Code below dealing with CLI was written by Michael Henein
*
*Code surrounding GUI was written by Ethan Rose with help from AI (grok.com)
*
* As per teacher instruction all open source was allowed for this projectI used AI as a tool to help me with syntax, error handling, 
* and for functions such as how to sort and display a list as a table in a gui. Below is a summary of what I was helped with. 
* At no point did I use AI to "do this project" for me. I used our teams list of design features and used AI as a tool to help me implement these features
* At no point did I utilize it to recommend better features or create the project all by itself with its own mind.
*### Works Cited
*Grok. Personal communication, xAI artificial intelligence assistance for Rust-based Linux process manager, 2025.
*A constructed list of the parts I had help from AI (grok.com). 
*## Acknowledgments
*Grok (xAI) assisted with this Linux process manager (personal communication, 2025):
*1. GUI layout with 1/5 left, 4/5 right panels using egui.
*2. Fixed E0502 borrowing errors and Roslyn::Button typo.
*3. Added sortable CPU/memory columns and process grouping.
*4. Increased Show/Hide Background Processes button size.
*5. Ensured chrome classified as foreground process.
*6. Configured table to show headers and "No processes found".
*7. Implemented background process toggle with tty_nr check.
*8. Removed debug print statements from update functions.
*9. Suggested debugging for process detection and WSL issues.
*10.Linking CLI and GUI to function as one
*
*
*******************************************************************************/
