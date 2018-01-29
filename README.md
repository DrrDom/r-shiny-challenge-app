# r-shiny-challenge-app

The shiny app to support challenges/competitions where participants should select among many items (names) the subset of them containing as many as possible of correct ones.

## Use
A user provides the specified number of selected entries (one per line) as a text or as a text file.  
A host provides the file with correct answers (one per line).  
The app makes comparison and return a score.  

## Setup
app.R:  
&nbsp;&nbsp;`title` - title of the page  
&nbsp;&nbsp;`max_submissions` - max number of allowed submissions from each group  
&nbsp;&nbsp;`n_entries` - number of entries which should be entered by participants (more or less entries will make submission invalid)  
&nbsp;&nbsp;`res_dir` - path to the dir where submissions of individual groups will be stored  
  
data/tokens.txt  
&nbsp;&nbsp;tab-separated file containing private tokens for each group and the name of the group
  
data/correct_answers.txt  
&nbsp;&nbsp;text file containing correct answers, one entry per line
  
data/challenge_description.htm  
&nbsp;&nbsp;html-file containing the description of the competition

#### Advanced setup
The score should be changed depending on the challenge goal - line 120 in app.R

## License
BSD-3