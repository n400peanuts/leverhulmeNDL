labpic.csv, piclab.csv, contingency.csv are data cleaned by Gorilla's https rubbish and contain all data.
labpic_errorType.csv, piclab_errorType.csv are data that are identical to labpic.csv and piclab.csv but
they have an extra column called "type_of_resp" that codes the errors made by participants as following:

- match: correct trials. Participants selected the label corresponding the fribble as attested in the learning
- mismatch-type1: incorrect trials. Items label is incorrect for this fribble type but the fribble has a high salience feature (shape) which is associated to another category (high or low is coded by correctFrequency column)
- mismatch-type2: incorrect trials. Items selected don't share any high salience feature with the label/fribble presented 
- errorControl: incorrect trials. Items are all blue bims incorrectly categorised as dep/wug/tob
- errorControl-high: incorrect trials. Items are either tobs, deps or wugs belonging to the HIGH frequency condition but participants selected the control condition (blue bims)
- errorControl-low: incorrect trials. Items are either tobs, deps or wugs belonging to the LOW frequency condition but participants selected the control condition (blue bims)
