exact-1-grams.csv sourced from http://www.peachnote.com/datasets.html on July 2 2018.

Many links on the website are broken, unfortunately. 
I couldn't find a license file.  

However, I could download the 1-grams file from 'Petrucci Music Library - Exact Chord Progressions', and that's what we call exact-1-grams.csv.

The documentation for this data file is as follows:

> This dataset contains chord progressions of up to four chords length and their counts. The chords represent all simultaneously active notes over all voices of a score. This means that the notes must not have the same onset time in order to appear in the same chord.

> Counts of progressions contained in scores for which no year of composition/first publication is known are stored under the "?" year.

This is not fully informative, so I think we have to extrapolate from other parts of the page. For example, here's the info for transposed chord progressions:

> The entries represent equivalence classes of chord sequences equivalent up to a pitch shift. If the first chord of a sequence consists of multiple notes, the pitch of the lowest note is not stored and the chord is starts with and underscore sign "_". The following number indicates the difference in semitones between the lowest and the second lowest notes. If the first chord consisted of a single note, then the ngram begins with a number indicating the difference in semitones bewteen that single note and the lowest note of the second chord.
