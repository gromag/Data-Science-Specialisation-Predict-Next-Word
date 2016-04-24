##Project Capstone

###Steps
1. Sample Corpus into Training set, Held Set and Test Set (60, 20, 20)%
2. Clean data
3. Tokenize into unigram, bigram and trigram
4. Calculate counts
4. Smooth counts, to take into account unseen events (using the Held Set, research), Simple Good-Turing require linear regression
5. Calculate probability
6. Kneser-Ney interpolation use absolute discounting and will account for context when checking on lower order grams.


