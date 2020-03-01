### Original Problem
#### Input
Start word: like `cat`

End word: like `dog`

Dictionary: list of words, including start and end words.
`cat, dog, cog, cot, bat, big` 
 
 #### Logic
 One word is single-step transformable to another word only if there's only one character different, and it's at the same position.
 
 One word is transformable to another words only if in every steps it's transformed to a word existed in the dictionary.
 
 #### Output
 Solved: true/false
 Steps: `cat, cot, cog`
 
 ### Extended Scenario
 In tracing the evolution of COVID-19, every generation of RNA is exactly like a word. 