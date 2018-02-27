# validity
A propositional logic prover in Haskell 

Todo:
- Tests (I do have a little feeling some stuff isn't right)
- Constant folding - e.g. when we have something like ~(A && B); (A && B) - we don't need to expand them out, we can just remove them on the basis of having a formula and its negation right there
- Improvements to the parser; 
    - Add in an actual symbol for the last item in the list (e.g. ∴) to make sure that it's actually desired to be the conclusion
- A syntax guide
- Predicate logic!
