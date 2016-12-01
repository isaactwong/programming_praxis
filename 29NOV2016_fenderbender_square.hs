-- Over at Mersenne Forum, fenderbender extends Cohen’s idea to make a ridiculously fast square predicate: he precalculates multiple moduli to reduce the operation from big integers to 32-bit integers, chooses the moduli after extensive testing, and tests the quadratic residues using a 64-bit bloom filter. The result is impressive. Where Cohen eliminates the expensive square root calculation in 99% of cases, fenderbender eliminates the expensive square root calculation in 99.92% of cases, and does it faster than Cohen. Go read fenderbender‘s explanation to see a beautiful combination of number theory, wonky programming, and sheer artistry.

-- Your task is to implement fenderbender‘s square predicate.

isSquareByFenderBender :: Integer -> Bool