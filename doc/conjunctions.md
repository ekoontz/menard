# Full sentences

## nl

surface | sem
-|-
ik zie Kim                  | see(I,Kim)
ik zie Kim niet             | neg(see(I,Kim))
ik zie Kim en Sandy         | and(see(I,Kim),see(I,Sandy) 
ik zie Kim maar niet Sandy  | and(see(I,Kim),not(I,Sandy))
ik zie niet Kim maar Sandy  | add(not(see(I,Kim)),see(I,Sandy))

## en

surface | sem
-|-
I see Kim                  | see(I,Kim)
I don't see Kim            | neg(see(I,Kim))
I see Kim and Sandy        | and(see(I,Kim),see(I,Sandy) 
I see Kim, not Sandy       | and(see(I,Kim),not(I,Sandy))
I see not Kim but Sandy    | add(not(see(I,Kim)),see(I,Sandy))


# Partial

## nl

surface | sem
-|-
zie Kim | see(X,Kim)
[zie Kim] niet | not(see(X,Kim))
Kim [en Sandy] | and(Y(X,Kim),Y(X,Sandy))
zie [Kim [en Sandy]] | and(see(X,Kim),see(X,Sandy))




