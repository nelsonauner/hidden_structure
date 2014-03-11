Task: Iterate between cluster and parameter updates


By bayes formula: 
P(u_i = b | c_i,v_i) = P(c_i | v_i, u_i = b) * some constant

X \sim Multinomial(f(Y*theta + U*Gamma)) = MN(f(Y*Beta)) #let Y = [Y,U] so B = [theta,gamma]

Step 1: Given cluster membership, fit parameter values theta and Gamma
Step 2: Given theta and Gamma, evaluate likelihood and switch to a better cluster if possible


What is the best way to update the cluster point? 
- The predicted point is YB, the contribution from the cluster point is 
- We can see the contribution from all memberships by looking at X_i * Gamma = [X_i under member=1, X_i under member=2, ...]

Results: 
We see that LL can be increased through iterations. See graph. 


Questions:
How do we choose how many topics to fit with? 
Best way to iterate?
mnlm and the way it deals with factor variables..
When doing a simple cluster membership with K clusters, the information of the last cluster is redundant - do we need to do something about that? 
