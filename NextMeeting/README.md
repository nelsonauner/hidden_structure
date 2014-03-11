Tasks: 
1. Iterate between cluster and parameter updates

$$P(u_i = b | c_i,v_i) = P(c_i | v_i, u_i = b)$$


-Initialize with some cluster DONE

-For each data point, switch to a better cluster if possible
What is the best way to update the cluster point? 
- The predicted point is YB, the contribution from the cluster point is 
- It's not necessary to make a full prediction for each point, we can simply maxim


Questions:
How do we choose how many topics to fit with? 
Best way to iterate?
mnlm and the way it deals with factor variables..
When doing a simple cluster membership with K clusters, the information of the last cluster is redundant, since
$$ \sum_{l \in K}{I_{x_{i} \in l} = 1$$
