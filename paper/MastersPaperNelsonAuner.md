

1. Abstract
2. Introduction


3. Theory and Model Specification

We assume that the count of unigrams $x_{ij}$ in documents for unique unigrams 
$i  \in (1,2,..,I)$ across $j \in (1,2,3,..,J)$ documents is distributed according to the multinomial. Surpressing $i$ by denoting $x_j$ as the vector $(x_{1,j}, x_{2,j}, ...,x_{I,j}$ , we have the following model:
$$ x_{j} \sim MN(q_{ij},m_{ij}) ; q_{j} = \frac{exp(\alpha_j + y_j \theta_j + u_j \Gamma_j)}{\sum{exp(\alpha_j + y_j \theta_j + u_j \Gamma_j)}}$$

where $y_j$ is the metadata, and $u_j$ is the factor membership associated with document $j$, and $\theta_j$ and $\Gamma_j$ are the distortion coeffecients for the respective metadata and factor membership.

Sufficient Reduction


4. Application
5. Graphs
6. Conclusion
7. Acknowledgements
8. References


