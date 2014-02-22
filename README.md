hidden_structure
=====

First project: adding mixture membership and latent factor structure as part of a massive multinomial regression -- e.g., for text phrase counts regressed onto document attributes.  This will be useful to model and control for unobserved heterogenaity between (or dependence within) documents.  


Our final model will have Ci ~ MN(f(alpha + Phi\*v_i + Gamma\*u_i)) as the likelihood,
where v\_i are known document attributes and u\_i is either a vector of mixture membership (e.g., [0,0,1]) or factor weights (e.g., [-.5,.67,2]).  

We'll alternate updates for [alpha,phi,gamma] given U and U given [alpha,phi,gamma]. There will be priors (penalties) on everything, which is how we guard against overfit. This is indeed a supervised factor model, since C_i `supervises' U

