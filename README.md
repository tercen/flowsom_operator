# FlowSOM operator

##### Description

Flow Cytometry Self Organising Maps `FlowSOM`: An algorithm that clusters cells based on chosen channels and maps them into populations.

##### Usage

Input projection|.
---|---
`row`   | represents the variables (e.g. channels, markers)
`col`   | represents the clusters (e.g. cells) 
`y-axis`| is the value of measurement signal of the channel/marker

Input parameters|.
---|---
`nclust`   | Number of clusters to make (default = `NULL`)
`maxMeta`   | Maximal number of cluster (ignored if `nclust` is not `NULL`)
`seed`   | Random seed
`xdim`   | Width of the grid
`ydim`   | Hight of the grid
`rlen`| Number of times to loop over the training data for each MST
`mst`| Number of times to build an MST
`alpha_start`| Start learning rate
`alpha_end`|  End learning rate
`dstf`| Distance function (1=manhattan, 2=euclidean, 3=chebyshev, 4=cosine)

Output relations|.
---|---
`cluster`| character, cluster label
`model`| character, name of the flowSOM model (to be used with other operators)

##### Details

The operator is a wrapper for the `FlowSOM` function of the `FlowSOM` R/Bioconductor package.

#### References

https://bioconductor.org/packages/FlowSOM/

##### See Also

[flowsom_mst_shiny_operator](https://github.com/tercen/flowsom_mst_shiny_operator)
