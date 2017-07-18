<script type="text/javascript" async
  src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-MML-AM_CHTML">
</script>
## Longest Increasing SubSequence

To find the longest common increasing subsequence of an array of size n.

We can calculate Longest increasing subsequence ending at index i as given by the below recurrence equation.

**Recurrence Equation** can be expressed as:


\\[ LIS(i)= \cases{
\max (LIS(j)+1), & \text{$\forall$ $j\in [0$ to $i-1$] and $A[j]<A[i]$} \cr 
1, & \text{ for $i=0$} 
}
\\]

If needed predecessor information can be stored and backtracked.

