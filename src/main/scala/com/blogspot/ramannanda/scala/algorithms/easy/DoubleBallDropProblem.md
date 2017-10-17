<script type="text/javascript" async src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-MML-AM_CHTML"> </script>
## Two Ball Drop Problem

**Problem**: You are given two glass balls of same configuration and you want to find the height or floor from which to drop the ball, so that it breaks i.e. the maximum floor from which you can drop the ball without breaking it.

**Solution**:

This problem is trivial once you realize that you have to use both balls.

So you have to find the optimal intervals to drop the ball, so that the total number of drops is minimum.

Let's say you drop the ball after every \\( m \\) interval and there are \\(n\\) floors. So total number of drops would be 

\\[Drops = \frac{n}{m}+m-1 \\]

Here the first part is because you need to do this at most n/m times and the second is because you then need to go from the last interval where the ball didn't break to the interval where ball breaks.

Minimizing the drops requires taking a derivative of this equation and solving for m or maximizing it.

\\[ \frac{\partial (Drops)}{\partial m} =  \frac{\partial }{\partial m} (\frac{n}{m}+m-1)\\] 

\\[m=\sqrt{n}\\]

Given this we pick m to be \\[\sqrt{n}\\]

So, for n=100 we pick m to be 10, 20, 30 etc. in the worst case the number of balls to be dropped is 19.