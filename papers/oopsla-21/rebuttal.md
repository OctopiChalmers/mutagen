We thank all the reviewers for their helpful and detailed feedback. We answer
reviewers' questions below -- reviewer #1, please refer to the answer given to
reviewer #2.

For the final version, we propose to use the extra available space to clarify
the questions and concerns of the reviewers to the furthest extent. Moreover, we
will expand on implementation details and usage examples of MUTAGEN.

Reviewer #2
-----------

> given the absence of a direct comparison against FuzzChick, how do you make
> the case that your technique is an improvement over the state of the art?

We clarify that IFC stack machine case study shows a direct comparison between
FuzzChick and MUTAGEN, where our tool shows a noticeable improvement in terms of
mean-time-to-failure, failure rate. We will update the introduction to reflect
that explicitly.


> on a related note, Table 3 suggests that Mutagen needs to generate more tests
> until failure than FuzzChick in at least 7 of the 20 cases. Based on these
> results, and given the synthetic nature of the bugs, how can you argue that
> the Mutagen technique is really better?

We argue that we are fair when comparing against FuzzChick for the following
reasons:

- Table 3 compares results only for the runs in which *FuzzChick actually finds
the injected bug* --- a distinction that benefits FuzzChick, since it shows a
low failure rate in this case study, and was only able to find 5 out of 20 bugs
with a 100% failure rate, whereas MUTAGEN achieves a 100% failure rate across all
bugs.

- Given the above observation, comparing only the MTTF across all runs
(bug-found or timeout) can skew the representation of the performance of both
tools since timed-out executions inflate the overall calculation. This is clear
in Fig. 4, where FuzzChick's MTTF clearly jumps over 10^5ms as soon as the
failure rate goes below 100%.

- We observed that FuzzChick tends to find bugs either relatively fast, or not
at all within the 1-hour testing budget. The unstable behavior of FuzzChick
could be alleviated by running it several times in a row using much shorter
timeout (e.g. 30 seconds). Under this consideration, for the comparison against
FuzzChick to be fair, our tool needs to be reliable (always find the bug)
without being (possibly orders of magnitude) slower on individual runs. Table 3
shows that this in the case, where the *A12* measure indicates that MUTAGEN is
not prone to need many more tests to find a bug that FuzzChick in most of the
cases (except only for bugs #15 and #18, which FuzzChick is not capable of
finding with a 100% success rate).

- We included mean-tests-to-failure in Table 3 merely as an indication of the
order of magnitude. This metric, however, is quite sensitive to outliers. For
this reason, we included the A12 measure between the two sets of measures, which
is much more robust against outliers and should be considered over simply
mean-tests-to-failure when comparing both approaches.


> how were bugs introduced in the benchmarks? If you introduced these manually,
> what steps were taken to mitigate potential bias?

In the FuzzChick article, the IFC stack machine case study uses a systematic
approach for introducing bugs. We simply translate (implement) this *full case
study* directly into Haskell along with the mechanism introducing the bugs.

The WebAssembly case study uses manually inserted bugs. Such bugs are of two 
different kinds:

 * Removing an existing integrity check (to weaken the WebAssembly 
   type-system/validator)
 * Replacing the implementation of an operation with a similar compatible one,
   e.g., (+) -> (-) (to simulate copy-and-paste induced bugs, inspired by one
   of the real bugs we found on `haskell-wasm` while developing this case study)


> how much time do the techniques require to generate tests?

We expect MUTAGEN to be not substantially slower than FuzzChick at generating
(or mutating) test cases. In both cases, the complexity is at most linear in the
size of the generated (or mutated) test case. 

However, test cases will not be fully generated/mutated unless the testing 
property actually fully evaluates them --- test cases are constructed "on the fly"
in tandem with the evaluation of the testing property. This is the case with most
random testing tools in Haskell, including QuickCheck. This lazy behavior greatly
difficults to measure generation/mutation overhead in a precise manner.

Reviewer #3
-----------

> It seems to be the case that Mutagen is effectively a new driver for
> QuickCheck. Is that right? How does one invoke Mutagen? Can you drop it in to
> replace QuickCheck? How is the tracing/coverage achieved?

Yes, MUTAGEN aims to be a drop-in replacement for QuickCheck as much as
possible. Our tool is invoked in the same way as QuickCheck, using an IO
function as a driver that accepts a testing property. 

The major difference lies in that MUTAGEN requires some extra constraints to be
satisfied, e.g., property inputs should be Mutable (automatically derivable
using Template Haskell). 

Tracing/coverage is achieved by instrumenting the user code using our own
source-to-source plugin to the Haskell compiler. The plugin can be enabled in a
per-module basis to add or remove tracing granularity if needed.


> How does Mutagen's memory profile compare to QuickCheck's and FuzzChick's?

Memory consumption in QuickCheck is almost negligible, as it is completely
black-box and "almost" stateless. FuzzChick, however, needs extra memory for two
purposes: the mutation queues and the edge-based tracing mechanism (a constant bitmap).
Like FuzzChick, MUTAGEN requires extra space for the mutation queues, whereas the
more precise path-based tracing mechanism implemented in MUTAGEN uses a trie data
structure that is expected to grow continuously as new executions are discovered.
In our experiments, we never observed this to be a problem, and multi-day executions
never exhausted the 32GB of memory available in our workstation. 

If memory consumption becomes a problem, MUTAGEN can be used with to edge-based
tracing, i.e., using a constant bitmap like AFL/FuzzChick, at the cost of less
trace granularity and a less precise FIFO prioritization of test cases (the only
metric available to prioritize them becomes the number of discovered edges.)


> How much retesting do you do? Do you ever run the exact same test twice?

To the best of our knowledge, there are two situations when retesting can occur:

- Randomly generating the same test case twice while on generation mode. This
depends on the quality of the random generator used.

- Mutating a test case back to its immediate ancestor. This can happen when a
mutation reverts a previous one. We initially introduced a complex mechanism to
avoid this, however, it did not show a big improvement, so we decided to keep
the mutation mechanism simple.


> In WASM, are you only testing single-module programs (p17)?

Yes. Testing multi-module interactions (a.k.a. WebAssembly scripts) using
MUTAGEN would be feasible, although we foresee that a more stateful approach
for mutating test cases would help to facilitate interaction between mutated 
WebAssembly modules --- we are currently investigating this possibility. 


> You fix 25 as the number of mutations in 'no reset' mode. Is that higher or
> lower than typical?

In our experiments, this depended heavily on the case study. The WebAssembly
case study requires almost no resets (since programs with new executions are
found continuously), whereas in the IFC stack machine case study we typically
observed about 6/7 resets before finding a counterexample. Setting the number of
random mutations to 25 was an arbitrary decision to evaluate what happened when
this parameter is *smaller* than the one optimized by MUTAGEN.


> Table 3 is just about the IFC example, right? What do these bug numbers
> correspond to? What's special about bugs 15 and 18 that makes FuzzChick so
> much better than Mutagen?

Yes. Bug numbers are systematically assigned. As mentioned above, FuzzChick is
better at finding bugs #15 and #18 when *it actually finds them*. In particular,
bug #18 is the second hardest one to find, where FuzzChick manages to find a
counterexample in only about 1 out of 4 executions. In this light, our tool trades
speed for effectiveness, finding the bugs reliably by using exhaustive mutations
at the cost of (potentially) needing longer to find some pathological bugs.
