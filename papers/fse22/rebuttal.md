We thank the reviewers for their valuable feedback. We focus on addressing the
concerns from Reviewer #1 and Reviewer #2.

R1Q1: Comparing MUTAGEN with FuzzChick directly is tricky as its code is not
maintained and only provides the IFC case study shown in our paper to compare
against. Given the good results we observed after comparing FuzzChick against
MUTAGEN, we focused on showing how MUTAGEN compares against QuickCheck, the
reference tool for property-based testing, under a real-world scenario. Any lack
of evaluation from there is due to time constraints. However, we argue that the
WebAssembly case study is representative of the scenario we envision MUTAGEN
being used effectively, i.e., testing programs that heavily manipulate complex
ASTs (e.g, compilers, interpreters), which are the kind of programs where our
type-driven approach excels at. Other testing scenarios comprising non-sparse
properties or with simpler input structures will most likely see better results
using QuickCheck due to its higher test throughput.

R1Q2: These results were performed directly in Coq.

R1Q3: Yes, mutations can be applied to every subexpression of the input test
case, regardless of its type. This is modularly implemented, and every
type-specific mutator can be automatically synthesized at compile time.

R2Q1.1: To the best of our knowledge, MUTAGEN is the first CGPT tool
implementation for Haskell. Moreover, our custom plugin-based high-level tracing
mechanism is also novel and provides flexibility not easily achievable with
low-level backends like `afl-gcc`, e.g., tracing only the coverage of certain
functions/modules as opposed to the entire binary. Hence, we not only aim to
improve against FuzzChick but also against QuickCheck to advocate for its
adoption in the Haskell community. In addition, the comparison with QuickCheck
gives us a baseline for MUTAGEN's overhead to improve upon.

Ale: Here it is too nich, why would they care about the Haskell community? Can
we rephrase it with more top-level arguments applicable to testing in general?

R2Q1.2: We decided not to focus on adapting our tool with random mutations to
simulate FuzzChick's behavior because this would additionally require designing
a power schedule to assign energy to test cases for the comparation to be fair.
As we argue in the paper, the implementation details of such components might
drastically affect the performance of the testing loop. Moreover, MUTAGEN uses a
high-level tracing mechanism (we instrument Haskell expressions), whereas
FuzzChick repurposes OCaml's AFL bitmap-based backend to get a trace signal. The
information provided by these two tracing backends is not directly comparable,
which further complicates porting FuzzChick's power schedule into MUTAGEN.

R2Q1.3: Refer to R1Q1.

R2Q2.1: Exhaustiveness in MUTAGEN ensures that the same mutation is evaluated
exactly once, as opposed to perhaps none or perhaps a thousand times. This can
both increase reliability and reduce time wasted on repeated effort. Our
intuition is that random mutation might perform better when the bug area is
relatively big, whereas exhaustive mutations are better when bugs need very a
input specific pattern to get triggered.


Regarding the missing related work, we will update our discussion accordingly.
In particular, we remark that:

* If we establish the similarity between grammars and values described by
  algebraic data types, both MUTAGEN and [1] aim to cover all grammar features
  of the input space. However, MUTAGEN uses the trace feedback to decide when to
  grow recursive grammar nodes one step at a time, whereas [1] unfolds these
  steps into exhaustively testing k-grams pairs of grammar constructions.

* In contrast to Zest, which mutates the random choices made by generators
  producing syntactically valid inputs, MUTAGEN mutates the inputs themselves.
  This gives us an advantage to perform certain optimizations, e.g., leveraging
  lazy evaluation to avoid mutating the unevaluated subexpressions of a given
  input -- this paper does not yet demonstrate this feature empirically.

* Similar to MUTAGEN, BeDivFuzz separates mutations into "structure-changing"
  and "structure-preserving", which closely relate to MUTAGEN's PURE and RANDOM
  mutant kinds, respectively. In BeDivFuzz, this distinction is used to search
  for diverse input structures (via structure-changing mutations) and then apply
  structure-preserving mutations to them to produce structure-equivalent
  variants. In MUTAGEN, the distinction is used to avoid testing *every*
  structure-equivalent variant exhaustively.

* While RLCheck seems to effectively avoid P4, we argue that MUTAGEN still
  follows a valuable approach that prioritizes simplicity: instead of adding
  reinforcement learning to dynamically tune the power schedule, our approach
  simply removes it altogether. Simplicity is, of course, not a valid metric to
  find bugs, but serves as a clear baseline to improve upon.

* The MOpt mutation scheduling optimization designed by Lye et al. is effective
  at improving the behavior of the bit-level mutators used in traditional
  fuzzing tools. However, we argue that bit-level mutations are still far from
  the expressive power achieved by high-level (as well as grammar-based)
  mutation tools, where a single mutation can, e.g., transform an `if` statement
  into a `while` loop by operating directly at the AST level.