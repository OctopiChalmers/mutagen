We thank the reviewers for their quality feedback.

R1Q1: Comparing MUTAGEN with FuzzChick directly is tricky as its code is not
maintained and only provides the IFC case study shown in our paper to compare
against. Given the good results we observed after comparing it against MUTAGEN,
we focused on showing how MUTAGEN compares against QuickCheck, the reference
tool in Haskell, to (hopefully) position it as a contender in that space. Any
lack of evaluation from there is simply due to time constraints. However, we
envision MUTAGEN being effectively used for testing programs that heavily
manipulate ASTs (e.g, compilers, interpreters) -- the kind of programs Haskell
shines at.

R1Q2: These results were performed directly in Coq.

R1Q3: Yes, mutations are applied to every subexpression of the input test case.
This is implemented modularly using Haskell's type class mechanism.

R2Q1.1: To the best of our knowledge, MUTAGEN is the first CGPT tool
implementation for Haskell. Hence, we not only aim to improve against FuzzChick,
but also against QuickCheck in order to advocate its adoption in that community.

R2Q1.2: We decided not to focus on degrading our tool with random mutations to
simulate FuzzChick's behavior because this would additionally require designing
a power schedule to assign energy to test cases. As we argue in the paper, the
implementation details of such component might drastically affect the
performance of the testing loop. Moreover, MUTAGEN uses a high-level tracing
mechanism (we instrument Haskell expressions), whereas FuzzChick repurposes
OCaml's AFL bitmap-based backend to get a trace signal. The information provided
by these two tracing backends is nos directly comparable, which further
complicates porting FuzzChick's power schedule into MUTAGEN.

R2Q1.3: Refer to R1Q1.

R2Q2.1: Exhaustiveness ensures that the same mutation is evaluated exactly once,
as opposed to perhaps none or perhaps a thousand times. This can both increase
reliabilty and reduce time spent on testing the same mutated test case. Our
intuition is that random mutation might perform better when the bug area is
relatively big, whereas exhaustive mutations are better when bugs need very a
input specific pattern to get triggered.

Regarding the missing related work, we will update our discussion accordingly
with the following remarks:

* [1]:

* [2]:

* [3]:

* [4]:

* [6]: