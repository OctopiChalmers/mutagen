OOPSLA 2021 Paper #131 Reviews and Comments
===========================================================================
Paper #131 Mutagen: Coverage-Guided, Property-Based Testing using
Exhaustive Type-Preserving Mutations


Review #131A
===========================================================================

Overall merit
-------------
4. Accept

Review confidence
-----------------
3. High

Paper summary
-------------
This paper presents *Mutagen*, a property-based random testing tool for Haskell that aims to improve generated test quality through various techniques, primary among them being the use of code coverage *a la* AFL and libfuzzer. Mutagen takes inspiration from *FuzzChick*, which also used code coverage, but makes several improvements, in particular to (a) mutation strategy, (b) the use of randomness (c) and the kind of coverage, favoring path-based rather than edge-based coverage (which is used in a novel queuing strategy). Put together, these techniques are shown to beat FuzzChick on its own case study, and to do well on a new case study on Haskell-wasm, beating vanilla QuickCheck tasked to find the same set of (injected) bugs.

Strengths and Weaknesses
------------------------
Strengths:

- The paper is well written, with extremely clear explanations and descriptions, and compelling justifications for why the proposed ideas should (and indeed seem to) work
- The ideas are interesting and in aggregate they are shown to be useful through a careful performance evaluation (which improves on FuzzChick's evaluation methodology)

Weaknesses:

- The test generation rate is very slow, e.g, 20x and 75x slower than QuickCheck for the shared example. This means that for targets that don't have sparse preconditions, or rare/complicated code paths, Mutagen may not be a very good idea. There is no evaluation exploring this potential problem, and no proposed solution for it.
- There are only two case studies; more examples would help convince us that these techniques have broader applicability.

Comments for author
-------------------
I enjoyed reading this paper. The proposed techniques make sense; I look forward to seeing where these techniques can go next, especially in being able to handle all properties well (enough), not just those with sparse preconds.

For the evaluation, I could use some more information: 
- FuzzChick is written in Coq/OCaml but Mutagen is written in Haskell; how do these differences manifest? It might be nice to see base generation rates for both, like in Table 4
- What versions of OCaml, Haskell, etc. were used? I presume the OCaml native compiler was used to compile FuzzChick, and the C libraries were compiled with optimizations enabled?
- The tools do not use parallelism, right?
- Some more explanation of the $A_{12}$ measure would be useful --- what is "Value" vs. "Estimate" ?

I might have liked to see a bit more careful comparison to related work in the fuzzing literature that's closest to Mutagen. A lot of work seems not too related, but work like CollAFL, which also uses path-based coverage, is hardly discussed. For example, does it use the same priority queuing strategy? What other differences are there?

Indeed, I wonder: Is path-based coverage useful for determining "interestingness" too, e.g., a path with the same edges but in a different order than a previous path might be considered interesting here, whereas for AFL it would not be. Or, is path coverage just useful for prioritization?

I'd also like to see some discussion about the weakness I mentioned above, regarding test generation rate.

Typos, nits:

p. 8: "turns to be" --> "turns out to be"

p. 14: The discussion in the second para of section 5 seems premature; why not defer talking about the injected bugs etc. until 5.2 when it's in context, e.g., when we know what the "validator" is?

Questions for authors’ response
---------------------------------
- How did you decide on what bugs to inject? How concerned are you about their realism? For the IFC example, there was a systematic strategy; what about for WASM?



Review #131B
===========================================================================

Overall merit
-------------
1. Reject

Review confidence
-----------------
3. High

Paper summary
-------------
The paper presents an approach for coverage-guided Property Based Testing (PBT). 
The main idea of PBT is to test systems by executing them on randomly generated
inputs, deriving new inputs from previously used one by applying mutation operators,
and checking executable properties that take the form of boolean predicates.
Concretely, the paper presents a technique that aims to improve on Lampropoulos'
FuzzChick tool by enumerating the possible mutations of a test case exhaustively,
and avoiding the need for a "power schedule" that specifies how much of the testing
budget should be dedicates to particular test cases. A number of heuristics are
explored, including the scheduling policy and the number of random mutations.
The work has been evaluated by way of two cases studies, involving systems
(IFC Stack Machine and WebAssembly Engine) into which faults have been seeded.
In these case studies, a comparison is made against a traditional PBT approach
using QuickCheck. I found the results difficult to interpret but it seems that
Mutagen generally finds more bugs and finds them more quickly than the traditional
PBT approach.

Strengths and Weaknesses
------------------------
  + fuzz testing is an important problem, and current PBT techniques such as
    FuzzChick have limitations that reduce their effectiveness

  + the paper presents some simple ideas that appear to improve the effectiveness
    of PBT

  - the intellectual contribution of the paper appears to be rather small

  - there is no direct comparison against FuzzChick, which appears to be the
    state of the art. In my opinion, some kind of direct comparison against
    FuzzChick or a reimplementation of FuzzChick is essential

  - in the case studies, it appears that the authors introduced the bugs 
    themselves (manually?), which creates the possibility of bias. It would be better
    to rely solely on real bugs, or on an existing tool for introducing bugs.

  - the experimental evaluation is poorly organized, presented and explained

  - the number of case studies/experiments (2) is too small, in my opinion

  - the paper suffers from turgid writing in many places

Comments for author
-------------------
Overall, the paper appears to make a modest technical/intellectual contribution.
Unfortunaty, the evaluation of the approach has several significant problems that
reduce my enthusiasm for the work.

  - p2, exhaustive generation of mutants: a question that immediately comes to mind is
    whether this approach is practical.  If the number of mutants thus created is very
    large, it seems that this can lead to space problems, and what is the point of
    storing very many mutants if there isn't sufficient time to execute them.

  - the terminology "energy" and "power schedule" is not a very useful metaphor. 
    Why not simply refer to the time budget associated with test cases?

  - please explain all the functions that are being referred to in Algorithms
    (e.g., "WithTrace", "Interesting") and refer to them in the text

  - I suggest adding line numbers to Algorithms and referring to these in the text

  - p8: "Despite this, the authors..."   Which authors are being referred to here?
    The present submission? Or Lampropoulos et al.?

  - p8: how is the notion "interesting" defined?

  - p10: please include a brief definition of the term Rose tree

  - p14: what is a hyper-property?

  - p15: "we can use a clever trick". Please avoid self-congratulatory wording like this.

  - I find the presentation of the case studies in two sections (section 5: experimental
    setup, section 6: presentation of results) not a very natural way of organizing the
    content. It would make more sense to me to present the setup and results for each
    case study in a separate section

  - p16: "we systematically introduce several bugs".  In general, it is problematic
    if authors introduce bugs in systems used to evaluate their own work, because
    this may introduce bias (authors may advertently or inadvertently introduce bugs
    that their system can easily find).  It would be better to rely on real bugs
    to avoid bias. At the very least, there should be a discussion about how bias
    was mitigated.

  - p16: "Unlike most other programming languages, its behavior is fully specified".
    I find this phrase inaccurate and needlessly offensive -- most languages (Java, C,
    C++, JavaScript) have a specification. While this may not always be formal and
    precise, it does define their behavior.

  - p17: it is a little disappointing that despite the very general nature of
    property-based testing, in the end, the evaluation amounts to running tests
    against a reference implementation. I would have preferred to see an
    evaluation that better reflects the generality of the approach.

  - the discussion of experimental results in section 6 is _very_ unclear. Some 
    specific issues:
      - it is confusing that the injected bugs are listed in a seemlingly
        arbitrary order in Fig 4
      - there is no discussion in the text of how these results should be interpreted
      - Table 3 suggests that Mutagen needs to generate more tests until failure
        than FuzzChick in at least 7 of the 20 cases.  Based on these results,
        and given the synthetic nature of the bugs, how can you argue that the
        Mutagen technique is really better?
      - that said, I'm even confused about the fact that there _is_ a comparison
        against FuzzChick, given that the paper states on page 14 that "Sadly,
        comparing Mutagen with FuzzChick has unfortunately been out of scope of this
        work.". Based on this text, shouldn't Table 3 refer to a "traditional PBT
        implementation"?
      - another issue with Table 3 is that it doesn't state how much time the
        techniques require to generate tests. 

  - according to Wikipedia, a mutagen is "a physical or chemical agent that permanently changes genetic material, usually DNA, in an organism and thus increases the frequency of mutations above the natural background level. As many mutations can cause cancer, such mutagens are therefore carcinogens, although not all necessarily are.".
   given this description, I wonder if it would be better to choose a name that does not
   have this kind of negative association

Minor comments:

  - abstract: "our systems" sounds a bit odd. How about just "systems"?

  - p4: "Such data structure" --> "Such a data structure"

  = p5: "...is in fact quite bad." Please explain why.

  - p18: "%100" --> "100%"

Questions for authors’ response
---------------------------------
- given the absence of a direct comparison against FuzzChick, how do you make the case that your technique is an improvement over the state of the art?

- on a related note, Table 3 suggests that Mutagen needs to generate more tests until failure than FuzzChick in at least 7 of the 20 cases. Based on these results, and given the synthetic nature of the bugs, how can you argue that the Mutagen technique is really better?

- how were bugs introduced in the benchmarks?  If you introduced these manually, what steps were taken to mitigate potential bias?

- how much time do the techniques require to generate tests?



Review #131C
===========================================================================

Overall merit
-------------
3. Weak accept

Review confidence
-----------------
3. High

Paper summary
-------------
Property-based testing (PBT) and fuzzing are powerful and popular
techniques for testing programs. Mutagen is a new, hybrid approach
that combines ideas from both disciplines.

Mutagen is in the QuickCheck lineage of PBT tools: generally speaking,
it generates random values as parameters for various
testing/validation properties, where crashes or failures of the
property indicate a bug. Such an approach to PBT depends on the qualiy
of the random value generators. If the acceptable domain of a function
is sparse---BSTs, well typed programs---then PBT will spend most of
its time discarding invalid inputs.

FuzzChick is prior work (for Coq) that uses ideas from fuzzing to
amplify the valid inputs it can find, randomly mutating valid inputs
to try to 'tweak' them into new (also valid) inputs.

Mutagen addresses several weaknesses in FuzzChick, improving its
coverage metric, refining the mutation process to allow pure and
randomized mutations, considering more mutations in more positions
more exhaustively, and using adaptive queueing to determine when to
use random mutations.

Mutagen is evaluated against FuzzChick on a well known IFC example
from the literature, ported to Haskell; it is evaluated against
QuickCheck to find new bugs and issues in a Haskell implementation of
WASM. (It can't be compared against FuzzChick on this example because
of Coq/Haskell language mismatches.)

Strengths and Weaknesses
------------------------
+ Important area.

+ Good refinement of existing techniques.

+ Lucid explanation.

+ Strong evaluation.

- Refinement, not wholesale novelty.

Comments for author
-------------------
I enjoyed the clarity of this paper, and its contribution is a nice
one. Mutagen is a tool I would use! I particularly like that this
paper presents its context well, and I suspect that one need only be
familiar with QuickCheck to understand the core ideas here.

The thing holding be back from a stronger acceptance score is the
sense of incrementality. Mutagen is a nice advance, but PBT/fuzzing is
a dense space... and Mutagen is not uniformly better than FuzzChick.

I wish there were more discussion of the implementation itself. How
does one invoke Mutagen? Can you drop it in to replace QuickCheck? How
is the tracing/coverage achieved? It seems to be the case that Mutagen
is effectively a new driver for QuickCheck. Is that right?

The discussion of the power schedule on page 2 confused. Mutagen's
enumerations imply _some_ schedule. I would have appreciated some
metrics on the numbers of mutations produced for different types (as
compared with FuzzChick). Similarly, when you talk about "novelty" on
p3, it'd be good to give a hint to how you measure that. Later on, you
fix 25 as the number of mutations in 'no reset' mode. Is that higher
or lower than typical?

When you cite 600 hours of computing time, how much of that is
parallelizable? The mutation queues could be shared between multiple
testers... have you explored this idea?

The critique of using only MTTF is a good one (p8), but it's also the
case that different fuzzers find different bugs---using exhaustiveness
on some particular suite of bugs risks biasing our research on fuzzers
to those that only find certain classes of bugs. We can see this
effect later on, when FuzzChick significantly outperforms Mutagen on a
few tests. Table 3 is just about the IFC example, right? What do these
bug numbers correspond to? What's special about bugs 15 and 18 that
makes FuzzChick so much better than Mutagen?

I wish you had included an evaluation of memory use---the move to
exhaustive generation of mutants means using more memory, at least in
principle. (Predicting how Haskell requires advanced witchcraft!)

The `positions` and `inside` functions remind me of Scrap Your
Boilerplate. Does your implementation use that? Would it benefit from
it?

I was confused by the discussion of enqueueing things for mutation
again (p14). How much retesting do you do? Do you ever run the exact
same test twice?

In WASM, are you only testing single-module programs (p17)?

In Figures 4 (resp. 5), you should indicate that the cases are sorted
by FuzzChick time (resp. QuickCheck), ascending. You should add dashed
lines on the graph to indicate 1min, 10min, 30min, and 1 hour (or some
other approriate breaks).

# Uneven language

The exposition of this paper is extremely clear with only a few
exceptions, but the language is uneven. I recommend a close "read
aloud" pass to find issues with agreement and idiomatic English.

p3 "strenght"

p4 "Such [a] data structure"

p5 "follows a simple type-directed fashion" sounds odd

p9 "confused by" s/by/with/

p10 "resolve in" odd word choice

p12 "In contrast, execution tractes [missing word] Mutagen"

p14 "out of the scope of this [part of the] work" ?

p17 "a with a"

p18 "Lampropoulos et al.."

p23 "in this particular scenario" do you mean on these particular
systems under test, these properties, both, or something else?

# Minutae

Please submit with `review` in the options of `\documentclass`, so
that reviews can make use of line numbers.

I find listings of pseudocode much easier to read with sans-serif or
typewriter faces.

You should write the percent sign after the number, i.e., 100%, not
%100.

I appreciate that your color choices in Figures 4 and 5 are distinct
in B&W. Thank you!

Questions for authors’ response
---------------------------------
It seems to be the case that Mutagen is effectively a new driver for
QuickCheck. Is that right? How does one invoke Mutagen? Can you drop
it in to replace QuickCheck? How is the tracing/coverage achieved?

How does Mutagen's memory profile compare to QuickCheck's and
FuzzChick's?

How much retesting do you do? Do you ever run the exact
same test twice?

In WASM, are you only testing single-module programs (p17)?

You fix 25 as the number of mutations in 'no reset' mode. Is that
higher or lower than typical?

Table 3 is just about the IFC example, right? What do these bug
numbers correspond to? What's special about bugs 15 and 18 that makes
FuzzChic so much better than Mutagen?
