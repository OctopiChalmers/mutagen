ESEC/FSE 2022 Paper #61 Reviews and Comments
===========================================================================
Paper #61 Mutagen: Coverage-Guided, Property-Based Testing using Exhaustive
Structure-Preserving Mutations


Review #61A
===========================================================================

Overall merit
-------------
3. Weak accept

Paper summary
-------------
The paper presents MUTAGEN, a new technique for coverage-guided property-based testing (CGPT), implemented in Haskell. MUTAGEN's main contributions include (1) automatic generation of deterministic (exhaustive) and random mutators based on data type definitions, (2) an adaptive technique for scheduling random mutations, and (3) the use of LIFO queueing of newly discovered valid test inputs to speed up exploration. An evaluation is presented on an IFC Stack Machine implementation and a WASM implementation in Haskell; the results show that MUTAGEN improves over FuzzChick on IFC and on Quickcheck on haskell-wasm in terms of reliability and speed of uncovering (mainly planted) bugs.

Strengths
---------
+ Enumerates and addresses important problems faced by current CGPT techniques
+ Novel solution of deriving systematic mutations based on type definitions
+ Interesting case studies on IFC and WASM
+ Separate evaluation of heuristics-based design decisions (LIFO queuing and random adaptation)

Weaknesses
----------
- Evaluation scope is quite small (only two target programs)
- Missing a bunch of related work in this space

Comments for author
-------------------
The paper is well written and easy to read. 

The paper seems to simultaneously address a number of problems that arise with Lampropoulos et al.'s formulation of CGPT:

P1: Random-only structure-preserving mutations on input generators may miss out on interesting cases that arise due to a specific substructures.

P2: Top-down recursive application of mutations on inputs produced by the generator turn out to be "shallow" because deeply nested subexpressions have low probability of being mutated.

P3: AFL-style mutation scheduling is ineffective.

P4: Heuristics require fine tuning and can be hard to generalize.

The paper presents several innovations to address these concerns. P1 and P2 are addressed using a scheme for systematically deriving exhaustive mutators, which I found to be novel and well executed. P3 and P4 are addressed by altering the mutation queueing strategy and power schedules; I found these contributions to be a bit more hand-wavy, but these design decisions were evaluated independently, which is good.

The main disappointment I had when reading the paper was the lack of discussion of or comparison with related work that has addressed P1--P4. For example, Havrikov and Zeller [1] have proposed an algorithm for systematically covering input structure in a bounded fashion. MOPT [2] uses particle-swarm optimization to address P3. More significantly, the paper ignores a number of closely related developments in CGPT over Java programs using JQF. For example, Zest [3] performs mutations on the bytes representing underlying pseduo-random choices in input generators, thereby avoiding problem P2. RLCheck [4] avoids P4 by using reinforcement learning to adapt based on program feedback. More recently, BeDivFuzz [3] proposes templates for structure-preserving mutations, which would also partially address P1.  The last two papers actually use binary tree generators as a running example, making it possible to draw many conceptual similarities to the issues presented in this paper.

I understand that MUTAGEN takes advantage of the type system in functional languages such as Haskell, and thus the innovations presented in this paper are not directly applicable to fuzzing Java or C programs. However, the paper should make this nuance explicit rather than purport to be identifying the above problems in CGPT/fuzzing for the first time.

The evaluation presented in this paper provides a very detailed case study of IFC and haskell-wasm. However, just having two target programs is quite a small sample size for a paper on testing / fuzzing. The "threats to validity" section does not mention ANY threats to external validity. How do we know the results will generalize beyond these two targets? The paper would be much stronger if there were more target programs to apply MUTAGEN on.

Finally, I was confused by the discussion on MTTF when some runs do not find a bug. The paper says (line 893): "since the MTTF aggregates all runs regardless of if they found a bug or timed out, this metric is sensitive to the timeout used on each experiment". I do not think this is a sound measurement. MTTF should only aggregate over runs where the bug was found. If a bug was found in only one run but very quickly, then it's MTTF will be very small but it's reliability will be very low; that's fine. Calculating an MTTF that uses the timeout value as the TTF for runs where a bug was not found leads to numbers that are not very meaningful (as the paper also identifies).



# References

[1] Havrikov and Zeller, "Systematically covering input structure", ASE 2019

[2] Lyu et al. "MOPT: Optimized mutation scheduling for fuzzers", USENIX Sec'19

[3] Padhye et al. "Semantic Fuzzing with Zest", ISSTA 2019

[4] Reddy et al. "Quickly generating diverse valid test inputs with reinforcement learning", ICSE 2020

[5] Nguyen and Grunske, "BeDivFuzz: Integrating Behavioral Diversity into Generator-based Fuzzing", ICSE 2022


# Minor
- Typo on line 235: "libFuzzer 2019"
- Line 392: Should there be subscript for `def` to indicate that it is type specific, e.g. `def_t`?
- In several places, the paper uses font colors (red and blue) to highlight parts of tables. These colors don't show up in a greyscale printed version of the paper. I recommend adding a shaded background or other highlight that works with greyscale.
- Typo on line 781: "where reported" --> "were reported"


# Funny
- Use of term "boring" for non-"interesting" inputs. Gave me a chuckle.

Important questions for authors to respond to during the rebuttal
-----------------------------------------------------------------
Q1. Why were only two target programs chosen to evaluate MUTAGEN? What are some examples of other Haskell programs where MUTAGEN might be useful?

Q2. For the results showing FuzzChick's performance on the IFC stack machine (e.g. Figure 3), was this performed in Coq or was FuzzChick ported to Haskell?

Q3. Will the PURE mutators still be useful when interesting input types (e.g. trees) are encapsulated in other data structures (e.g. a list)? For example, if the property test took as input a list of trees (`[Tree int]`), then will the pure mutations be applied only on the data constructors for the list type, or will it also perform subtree swaps, etc. as shown in Figure 1?



Review #61B
===========================================================================

Overall merit
-------------
2. Weak reject

Paper summary
-------------
Coverage-guided, property-based testing (CGPT) was proposed by
Lampropoulos et al. [30] to leverage coverage feedback popularized by
tools like AFL to improve random property-based testing. This paper
introduces the Mutagen framework with the goal of improving the
performance and effectiveness of CGPT. Specifically, it identifies
some aspects of CGPT that could be improved: random mutations are
often ineffective at transforming tests, and interesting tests are not
well prioritized and scheduled.

The key idea behind Mutagen is to apply mutations exhaustively.
Specifically, given an interesting test, it precomputes and schedules
every applicable structure-preserving mutation exactly once to avoid
omitting interesting transformations because of randomness (thus, a
power schedule also becomes unnecessary).

It distinguishes two types of mutations: deterministic mutations that
can be exhaustively applied, and nondeterministic mutations that need
to be randomly sampled (e.g., numeric values). It also has two
heuristics to improve scalability and effectiveness: It uses LIFO
scheduling with priority (defined in terms of "novelty" of a test with
respect to existing ones), and it dynamically controls the frequency
of the random mutations by monitoring how often interesting tests are
generated (e.g., increasing random mutations when this frequency
stalls).

Mutagen was realized and evaluated via two case studies that require
generating structured inputs: the Information-Flow Control (IFC) stack
machine used by Lampropoulos et al.'s FuzzChick [30], and an existing
WebAssembly engine in Haskell. In the IFC stack machine case study,
Mutagen was shown to outperform FuzzChick with respect to time to
failure and failure rate. The WebAssembly case study demonstrates
Mutagen's usage in a more realistic setting. Mutagen was shown to
reliably find 15 injected bugs in the validator and interpreter, and
an additional 3 previously unknown bugs. In this case study, the paper
also evaluates Mutagen's performance versus QuickCheck, the
widely-used RPBT tool for Haskell.

Strengths
---------
S1: Improving coverage-guided, property-based testing (CGPT) is an
interesting, relevant problem to help expand CGPT's effectiveness and
applicability for testing settings that require highly structured
inputs.

S2: The paper introduces novel techniques and heuristics in the CGPT
context that are realized in the tool Mutagen and evaluated via two
case studies.

S3: The paper is well written and clearly presented.

Weaknesses
----------
W1: I find the provided evidence not entirely convincing, especially
regarding some of the work's key design choices, e.g., the exhaustive
mutation. It would be important to better understand their tradeoffs.

W2: The second case study for WebAssembly is unfortunately not with
respect to FuzzChick, but QuickCheck, so I believe that the comparison
is not quite meaningful and insightful.

W3: Related to W2, instead of only two case studies, it would have
been more compelling and helpful to understand the generality of the
results if Mutagen had been evaluated on more subjects. I wonder
whether there were any challenges that prevented such a more extensive
evaluation.

Comments for author
-------------------
The topic of the paper is relevant and interesting; more practical
CGPT will find many applications, and the problem definitely needs
more attention from the community. I am happy that this work tackles
this problem head-on. The work is based on novel insights and
observations, and appears to be carefully and systematically
developed. The paper is also a pleasure to read. It is well written,
and both the high-level motivations and intuitions, and the detailed
techniques are clearly presented.

My main reservations are with respect to the work's evaluation and the
presented evidence, which I will detail with respect to the weaknesses
mentioned above.

The paper comments: "Unfortunately, comparing Mutagen with FuzzChick
on this case study has been out of the scope of this work, since
porting haskell-wasm into Coq, and later adapting our test suite to
work with FuzzChick requires a subtantial effort."

I follow this argument, but would argue that porting haskell-wasm into
Coq would not be needed to allow for an evaluation of the main
novelties and claims of the work, such as that exhaustive mutation
generation is superior than FuzzChick's random mutations:

(1) Comparing Mutagen with QuickCheck does not help evaluate Mutagen's
novelties as Mutagen's goal is to improve upon CGPT as realized in
FuzzChick, not RPBT as realized in QuickCheck.

(2) One could take Mutagen and perform an ablation study like the
paper did in parts of its evaluations. In particular, one could
customize Mutagen, instead of using exhaustive mutation, to use random
mutation as in FuzzChick. In addition to the existing evaluation,
this should allow a more direct and meaning assessment of the the main
techniques in Mutagen.

(3) With the above setup, one could also be able to perform evaluation
on more subjects. The two case studies are interesting, but they do not
help understand the generality and robustness of the results.


Other/minor:

C1: Figure 1

Why would it be the most profitable to enumerate exhaustively? Would
for example being exhaustive with respect to a random subset also be a
viable option? This question is minor and is purely for curiosity.

C2: Section 4.1: "Our assumption is that test case executions
branching at shallower depths from already executed ones are more
likely to discover new portions of the system under test, and hence we
want to prioritize them."

Intuitively, this assumption does make sense, but is there some
empirical evidence to support it?

Important questions for authors to respond to during the rebuttal
-----------------------------------------------------------------
Q1: Please respond to the comments regarding the evaluation against
QuickCheck on the WebAssembly case study.

Q2: Please respond to the comments regarding why the exhaustive
enumeration is generally superior vs. random enumeration as in
FuzzChick.



Review #61C
===========================================================================

Overall merit
-------------
3. Weak accept

Paper summary
-------------
The paper introduces Mutagen, an enumeration-based approach to coverage-guided, property-based testing that is reminiscent of Korat which uses a method precondition to automatically generate all (nonisomorphic) test cases up to a given small size and then executes the method on each test case using the method postcondition as a test oracle. The key idea is to exhaustively enumerate the structure of the input objects and to randomly generate values for "large enumeration types", such as the integers. The authors propose two heuristics: (i) to fuzz seeds first that were added last and to fuzz seeds with higher priority that diverge earlier from the previously exercised paths, and (ii) to dynamically double the number of inputs generated from a seed (by fuzzing large enumeration types) every time the discovery rate for coverage-increasing inputs drops below a certain value (e.g., 1000 non-coverage-increasing generated inputs). The approach is evaluated on two case studies.

Strengths
---------
+ Significance: The hypothesis is valid and interesting to explore. Moreover, Coverage-guided, property-based testing already finds application in practice (e.g., Hypothesis in Python). Hence, it is important to improve the efficiency of this technique, as proposed in this paper.
+ Presentation: The problem is well motivated. The algorithms give a nice procedural overview of the proposed technique. The running example and case studies well illustrate the challenges of existing techniques and the opportunities of the presented technique.

Weaknesses
----------
- Evidence: The evidence for the central claim is not very convincing. We only see results for two case studies.

Comments for author
-------------------
I enjoyed reading this paper. It starts with a nice overview of recent progress in property-based testing and goes on to illustrate the opportunities and procedure of Mutagen. From a fuzzing perspective, I was concerned at first that an (exhaustive) enumeration is often outperformed by a randomize approach. For instance, in AFL there exists a "deterministic mode" which enumerates all mutators for every byte in every seed. Deterministic mode had been default-enabled until recently when experiments showed that disabling it actually substantially improves the coverage achieved over time. However, after thinking about it for a bit, I feel like an enumeration of structural variants of a seed (versus of the "flat" representation of the input) is different. I am happy that the case studies seem to support this intuition, but an empirical evaluation would bring more convincing evidence. In fact, I feel the paper would be substantially stronger if only a proper experiment was conducted that involved multiple programs and a statistical analysis.

In Section 3.1, the authors define their own mutators. I am wondering whether (or how) structural mutators discussed in previous work (e.g., FuzzChick) can be reused? Given a seed and a set of structural mutations, the total set of seed variants can also be enumerated, right?

In Section 3.2, we learn about a procedure invoked when both queues are empty. Can this ever happen? I thought every generated input that does not increase coverage will still end up in the queue of "discarded test cases", no?

In Section 4.1, I am wondering whether the LIFO scheduling would imply something like a depth-first search?

MINOR
* The difference between test cases that are "discarded" and those that are "thrown away" should be made more clear. There is a queue of discarded test cases.
* The paper talks about exhaustive enumeration (3.1.) and random mutations with a bounded (but increasing) number of random mutation (4.2). It would help if you could more explicitly distinguish enumeration of structure and the random mutation of "large enumeration types".
* In Section 3.1, it would help if you could elaborate on your notation. For instance, in line 362, what is i and j?
* References [25] and [26] are duplicated.

Important questions for authors to respond to during the rebuttal
-----------------------------------------------------------------
The authors are welcome to address the questions of the other reviewers.
