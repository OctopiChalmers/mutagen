Negative comments:

* The test generation rate is very slow, e.g, 20x and 75x slower than QuickCheck for the shared example. This means that for targets that don't have sparse preconditions, or rare/complicated code paths, Mutagen may not be a very good idea. There is no evaluation exploring this potential problem, and no proposed solution for it.

* There are only two case studies; more examples would help convince us that these techniques have broader applicability.

* For the evaluation, I could use some more information:
	+ FuzzChick is written in Coq/OCaml but Mutagen is written in Haskell; how do these differences manifest? It might be nice to see base generation rates for both, like in Table 4
	+ What versions of OCaml, Haskell, etc. were used? I presume the OCaml native compiler was used to compile FuzzChick, and the C libraries were compiled with optimizations enabled?
	+ The tools do not use parallelism, right?
	+ Some more explanation of the $A_{12}$ measure would be useful --- what is "Value" vs. "Estimate" ?

* I might have liked to see a bit more careful comparison to related work in the fuzzing literature that's closest to Mutagen. A lot of work seems not too related, but work like CollAFL, which also uses path-based coverage, is hardly discussed. For example, does it use the same priority queuing strategy? What other differences are there?

* Indeed, I wonder: Is path-based coverage useful for determining "interestingness" too, e.g., a path with the same edges but in a different order than a previous path might be considered interesting here, whereas for AFL it would not be. Or, is path coverage just useful for prioritization?

* p. 14: The discussion in the second para of section 5 seems premature; why not defer talking about the injected bugs etc. until 5.2 when it's in context, e.g., when we know what the "validator" is?

* the intellectual contribution of the paper appears to be rather small

* in the case studies, it appears that the authors introduced the bugs themselves (manually?), which creates the possibility of bias. It would be better to rely solely on real bugs, or on an existing tool for introducing bugs.

* the number of case studies/experiments (2) is too small, in my opinion

* p2, exhaustive generation of mutants: a question that immediately comes to mind is whether this approach is practical. If the number of mutants thus created is very large, it seems that this can lead to space problems, and what is the point of storing very many mutants if there isn't sufficient time to execute them.

* the terminology "energy" and "power schedule" is not a very useful metaphor. Why not simply refer to the time budget associated with test cases?

* please explain all the functions that are being referred to in Algorithms (e.g., "WithTrace", "Interesting") and refer to them in the text

* I suggest adding line numbers to Algorithms and referring to these in the text

* p8: "Despite this, the authors..." Which authors are being referred to here? The present submission? Or Lampropoulos et al.?

* p8: how is the notion "interesting" defined?

* p10: please include a brief definition of the term Rose tree

* p14: what is a hyper-property?

* I find the presentation of the case studies in two sections (section 5: experimental setup, section 6: presentation of results) not a very natural way of organizing the content. It would make more sense to me to present the setup and results for each case study in a separate section

* p16: "we systematically introduce several bugs". In general, it is problematic if authors introduce bugs in systems used to evaluate their own work, because this may introduce bias (authors may advertently or inadvertently introduce bugs that their system can easily find). It would be better to rely on real bugs to avoid bias. At the very least, there should be a discussion about how bias was mitigated.

* p16: "Unlike most other programming languages, its behavior is fully specified". I find this phrase inaccurate and needlessly offensive -- most languages (Java, C, C++, JavaScript) have a specification. While this may not always be formal and precise, it does define their behavior.

* p17: it is a little disappointing that despite the very general nature of property-based testing, in the end, the evaluation amounts to running tests against a reference implementation. I would have preferred to see an evaluation that better reflects the generality of the approach.

* the discussion of experimental results in section 6 is very unclear. Some specific issues:

	+ it is confusing that the injected bugs are listed in a seemlingly arbitrary order in Fig 4
there is no discussion in the text of how these results should be interpreted

	+ Table 3 suggests that Mutagen needs to generate more tests until failure than FuzzChick in at least 7 of the 20 cases. Based on these results, and given the synthetic nature of the bugs, how can you argue that the Mutagen technique is really better?

	+ that said, I'm even confused about the fact that there is a comparison against FuzzChick, given that the paper states on page 14 that "Sadly, comparing Mutagen with FuzzChick has unfortunately been out of scope of this work.". Based on this text, shouldn't Table 3 refer to a "traditional PBT implementation"?

	+ another issue with Table 3 is that it doesn't state how much time the techniques require to generate tests.

* abstract: "our systems" sounds a bit odd. How about just "systems"?

* p4: "Such data structure" --> "Such a data structure"

* p5: "...is in fact quite bad." Please explain why.

* p18: "%100" --> "100%"

* The thing holding be back from a stronger acceptance score is the sense of incrementality. Mutagen is a nice advance, but PBT/fuzzing is a dense space... and Mutagen is not uniformly better than FuzzChick.

* I wish there were more discussion of the implementation itself. How does one invoke Mutagen? Can you drop it in to replace QuickCheck? How is the tracing/coverage achieved? It seems to be the case that Mutagen is effectively a new driver for QuickCheck. Is that right?

* The discussion of the power schedule on page 2 confused. Mutagen's enumerations imply some schedule. I would have appreciated some metrics on the numbers of mutations produced for different types (as compared with FuzzChick). Similarly, when you talk about "novelty" on p3, it'd be good to give a hint to how you measure that. Later on, you fix 25 as the number of mutations in 'no reset' mode. Is that higher or lower than typical?

* When you cite 600 hours of computing time, how much of that is parallelizable? The mutation queues could be shared between multiple testers... have you explored this idea?

* The critique of using only MTTF is a good one (p8), but it's also the case that different fuzzers find different bugs---using exhaustiveness on some particular suite of bugs risks biasing our research on fuzzers to those that only find certain classes of bugs. We can see this effect later on, when FuzzChick significantly outperforms Mutagen on a few tests. Table 3 is just about the IFC example, right? What do these bug numbers correspond to? What's special about bugs 15 and 18 that makes FuzzChick so much better than Mutagen?

* I was confused by the discussion of enqueueing things for mutation again (p14). How much retesting do you do? Do you ever run the exact same test twice?

* In Figures 4 (resp. 5), you should indicate that the cases are sorted by FuzzChick time (resp. QuickCheck), ascending. You should add dashed lines on the graph to indicate 1min, 10min, 30min, and 1 hour (or some other approriate breaks).

* Please submit with review in the options of \documentclass, so that reviews can make use of line numbers.

* Small novelty over FuzzChick

* Evaluation on only two programs (where one seems rather small) and no full comparison to the state-of-the-art

* One major issue is the limited novelty over prior work, especially FuzzChick. The "big idea" of combining coverage-guided fuzzing, property-based testing, and high-level mutations of interesting inputs has already been described in the FuzzChick. What's left as novelty here is replacing random mutation with exhaustive mutation, which is a straightforward change, and heuristics to tune the approach. Overall, this makes the paper too incremental to support an OOPSLA paper based on its novelty.

* The major issue is the evaluation, which could (but unfortunately doesn't) compensate for the limited novelty:

	+ As shown in several prior papers (e.g., see Klees et al., CCS'18), the selection of benchmarks may heavily influence whether one fuzzing technique outperforms another. The only way to reduce the risk of drawing conclusions that won't generalize is to use a large set of benchmarks. However, the evaluation presented here uses only two benchmark programs, one of which doesn't even seem to qualify as "real-world".

	+ For the one and only more complex benchmark program (haskell-wasm), the evaluation doesn't compare against the state-of-the-art. Given the similarity to FuzzChick, this would be a good candidate for a comparison. (I understand the problem of different target languages, but authors are free to choose which language they target, and should consider what it implies for the evaluation when making this choice.)

	+ Most of the bugs used in the evaluation have been manually injected by the authors. Are these bugs realistic? How do we know that there wasn't any (subconscious) bias toward bugs where the new approach works well? A more convincing setup would be to use known, real bugs (e.g., from past issues in the target programs) or a much larger set of automatically injected bugs.

* Other suggestions for improving the paper:

	+ The paper should provide statistics about the benchmark programs, e.g., their number of lines of code.

	+ As the evaluation target PL implementations, the related work should discuss the vast amounts of work on automated compiler testing (e.g., see Chen et al., CSUR'20, for a recent survey).
