# The fragmented world of Prolog packages and how to navigate it

## Logtalk provides the closest thing to cross-engine compatibility

**Logtalk functions as a "Babel for Prolog"**—an object-oriented logic programming language that compiles down to 14 different Prolog backends through adapter files. It supports SWI-Prolog, SICStus, GNU Prolog, YAP, ECLiPSe, XSB, Scryer, Trealla, Tau Prolog, and several others.

Logtalk's **bundled library** is genuinely portable and substantial:

- **Data structures**: lists, sets, queues, heaps, AVL trees, red-black trees, zippers, union-find
- **I/O and parsing**: json, csv, tsv, xml_parser, base64, cbor
- **Meta-programming**: maplist-style predicates, fold, filter operations
- **OS abstraction**: portable filesystem operations, environment variables, process control
- **Type checking**: 40+ type predicates, QuickCheck-style arbitrary value generation
- **Utilities**: random numbers, dates, UUIDs, optional/expected monads, logging

Logtalk also has its own **packs tool** (currently in beta)—a decentralized package manager that works across backends. Users add registries they trust, and packages work on any supported Prolog implementation.

For serious cross-engine development, Logtalk is the pragmatic choice. You write Logtalk code once, and it runs on whichever Prolog backend you prefer. The tradeoff is learning Logtalk's object-oriented paradigm on top of Prolog.

## No "lodash for Prolog" exists, but alternatives emerge

There's no single universally-adopted utility library equivalent to lodash. The closest resources:

**Wouter Beek's Prolog Library Collection** (github.com/wouterbeek/prolog_library_collection) provides DCG utilities, date/time handling, HTTP enhancements, and dictionary extensions—but targets SWI-Prolog specifically.

**awesome-prolog** (github.com/klaudiosinani/awesome-prolog) with 530+ stars curates implementations, learning resources, testing frameworks, database libraries, and AI tools. It's the best starting point for discovering what exists.

The fundamental issue is that ISO Prolog standardizes only basic language features—syntax, core built-ins, arithmetic, I/O streams, control structures—but explicitly **not** utility libraries. Each implementation built its own extensions independently.

## Curated collections and modern implementations offer hope

**Scryer Prolog** (github.com/mthom/scryer-prolog) represents a modern standards-focused effort. Written in Rust, it aims to be "what GHC is to Haskell"—a production-quality system with strict ISO conformance. It includes built-in libraries for constraints (clpz, clpb), cryptography, HTTP, and DCGs. Scryer's commitment to ISO makes it a natural testing ground for portable code.

**The SWI-Prolog GitHub organization** hosts 50+ repositories covering HTTP, JSON, SWISH web IDE, and C++ interfaces. Combined with the pack registry, SWI offers the richest single-implementation ecosystem.

For developer tooling, Logtalk bundles **lgtunit** (unit testing with QuickCheck), **lgtdoc** (API documentation), **diagrams** (inheritance/dependency visualization), **debugger**, **dead_code_scanner**, and **code_metrics** (cyclomatic complexity, Halstead metrics)—all portable across backends.

## Why fragmentation persists despite decades of effort

The 2022 paper "Fifty Years of Prolog and Beyond" in Theory and Practice of Logic Programming represents the community's recognition of these challenges. Authors from multiple implementations performed a SWOT analysis and proposed directions for improving compatibility—but acknowledged that "programming in the large is considered complex because not all Prolog compilers support modules" and "there are compatibility problems between the module systems of the major compilers."

## Practical strategies for navigating the ecosystem

Given this landscape, several approaches make sense depending on your needs:

**For cross-engine portability**, Logtalk is the established solution. Its adapter-based architecture, portable libraries, and package manager handle the compatibility work. The cost is learning an additional language layer.

**For maximum standards compliance**, Scryer Prolog plus careful adherence to ISO core predicates offers the cleanest path. Code written this way has the best chance of working across implementations—but you'll sacrifice many conveniences that implementations provide as extensions.

**For "polyfill" style compatibility**, use conditional compilation (supported by ECLiPSe, GNU, SICStus, SWI, YAP, XSB, Ciao, Scryer):

```prolog
:- if(current_prolog_flag(dialect, swi)).
    % SWI-specific code
:- elif(current_prolog_flag(dialect, gnu)).
    % GNU-specific code  
:- endif.
```