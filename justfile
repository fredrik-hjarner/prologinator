# ============================================================================
# Ciao Prolog commands
# ============================================================================
# NOTE: In Ciao Prolog, load files with:
#   - use_module('filename.pl') for modules
#   - ensure_loaded('filename.pl') for non-modules
#   NOT [filename] like in SWI-Prolog!
#
# Check if a Prolog file compiles (reports errors/warnings, exits on error)
# Usage: just check <filename>
# Example: just check game
check file:
	cd prolog && rm -f {{file}}.po {{file}}.itf && ciaoc -c {{file}}.pl

# Test if a Prolog file is compatible with Ciao Prolog (interactive)
# Usage: just consult <filename>
# Example: just consult game
consult file:
	cd prolog && rlwrap ciaosh -e "use_module('{{file}}')"

# Test if the operators actually work in Ciao Prolog
# This loads the operators module and tests parsing of operator expressions
# ciao-test-operators:
# 	cd prolog && ciaosh -e "use_module('operators'), X = atk:hi, Y = (atk:hi & atk:lo), Z = ((atk:hi & atk:hi) vs (atk:lo & atk:lo)), write('Operators work!'), nl, write('X = '), write(X), nl, write('Y = '), write(Y), nl, write('Z = '), write(Z), nl" -t halt

# Start Ciao Prolog interactive shell (with readline support via rlwrap)
ciao:
	rlwrap ciaosh

# Run unit tests
# Usage: just test
# Example: just test
# Note: Tests are defined using :- test assertions in the source file
test:
	cd prolog && \
		rm -f run_tests.po run_tests.itf && \
		ciaosh -u run_tests.pl -t halt 2>&1 | tee /tmp/test_output.txt && \
		(grep -q "Passed:.*100.00%" /tmp/test_output.txt && echo "✅ All tests passed" && exit 0) || \
		(echo "❌ Tests failed" && exit 1)

# Clean Ciao Prolog compilation artifacts (.itf, .po, .asr, .ast files)
clean:
	find prolog -name "*.itf" -o -name "*.po" -o -name "*.asr" -o -name "*.ast" | xargs rm -f

# ============================================================================
# CiaoPP static analysis commands
# ============================================================================
# Available mode domains (for -fmodes flag):
#   Fast domains: pd, pdb, gr, def, eterms, ptypes
#   Medium domains: share, son, shareson, sharefree
#   Slow/TIMEOUT domains: shfr (default - TIMEOUTS >120s), shfrson, shfrnv,
#                         sharing_amgu, sharing_clique, sharefree_amgu, etc.
# NOTE: Some analysis domains can be very slow or timeout:
#   - shfr (default): TIMEOUTS on complex files (>120s) - DO NOT USE
#   - pd: Fast, recommended for most cases (~7ms)
#   - eterms: Fast for type analysis (~56ms)
#   - gr, share, etc.: May be slow on complex code

# Analyze a Prolog file with CiaoPP (uses pd domain - faster than default shfr)
# NOTE: Default shfr domain TIMEOUTS (>120s), so we use pd instead
# Usage: just analyze <filename>
# Example: just analyze game
analyze file:
	ciaopp -A prolog/{{file}}.pl -fmodes=pd

# Analyze with specific mode domain (e.g., pd, shfr, eterms)
# WARNING: shfr domain TIMEOUTS (>120s) on this codebase
# Usage: just analyze-mode <filename> <domain>
# Example: just analyze-mode game pd
analyze-mode file domain:
	ciaopp -A prolog/{{file}}.pl -fmodes={{domain}}

# Verify assertions in a Prolog file (uses pd domain - faster than default shfr)
# NOTE: Default shfr domain TIMEOUTS (>120s), so we use pd instead
# Usage: just verify <filename>
# Example: just verify game
verify file:
	ciaopp -V prolog/{{file}}.pl -fmodes=pd

# Optimize a Prolog file (specialization, etc.)
# NOTE: Use -pmodes=pd (internal flag) to change domain, NOT -fmodes
# WARNING: Default shfr domain may timeout on complex files - use -pmodes=pd
# Available mode domains: pd, pdb, gr, def, share, son, shfr, etc. (see top comment)
# Usage: just optimize <filename>
# Example: just optimize game
# Example with pd domain: ciaopp -O prolog/game.pl -pmodes=pd
optimize file:
	ciaopp -O prolog/{{file}}.pl -pmodes=pd

# Interactive CiaoPP menu (for advanced configuration)
# Usage: just ciaopp-interactive <filename>
# Example: just ciaopp-interactive game
ciaopp-interactive file:
	ciaopp -Q prolog/{{file}}.pl

# Start CiaoPP interactive toplevel
ciaopp:
	ciaopp -T

