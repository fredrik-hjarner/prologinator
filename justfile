# ============================================================================
# Ciao Prolog commands
# ============================================================================
# NOTE: In Ciao Prolog, load files with:
#   - use_module('filename.pl') for modules
#   - ensure_loaded('filename.pl') for non-modules
#   NOT [filename] like in SWI-Prolog!
#
# Default: List all available commands
default:
	@just --list

# Check if a Prolog file compiles (reports errors/warnings, exits on error)
# Usage: just check <filename>
# Example: just check game
check file:
	cd prolog && rm -f {{file}}.po {{file}}.itf && ciaoc -c {{file}}.pl
	just clean

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
# Usage: just test [module]
# Examples:
#   just test game        # runs game.pl tests
#   just test execute_action  # runs execute_action.pl tests
# Note: Tests are defined using :- test assertions in the source file
test module:
	cd prolog && \
		rm -f run_tests.po run_tests.itf && \
		timeout 10 bash -c 'TEST_MODULE="{{module}}" ciaosh -u run_tests.pl -t halt 2>&1 | tee /tmp/test_output.txt' && \
		(grep -q "Passed:.*100.00%" /tmp/test_output.txt && echo "✅ All tests passed" && just clean && exit 0) || \
		(echo "❌ Tests failed" && just clean && exit 1)

# Clean Ciao Prolog compilation artifacts and CiaoPP generated files
# Removes: .itf, .po, .asr, .ast, *_co.pl, *_pd_*.pl, *_eterms_*.pl, *_codegen_*.pl, .testin, .testout, run_tests, temporary checker files
# Usage: just clean
clean:
	@find prolog -name "*.itf" -o -name "*.po" -o -name "*.asr" -o -name "*.ast" | xargs rm -f 2>/dev/null || true
	@find prolog -name "*_co.pl" -o -name "*_pd_*.pl" -o -name "*_eterms_*.pl" -o -name "*_codegen_*.pl" | xargs rm -f 2>/dev/null || true
	@find prolog -name "*.testin" -o -name "*.testout" -o -name "*.testout-saved" -o -name "*.testin-saved" | xargs rm -f 2>/dev/null || true
	@find prolog -name "run_tests" -type f | xargs rm -f 2>/dev/null || true
	@find prolog -name "*_co.pl" -type l | xargs rm -f 2>/dev/null || true
	@find prolog -name "*.po-tmpciao*" -o -name "*_flycheck_tmp_co.*" | xargs rm -f 2>/dev/null || true

# ============================================================================
# CiaoPP static analysis commands
# ============================================================================
# 
# MODE DOMAINS (for -fmodes flag):
#   Mode analysis checks variable instantiation (bound/unbound, ground, etc.)
#   
#   Fast domains:
#     - pd: Pair sharing (fast, ~7ms) - RECOMMENDED for most cases
#     - pdb: Pair sharing with backward analysis
#     - gr: Groundness analysis
#     - def: Definiteness analysis
#   
#   Medium domains:
#     - share: Sharing analysis
#     - son: Sharing or not analysis
#     - shareson: Sharing or not combined
#     - sharefree: Sharing and freeness
#   
#   Slow/TIMEOUT domains:
#     - shfr: Sharing + freeness (default, TIMEOUTS >120s) - DO NOT USE
#     - shfrson, shfrnv, sharing_amgu, sharing_clique, sharefree_amgu, etc.
#
# TYPE DOMAINS (for -ftypes flag):
#   Type analysis checks term types (list, integer, etc.)
#
#   Available domains:
#     - eterms: Regular types with widening (DEFAULT, fast ~56ms) - RECOMMENDED
#     - ptypes: Predefined types only
#     - deftypes: Defined types
#     - none: No type analysis
#
# NOTE: CiaoPP automatically runs type analysis (eterms) by default when you
#       specify -fmodes. You don't need to explicitly add -ftypes=eterms unless
#       you want to use a different type domain.

# Analyze a Prolog file with CiaoPP (uses pd domain - faster than default shfr)
# NOTE: Default shfr domain TIMEOUTS (>120s), so we use pd instead
# Usage: just analyze <filename>
# Example: just analyze game
analyze file:
	ciaopp -A prolog/{{file}}.pl -fmodes=pd
	just clean

# Analyze with specific mode domain (e.g., pd, shfr, eterms)
# WARNING: shfr domain TIMEOUTS (>120s) on this codebase
# Usage: just analyze-mode <filename> <domain>
# Example: just analyze-mode game pd
analyze-mode file domain:
	ciaopp -A prolog/{{file}}.pl -fmodes={{domain}}
	just clean

# Verify assertions in a Prolog file
# Uses: pd for modes + eterms for types (CiaoPP runs both by default)
# Shows a readable summary of verification issues
# Usage: just verify <filename>
# Example: just verify game
verify file:
	timeout 60 bash -c 'ciaopp -V prolog/{{file}}.pl -fmodes=pd -ftypes=eterms 2>&1' | tee /tmp/verify_strict_output.txt > /dev/null
	@VERIFY_OUTPUT_FILE=/tmp/verify_strict_output.txt bun run scripts/parse_verify_output.ts || EXIT_CODE=$$?; \
	if [ "$$EXIT_CODE" = "1" ]; then \
		just clean; \
		exit 1; \
	fi
	@just clean

# Optimize a Prolog file (specialization, etc.)
# NOTE: Use -pmodes=pd (internal flag) to change domain, NOT -fmodes
# WARNING: Default shfr domain may timeout on complex files - use -pmodes=pd
# Available mode domains: pd, pdb, gr, def, share, son, shfr, etc. (see top comment)
# Usage: just optimize <filename>
# Example: just optimize game
# Example with pd domain: ciaopp -O prolog/game.pl -pmodes=pd
optimize file:
	ciaopp -O prolog/{{file}}.pl -pmodes=pd
	just clean

# Interactive CiaoPP menu (for advanced configuration)
# Usage: just ciaopp-interactive <filename>
# Example: just ciaopp-interactive game
ciaopp-interactive file:
	ciaopp -Q prolog/{{file}}.pl

# Start CiaoPP interactive toplevel
ciaopp:
	ciaopp -T

