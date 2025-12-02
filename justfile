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
# Example: just check engine
check file:
	cd prolog && rm -f {{file}}.po {{file}}.itf && ciaoc -c {{file}}.pl
	just clean

# Test if a Prolog file is compatible with Ciao Prolog (interactive)
# Usage: just consult <filename>
# Example: just consult engine
consult file:
	cd prolog && rlwrap ciaosh -e "use_module('{{file}}')"

# Test if the operators actually work in Ciao Prolog
# This loads the operators module and tests parsing of operator expressions
# ciao-test-operators:
# 	cd prolog && ciaosh -e "use_module('operators'), X = atk:hi, Y = (atk:hi & atk:lo), Z = ((atk:hi & atk:hi) vs (atk:lo & atk:lo)), write('Operators work!'), nl, write('X = '), write(X), nl, write('Y = '), write(Y), nl, write('Z = '), write(Z), nl" -t halt

# Start Ciao Prolog interactive shell (with readline support via rlwrap)
ciao:
	rlwrap ciaosh

# Run the game
# Usage: just run game
run game:
	cd prolog && ciaosh -l game.pl -e "main" -t halt

# Run unit tests
# Usage: just test [module]
# Examples:
#   just test engine      # runs engine.pl tests
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
	@find prolog ciao_experiments ciaopp \( -name "*.itf" -o -name "*.po" -o -name "*.asr" -o -name "*.ast" \) -type f -delete 2>/dev/null || true
	@find prolog ciao_experiments ciaopp \( -name "*_co.pl" -o -name "*_pd_*.pl" -o -name "*_eterms_*.pl" -o -name "*_codegen_*.pl" \) -type f -delete 2>/dev/null || true
	@find prolog ciao_experiments ciaopp \( -name "*.testin" -o -name "*.testout" -o -name "*.testout-saved" -o -name "*.testin-saved" \) -type f -delete 2>/dev/null || true
	@find prolog ciao_experiments ciaopp -name "run_tests" -type f -delete 2>/dev/null || true
	@find prolog ciao_experiments ciaopp -name "*_co.pl" -type l -delete 2>/dev/null || true
	@find prolog ciao_experiments ciaopp \( -name "*.po-tmpciao*" -o -name "*_flycheck_tmp_co.*" \) -type f -delete 2>/dev/null || true

# Run CiaoPP analysis script on a file
# Usage: just ciaopp <filename>
# Example: just ciaopp ciao_experiments/correct_types2.pl
ciaopp file:
	ciaoc -x ciaopp/ciaopp ciaopp/ciaopp.pl && ./ciaopp/ciaopp {{file}} 2>&1| scripts/prettify-ciaopp-output
	just clean

