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

# Clean Ciao Prolog compilation artifacts (.itf, .po, .asr, .ast files)
clean:
	find prolog -name "*.itf" -o -name "*.po" -o -name "*.asr" -o -name "*.ast" | xargs rm -f

