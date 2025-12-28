% ==========================================================
% THE PROLOGINATOR MONOLITH BUILD
% ==========================================================

#include "settings.pp"

% 1. Global Imports (Import all libraries ONCE)
#include "../prolog/imports.pl"

% 1.5. Discontiguous Declarations (must be before any clauses)
#include "../prolog/discontiguous.pl"

% 1.6. Dynamic Declarations (must be before any clauses)
#include "../prolog/dynamic.pl"

% 1.7. Operators (must be before any clauses)
#include "../prolog/operators.pl"

% 2. Types (foundational - no dependencies on game logic)
#include "../prolog/types/accessors/ctx.pl"
#include "../prolog/types/accessors/obj.pl"
#include "../prolog/types/constructors.pl"

% 3. Utilities and Macros
#include "../prolog/util/util.pl"

% 4. Action Resolution and Builtins
#include "../prolog/resolve_action.pl"

% 5. Execute Action
#include "../prolog/execute_action.pl"

% 6. Action Implementations (all actions)
#include "../prolog/actions/wait.pl"
#include "../prolog/actions/log.pl"
#include "../prolog/actions/loop.pl"

% 7. Core Engine Components
#include "../prolog/tick_action_streams.pl"
#include "../prolog/tick_object.pl"

% 8. Main Engine
#include "../prolog/engine.pl"

