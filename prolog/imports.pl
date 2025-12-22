% Centralized library imports
% Imports all libraries used across the codebase

:- use_module(library(assoc)).

#ifndef SWI
:- use_module(library(between)).
#endif

:- use_module(library(charsio)).

#ifndef SWI
:- use_module(library(clpz)).
#else
:- use_module(library(clpfd)).
#endif

:- use_module(library(dif)).

#ifndef SWI
:- use_module(library(format)).
#endif

#ifndef SWI
:- use_module(library(iso_ext)).
#endif

:- use_module(library(lists)).

#ifndef SWI
:- use_module(library(os)).
#endif

#ifndef SWI
:- use_module(library(reif)).
#endif

:- use_module(library(time)).

