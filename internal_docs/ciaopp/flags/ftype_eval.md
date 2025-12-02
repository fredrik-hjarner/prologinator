## Downsides of `-ftype_eval=on`

### 1. Known bugs (precision issues)
There is a documented bug in the codebase:

```prolog
:- doc(bug, "2 commented out last clause of eterms_very_special_builtin. 
      This may lose precision, but otherwise type_eval produces wrong
      results")
```

A more general clause that evaluated predicates with `sideff(free)` and `eval` properties was commented out because it produced wrong results for the "stream interpreter" case study.

### 2. Limited scope
`type_eval` only applies to specific builtins:
- `is/2` (arithmetic evaluation)
- `functor/3`
- `arg/3`
- Comparison operators: `>/2`, `>=/2`, `=</2`, `</2`

It does not apply to arbitrary predicates, even if they have `eval` properties.

### 3. Default is `off`
The flag defaults to `off`, suggesting it's not always safe or desirable.

### 4. Potential precision loss
The bug comment mentions "may lose precision" in some cases, though the alternative (without it) can produce wrong results.

### 5. Performance
Concrete evaluation is likely slower than abstract evaluation, though no explicit performance warnings were found.

## Recommendation

Use `-ftype_eval=on` when:
- You need to verify integer arithmetic assertions
- You're working with the supported builtins (`is/2`, comparisons, etc.)
- The codebase is relatively simple

Be cautious when:
- Working with complex programs (like the stream interpreter case)
- Using predicates with `eval` properties that aren't explicitly supported
- You notice unexpected analysis results

For your simple arithmetic examples, `-ftype_eval=on` works well and is necessary to verify integer arithmetic. For larger codebases, test it and watch for unexpected results.

The trade-off: better precision for arithmetic vs. potential bugs in edge cases.