# Module Extension Test

This folder tests if a module can be extended across multiple files.

## Result: **Modules CANNOT be extended across multiple files**

### What happens:

When you declare the same module name in multiple files:
- **Scryer Prolog does NOT allow redefining a module**
- Attempting to load a second file that declares the same module name causes issues
- Predicates from the second file may not be accessible
- The behavior is undefined/unreliable

### Test Files:

- `base.pl` - Defines `extend_test` module with `pred1/1` and `pred2/1`
- `extend.pl` - Attempts to also define `extend_test` module with `pred3/1` and `pred4/1`
- `test_extend.pl` - Tests loading base then extend
- `test_extend_reverse.pl` - Tests loading extend then base

### Test Results:

- Loading `base.pl` works - `pred1/1` and `pred2/1` are accessible
- Loading `extend.pl` after `base.pl` - `pred3/1` and `pred4/1` are **NOT accessible** (existence_error)
- Loading `extend.pl` first works - `pred3/1` and `pred4/1` are accessible  
- Loading `base.pl` after `extend.pl` - `pred1/1` and `pred2/1` are **NOT accessible** (existence_error)

### Conclusion:

**You CANNOT extend a module across multiple files by declaring the same module name.**

To extend a module, you must:
1. **Use a single file** - Put all predicates in one module file, OR
2. **Use `multifile` directives** - Allow predicates to be defined across files with `:- multifile predicate/arity`, OR
3. **Manually combine** - Combine all predicates into one module file