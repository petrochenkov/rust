# Suggestion spans

When reporting an error or a warning we often make a suggestion how to fix that error by adding
some code.

For example, we may suggest inserting `&` to the left from the expression when that expression has
type `T` instead of expected `&T`.
Below I will use expressions as examples of code fragments for suggestions, and use the adddress
taking as an example of a suggestion with code insertion.

Let's talk about expressions, the same approach generalizes to other nodes too.

Suppose we have an expression that is passed to a macro as an argument.

```rust
macro_rules! mac {
    ($expr:expr) => {
        $expr + 1
    }
}

mac!(a);
mac!(a + b);
```

```rust
block | expr | ident | item | lifetime | literal
meta | pat | pat_param | path | stmt | tt | ty | vis
```

We want to make a suggestion to add some code to the left or to the right of the expression.
I'll assume that a piece of code has the same context for left and right insertions, and not
separate contexts. Spans of tokens to the left/right are available during parsing,

- `tt` - cannot add `&` to input, `&a` doesn't match `tt`
- `ident` - cannot add `&` to input, `&a` doesn't match `ident`
- `path` - cannot add `&` to input, `&a` doesn't match `path`
- `expr` - can add `&` to input, `&a` still matches `expr`
- `(tt)*` - can add `&` to input, `&a` still matches a `tt` sequence

Always can add `&` inside the macro body: `&$expr`, `&$path`, `&$ident`, `&$tt` will all work.
Still not reliable though, the suggestion can make sense for one expansion of a macro, and not make
sense for another expansion of the same macro.

Any code removal may generally have the same issues as code insertion, but to a less extent because
you cannot remove anything from `tt` or `ident`, and removing something from `expr` or `path` still
keep it an `expr` or `path`.

Need access to HIR ID to get surrounding span as a span of the HIR parent node.
If HIR ID is unavailable the parent span needs to be passed from a place where it is available.
Doesn't work before HIR is built, need to pass parent spans around manually.

Interesting example: suggestion to add `unsafe(...)` around unsafe attributes.
Can only be inserteed into input if the fragment is passed as `meta`,
can only be inserted into macro body if the fragment is passed as `ident`/`tt`.
Hard to determine the parent context because there is currently no node to save the span of `$meta`
in AST. Idea: can try to add it to the metavar span table, or just skip the suggestion.

Tokens from `(tt)*` sequences don't currently keep metavar spans.
So `span_in_ctxt` will be the same as `span`.
Typically used for passing arbitrarily large amounts of code that needs to be considered by itself,
and not in context of a macro.

`$tt:tt + $tt:tt`
Cannot special case simple cases (like single identifier or path), more complex cases can arrive
from non-`expr` matchers too.
Mostly reliable and really not reliable.

### Which span to keep in AST/HIR

- Exact span - better for *showing* any diagnostics
- Span in context - better for suggesting changes to code
- Both spans - too expensive

If we have HIR we can go in both directions
- From in-context to precise - look at expr kind, go to path span if the expression is a span,
  go to identifier if the path is a single segment without generic arguments
- From precise to in-context - get at HIR parent and do `span.with_neighbor(parent_span)`

### Strict alternative

Do not emit code insertion suggestions at all for ... what exactly?

Need to know both exact and in-context spans.

- If `span == span_in_ctxt`.
    - If `!from_expansion(span)`
        - Can make a suggestion
    - If `from_expansion(span)`
        - Cannot make a suggestion
        - If the span is from something that we cannot modify (external macro, desugaring,
          maybe procedural macro), then we certainly cannot make a suggestion.
        - If the span is from a "good" macro (e.g. local `macro_rules`) then the suggestion
          will be unreliable due to the hazard of being specific to a single macro expansion
          but not others.
- If `span != span_in_ctxt`
    - Some macros are certainly involved and we cannot make a reliable suggestion
    - In particular, `from_expansion(span_in_ctxt)` will always be true
    - If `span_in_ctxt` comes from a good macro, we can make an unreliable suggestion.
    - If `span_in_ctxt` comes from a bad macro, then don't make a suggestion

What to do if we don't have an in-context span, or HIR to obtain it?
- Preferably, pass the necessary HIR node or ID, or its span
- Otherwise report the suggestion as unreliable

### Less strict alternative

Emit the suggestions with `Applicability::MaybeIncorrect` or something like that for suspicious
cases, e.g.
- `span == span_in_ctxt && in_good_macro(span)`
- TODO

### A number of existing heuristics

Used randomly and inconsistently.
find_ancestor_inside
find_ancestor_in_same_ctxt
find_ancestor_inside_same_ctxt
find_oldest_ancestor_in_same_ctxt
can_be_used_for_suggestions
from_expansion
in_derive_expansion
is_desugaring
in_external_macro
is_from_async_await
