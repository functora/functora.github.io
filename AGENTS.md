## Agent Boundary: Local Code and Documents Only

- **The agent may edit local source code, documentation, tests, configuration, and local build artifacts when requested. It must never mutate remote state.**
- **The agent must never publish, upload, deploy, release, submit, register, or otherwise expose anything to any external system.** This includes crates.io, every other package registry, Git repositories and forge APIs, container registries, artifact stores, cloud services, app stores, CI/CD systems, and any similar destination.
- **Never invoke an upload-capable command, even if it is expected to fail, is being used to test credentials, or is exposed through a script or Nix app.** Forbidden examples include `cargo publish` without `--dry-run`, `cargo yank`, `npm publish`, `twine upload`, `gem push`, `docker push`, `oras push`, `nix flake publish`, `gh release create`, and equivalent commands from other tools.
- Never use a registry or repository API with a write operation, including `POST`, `PUT`, `PATCH`, `DELETE`, or equivalent actions. This includes creating or changing issues, pull requests, reviews, comments, discussions, wiki pages, releases, tags, branches, packages, repository settings, and other remote resources.
- Never upload release artifacts to GitHub Releases, Git LFS, package registries, Google Play, Apple App Store, artifact buckets, or any other remote destination.
- Never run `cargo login`, `npm login`, or any other command that creates or changes registry credentials. Never pass credentials to a command that can write remote state.
- Read-only remote inspection is allowed only when it is necessary to understand the task or verify prepared release state. It must not create or update any remote resource.
- When asked to release or publish, prepare the release locally: update versions and documentation, synchronize lockfiles, build and test artifacts, run non-mutating package or dry-run checks, verify the final diff, and provide the exact commands the user can run manually. Stop before every remote write.
- Words such as "continue", "finish", "prepare for release", "verify publishing", or "test publishing" never authorize a publication or any other remote mutation. Only the human user performs remote publication.
- If a command's remote behavior is uncertain, assume that it may publish and do not run it. Inspect local scripts and flake applications before invoking release-related commands.

## Git

- **Git history, refs, index, remotes, and the user's existing work are strictly read-only.**
- The agent may create or edit local files as requested, but must never discard, overwrite, or rewrite unrelated user work.
- **NEVER commit, amend, push, or create pull requests.** The user reviews all changes and commits them manually. Leave the working tree dirty for review.
- **NEVER mutate Git history, refs, the index, remotes, or the user's work.** Do not run `git commit`, `git amend`/`--amend`, `git push`, `git pull`, `git fetch`, `git add`, `git reset`, `git checkout`/`switch`/`restore`, `git branch`/`tag`, `git stash`, `git merge`/`rebase`/`cherry-pick`/`revert`, `git remote add`/`remove`/`set-url`, or any other command that creates, rewrites, transfers, or discards commits, branches, refs, index entries, remotes, or files.
- Only read-only inspection is allowed: `git status`, `git diff`, `git log`, `git show`, `git ls-files`, `git diff --stat`, etc. Use these to review the current state and verify only intended files were changed.

## Coding Guidelines

### Project Understanding

- Read the `README.md` file if available.
- Study the project source code before making changes.
- Understand the target module, its dependencies, surrounding code, and nearby tests before modifying it.
- Follow the existing architecture, abstractions, naming conventions, APIs, and established project patterns.
- Examples of good code can be found in the latest versions of the crates `functora-tagged` and `functora`. Study their source code and use a similar style.

### Monorepo

- Take advantage of the git monorepo as the single source of truth for all code: prefer the local monorepo copies of libraries over external releases.
- Depend on the local monorepo versions of shared crates (e.g., `functora-tagged`, `functora`) rather than the versions hosted in the crates registry.
- First extend and improve the shared library in the monorepo when a reusable generalization, refinement, refinement bug fix, or other widely beneficial capability is genuinely warranted across multiple crates.
- Then consume the improved monorepo library from the dependent crates, rather than duplicating the behavior or waiting for a crates.io release.
- Do not generalize or refactor libraries speculatively; extend them only with real, concrete, reproducible need as demonstrated by actual use.
- If it makes sense to add widely used invariant-preserving behavior to `functora-tagged`, implement it in the monorepo and use the monorepo `functora-tagged` instead of the crates-registry version.
- Ensure that any change to a shared library keeps its public contracts stable, or updates all affected callers and tests in the same change.

### Change Discipline

- Before making changes, understand the existing behavior and identify the smallest appropriate change.
- For bug fixes, **first reproduce the bug with a failing automated test**, then fix the production code. Do not patch production code first and add a test afterward.
- Prefer small, local, targeted changes over broad rewrites or unrelated refactoring.
- Do not reformat, reorganize, rename, or otherwise modify unrelated files or code.
- Preserve existing behavior outside the scope of the requested change.
- Do not weaken compiler, linter, formatting, test, logging, security, or other project settings to make a change pass.
- Do not disable warnings, suppress lints, reduce test coverage requirements, remove validation, or relax security checks as a workaround.
- Do not introduce abstractions unless they remove real duplication, define a real boundary, or provide a clear architectural benefit.
- Do not introduce speculative abstractions or generalized frameworks for a single use case.
- Do not accidentally change public JSON, API contracts, configuration shapes, serialization formats, module boundaries, or other externally observable behavior.
- Treat public interfaces and data formats as stable unless changing them is explicitly required by the task.
- When a public contract must change, make the change explicit and update all affected tests and callers.

### Functional Style

- Write code in a **strictly functional style while ensuring high efficiency**.
- Avoid mutable variables (`mut`) whenever possible.
- Avoid imperative constructs such as `for`, `while`, `loop`, and `return`.
- Instead, use functional iterators, combinators, expression-oriented control flow, method chaining, pipelining, and the `?` operator.
- Prefer immutable data and explicit data flow.
- Eliminate redundant closures and variables.
- Eliminate redundant code.
- Maximize method chaining. Always favor chaining and pipelining over temporary one-time variables.
- Keep code as flat as possible.
- Avoid unnecessary nesting of code blocks and nested brackets such as `(.. (..) ..)` or `{.. { .. } ..}`.
- Deal with complexity through `?`, chaining, iterators, and piping rather than deeply nested control flow.
- Prefer small, focused functions with a single clear responsibility.
- Introduce a temporary variable when it materially improves readability, avoids repeated computation, or is required by ownership or lifetime constraints. Do not create one merely to hold a value used once.
- Always prefer `(..expr..).pipe(Ok)` over `Ok(..expr..)` unless `(..expr..)` is very short and simple.
- Use meaningful, context-clear identifiers with a preference for brevity when the meaning remains unambiguous.
- Prefer domain-specific names over generic names such as `data`, `value`, `thing`, `manager`, `helper`, or `utils` when a more precise name is appropriate.
- Do not include comments. The code must be self-explanatory through its types, names, structure, and abstractions.
- Documentation is the exception: documentation (e.g. doc comments) that already exists in the source code must be preserved, not removed along with other comments. Do not create new documentation unless the user explicitly asks for it.
- Keep the code DRY.
- Remove redundant code rather than preserving duplication.
- Do not sacrifice correctness, readability, or efficiency merely to make code shorter or eliminate a variable.

### Efficiency and Dependencies

- Prefer `const` over non-`const` wherever possible, including `const` functions, values, and bindings, so that computation happens at compile time and immutability is guaranteed.
- Prefer `&str` over `String` wherever possible, including for parameters, return values, and data fields, when it does not add a lot of redundant complexity such as awkward lifetimes or generic parameters.
- Avoid `.clone()` unless cloning is genuinely required by ownership semantics.
- Avoid unnecessary allocations, copies, conversions, intermediate collections, and other needless work.
- Prefer zero-cost abstractions and efficient iterator-based solutions.
- Prefer the standard library exclusively whenever possible.
- Avoid extra dependencies.
- Introduce a dependency only when it provides substantial value that cannot reasonably be achieved with the standard library or existing dependencies.
- Keep dependencies narrow and minimal.
- Do not add a dependency merely for convenience when an existing dependency or the standard library provides an appropriate solution.

### Error Handling

- **The code must be panic-free.**
- Do not use `panic!`, `unreachable!`, `unimplemented!`, `todo!`, `unwrap()`, `expect()`, or other panic-producing operations in production code unless the operation is provably unreachable and there is no fallible alternative.
- Every fallible operation must be explicitly handled.
- Never swallow, suppress, ignore, or silently discard errors.
- If an error cannot be meaningfully recovered from at the current layer, propagate it explicitly using `Result::Err` and `?`.
- Handle errors at the layer that has enough context to recover meaningfully; otherwise propagate them upward.
- **Always use plain `?` to propagate errors whenever a `From`/`Into` impl exists between the underlying error and the target error type. This covers every `map_err` form that merely repackages the error, e.g. `.map_err(Error::from)`, `.map_err(AppError::LibError)`, `.map_err(Error::Variant)` - as long as a `From`/`Into` impl exists, `?` is the default and mandatory propagation form, and such repackaging must not be used.**
  - Example: `let x = parse()?;` not `let x = parse().map_err(AppError::LibError)?;`
- **Provide the matching `From`/`Into` impl for every wrapping error variant, so that `?` remains possible everywhere; do not leave wrapping variants that would force `map_err`.**
  - Example: for a variant `LibError(LibError)`, implement `From<LibError>` for the error type.
- Use `map_err` only when it adds meaningful typed context (e.g. a variant wrapping the source error together with additional context fields), or when converting across a public/generic boundary with a fixed target error type where `From` cannot apply.
- Never fabricate a generic placeholder error when the real underlying error is available; wrap the actual error in a precise variant instead.
  - Example: `Err(Error::Foreign(e))` rather than `Err(_) => Err(Error::Generic)`.
- Preserve the original error of narrowing conversions (e.g. `TryFromIntError`) in a typed variant with context rather than discarding it.
  - Example: `u32::try_from(n).map_err(|e| Error::Conversion { context, source: e })?`.
- Use explicit, domain-specific error enums.
- Error enum variants must precisely describe what happened or which operation failed.
- Avoid vague variants such as `Error`, `Failure`, `Custom`, `Other`, or `Unknown` when a precise variant is possible.
- **Never lose information from an underlying error.**
- Error variants must preserve the complete underlying error by wrapping the original error value rather than converting it to a string or replacing it with a generic error.
- Never stringify errors for propagation, including patterns such as:
  `map_err(|e| Error::Something(e.to_string()))`.
- Prefer typed variants such as:
  `Error::Something(e)`.
- When additional context is required, store it alongside the underlying error:
  `Error::Something { context, source }`.
- Do not use `Box<dyn Error>`, `String`, or other type-erasing representations merely to avoid defining a precise error enum when the underlying error type is known.
- Error conversions must preserve all meaningful information and the strongest useful type information.
- Do not catch an error only to rethrow it unchanged. Use `?` directly.
- Do not convert between error types unless the conversion is intentional, typed, and preserves all relevant information.

### Resource Safety and Concurrency

- Avoid global mutable state.
- Prefer explicit ownership and dependency passing over hidden global state or implicit shared state.
- Avoid hidden blocking calls, especially in asynchronous or concurrent code.
- Blocking operations must be explicit and execute in an appropriate context.
- Avoid unbounded concurrency.
- Concurrency must have explicit and appropriate bounds.
- Avoid uncontrolled task spawning, unbounded queues, unbounded memory growth, and other forms of resource exhaustion.
- Prefer bounded concurrency, backpressure, and explicit resource ownership where applicable.
- The code must be strictly free of memory leaks, race conditions, and deadlocks at every level, backend and frontend alike: connections, pools, caches, event listeners, subscriptions, timers, and DOM references included.
- Memory must be released deterministically; nothing may persist past its last use, and no object may be retained indirectly through listeners, closures, or caches once it is no longer needed.
- Shared state accessed from concurrent or async contexts must be properly synchronized; no data race or torn read may ever be possible.
- Locking disciplines must be minimal and uniform so that cycles, reentrancy, double-acquire, and other deadlock sources are impossible by construction.
- **There must be no zombie or unmanaged threads.** Every thread, task, or spawned process must have an explicit owner, a bounded and known lifetime, and a deterministic teardown path that runs even when the surrounding operation fails, is cancelled, or its error is ignored.
- Threads, tasks, and processes must be cleaned up when their operation ends, fails, or is forgotten; no leftover worker may keep running in the background.
- Prevent threads from outliving their owner: joins, cancels, drops, and shutdown hooks must be explicit and reachable on all control-flow paths, including early returns, `?`, and `panic`.
- Unbounded or unclaimed work must be detectable: track spawned work so none can be silently abandoned, and fail loudly if cleanup is impossible.
- Ensure resources are released deterministically and safely.
- Do not hide internal I/O, allocation, synchronization, or blocking operations behind innocuous-looking APIs.
- Prefer resource-safe designs whose ownership and lifetime behavior is apparent from the types and APIs.

### Abstractions and API Design

- **Strongly avoid introducing custom traits unless absolutely necessary.**
- Always prefer generics (parametric polymorphism) over ad-hoc (trait) polymorphism for code abstraction and generalization.
- Prefer generic functions and types over trait definitions whenever a plain generic parameter suffices; introduce a trait only when it defines a real, well-defined boundary or behavior that generics by themselves cannot express.
- Avoid unnecessary traits.
- Traits with only one implementation should normally be avoided unless there is a strong, concrete reason to introduce the abstraction.
- A single-implementation trait is justified only when it provides a required external API, meaningful generic abstraction, object-safe boundary, associated-type abstraction, compile-time polymorphism, or a well-defined architectural boundary that cannot be expressed with a plain generic parameter.
- Do not introduce traits merely for speculative future implementations, test mocking, or abstraction for its own sake.
- Avoid large utility objects that accumulate unrelated responsibilities or become dumping grounds for miscellaneous functionality.
- Prefer small, focused types and functions with clear responsibilities.
- Avoid large argument lists.
- When several arguments form a coherent domain concept, consider representing that concept with an explicit domain-specific type rather than passing many loosely related values.
- Do not introduce parameter objects merely to avoid a reasonable number of arguments.
- Prefer explicit public API types over vague, overly generic, or type-erased interfaces.
- Keep public APIs narrow, predictable, and domain-specific.
- Avoid accidental changes to public JSON representations, configuration shapes, serialization formats, module boundaries, or API contracts.

### Plain ASCII Only

- Code, documentation, and content should default to simple ASCII characters. For example, prefer `-` over `—` or `–`, `"` and `'` over curly quotes, `...` over `…`, and `->` over `→`.
- Prefer plain ASCII letters: no accented or non-Latin characters; write `1st`, `2nd`, `3rd` instead of superscripts, and `e.g.`, `i.e.`, `etc.` with periods.
- Unicode, icons, and emojis are acceptable in code, documentation, and content when the user explicitly asks for them or when they are already present in the code. Preserve existing unicode, icons, and emojis exactly as they are; never strip, transliterate, or replace them during refactors (e.g. localized UI strings, emoji/icon brand glyphs, flag emojis in language menus, and non-Latin legal or user-facing texts).

### Conversions

- **`From`/`Into`/`TryFrom`/`TryInto` impls must exist only when there is exactly one correct way to convert one type into another.**
- A conversion impl must always be right and always make sense on any input value; for `TryFrom`/`TryInto` it is acceptable for the conversion to fail, but it must never be misleading or lossy in a way that depends on context.
- When multiple right ways to convert between the types exist, do not provide these impls; use named, explicit conversion functions or methods instead.

### Macros

- **Strongly avoid implementing custom macros unless absolutely necessary.** Prefer conventional Rust, plain traits, generics, and derives, which are idiomatic, readable, and debuggable.
- Treat macros as a last resort, used only when language expressiveness is genuinely insufficient.
- Prefer libraries that deliver the same expressiveness without macros (e.g., `functora-tagged`'s macro-free newtypes and derived traits) over reaching for a macro.

### Trait Implementations

- Whenever a trait can be derived - by Rust's built-in derives (`Debug`, `Clone`, `Copy`, `PartialEq`, `Eq`, `Hash`, `Default`, ...) or by derive machinery from a package already used in the project (`thiserror::Error`, ...) - always derive it. Derived implementations are always preferred over hand-written ones, especially for standard traits and extended derive machinery such as `thiserror`.
  - Example: `#[derive(Clone, Debug, PartialEq, Eq)]` instead of hand-written `PartialEq`/`Clone`/`Debug` impls.
- Never hand-write an implementation of a derivable trait, e.g. to work around a field type lacking the trait, or to bend semantics so that a type compiles.
- Hand-write an implementation only when the trait genuinely cannot be derived, or the derive cannot express the required non-standard semantics.
- When a field or enum variant wraps internals (from the standard library or a third-party crate) that lack an implementation our design needs, the orphan rule prevents impl'ing the trait on the internals directly. Always solve this with a thin newtype over the internals, implementing the missing traits on the newtype.
  - Example: `struct Wrapper(SomeForeignType);` + `impl PartialEq for Wrapper { ... }`.
- Keep conversion between the internals and the newtype smooth and cheap: `From<inner>` (and `From<newtype>` for the inner where possible), `Deref` to the inner where appropriate, and transparent `Display`/`Error` (`#[error(transparent)]`), so error reporting and `?` propagation stay unchanged.
- For missing equality on foreign internals, prefer an Arc-based identity wrapper over judgment equality: hold the inner in `Arc` and implement `PartialEq`/`Eq` with `Arc::ptr_eq`, giving a one-line impl that can never report two distinct values as equal.
  - Example: `pub struct IoError(pub Arc<std::io::Error>);` with `fn eq(&self, other: &Self) -> bool { Arc::ptr_eq(&self.0, &other.0) }`, and `From<inner>` wrapping with `Arc::new`.
  - Identity equality is conservative and therefore safe for memoization and diffing (e.g. dioxus props): equality means the same instance, so skipping a re-render is always correct, and cloned wrappers stay equal because `Arc` clones share the allocation.
  - It costs one heap allocation per construction and means two distinct instances are never equal even with identical content; the inner must be immutable and instance-stable (no interior mutability); the payload remains accessible through the newtype field.
- The same Arc wrapper is also the preferred workaround for missing `Clone` on the same internals (e.g. `std::io::Error`, `serde_json::Error`): `Arc<T>` is `Clone` for any `T`, so `#[derive(Clone)]` on the wrapper and its containers works without a hand-written impl.
  - Sharing-based cloning is required for coherence with identity equality: `x.clone() == x` must hold, which only a shared allocation guarantees; a deep-copying `Clone` (e.g. over `Box<T>`) would break both equality and memoization.
- When genuine value equality is required by the design (not merely memoization), compare the semantically meaningful parts of the inner value explicitly, never its string representation or raw bytes.
  - Example: compare `std::io::Error` by `io::ErrorKind` rather than by message text.

### Type System and Invariants

- **CRITICAL: when modeling data and state, always prefer algebraic data types (ADTs, enums) over flat mega-structures with multiple mutually exclusive fields and boolean or other flags indicating the current state variant.**
- **If parts or fields of the state are mutually exclusive, it MUST be an enum with separate variants, not a struct with dispatching flags and implicit logic.**
- **It must NOT be possible to construct invalid state at all; each enum variant SHOULD represent a valid state.**
- **This applies to state transitions as well: moving from one variant to another must go through the type system (e.g., a `match` that maps one enum variant to the appropriate valid target variant), never through runtime field toggling or flag mutation that permits transient or invalid intermediate states.**
- Prefer types that guarantee invariant preservation through the type system instead of locally checking invariants in multiple places.
- Make invalid states unrepresentable by encoding constraints in the type, enforced once at construction and preserved by every operation.
- Prefer `functora-tagged` newtypes over blind scalar types (`u64`, `String`, `f64`, and similar) to give values precise, invariant-preserving types.
- Use refined newtypes such as `Tagged<T, D, F>` with appropriate refineries (`FCrude`, `FPositive`, `FNonNeg`, `FNonEmpty`, `FZeroInclToOneIncl`, etc.) so that invariants are checked once and subsequently guaranteed, eliminating repeated checks, `unwrap()`, and `expect()` at call sites.
- Combine multiple refinement rules with a single flat composite refinery rather than nesting `Tagged` types.
- Prefer the infallible, invariant-exploiting operations of refined newtypes (e.g., `FNonEmpty` methods like `first()`, `last()`, `minimum()`, `singleton()`, `extend()`, `map()`) over fallible or `Option`-returning standard-library analogues that require local handling.
- Use newtype dimensions to distinguish values with identical representations (e.g., `UserId` vs `ProductId`) and prevent accidental misuse.
- Prefer dimensional types from the `num` module (`Identity`, `Atomic`, `Times`, `Per`) over raw numerics when units are involved, so that mixing units (e.g., adding meters to seconds) is rejected at compile time.
- Verify that invariants are actually preserved by property-based testing, not just a handful of hand-picked examples, especially for roundtrip behaviors such as encode-decode, `FromStr`/`Display`, serialize/deserialize, and similar roundtrips.

### Avoid Scalar Blindness and Scalar Overloading

- **Avoid scalar blindness: never model a value with a blind raw scalar (`String`, `Int`/`u64`, `Bool`, `f64`) when a precise, domain-specific type exists; types must stay meaningful even outside the defining module.**
- Wrap same-representation domain concepts in separate newtypes (`Password`, `PasswordSalt`, `PasswordHash`, `UserId` vs `ProductId`) so that accidental swaps become compile-time errors.
- **Avoid scalar overloading: never represent a smaller, fixed set of possibilities with a wider, overloaded type.**
- Do not use a `String` where a finite enum belongs (e.g. a `String` locale accepting `"fr"`, `"fr-fr"`, `"fr-FR"`, and any other string); use `Locale = En | Fr | Ru | Ee` so invalid values cannot even be represented.
- Do not use an `Int`/`u64` or `f64` where a finite enum or unit-denoted type belongs (e.g. a bare integer currency code instead of `Currency = Usd | Eur | Btc`); restrict the domain to what is actually valid.
- Avoid boolean blindness: when the meaning of a `Bool` depends on the surrounding field or call site, replace it with a precise two-variant enum (`FilterResult = Discard | Keep`, `TimeKind = WorkingTime | BreakTime`, `Strictness = Strict | NotStrict`); a bare boolean loses its context outside the data model.
- When a function takes multiple scalars that denote distinct domain concepts, wrap them in named types instead of passing a positional sequence of same-typed scalars, so the compiler rejects misplaced or meaningless argument combinations.

### Exhaustive Matching

- **Always match on typed enum values, never on blind strings or numbers.** Selection state, menu choices, routes, and dispatch keys must be enum variants (e.g. `ActionMode::Create`, `ComponentId::Button`), not `usize` indexes into an option list and not `&str` ids compared by value. Widget and callback APIs must bind enum values directly (e.g. entries as `&[(T, String)]` pairs carrying the value alongside its display label), so invalid selections cannot even be constructed and no positional or string lookup can drift out of sync.
- **Every `match` on a closed enum must be complete and exhaustive: list each variant explicitly and never use a catch-all `_` arm.** A wildcard silently swallows unhandled variants, so adding a variant later still compiles while quietly dropping behavior at runtime. An exhaustive match turns every enum change into a compile error at exactly the sites that must decide what to do, which is the point.
- Grouping is fine when several variants genuinely share behavior: use explicit or-patterns (e.g. `Preview::Video(_) | Preview::Audio(_) | Preview::Pdf(_) => ...`), never `_`. Or-patterns stay exhaustive: adding a variant still fails compilation until it is placed.
- Reserve `_` strictly for open domains the type system cannot enumerate: `&str`/`char`/`i32` from external input (parsers, wire formats, OS statuses), huge foreign enums, or numeric fallbacks (e.g. month lengths). Even there, name the meaningful cases explicitly and map the remainder to an explicit `None` or fallback rather than silently doing work.
- Rationale: **compile-time safety during changes, refactorings, and improvements.** An exhaustive match shows the complete set of possibilities explicitly at every decision site, so a reader never loses context about what can happen, and an author extending an enum cannot forget to handle the new case somewhere: the compiler lists every site. A `_` arm defeats this: it compiles today and silently misbehaves tomorrow.

### Complexity and Structure

- Prefer simple, composable expressions over deeply nested control flow.
- Use `?` to flatten fallible control flow.
- Use method chaining, iterator combinators, and piping to flatten data transformations.
- Prefer early error propagation over deeply nested `match` or `if` structures.
- Avoid nested blocks whenever a flatter equivalent is clear.
- Avoid temporary one-time variables when the expression can be safely and clearly inlined.
- Use variables when they improve readability, prevent repeated computation, or are required by ownership or lifetime constraints.
- Keep functions small and focused.
- Keep types focused and avoid types with excessive responsibilities.
- Avoid large utility modules and miscellaneous helper collections.
- Prefer explicit domain boundaries over generic catch-all abstractions.

### Testing

- For bug fixes, **first add a regression test that fails against the existing implementation** and demonstrates the bug. Only then modify production code.
- Add tests for all new behavior and relevant edge cases.
- Test error and failure paths explicitly.
- Tests must verify observable behavior and public contracts rather than mirror implementation details.
- Avoid implementation-mirroring tests that assert private structure, incidental call sequences, specific algorithms, or internal implementation details when multiple implementations could satisfy the same behavior.
- Prefer deterministic tests.
- Prefer property-based testing (e.g., `proptest` or `quickcheck`) for invariant-preserving behaviors such as encode-decode roundtrips, using a fixed seed and shrinking so that any failing case is minimal and reproducible.
- Avoid reliance on timing, arbitrary sleeps, uncontrolled randomness, ambient global state, external services, or machine-specific behavior unless the behavior itself is what is being tested.
- Do not write tests solely to increase coverage; tests must provide meaningful behavioral guarantees.
- All tests should be located in the `./tests` directory.
- Test coverage should be as high as possible, with particular attention to error paths, edge cases, and failure conditions.

### Lint Settings

- Every crate's `Cargo.toml` must keep the following lint configuration exactly:

```toml
[lints.rust]
unused_results = "warn"
[lints.clippy]
pedantic = "warn"
# strict
or_fun_call = "warn"
must_use_unit = "warn"
ignored_unit_patterns = "warn"
let_underscore_must_use = "warn"
map_unwrap_or = "warn"
unwrap_used = "warn"
expect_used = "warn"
shadow_same = "warn"
shadow_reuse = "warn"
shadow_unrelated = "warn"
# relaxed
similar_names = { level = "allow", priority = 1 }
too_many_lines = { level = "allow", priority = 1 }
wildcard_imports = { level = "allow", priority = 1 }
missing_errors_doc = { level = "allow", priority = 1 }
semicolon_if_nothing_returned = { level = "allow", priority = 1 }
```

- Lint configuration must remain identical across all crates; when changing it, update every crate's `Cargo.toml` in the same change.
- Do not relax, add, or remove entries workaround-wise to make code pass; code must satisfy these lints as written.
- Do not override lints through a `clippy.toml` file, `#![allow(..)]` / `#[allow(..)]` attributes, `#[expect(..)]`, or any other in-file or configuration-level suppression to make code pass; per-crate `Cargo.toml` lint configuration is the only permitted place to set lint levels, and it must match the configuration above exactly.
- The only exception is absolutely necessary third-party macros (e.g., Dioxus's `#[component]` attribute): when such a macro is required, it is acceptable to relax lint rules locally, at that particular place, for macro-related clippy issues only.

### Validation

- To check Rust code, use:
  `cargo clippy --all-features --all-targets`
- The code should compile successfully and have no warnings or errors.
- Do not suppress or disable warnings to make the code pass.
- To test Rust code, use:
  `cargo test --all-features --all-targets`
- Run clippy and tests with `--all-targets` so that the `./tests` directory, examples, and benches are compiled and linted as well.
- To get the Rust test coverage report, use:
  `cargo tarpaulin --all-features --engine llvm -o Lcov`
- The coverage report is written to `lcov.info`.
- To audit the dependency tree, use:
  `nix develop -c cargo-audit audit --file Cargo.lock` and `nix develop -c cargo-deny check`
  If the flake exposes an application that runs both, use that instead.
- Do not consider the task complete until the implementation passes the relevant checks.
- Do not weaken or bypass any validation to make the implementation pass.

### Finalization

- The final step is always to format the code with:
  `cargo fmt`
- Do not make code changes after the final `cargo fmt` step.
- Do not reformat unrelated files merely to satisfy formatting.
- Before finishing, verify that only files relevant to the task were changed.
- Verify that no unintended public behavior, configuration, API, serialization format, module boundary, or project setting was modified.
- The resulting code must be formatted, warning-free, panic-free, resource-safe, and fully tested.

---

## Security (Dependency Auditing, Threat Modeling, Secure Coding)

These rules apply to every crate in the repository and are written so that
they transfer unchanged to any other repository. When the project already
has a security policy, a threat model, or its own auditing pipeline, follow
that as well: the rules below are the floor, not the ceiling.

### Generic Rust Security Tooling

The dependency tree is part of the security boundary. Audit it with the
generic Rust security tooling that the flake provides, so the checks are
reproducible on any machine and never depend on a globally installed tool.

- **`cargo-audit`** finds known vulnerabilities in the resolved dependency
  graph using the RustSec advisory database. Run it as
  `cargo-audit audit --file Cargo.lock` from the directory that holds the
  lockfile.
- **`cargo-deny`** checks the license policy, the duplicate-version policy,
  the wildcard policy and the source policy. It reads a `deny.toml` next to
  the manifest; another location is selected with
  `cargo deny check --config <path>`. Run it as `cargo-deny check`.
- **`cargo tree` / `cargo tree -i <crate>`** explain why a dependency is in
  the graph. Use it before adding, removing or upgrading a dependency.
- **`cargo deny check licenses`** fails on a license that is not on the
  allowlist. Adding a dependency with a new license means adding it to the
  policy deliberately, with a reason.

**Running the tools.** Nix is the single source of truth for the development
environment (see the Nix / Flake Infrastructure section). The auditing tools
come from the flake, never from a global install, so the versions and the
advisory database are the pinned ones:

```
nix develop -c cargo-audit audit --file Cargo.lock
nix develop -c cargo-deny check
```

Run them from the directory that holds the manifest, or point them at it:
`cargo-deny` accepts `--manifest-path` and `cargo-audit` takes the lockfile
through `--file`, so a workspace can be audited from any subdirectory. The
`cargo audit` wrapper looks for a binary in `~/.cargo/bin` and fails inside
the dev shell; call the `cargo-audit` binary directly.

If the flake already exposes an application that runs both tools over every
crate, use that application - `nix develop -c <app>` or `nix run .#<app>` -
instead of repeating the commands by hand. Add such an application when the
repository has none and the audit would otherwise be repeated for every
crate.

**Rules for dependencies.**

- Never install an auditing tool globally. If a tool is missing, add it to
  the flake and expose it through the dev shell or a flake application.
- A vulnerability advisory is a hard failure. Upgrade the dependency; never
  add an `ignore` entry for a real vulnerability. An `ignore` entry needs a
  written justification and a tracking issue.
- An *unmaintained* advisory is not a vulnerability. If the policy tolerates
  them they are reported but do not break the build. Record them where the
  repository tracks accepted risk, so the decision stays visible.
- A yanked version is a hard failure.
- Lockfiles are committed. A workspace has one lockfile at the root;
  separate publishable crates may each carry their own. Update every
  lockfile the change affects, in the same change, and never let two of
  them disagree about the same dependency.
- When a dependency is upgraded, read its changelog for behavior changes, not
  just its advisory page. A major-version bump changes the API and needs the
  call sites and the tests updated with it.
- Prefer a feature-gated dependency over a heavier one. A capability that
  reaches untrusted input - a network fetch, a file loader, a sanitizer
  bypass - belongs behind an opt-in Cargo feature that the targets that do
  not need it do not enable.

### Security Findings and Best Practices

The patterns below are generic. They apply to any crate in the repository,
whatever it stores, transports or renders.

- **Never let untrusted input decide an allocation size.** A declared length
  in an archive, a file header, a compressed stream, or a network frame is
  attacker controlled. Bound it, cap it, and never pre-allocate from it.
  Read into a bounded buffer and verify the real number of bytes afterwards.
- **Bound every parser.** Cap the number of entries, the size of one entry
  and the total size of a document. A parser without limits is a denial of
  service with a file extension.
- **Reduce a path to one safe component before it reaches the file system.**
  Strip directory separators, drop `..`, drop leading dots, and reject
  control characters. A name from an archive, an upload or a remote listing
  is untrusted input.
- **An empty ciphertext is a decryption failure.** Every ciphertext carries
  an authentication tag, and the tag must be verified even when the payload
  is empty. A chunk loop that iterates over a zero-length payload skips the
  tag and authenticates a truncated message.
- **Refuse an empty password or key for protected content.** An empty key
  is a valid key, so it decrypts. It must be rejected before the KDF runs.
- **Honour the recorded algorithm.** When metadata names a KDF, a cipher or
  a codec, use that one. Silently substituting a default breaks the
  contract between the writer and the reader, and can silently downgrade the
  protection of data that was already stored.
- **Zeroize secret material.** Use `zeroize::Zeroizing` for passwords, keys
  and other buffers that outlive their scope. A plain `Vec<u8>` leaves the
  key in freed heap memory.
- **Keep secrets out of `Debug`.** A derived `Debug` prints every field,
  including the password, the plaintext body and the payload bytes. Write a
  manual `Debug` that redacts them, and keep the non-secret metadata -
  names, sizes, versions - visible for diagnostics.
- **Do not let the environment weaken the cryptography.** A KDF cost, a
  cipher, or a verification budget that an environment variable can lower
  turns published data into a cheap offline target. Keep the parameters as
  compile-time constants.
- **Do not put a secret in a URL, a filename, a log line, or an error
  message.** URLs end up in history, referrers and screenshots; filenames end
  up in directory listings.
- **Authenticate the metadata, not only the payload.** The cipher choice and
  the KDF must be bound as AEAD associated data, and the application or
  format prefix must be bound too, so a message cannot be replayed into
  another context.
- **Use a fresh nonce and a fresh salt per message.** Never derive either
  from the plaintext, the password or a counter the reader cannot verify.
- **Treat a sanitizer as a last layer, not the only one.** Keep the
  sanitizing library current, and do not feed it raw remote content.
- **Keep outbound network access opt-in.** A markdown or image renderer that
  can fetch a remote URL turns user content into a tracking pixel. Gate the
  fetching features behind Cargo features the application does not enable.
- **Do not overstate the security of the product.** "Authenticated
  encryption" means confidentiality and integrity, not authorship. A
  password-based scheme has no forward secrecy. A shared link reveals the
  ciphertext, so the password can be attacked offline. A plain mode is
  encoding, not encryption. Write these limits down, in the README, in the
  in-app text and in the `SECURITY.md`, in every language the application
  ships.
- **Document the threat model in the crate.** Every user-facing crate keeps
  a `SECURITY.md` with its assets, its trust boundaries, its adversaries,
  its guarantees, its explicit non-goals and the status of every finding
  that still applies. A fixed finding does not stay in the policy: the code
  and its regression tests are the evidence of the guarantee, and the git
  history is the record of the finding. A finding you accepted instead of
  fixing stays, and the explicit non-goals say so. Render the same document
  in the app when the crate has a UI, so the user reads the same text the
  maintainers do.

### Reporting a Vulnerability

Report a suspected vulnerability privately, through the channel the project
publishes for it - usually the address or security form in the crate's
`SECURITY.md` or in the repository's `SECURITY.md`. Never open a public
issue for a working exploit, and never publish a proof of concept before a
fix is released.

---

## Nix / Flake Infrastructure (Code Infrastructure, Tooling, Build, Test, Deploy)

### Core Principle

**Nix is the single source of truth for the entire development environment.** Every tool, compiler, SDK, library, and dependency required to build, test, run, and deploy the project must be declared and pinned in the flake (`flake.nix` / `flake.lock`). Nothing lives outside the flake. The host system is treated as bare hardware: it provides only the Nix package manager itself. All compilers, interpreters, build tools, system libraries, SDKs, and runtime dependencies are provided by the dev shell or flake outputs, never by global system installations.

If a tool is needed, it must be added to the flake — never installed globally via `cargo install`, `apt`, `brew`, `npm -g`, `rustup`, `pip`, or any other package manager outside Nix.

### Why Nix Flakes for an Agent

1. **Reproducible environments.** `flake.lock` pins every input commit. `nix develop` produces the exact same compiler versions, toolchain, and dependencies on every machine, every time. There is no "works on my machine" problem.

2. **No host pollution.** Nothing is installed globally. The dev shell is a pure, isolated environment that exists only for the duration of the session. This eliminates dependency conflicts between projects and keeps the host clean.

3. **Declarative tooling.** Every compiler, SDK, linker, and system library is declared explicitly in `flake.nix`. An agent can read `flake.nix` and know exactly what tools are available and what versions they are — no guessing, no `which` or `where` hunting.

4. **Cross-compilation without complexity.** Rust targets, WASM targets, Android NDK toolchains, and cross-linkers are declared in the flake. The agent never needs to install NDKs, SDKs, or cross-compilers manually; they appear automatically in the dev shell environment variables (`CARGO_TARGET_*_LINKER`, `CC_*`, `AR_*`, etc.).

5. **Verifiable builds.** `nix build` produces store paths that are content-addressed and immutable. If it builds in the flake, it builds identically everywhere.

6. **Self-documenting.** The flake IS the documentation. `flake.nix` lists every input, every system dependency, every Rust target, every build input. `flake.lock` pins every transitive dependency. No separate README "installation instructions" section can drift out of date because the flake is executable.

### How the Agent Should Work with Flakes

**Enter the environment first.** Always start work inside the dev shell:

```bash
nix develop
```

(or via direnv if `.envrc` is present). Never run compilers, test runners, or linters outside `nix develop` — if a command works only outside the shell, the flake configuration is incomplete and must be fixed in `flake.nix`, not worked around.

**Treat `flake.nix` as project infrastructure.** When adding a dependency (language package, system library, SDK, compiler toolchain, cross-compilation target), add it to `flake.nix` first. The flake is the project's `package.json` / `Cargo.toml` / `requirements.txt` for system-level tooling, and it must stay in sync with any language-level lock files.

**Pin everything.** `flake.lock` must be committed. Do not regenerate it without understanding what changed and updating all dependent lock files. Language-level lock files (e.g. `Cargo.lock`, `package-lock.json`, `poetry.lock`) must be pinned and kept in sync with the flake's `cargoLock` / `npmLock` / equivalent declarations.

**Use flake outputs for reusable commands.** Common operations (build, test, format, lint, serve, release) should be exposed as flake apps (`apps.` outputs) or dev shell helper scripts so they can be invoked via `nix develop -c <command>` or `nix run .#<command>`. This makes the project's operational interface explicit and versioned.

**Never assume host tools.** The agent must never rely on `rustc`, `cargo`, `node`, `npm`, `python`, `gradle`, `javac`, `make`, `gcc`, or any other tool being installed on the host. If the project needs it, it belongs in `flake.nix`.

**Use the flake's toolchain, not manually located store binaries.** Do not hunt for `rustc`, `cargo`, or other compilers under `/nix/store` directly. Binaries found there may be older than the flake's declared toolchain and lack features needed by dependencies (for example, `rustc 1.86` does not support `let_chains`, which some crates require). Always enter the dev shell via `nix develop` so the flake's pinned `rustToolchain` and `shell.packages` are on `PATH`.

**Run build and test commands from the manifest directory.** Commands such as `cargo test`, `cargo clippy`, `npm test`, or `mix test` must be run inside the directory that contains the project manifest (`Cargo.toml`, `package.json`, `mix.exs`, etc.), not from the flake repository root. The root typically has no manifest of its own.

**Declare frequently-run commands as flake shell applications.** Inline commands passed to `nix develop -c '...'` can fail because the dev shell environment is not fully applied to the inline invocation. For commands run regularly, define them as `pkgs.writeShellApplication` entries in `flake.nix` (like the `verify`, `profile`, and `coverage` helpers shown in this project's flake), so they are versioned, reusable, and invocable via `nix develop -c <name>` or `nix run .#<name>`.

**System services might not be running.** `nix develop` provides client libraries and tools, but whether databases, caches, or other daemons are running depends on the project setup — some services may start automatically via process-compose or direnv hooks, others may not. Integration tests that need Postgres, Redis, or similar services should verify the service is reachable before running, and start it manually if needed.

**Cross-compilation is declared, not improvised.** If the project compiles for WASM, Android, iOS, or other non-native targets, the target triplets and their linkers must be declared in the flake toolchain configuration. The agent should read the flake's toolchain section to discover available targets rather than installing SDKs manually.

**If it builds in the flake, it ships from the flake.** `nix build` produces immutable store paths. Releases, deployments, and CI should consume these store paths directly, never ad-hoc host builds.

**Keep the project directory free of temporary files.** When writing, testing, or debugging code, any temporary files, scratch work, experiment outputs, or leftovers must be located in a `/tmp` subdirectory (e.g. `/tmp/opencode/...`), never inside the project repository. The project directory must not be polluted with temporary files or artifacts from agent work, testing, or experimentation. If a tool writes outputs by default into the project tree (coverage reports, flamegraphs, build artifacts, etc.), either redirect them to `/tmp` or add them to `.gitignore` and clean them up before finishing.

### Reproducibility Rules

1. **All dependencies are declared in the flake.** No system packages, no language-version managers (`rustup`, `nvm`, `pyenv`, `asdf`), no global package installs. Every tool needed for build, test, or deploy is in `flake.nix`.
2. **`flake.lock` is committed and pinned.** It records exact input commits. Do not regenerate it without reviewing the diff and updating all dependent language-level lock files.
3. **Language-level lock files are pinned.** `Cargo.lock`, `package-lock.json`, `poetry.lock`, `Gemfile.lock`, etc. are committed and kept in sync with the flake's lock declarations.
4. **When adding a dependency, update both layers.** Add the system dependency to `flake.nix` and update the language-level lock file inside `nix develop`.
5. **When adding a tool, add it to the dev shell.** Any compiler, linter, formatter, SDK, or system library needed for development must appear in `shell.packages` or as a flake output. Do not expect it on the host.
6. **When adding a compilation target, add it to the toolchain.** Cross-compilation targets (WASM, Android, iOS, embedded) and their linkers are declared in the flake's toolchain configuration.
7. **Use `nix develop -c <command>` for reproducible, scriptable execution.**

### Host System Assumptions

The host system provides only:

- The Nix package manager (single-user or multi-user)
- Git (for reading the repository)

Everything else — compilers, interpreters, build tools, SDKs, system libraries, fonts, runtimes — is provided by the flake. If a command fails because a tool is missing, the fix is to add that tool to `flake.nix`, not to install it on the host.
