{pkgs ? import <nixpkgs> {}}: let
  unst = import ./nixpkgs-unstable.nix;
  nixpak = import ./nixpak.nix;
  mkNixPak = nixpak.lib.nixpak {
    inherit (pkgs) lib;
    inherit pkgs;
  };
  app = pkgs.writeShellApplication {
    name = "vibe";
    text = "alacritty";
    runtimeInputs = with pkgs; [
      (import ./../pub/vi/nix/default.nix {ai = true;})
      fontconfig
      busybox
      curl
      wget
      git
      ripgrep
      alacritty
      xdg-utils
      python3Minimal
      tmux
      nix
      direnv
      nix-direnv
      asciinema
      asciinema-agg
      (writeShellApplication {
        name = "notify-user";
        text = ''
          for _ in $(seq 3); do
            ${mpv}/bin/mpv \
              "${sound-theme-freedesktop}/share/sounds/freedesktop/stereo/complete.oga"
          done
        '';
      })
    ];
  };
  passwd = pkgs.writeTextFile {
    name = "passwd";
    text = "vibe:x:1000:1000:vibe:/tmp:/bin/sh";
  };
  agentsMd = pkgs.writeTextFile {
    name = "AGENTS.md";
    text = ''
      ## Coding Guidelines

      ### Project Understanding

      * Read the `README.md` file if available.
      * Study the project source code before making changes.
      * Understand the target module, its dependencies, surrounding code, and nearby tests before modifying it.
      * Follow the existing architecture, abstractions, naming conventions, APIs, and established project patterns.
      * Examples of good code can be found in the latest versions of the crates `functora-tagged` and `functora`. Study their source code and use a similar style.

      ### Monorepo

      * Take advantage of the git monorepo as the single source of truth for all code: prefer the local monorepo copies of libraries over external releases.
      * Depend on the local monorepo versions of shared crates (e.g., `functora-tagged`, `functora`) rather than the versions hosted in the crates registry.
      * First extend and improve the shared library in the monorepo when a reusable generalization, refinement, refinement bug fix, or other widely beneficial capability is genuinely warranted across multiple crates.
      * Then consume the improved monorepo library from the dependent crates, rather than duplicating the behavior or waiting for a crates.io release.
      * Do not generalize or refactor libraries speculatively; extend them only with real, concrete, reproducible need as demonstrated by actual use.
      * If it makes sense to add widely used invariant-preserving behavior to `functora-tagged`, implement it in the monorepo and use the monorepo `functora-tagged` instead of the crates-registry version.
      * Ensure that any change to a shared library keeps its public contracts stable, or updates all affected callers and tests in the same change.

      ### Change Discipline

      * Before making changes, understand the existing behavior and identify the smallest appropriate change.
      * For bug fixes, **first reproduce the bug with a failing automated test**, then fix the production code. Do not patch production code first and add a test afterward.
      * Prefer small, local, targeted changes over broad rewrites or unrelated refactoring.
      * Do not reformat, reorganize, rename, or otherwise modify unrelated files or code.
      * Preserve existing behavior outside the scope of the requested change.
      * Do not weaken compiler, linter, formatting, test, logging, security, or other project settings to make a change pass.
      * Do not disable warnings, suppress lints, reduce test coverage requirements, remove validation, or relax security checks as a workaround.
      * Do not introduce abstractions unless they remove real duplication, define a real boundary, or provide a clear architectural benefit.
      * Do not introduce speculative abstractions or generalized frameworks for a single use case.
      * Do not accidentally change public JSON, API contracts, configuration shapes, serialization formats, module boundaries, or other externally observable behavior.
      * Treat public interfaces and data formats as stable unless changing them is explicitly required by the task.
      * When a public contract must change, make the change explicit and update all affected tests and callers.

      ### Functional Style

      * Write code in a **strictly functional style while ensuring high efficiency**.
      * Avoid mutable variables (`mut`) whenever possible.
      * Avoid imperative constructs such as `for`, `while`, `loop`, and `return`.
      * Instead, use functional iterators, combinators, expression-oriented control flow, method chaining, pipelining, and the `?` operator.
      * Prefer immutable data and explicit data flow.
      * Eliminate redundant closures and variables.
      * Eliminate redundant code.
      * Maximize method chaining. Always favor chaining and pipelining over temporary one-time variables.
      * Keep code as flat as possible.
      * Avoid unnecessary nesting of code blocks and nested brackets such as `(.. (..) ..)` or `{.. { .. } ..}`.
      * Deal with complexity through `?`, chaining, iterators, and piping rather than deeply nested control flow.
      * Prefer small, focused functions with a single clear responsibility.
      * Introduce a temporary variable when it materially improves readability, avoids repeated computation, or is required by ownership or lifetime constraints. Do not create one merely to hold a value used once.
      * Always prefer `(..expr..).pipe(Ok)` over `Ok(..expr..)` unless `(..expr..)` is very short and simple.
      * Use meaningful, context-clear identifiers with a preference for brevity when the meaning remains unambiguous.
      * Prefer domain-specific names over generic names such as `data`, `value`, `thing`, `manager`, `helper`, or `utils` when a more precise name is appropriate.
      * Do not include comments. The code must be self-explanatory through its types, names, structure, and abstractions.
      * Keep the code DRY.
      * Remove redundant code rather than preserving duplication.
      * Do not sacrifice correctness, readability, or efficiency merely to make code shorter or eliminate a variable.

      ### Efficiency and Dependencies

      * Prefer `const` over non-`const` wherever possible, including `const` functions, values, and bindings, so that computation happens at compile time and immutability is guaranteed.
      * Prefer `&str` over `String` wherever possible, including for parameters, return values, and data fields, when it does not add a lot of redundant complexity such as awkward lifetimes or generic parameters.
      * Avoid `.clone()` unless cloning is genuinely required by ownership semantics.
      * Avoid unnecessary allocations, copies, conversions, intermediate collections, and other needless work.
      * Prefer zero-cost abstractions and efficient iterator-based solutions.
      * Prefer the standard library exclusively whenever possible.
      * Avoid extra dependencies.
      * Introduce a dependency only when it provides substantial value that cannot reasonably be achieved with the standard library or existing dependencies.
      * Keep dependencies narrow and minimal.
      * Do not add a dependency merely for convenience when an existing dependency or the standard library provides an appropriate solution.

      ### Error Handling

      * **The code must be panic-free.**
      * Do not use `panic!`, `unreachable!`, `unimplemented!`, `todo!`, `unwrap()`, `expect()`, or other panic-producing operations in production code unless the operation is provably unreachable and there is no fallible alternative.
      * Every fallible operation must be explicitly handled.
      * Never swallow, suppress, ignore, or silently discard errors.
      * If an error cannot be meaningfully recovered from at the current layer, propagate it explicitly using `Result::Err` and `?`.
      * Handle errors at the layer that has enough context to recover meaningfully; otherwise propagate them upward.
      * Use `?` instead of `.map_err(..)` wherever possible.
      * Do not use `map_err` merely to repackage an error that can be propagated directly with `?`.
      * Use `map_err` when meaningful typed context must be added and cannot be expressed cleanly otherwise.
      * Use explicit, domain-specific error enums.
      * Error enum variants must precisely describe what happened or which operation failed.
      * Avoid vague variants such as `Error`, `Failure`, `Custom`, `Other`, or `Unknown` when a precise variant is possible.
      * **Never lose information from an underlying error.**
      * Error variants must preserve the complete underlying error by wrapping the original error value rather than converting it to a string or replacing it with a generic error.
      * Never stringify errors for propagation, including patterns such as:
        `map_err(|e| Error::Something(e.to_string()))`.
      * Prefer typed variants such as:
        `Error::Something(e)`.
      * When additional context is required, store it alongside the underlying error:
        `Error::Something { context, source }`.
      * Do not use `Box<dyn Error>`, `String`, or other type-erasing representations merely to avoid defining a precise error enum when the underlying error type is known.
      * Error conversions must preserve all meaningful information and the strongest useful type information.
      * Do not catch an error only to rethrow it unchanged. Use `?` directly.
      * Do not convert between error types unless the conversion is intentional, typed, and preserves all relevant information.

      ### Resource Safety and Concurrency

      * Avoid global mutable state.
      * Prefer explicit ownership and dependency passing over hidden global state or implicit shared state.
      * Avoid hidden blocking calls, especially in asynchronous or concurrent code.
      * Blocking operations must be explicit and execute in an appropriate context.
      * Avoid unbounded concurrency.
      * Concurrency must have explicit and appropriate bounds.
      * Avoid uncontrolled task spawning, unbounded queues, unbounded memory growth, and other forms of resource exhaustion.
      * Prefer bounded concurrency, backpressure, and explicit resource ownership where applicable.
      * The code must be strictly free of memory leaks, race conditions, and deadlocks at every level, backend and frontend alike: connections, pools, caches, event listeners, subscriptions, timers, and DOM references included.
      * Memory must be released deterministically; nothing may persist past its last use, and no object may be retained indirectly through listeners, closures, or caches once it is no longer needed.
      * Shared state accessed from concurrent or async contexts must be properly synchronized; no data race or torn read may ever be possible.
      * Locking disciplines must be minimal and uniform so that cycles, reentrancy, double-acquire, and other deadlock sources are impossible by construction.
      * **There must be no zombie or unmanaged threads.** Every thread, task, or spawned process must have an explicit owner, a bounded and known lifetime, and a deterministic teardown path that runs even when the surrounding operation fails, is cancelled, or its error is ignored.
      * Threads, tasks, and processes must be cleaned up when their operation ends, fails, or is forgotten; no leftover worker may keep running in the background.
      * Prevent threads from outliving their owner: joins, cancels, drops, and shutdown hooks must be explicit and reachable on all control-flow paths, including early returns, `?`, and `panic`.
      * Unbounded or unclaimed work must be detectable: track spawned work so none can be silently abandoned, and fail loudly if cleanup is impossible.
      * Ensure resources are released deterministically and safely.
      * Do not hide internal I/O, allocation, synchronization, or blocking operations behind innocuous-looking APIs.
      * Prefer resource-safe designs whose ownership and lifetime behavior is apparent from the types and APIs.

      ### Abstractions and API Design

      * **Strongly avoid introducing custom traits unless absolutely necessary.**
      * Always prefer generics (parametric polymorphism) over ad-hoc (trait) polymorphism for code abstraction and generalization.
      * Prefer generic functions and types over trait definitions whenever a plain generic parameter suffices; introduce a trait only when it defines a real, well-defined boundary or behavior that generics by themselves cannot express.
      * Avoid unnecessary traits.
      * Traits with only one implementation should normally be avoided unless there is a strong, concrete reason to introduce the abstraction.
      * A single-implementation trait is justified only when it provides a required external API, meaningful generic abstraction, object-safe boundary, associated-type abstraction, compile-time polymorphism, or a well-defined architectural boundary that cannot be expressed with a plain generic parameter.
      * Do not introduce traits merely for speculative future implementations, test mocking, or abstraction for its own sake.
      * Avoid large utility objects that accumulate unrelated responsibilities or become dumping grounds for miscellaneous functionality.
      * Prefer small, focused types and functions with clear responsibilities.
      * Avoid large argument lists.
      * When several arguments form a coherent domain concept, consider representing that concept with an explicit domain-specific type rather than passing many loosely related values.
      * Do not introduce parameter objects merely to avoid a reasonable number of arguments.
      * Prefer explicit public API types over vague, overly generic, or type-erased interfaces.
      * Keep public APIs narrow, predictable, and domain-specific.
      * Avoid accidental changes to public JSON representations, configuration shapes, serialization formats, module boundaries, or API contracts.

      ### Macros

      * **Strongly avoid implementing custom macros unless absolutely necessary.** Prefer conventional Rust, plain traits, generics, and derives, which are idiomatic, readable, and debuggable.
      * Treat macros as a last resort, used only when language expressiveness is genuinely insufficient.
      * Prefer libraries that deliver the same expressiveness without macros (e.g., `functora-tagged`'s macro-free newtypes and derived traits) over reaching for a macro.

      ### Type System and Invariants

      * **CRITICAL: when modeling data and state, always prefer algebraic data types (ADTs, enums) over flat mega-structures with multiple mutually exclusive fields and boolean or other flags indicating the current state variant.**
      * **If parts or fields of the state are mutually exclusive, it MUST be an enum with separate variants, not a struct with dispatching flags and implicit logic.**
      * **It must NOT be possible to construct invalid state at all; each enum variant SHOULD represent a valid state.**
      * **This applies to state transitions as well: moving from one variant to another must go through the type system (e.g., a `match` that maps one enum variant to the appropriate valid target variant), never through runtime field toggling or flag mutation that permits transient or invalid intermediate states.**
      * Prefer types that guarantee invariant preservation through the type system instead of locally checking invariants in multiple places.
      * Make invalid states unrepresentable by encoding constraints in the type, enforced once at construction and preserved by every operation.
      * Prefer `functora-tagged` newtypes over blind scalar types (`u64`, `String`, `f64`, and similar) to give values precise, invariant-preserving types.
      * Use refined newtypes such as `Tagged<T, D, F>` with appropriate refineries (`FCrude`, `FPositive`, `FNonNeg`, `FNonEmpty`, `FZeroInclToOneIncl`, etc.) so that invariants are checked once and subsequently guaranteed, eliminating repeated checks, `unwrap()`, and `expect()` at call sites.
      * Combine multiple refinement rules with a single flat composite refinery rather than nesting `Tagged` types.
      * Prefer the infallible, invariant-exploiting operations of refined newtypes (e.g., `FNonEmpty` methods like `first()`, `last()`, `minimum()`, `singleton()`, `extend()`, `map()`) over fallible or `Option`-returning standard-library analogues that require local handling.
      * Use newtype dimensions to distinguish values with identical representations (e.g., `UserId` vs `ProductId`) and prevent accidental misuse.
      * Prefer dimensional types from the `num` module (`Identity`, `Atomic`, `Times`, `Per`) over raw numerics when units are involved, so that mixing units (e.g., adding meters to seconds) is rejected at compile time.
      * Verify that invariants are actually preserved by property-based testing, not just a handful of hand-picked examples, especially for roundtrip behaviors such as encode-decode, `FromStr`/`Display`, serialize/deserialize, and similar roundtrips.

      ### Avoid Scalar Blindness and Scalar Overloading

      * **Avoid scalar blindness: never model a value with a blind raw scalar (`String`, `Int`/`u64`, `Bool`, `f64`) when a precise, domain-specific type exists; types must stay meaningful even outside the defining module.**
      * Wrap same-representation domain concepts in separate newtypes (`Password`, `PasswordSalt`, `PasswordHash`, `UserId` vs `ProductId`) so that accidental swaps become compile-time errors.
      * **Avoid scalar overloading: never represent a smaller, fixed set of possibilities with a wider, overloaded type.**
      * Do not use a `String` where a finite enum belongs (e.g. a `String` locale accepting `"fr"`, `"fr-fr"`, `"fr-FR"`, and any other string); use `Locale = En | Fr | Ru | Ee` so invalid values cannot even be represented.
      * Do not use an `Int`/`u64` or `f64` where a finite enum or unit-denoted type belongs (e.g. a bare integer currency code instead of `Currency = Usd | Eur | Btc`); restrict the domain to what is actually valid.
      * Avoid boolean blindness: when the meaning of a `Bool` depends on the surrounding field or call site, replace it with a precise two-variant enum (`FilterResult = Discard | Keep`, `TimeKind = WorkingTime | BreakTime`, `Strictness = Strict | NotStrict`); a bare boolean loses its context outside the data model.
      * When a function takes multiple scalars that denote distinct domain concepts, wrap them in named types instead of passing a positional sequence of same-typed scalars, so the compiler rejects misplaced or meaningless argument combinations.

      ### Complexity and Structure

      * Prefer simple, composable expressions over deeply nested control flow.
      * Use `?` to flatten fallible control flow.
      * Use method chaining, iterator combinators, and piping to flatten data transformations.
      * Prefer early error propagation over deeply nested `match` or `if` structures.
      * Avoid nested blocks whenever a flatter equivalent is clear.
      * Avoid temporary one-time variables when the expression can be safely and clearly inlined.
      * Use variables when they improve readability, prevent repeated computation, or are required by ownership or lifetime constraints.
      * Keep functions small and focused.
      * Keep types focused and avoid types with excessive responsibilities.
      * Avoid large utility modules and miscellaneous helper collections.
      * Prefer explicit domain boundaries over generic catch-all abstractions.

      ### Testing

      * For bug fixes, **first add a regression test that fails against the existing implementation** and demonstrates the bug. Only then modify production code.
      * Add tests for all new behavior and relevant edge cases.
      * Test error and failure paths explicitly.
      * Tests must verify observable behavior and public contracts rather than mirror implementation details.
      * Avoid implementation-mirroring tests that assert private structure, incidental call sequences, specific algorithms, or internal implementation details when multiple implementations could satisfy the same behavior.
      * Prefer deterministic tests.
      * Prefer property-based testing (e.g., `proptest` or `quickcheck`) for invariant-preserving behaviors such as encode-decode roundtrips, using a fixed seed and shrinking so that any failing case is minimal and reproducible.
      * Avoid reliance on timing, arbitrary sleeps, uncontrolled randomness, ambient global state, external services, or machine-specific behavior unless the behavior itself is what is being tested.
      * Do not write tests solely to increase coverage; tests must provide meaningful behavioral guarantees.
      * All tests should be located in the `./tests` directory.
      * Test coverage should be as high as possible, with particular attention to error paths, edge cases, and failure conditions.

      ### Validation

      * To check Rust code, use:
        `cargo clippy --all-features`
      * The code should compile successfully and have no warnings or errors.
      * Do not suppress or disable warnings to make the code pass.
      * To test Rust code, use:
        `cargo test --all-features`
      * To get the Rust test coverage report, use:
        `cargo tarpaulin --all-features --engine llvm -o Lcov`
      * The coverage report is written to `lcov.info`.
      * Do not consider the task complete until the implementation passes the relevant checks.
      * Do not weaken or bypass any validation to make the implementation pass.

      ### Finalization

      * The final step is always to format the code with:
        `cargo fmt`
      * Do not make code changes after the final `cargo fmt` step.
      * Do not reformat unrelated files merely to satisfy formatting.
      * Before finishing, verify that only files relevant to the task were changed.
      * Verify that no unintended public behavior, configuration, API, serialization format, module boundary, or project setting was modified.
      * The resulting code must be formatted, warning-free, panic-free, resource-safe, and fully tested.
    '';
  };
  codexToml = pkgs.writeTextFile {
    name = "codex";
    text = ''
      profile = "free"
      sandbox_mode = "danger-full-access"

      [features]
      unified_exec = true
      streamable_shell = true
      apply_patch_freeform = true
      web_search_request = true
      ghost_commit = true

      [profiles.free]
      model = "arcee-ai/trinity-large-preview:free"
      model_provider = "openrouter"

      [profiles.lite]
      model = "gemini-2.5-flash-lite"
      model_provider = "gemini"

      [profiles.std]
      model = "gemini-2.5-flash"
      model_provider = "gemini"

      [profiles.pro]
      model = "gemini-2.5-pro"
      model_provider = "gemini"

      [model_providers.openrouter]
      name = "openrouter"
      base_url = "https://openrouter.ai/api/v1"
      env_key = "OPENROUTER_API_KEY"

      [model_providers.gemini]
      name = "gemini"
      base_url = "https://generativelanguage.googleapis.com/v1beta/openai"
      env_key = "GEMINI_API_KEY"
    '';
  };
  sandbox = mkNixPak {
    config = {sloth, ...}: {
      app.package = app;
      gpu.enable = true;
      gpu.provider = "bundle";
      fonts.enable = true;
      fonts.fonts =
        builtins.filter pkgs.lib.attrsets.isDerivation (
          builtins.attrValues pkgs.nerd-fonts
        )
        ++ [
          pkgs.dejavu_fonts
          pkgs.noto-fonts-color-emoji
        ];
      pasta.enable = true;
      locale.enable = true;
      etc.sslCertificates.enable = true;
      bubblewrap = {
        network = true;
        sockets.pulse = true;
        sockets.wayland = true;
        bind.ro = [
          "/bin/sh"
          "/usr/bin/env"
          "/run/current-system/sw/bin/bash"
          "/run/current-system/sw/bin/less"
          [(toString passwd) "/etc/passwd"]
          ["${pkgs.bash}/bin/bash" "/bin/bash"]
          (sloth.concat' sloth.homeDir "/.bashrc")
          (sloth.concat' sloth.homeDir "/.bash_profile")
          (sloth.concat' sloth.homeDir "/.config/tmux")
          (sloth.concat' sloth.homeDir "/.config/alacritty")
          (sloth.concat' sloth.homeDir "/.config/nvim/coc-settings.json")
          [
            (toString agentsMd)
            (sloth.concat' sloth.homeDir "/.qwen/QWEN.md")
          ]
          [
            (toString agentsMd)
            (sloth.concat' sloth.homeDir "/.codex/AGENTS.md")
          ]
          [
            (toString agentsMd)
            (sloth.concat' sloth.homeDir "/.gemini/GEMINI.md")
          ]
          [
            (toString agentsMd)
            (sloth.concat' sloth.homeDir "/.claude/CLAUDE.md")
          ]
          [
            (toString codexToml)
            (sloth.concat' sloth.homeDir "/.codex/config.toml")
          ]
          [
            (toString ../cfg/opencode.json)
            (sloth.concat' sloth.homeDir "/.config/opencode/opencode.jsonc")
          ]
          [
            (toString ../cfg/opencode-notify.ts)
            (sloth.concat' sloth.homeDir "/.config/opencode/notify.ts")
          ]
        ];
        bind.rw = [
          [
            (sloth.mkdir (sloth.concat' sloth.homeDir "/vibe"))
            sloth.homeDir
          ]
        ];
        tmpfs = [
          "/tmp"
        ];
        env.NIX_CONFIG = "experimental-features = nix-command flakes";
        env.NIXPKGS_ALLOW_UNFREE = "1";
      };
    };
  };
in
  sandbox.config.env
