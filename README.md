# hive-build — the canonical release library for hive Clojure packages

<!-- hive-badges -->

[![Clojars Project](https://img.shields.io/clojars/v/io.github.hive-agi/hive-build.svg)](https://clojars.org/io.github.hive-agi/hive-build)
[![cljdoc](https://cljdoc.org/badge/io.github.hive-agi/hive-build)](https://cljdoc.org/d/io.github.hive-agi/hive-build/CURRENT)
[![release](https://github.com/hive-agi/hive-build/actions/workflows/release.yml/badge.svg)](https://github.com/hive-agi/hive-build/actions/workflows/release.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

<!-- /hive-badges -->

`hive-build.api` is the shared `tools.build` entrypoint every hive package uses
to jar, verify, version and publish. Consumers add one alias and get the whole
pipeline — there is **no per-repo `build.clj`**.

```edn
;; deps.edn, under :aliases
:build {:deps {io.github.hive-agi/hive-build {:mvn/version "0.1.0"}}
        :jvm-opts ["-Xmx1g"]
        :ns-default hive-build.api}
```

Version source of truth is each repo's **`VERSION` file** — the same value its
release workflow tags as `v{VERSION}` — so the Maven coord matches the git-tag
coord 1:1. Per-repo coordinates (`:lib :minor :license :scm-url :src-dirs
:publish`) live in `./version.edn`.

## Tasks

`clojure -T:build <task>`:

| task             | effect                                                            |
|------------------|------------------------------------------------------------------|
| `clean`          | remove `target/`                                                  |
| `jar`            | build a source jar under `target/`                               |
| `jar-aot`        | AOT jar (apps, not libs)                                          |
| `install`        | jar + install to local `~/.m2` (offline verification, no network)|
| `bump`           | read/write `VERSION` — `bump :level :patch\|:minor\|:major`       |
| `verify-license` | assert the declared license is present and consistent            |
| `kondo`          | clj-kondo gate                                                    |
| `changelog`      | regenerate `CHANGELOG.md` from the commits between release tags   |
| `readme-examples`| run `README.md`'s clojure blocks in a new JVM, refute `;; =>` claims |
| `deploy`         | jar + publish current `VERSION` per `version.edn :publish`        |

`deploy` publishes to `:clojars`, `:gitea`, `:gitea-source`, or `:none` as
declared in `version.edn`, and is idempotent — a version already in the target
registry HEAD-checks and skips.

## Changelog

`changelog` regenerates `CHANGELOG.md` from the conventional-commit history
between `v*` tags, in [Keep a Changelog](https://keepachangelog.com/en/1.1.0/)
1.1.0 shape. `feat` becomes Added, `fix` Fixed, `perf` Faster, `refactor`
Changed; breaking changes are lifted above every section whatever their kind;
`docs`/`test`/`build`/`ci`/`chore`/`style`/`revert` are counted in one line
rather than printed, so a release of pure housekeeping reads as one instead of
as an empty release.

```
clojure -T:build changelog                 # newest 25 releases
clojure -T:build changelog :depth 50
```

The file is **generated and disposable**: every run rewrites it in full, and
nothing authored inside it survives. What a generator cannot derive is authored
in `changelog.d/` instead and spliced in:

| path                      | effect                                            |
|---------------------------|---------------------------------------------------|
| `changelog.d/<version>.md`| prose under that release's heading                |
| `changelog.d/preamble.md` | replaces the generated header                     |

That is what keeps the file and the storefront's release notes from disagreeing:
`hive-build.promote.notes` is the single definition of what a release note is,
and both are projections of it. Divergence gets fixed by deduplicating the
definition, never by writing the changelog a second time by hand.

## README examples

`readme-examples` treats every fenced ` ```clojure ` block in `README.md` as a
claim that the code runs against this library, and every `;; => value` line
after a form as a claim about what it answers. The blocks are rendered into
one program under `target/` and run with `clojure -M` in a **new JVM** on the
project's own classpath, so nothing passes because the session that wrote the
README happened to have it loaded. A refuted claim or a form that throws
exits non-zero and the task throws, which is the point: it runs before `bump`
in a release workflow, next to the tests.

```
clojure -T:build readme-examples                    # README.md, project :deps
clojure -T:build readme-examples :aliases '[:test]' # with an alias on the classpath
```

Mark a block ` ```clojure no-run ` to keep it out. A README with no runnable
block claims nothing and passes. The observed value is what goes after `;; =>`,
pasted from a cold run, never paraphrased from a docstring.

## Publishability

A library is publishable only if every **runtime** `:deps` entry is
`:mvn/version`. `:git/tag` / `:git/sha` / `:local/root` runtime coords cannot
form a complete Maven pom — keep those in `:test` / `:dev` aliases, which are
excluded from the pom.

## Self-hosting lag

hive-build packages itself with the **published** hive-build named by its own
`:build` alias, so a change to how artifacts are packaged reaches hive-build's
own jar one release later than it reaches everyone else's. A release that
introduces such a change therefore ships the new behaviour in its source and
not in its own artifact; the release after it, with the `:build` alias bumped,
is the first one built by it. Verify with:

```bash
unzip -p target/hive-build-<version>.jar '*/pom.xml' | grep -c '<repositories'
```

## Scaffolding a new repo

Use **[`bb-build`](../bb-build)** — a lein-new-style generator that writes a
repo's `version.edn` + release workflow and prints the `:build` alias above:

```bash
bb-build new hive-help ../hive-help              # public -> Clojars
bb-build new hive-premium ../hive-premium --kind gitea   # private -> Gitea Maven
```

## License

MIT — see [LICENSE](LICENSE). Its security-free build tooling is safe to be
public; it sits on the release path of every hive package, so its dependency
surface (`tools.build`, `deps-deploy`, `malli`) is deliberately small.
