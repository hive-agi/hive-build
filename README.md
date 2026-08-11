# hive-build — the canonical release library for hive Clojure packages

`hive-build.api` is the shared `tools.build` entrypoint every hive package uses
to jar, verify, version and publish. Consumers add one alias and get the whole
pipeline — there is **no per-repo `build.clj`**.

```clojure
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
| `deploy`         | jar + publish current `VERSION` per `version.edn :publish`        |

`deploy` publishes to `:clojars`, `:gitea`, `:gitea-source`, or `:none` as
declared in `version.edn`, and is idempotent — a version already in the target
registry HEAD-checks and skips.

## Publishability

A library is publishable only if every **runtime** `:deps` entry is
`:mvn/version`. `:git/tag` / `:git/sha` / `:local/root` runtime coords cannot
form a complete Maven pom — keep those in `:test` / `:dev` aliases, which are
excluded from the pom.

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
