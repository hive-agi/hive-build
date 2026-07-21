# hive-build — Clojars publishing infra for hive leaf libs

Version source of truth is each repo's existing **`VERSION` file** (the same value
its `release.yml` tags as `v{VERSION}`), so the Clojars coord matches the git-tag
coord 1:1 — migrating a consumer is `{:git/tag "v0.5.1"}` → `{:mvn/version "0.5.1"}`.
If a repo has no `VERSION` file, build.clj falls back to datahike-style
`0.{minor}.{git-commit-count}` (minor from `version.edn`, needs CI `fetch-depth: 0`).

This publishing runs *alongside* the existing GitHub tag-release `release.yml` — it
lives in a separate `clojars.yml` and does not replace it.

## Files
- `build.clj` — generic `tools.build` script (jar / install / deploy). Reads
  per-repo coordinates from `./version.edn`. Copied verbatim into each repo.
- `version.edn` (per repo) — `{:lib :minor :license :scm-url :src-dirs}`.
- `workflows/release.yml` — GitHub Action: publishes on every push to `main`.
- `rollout.sh` — installs the above into a target leaf-lib repo.

## One-time Clojars setup
1. Clojars account → **Deploy Tokens** → create a token.
2. Verify the group **`io.github.hive-agi`**: create a public GitHub repo
   `hive-agi/clojars-io.github.hive-agi` (Clojars checks org ownership). This
   group matches the existing `:git/tag` coord names, so consumers swap
   `{:git/tag ..}` → `{:mvn/version "0.MINOR.NNNN"}` with no rename.
3. GitHub repo secrets: `CLOJARS_USERNAME`, `CLOJARS_DEPLOY_TOKEN`.

## Per-repo (leaf libs only)
A lib is publishable only if every **runtime** `:deps` entry is `:mvn/version`.
`:git/tag` / `:local/root` runtime deps cannot go into a Maven pom — keep those
in `:test`/`:dev` aliases (which are excluded from the pom) or the consumer's
graph will be incomplete.

```bash
./rollout.sh ../hive-help hive-help 1     # drops build.clj + workflow + version.edn
# add the :build alias to deps.edn (rollout prints it)
(cd ../hive-help && clojure -T:build install)   # verify locally (~/.m2), no network
```

## Publish
```bash
export CLOJARS_USERNAME=... CLOJARS_PASSWORD=<token>
clojure -T:build deploy
```
Or just push to `main` and let the Action do it.

## Tasks
| `clojure -T:build <task>` | effect |
|---|---|
| `jar`     | build source jar under `target/` |
| `install` | jar + install to local `~/.m2` (offline verification) |
| `deploy`  | jar + push to Clojars (needs token env) |

## Migrating consumers off git coords
Once a lib is on Clojars, replace in downstream `deps.edn`:
`io.github.hive-agi/hive-help {:git/tag "v0.1.0" :git/sha "…"}` →
`io.github.hive-agi/hive-help {:mvn/version "0.1.NNNN"}`.
`bb-depsolve upgrade --apply` then keeps the `:mvn/version` pins current.

## Not covered (yet)
- AOT / uberjars (these are source jars — right for libs).
- Repos with git/local runtime deps (hive-knowledge, hive-mcp): app-like, keep on git coords.
