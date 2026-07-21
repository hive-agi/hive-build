#!/usr/bin/env bash
# Install the hive Clojars-publishing infra into a target leaf-lib repo.
#
#   rollout.sh <repo-dir> <lib-short-name> [minor]
#   e.g. rollout.sh ../hive-help hive-help 1
#
# Drops build.clj + .github/workflows/release.yml, generates version.edn, and
# reminds you to add the :build alias. Does NOT deploy — run `clojure -T:build
# deploy` yourself once CLOJARS_USERNAME/CLOJARS_PASSWORD are set.
#
# WARNING: only for LEAF libs whose runtime :deps are all :mvn/version. Libs with
# :git/tag or :local/root runtime deps cannot produce a complete Maven pom.
set -euo pipefail

INFRA="$(cd "$(dirname "$0")" && pwd)"
REPO="${1:?usage: rollout.sh <repo-dir> <lib-short-name> [minor]}"
LIB="${2:?missing lib short name}"
MINOR="${3:-1}"

[ -f "$REPO/deps.edn" ] || { echo "no deps.edn in $REPO" >&2; exit 1; }

# Reject if runtime :deps (before :aliases) carry git/local coords.
runtime="$(sed '/:aliases/q' "$REPO/deps.edn")"
if grep -qE ":local/root|:git/(tag|url)" <<<"$runtime"; then
  echo "REFUSED: $REPO has git/local runtime deps — not Clojars-publishable as-is." >&2
  exit 2
fi

cp "$INFRA/build.clj" "$REPO/build.clj"

# CI home follows the repo's origin: GitHub-homed libs get .github/workflows,
# Gitea-only (premium) libs get .gitea/workflows — never both, and premium
# repos must never grow a .github dir.
remote="$(git -C "$REPO" remote get-url origin 2>/dev/null || true)"
if [[ "$remote" == *github.com* ]]; then
  wfdir="$REPO/.github/workflows"
  src="release.yml"          # Clojars deploy, CLOJARS_* secrets
else
  wfdir="$REPO/.gitea/workflows"
  src="release-gitea.yml"    # private registry, MAVEN_* secrets
fi
mkdir -p "$wfdir"
cp "$INFRA/workflows/$src" "$wfdir/release.yml"

srcdirs='["src"]'
[ -d "$REPO/resources" ] && srcdirs='["src" "resources"]'
cat > "$REPO/version.edn" <<EOF
{:lib      io.github.hive-agi/$LIB
 :minor    $MINOR
 :license  {:name "EPL-2.0" :url "https://www.eclipse.org/legal/epl-2.0/"}
 :scm-url  "https://github.com/hive-agi/$LIB"
 :src-dirs $srcdirs}
EOF

echo "Installed infra into $REPO (io.github.hive-agi/$LIB, minor=$MINOR, src-dirs=$srcdirs)."
echo "NEXT: add the :build alias to $REPO/deps.edn:"
echo '  :build {:deps {io.github.clojure/tools.build {:mvn/version "0.10.9"}'
echo '                 slipset/deps-deploy {:mvn/version "0.2.2"}} :ns-default build}'
echo "Then verify: (cd $REPO && clojure -T:build install)"
