#!/bin/bash

YAML_FILE="spago.yaml"  # or spago.yaml if that's your file
BRANCH="main"
OWNER="m-bock"
REPO="purescript-patchdown"

# Get latest commit SHA
LATEST_COMMIT=$(curl -s "https://api.github.com/repos/$OWNER/$REPO/commits/$BRANCH" | jq -r '.sha')

if [[ -z "$LATEST_COMMIT" || "$LATEST_COMMIT" == "null" ]]; then
  echo "Failed to fetch commit from GitHub."
  exit 1
fi

echo "Latest commit: $LATEST_COMMIT"

# Force yq to output JSON, then use jq, then convert back to YAML
yq -j . "$YAML_FILE" | \
  jq ".workspace.extraPackages.patchdown.ref = \"$LATEST_COMMIT\"" | \
  yq -y . > "${YAML_FILE}.tmp" && mv "${YAML_FILE}.tmp" "$YAML_FILE"

echo "Updated $YAML_FILE with new ref: $LATEST_COMMIT"
