#!/usr/bin/env bash
set -euo pipefail

DEV="$1"

IMAGE_TAGS=(
  bucket-image
)

echo "Removing known images (any registry/tag)..."
for tag in "${IMAGE_TAGS[@]}"; do
  # Find any repo:tag whose repo ends with /$tag or equals $tag
  matches=$(podman images --format '{{.Repository}}:{{.Tag}}' \
    | grep -E "(^|.*/)${tag}(:|$)")

  if [ -n "${matches}" ]; then
    echo "$matches" | while read -r ref; do
      # Skip <none> tags that render as '<none>:<none>'
      if [[ "$ref" =~ ^\<none\> ]]; then continue; fi
      echo " - removing $ref"
      podman rmi -f "$ref" >/dev/null 2>&1 || true
    done
  else
    echo " - $tag not found"
  fi
done

echo "Removing dangling images..."
dangling_ids=$(podman images -q -f dangling=true || true)
if [ -n "${dangling_ids}" ]; then
  podman rmi -f ${dangling_ids}
else
  echo " - none"
fi

echo "Done."
