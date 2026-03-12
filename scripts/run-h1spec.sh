#!/usr/bin/env bash

set -euo pipefail

port="${1:-8080}"
host="${H1SPEC_HOST:-127.0.0.1}"

find_repo_root() {
  local dir="${PWD}"
  while [[ "${dir}" != "/" ]]; do
    if [[ -f "${dir}/dune-project" ]]; then
      printf '%s\n' "${dir}"
      return 0
    fi
    dir="$(dirname "${dir}")"
  done
  return 1
}

repo_root="$(find_repo_root || true)"
if [[ -z "${repo_root}" ]]; then
  printf 'Could not find repo root from %s\n' "${PWD}" >&2
  exit 1
fi

cd "${repo_root}"

server_pid=''
cleanup() {
  if [[ -n "${server_pid}" ]] && kill -0 "${server_pid}" 2>/dev/null; then
    kill "${server_pid}"
    wait "${server_pid}" 2>/dev/null || true
  fi
}
trap cleanup EXIT INT TERM

dune build examples/lwt/lwt_h1spec_server.exe
dune exec ./examples/lwt/lwt_h1spec_server.exe -- -p "${port}" &
server_pid=$!

for _ in $(seq 1 50); do
  if lsof -n -iTCP:"${port}" -sTCP:LISTEN >/dev/null 2>&1; then
    break
  fi
  sleep 0.1
done

if ! lsof -n -iTCP:"${port}" -sTCP:LISTEN >/dev/null 2>&1; then
  printf 'Timed out waiting for server on port %s\n' "${port}" >&2
  exit 1
fi

h1spec "${host}" "${port}"
