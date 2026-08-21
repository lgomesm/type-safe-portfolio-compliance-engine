#!/usr/bin/env bash

# Executando tds os fixtures json pelo executável real. Estou mantendo como um teste separado para complementar o Hspec 
# Portanto, minha ideia é que o hspec cverifique os valores tipados, enquanto esse script verifica a "fronteira" do 
# processo, incluindo os campos exibidos e os códigos de saída
set -u

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR" || exit 1

failures=0

contains() {
  local output="$1"
  local expected="$2"
  [[ "$output" == *"$expected"* ]]
}

# A CLI agora possui uma projecao visual. A bateria continua verificando o
# mesmo significado, mas nao fica acoplada ao layout textual anterior.
matches_status() {
  local output="$1"
  local expected="$2"
  case "$expected" in
    Approved) contains "$output" "[OK] APPROVED" ;;
    Rejected) contains "$output" "[X] REJECTED" ;;
    *) return 1 ;;
  esac
}

matches_risk() {
  local output="$1"
  local expected="$2"
  case "$expected" in
    Low) contains "$output" "[OK] LOW" ;;
    Medium) contains "$output" "[!] MEDIUM" ;;
    High) contains "$output" "[X] HIGH" ;;
    *) return 1 ;;
  esac
}

matches_approval() {
  local output="$1"
  local expected="$2"
  case "$expected" in
    AutoApproved) contains "$output" "[OK] AUTO APPROVED" ;;
    RequiresManualReview\ *) contains "$output" "[!] MANUAL REVIEW" ;;
    RejectedByPolicy) contains "$output" "[X] REJECTED BY POLICY" ;;
    *) return 1 ;;
  esac
}

check_case() {
  local fixture="$1"
  local expected_exit="$2"
  local expected_status="$3"
  local expected_risk="$4"
  local expected_approval="$5"
  local expected_error="${6:-}"

  local output actual_exit
  output="$(stack exec portfolio-compliance-engine -- --input "$fixture" 2>&1)"
  actual_exit=$?

  if [[ "$actual_exit" -ne "$expected_exit" ]]; then
    printf 'FAIL %s: expected exit %s, got %s\n' "$fixture" "$expected_exit" "$actual_exit"
    printf '%s\n' "$output"
    failures=$((failures + 1))
    return
  fi

  if [[ -n "$expected_status" ]] && ! matches_status "$output" "$expected_status"; then
    printf 'FAIL %s: missing status %s\n' "$fixture" "$expected_status"
    failures=$((failures + 1))
    return
  fi

  if [[ -n "$expected_risk" ]] && ! matches_risk "$output" "$expected_risk"; then
    printf 'FAIL %s: missing risk level %s\n' "$fixture" "$expected_risk"
    failures=$((failures + 1))
    return
  fi

  if [[ -n "$expected_approval" ]] && ! matches_approval "$output" "$expected_approval"; then
    printf 'FAIL %s: missing approval decision %s\n' "$fixture" "$expected_approval"
    failures=$((failures + 1))
    return
  fi

  if [[ -n "$expected_error" ]] && ! contains "$output" "$expected_error"; then
    printf 'FAIL %s: missing error %s\n' "$fixture" "$expected_error"
    failures=$((failures + 1))
    return
  fi

  printf 'PASS %s (exit %s)\n' "$fixture" "$actual_exit"
}

for fixture in test/fixtures/battery/*.json; do
  filename="$(basename "$fixture")"
  case "$filename" in
    01-*|02-*|03-*|04-*|05-*|06-*|07-*|08-*|09-*|10-*)
      check_case "$fixture" 0 Approved Low AutoApproved
      ;;
    11-*)
      check_case "$fixture" 1 Rejected High 'RequiresManualReview (Manager)'
      ;;
    12-*)
      check_case "$fixture" 1 Rejected High 'RequiresManualReview (Analyst)'
      ;;
    13-*)
      check_case "$fixture" 1 Rejected Medium 'RequiresManualReview (Analyst)'
      ;;
    14-*)
      check_case "$fixture" 1 Rejected Medium 'RequiresManualReview (Manager)'
      ;;
    15-*)
      check_case "$fixture" 1 Rejected High 'RequiresManualReview (Manager)'
      ;;
    16-*|17-*|18-*|19-*)
      check_case "$fixture" 1 Rejected Medium 'RequiresManualReview (Manager)'
      ;;
    20-*|21-*)
      check_case "$fixture" 1 Rejected Medium 'RequiresManualReview (CreditCommittee)'
      ;;
    22-*|25-*|28-*)
      check_case "$fixture" 1 Rejected Medium 'RequiresManualReview (Manager)'
      ;;
    23-*|24-*)
      check_case "$fixture" 1 Rejected High RejectedByPolicy
      ;;
    26-*)
      check_case "$fixture" 1 Rejected High 'RequiresManualReview (Analyst)'
      ;;
    27-*)
      check_case "$fixture" 1 Rejected Medium 'RequiresManualReview (Analyst)'
      ;;
    29-*)
      check_case "$fixture" 2 '' '' '' PercentageOutOfRange
      ;;
    30-*)
      check_case "$fixture" 2 '' '' '' PortfolioWeightsDoNotSumToOne
      ;;
    *)
      printf 'FAIL unexpected fixture: %s\n' "$fixture"
      failures=$((failures + 1))
      ;;
  esac
done

if [[ "$failures" -ne 0 ]]; then
  printf '\nScenario battery failed: %s case(s)\n' "$failures"
  exit 1
fi

printf '\nScenario battery passed: all fixtures matched their expected CLI contract\n'
