#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPOSITORY_ROOT="$(cd -- "$SCRIPT_DIR/../.." && pwd)"

RESULTS_DIR="${RESULTS_DIR:-$REPOSITORY_ROOT/results}"
DATA_DIR="${DATA_DIR:-$REPOSITORY_ROOT/data}"
CODE_ISOLATION_DIR="${CODE_ISOLATION_DIR:-$SCRIPT_DIR}"
CODE_PROFILING_DIR="${CODE_PROFILING_DIR:-$SCRIPT_DIR/../code_profiling}"
RUN_PROFILING="${RUN_PROFILING:-auto}"

if [ -z "${PYTHON_BIN:-}" ]; then
  if command -v python3 >/dev/null 2>&1; then
    PYTHON_BIN="python3"
  else
    PYTHON_BIN="python"
  fi
fi

mkdir -p "$RESULTS_DIR"
mkdir -p "$RESULTS_DIR/figures"
mkdir -p "$RESULTS_DIR/tables"
mkdir -p "$RESULTS_DIR/classification_results"

cd "$CODE_ISOLATION_DIR"
"$PYTHON_BIN" -m src.cli.main \
  --output "$RESULTS_DIR/tables/trigger_results.csv" \
  --plot \
  --figures-dir "$RESULTS_DIR/figures"

"$PYTHON_BIN" example/advanced_usage.py --output "$RESULTS_DIR"
"$PYTHON_BIN" example/basic_usage.py --output "$RESULTS_DIR"

if [ "$RUN_PROFILING" = "0" ]; then
  echo "Skipping profiling scripts: RUN_PROFILING=0"
elif [ ! -d "$CODE_PROFILING_DIR" ]; then
  echo "Skipping profiling scripts: $CODE_PROFILING_DIR not found"
elif ! command -v Rscript >/dev/null 2>&1; then
  echo "Skipping profiling scripts: Rscript not found"
else
  cd "$CODE_PROFILING_DIR"
  if [ -L data ]; then
    ln -sfn "$DATA_DIR" data
  elif [ ! -e data ]; then
    ln -s "$DATA_DIR" data
  fi

  for script in \
    "ComBat1.R" \
    "Heatmap.R" \
    "LDA_visualization.R" \
    "PCA.R" \
    "PLS-DA.R" \
    "RF+NNET+LDA+Confusion_matrix+ROC+PR.R" \
    "Significance_heatmap.R" \
    "t-SNE_cell.R"; do
    if [ -f "$script" ]; then
      Rscript "$script"
    else
      echo "Skipping missing R script: $script"
    fi
  done
fi
