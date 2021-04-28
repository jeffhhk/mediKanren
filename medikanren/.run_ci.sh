#!/bin/bash
adirRepo="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd .. && pwd )"
adirMk="$adirRepo/medikanren"
adirArtifacts="$adirRepo/ci_artifacts"

if raco test "$adirMk/configref.rkt" 2>&1 | tee "$adirRepo/ci_artifacts/raco_test.log"
then
    echo medikanren_run_ci > "$adirArtifacts/status/pass/medikanren1_run_ci"
else
    echo medikanren_run_ci > "$adirArtifacts/status/fail/medikanren1_run_ci"
fi




