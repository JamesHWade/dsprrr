import type { Iteration, Phase } from "./types";

function findSourceVars(code: string): Set<string> {
  const vars = new Set<string>();
  const matches = code.matchAll(/\.context\$(\w+_source)/g);
  for (const m of matches) {
    vars.add(m[1]);
  }
  return vars;
}

export function detectPhase(
  iter: Iteration,
  allIterations: Iteration[],
): Phase {
  const code = iter.code.toLowerCase();
  const idx = iter.iteration;

  // Early iterations that scan structure = orient
  if (idx <= 2 && /search\(/.test(code) && /source|file|struct/.test(code)) {
    return "orient";
  }

  // SUBMIT or final comparison patterns = identify_gap
  if (iter.is_final || /submit\s*\(/.test(code)) {
    return "identify_gap";
  }

  // Cross-referencing: accessing a different _source var than previous iterations
  const currentVars = findSourceVars(iter.code);

  if (currentVars.size > 0 && idx > 3) {
    const previousVars = new Set<string>();
    for (const prev of allIterations.slice(0, idx - 1)) {
      for (const v of findSourceVars(prev.code)) {
        previousVars.add(v);
      }
    }

    const newVars = [...currentVars].filter((v) => !previousVars.has(v));
    if (newVars.length > 0) {
      return "cross_reference";
    }
  }

  // Sass/SCSS tracing patterns
  if (/sass|scss|\$|@mixin|@include|@use|@forward/.test(code)) {
    return "trace_sass";
  }

  // General locate: peek/search/gregexpr
  if (/peek\(|search\(|gregexpr|grep/.test(code)) {
    return "locate";
  }

  // Default to orient for early, locate for later
  return idx <= 3 ? "orient" : "locate";
}

export function annotatePhases(iterations: Iteration[]): Iteration[] {
  return iterations.map((iter) => ({
    ...iter,
    phase: iter.phase ?? detectPhase(iter, iterations),
  }));
}
