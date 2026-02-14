import { useMemo } from "react";
import type { Iteration, Phase } from "@/lib/types";
import { annotatePhases } from "@/lib/phases";

interface PhaseProgress {
  phases: Phase[];
  currentPhase: Phase | null;
  phaseCounts: Record<Phase, number>;
  phaseTransitions: { iteration: number; phase: Phase }[];
}

export function usePhaseDetection(
  iterations: Iteration[],
  currentIndex: number,
): PhaseProgress {
  const annotated = useMemo(() => annotatePhases(iterations), [iterations]);

  return useMemo(() => {
    const visible = annotated.slice(0, currentIndex + 1);
    const phases = visible.map((it) => it.phase!);
    const currentPhase = phases.length > 0 ? phases[phases.length - 1] : null;

    const phaseCounts: Record<Phase, number> = {
      orient: 0,
      locate: 0,
      trace_sass: 0,
      cross_reference: 0,
      identify_gap: 0,
    };

    for (const p of phases) {
      phaseCounts[p]++;
    }

    const phaseTransitions: { iteration: number; phase: Phase }[] = [];
    let lastPhase: Phase | null = null;
    for (const iter of visible) {
      if (iter.phase !== lastPhase) {
        phaseTransitions.push({
          iteration: iter.iteration,
          phase: iter.phase!,
        });
        lastPhase = iter.phase!;
      }
    }

    return { phases, currentPhase, phaseCounts, phaseTransitions };
  }, [annotated, currentIndex]);
}
