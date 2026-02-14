import { cn } from "@/lib/utils";
import type { Phase } from "@/lib/types";
import { PHASE_INFO } from "@/lib/types";
import {
  Tooltip,
  TooltipContent,
  TooltipProvider,
  TooltipTrigger,
} from "@/components/ui/tooltip";

const PHASES_ORDER: Phase[] = [
  "orient",
  "locate",
  "trace_sass",
  "cross_reference",
  "identify_gap",
];

interface PhaseTimelineProps {
  phases: Phase[];
  currentPhase: Phase | null;
  phaseTransitions: { iteration: number; phase: Phase }[];
  totalIterations: number;
  currentIndex: number;
}

export function PhaseTimeline({
  phases,
  currentPhase,
  totalIterations,
  currentIndex,
}: PhaseTimelineProps) {
  const seenPhases = new Set(phases);

  return (
    <div className="rounded-lg border bg-card p-4">
      <div className="flex items-center gap-2 mb-3">
        <span className="text-xs font-medium text-muted-foreground uppercase tracking-wider">
          Investigation Phases
        </span>
        {currentIndex >= 0 && (
          <span className="text-xs text-muted-foreground">
            Step {currentIndex + 1}/{totalIterations}
          </span>
        )}
      </div>

      <TooltipProvider>
        <div className="flex gap-1.5">
          {PHASES_ORDER.map((phase) => {
            const info = PHASE_INFO[phase];
            const isSeen = seenPhases.has(phase);
            const isCurrent = phase === currentPhase;

            return (
              <Tooltip key={phase}>
                <TooltipTrigger asChild>
                  <div className="flex-1 space-y-1.5 cursor-default">
                    <div
                      className={cn(
                        "h-2 rounded-full transition-all duration-500",
                        isCurrent && "ring-2 ring-offset-1 ring-offset-background",
                      )}
                      style={{
                        backgroundColor: isSeen ? info.color : undefined,
                        ringColor: isCurrent ? info.color : undefined,
                      }}
                    >
                      {!isSeen && (
                        <div className="h-full rounded-full bg-muted" />
                      )}
                    </div>
                    <div className="text-center">
                      <span
                        className={cn(
                          "text-[10px] font-medium transition-colors",
                          isCurrent
                            ? "text-foreground"
                            : isSeen
                              ? "text-muted-foreground"
                              : "text-muted-foreground/50",
                        )}
                      >
                        {info.label}
                      </span>
                    </div>
                  </div>
                </TooltipTrigger>
                <TooltipContent side="bottom" sideOffset={4}>
                  {info.description}
                </TooltipContent>
              </Tooltip>
            );
          })}
        </div>
      </TooltipProvider>
    </div>
  );
}
