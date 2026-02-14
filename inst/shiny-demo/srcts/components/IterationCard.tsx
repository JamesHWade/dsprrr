import { cn } from "@/lib/utils";
import type { Iteration, Phase } from "@/lib/types";
import { PHASE_INFO } from "@/lib/types";
import { CodeBlock } from "./CodeBlock";
import { OutputBlock } from "./OutputBlock";
import { ConceptCallout } from "./ConceptCallout";

interface IterationCardProps {
  iteration: Iteration;
  isNew: boolean;
}

function getConceptCallout(
  iter: Iteration,
): { title: string; body: string } | null {
  const code = iter.code.toLowerCase();

  if (iter.iteration === 1) {
    return {
      title: "Context as Environment",
      body: "Context sits as R variables. The model only sees names and sizes - not the actual content. It must write code to explore.",
    };
  }

  if (iter.iteration <= 3 && /peek\(/.test(code) && !iter.is_final) {
    return {
      title: "Targeted Transfer",
      body: "peek() transfers a slice from programmatic space to token space. The model reads only what it needs, not the entire file.",
    };
  }

  if (/search\(/.test(code) && iter.iteration <= 4) {
    return {
      title: "Pattern Search",
      body: "search() returns matches, not entire files. The model finds needles without loading haystacks into context.",
    };
  }

  if (!iter.success && !iter.is_final) {
    return {
      title: "Self-Correction",
      body: "Failures are normal - the model sees the error and self-corrects. 2-4 errors per run is typical.",
    };
  }

  if (iter.is_final) {
    return {
      title: "Convergence",
      body: "SUBMIT() signals the model has enough evidence. It decides when to stop, not a fixed iteration count.",
    };
  }

  return null;
}

export function IterationCard({ iteration, isNew }: IterationCardProps) {
  const phase = iteration.phase as Phase | undefined;
  const phaseInfo = phase ? PHASE_INFO[phase] : null;
  const callout = getConceptCallout(iteration);

  return (
    <div
      className={cn(
        "relative group",
        isNew && "iteration-enter",
        iteration.is_final && "submit-pulse",
      )}
    >
      <div
        className={cn(
          "rounded-xl border bg-card overflow-hidden transition-all",
          !iteration.success && "border-l-4 border-l-destructive",
          iteration.is_final && "border-l-4 border-l-green-500 ring-1 ring-green-500/20",
        )}
      >
        {/* Header */}
        <div className="flex items-center gap-3 px-4 py-3 border-b bg-muted/30">
          <span className="flex items-center justify-center w-7 h-7 rounded-full bg-primary/10 text-primary text-sm font-semibold">
            {iteration.iteration}
          </span>

          {phaseInfo && (
            <span
              className="inline-flex items-center rounded-full px-2.5 py-0.5 text-xs font-medium text-white"
              style={{ backgroundColor: phaseInfo.color }}
            >
              {phaseInfo.label}
            </span>
          )}

          {iteration.is_final && (
            <span className="inline-flex items-center rounded-full px-2.5 py-0.5 text-xs font-medium bg-green-100 text-green-800 dark:bg-green-900/30 dark:text-green-400">
              SUBMIT
            </span>
          )}

          {!iteration.success && !iteration.is_final && (
            <span className="inline-flex items-center rounded-full px-2.5 py-0.5 text-xs font-medium bg-red-100 text-red-800 dark:bg-red-900/30 dark:text-red-400">
              Error
            </span>
          )}
        </div>

        {/* Reasoning */}
        <div className="px-4 py-3 text-sm text-muted-foreground border-b">
          <p className="italic">{iteration.reasoning}</p>
        </div>

        {/* Code */}
        <CodeBlock code={iteration.code} />

        {/* Output */}
        <div className="p-4 pt-0">
          <OutputBlock output={iteration.output} success={iteration.success} />
        </div>
      </div>

      {/* Educational callout */}
      {callout && (
        <div className="mt-3">
          <ConceptCallout title={callout.title} body={callout.body} />
        </div>
      )}
    </div>
  );
}
