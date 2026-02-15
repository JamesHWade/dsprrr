import { useState } from "react";
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

const LLM_QUERY_RE = /llm_query\s*\(/;

function truncateReasoning(text: string, maxLen: number): string {
  if (text.length <= maxLen) return text;
  return text.slice(0, maxLen) + "\u2026";
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

  if (LLM_QUERY_RE.test(code)) {
    return {
      title: "Recursive Sub-Query",
      body: "llm_query() delegates a sub-question to another LLM. The RLM can orchestrate multiple models — analyzing code snippets, comparing patterns, or synthesizing findings from different angles.",
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
  const [mobileExpanded, setMobileExpanded] = useState(false);
  const showBody = mobileExpanded || isNew;

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
        <button
          type="button"
          onClick={() => setMobileExpanded((v) => !v)}
          className="flex items-center gap-3 px-4 py-3 border-b bg-muted/30 w-full text-left lg:cursor-default"
        >
          <span className="flex items-center justify-center w-7 h-7 rounded-full bg-primary/10 text-primary text-sm font-semibold shrink-0">
            {iteration.iteration}
          </span>

          {phaseInfo && (
            <span
              className="inline-flex items-center rounded-full px-2.5 py-0.5 text-xs font-medium text-white shrink-0"
              style={{ backgroundColor: phaseInfo.color }}
            >
              {phaseInfo.label}
            </span>
          )}

          {LLM_QUERY_RE.test(iteration.code) && (
            <span className="inline-flex items-center rounded-full px-2.5 py-0.5 text-xs font-medium bg-violet-100 text-violet-800 dark:bg-violet-900/30 dark:text-violet-400 shrink-0">
              Sub-LM
            </span>
          )}

          {iteration.is_final && (
            <span className="inline-flex items-center rounded-full px-2.5 py-0.5 text-xs font-medium bg-green-100 text-green-800 dark:bg-green-900/30 dark:text-green-400 shrink-0">
              SUBMIT
            </span>
          )}

          {!iteration.success && !iteration.is_final && (
            <span className="inline-flex items-center rounded-full px-2.5 py-0.5 text-xs font-medium bg-red-100 text-red-800 dark:bg-red-900/30 dark:text-red-400 shrink-0">
              Error
            </span>
          )}

          {/* Mobile chevron */}
          <svg
            className={cn(
              "w-4 h-4 ml-auto text-muted-foreground transition-transform hidden max-lg:block",
              mobileExpanded && "rotate-180",
            )}
            fill="none"
            viewBox="0 0 24 24"
            strokeWidth={2}
            stroke="currentColor"
          >
            <path strokeLinecap="round" strokeLinejoin="round" d="m19.5 8.25-7.5 7.5-7.5-7.5" />
          </svg>
        </button>

        {/* Reasoning - full on desktop, truncated/full on mobile */}
        <div className="hidden lg:block px-4 py-3 text-sm text-muted-foreground border-b">
          <p className="italic">{iteration.reasoning}</p>
        </div>
        <div className={cn("lg:hidden px-4 py-3 text-sm text-muted-foreground border-b")}>
          <p className="italic">
            {showBody ? iteration.reasoning : truncateReasoning(iteration.reasoning, 120)}
          </p>
        </div>

        {/* Code */}
        <div className={cn("lg:block", showBody ? "block" : "hidden")}>
          <CodeBlock code={iteration.code} />
        </div>

        {/* Output */}
        <div className={cn("p-4 pt-0", "lg:block", showBody ? "block" : "hidden")}>
          <OutputBlock output={iteration.output} success={iteration.success} />
        </div>
      </div>

      {/* Educational callout */}
      {callout && (
        <div className={cn("mt-3", "lg:block", showBody ? "block" : "hidden")}>
          <ConceptCallout title={callout.title} body={callout.body} />
        </div>
      )}
    </div>
  );
}
