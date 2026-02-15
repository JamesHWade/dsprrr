import { cn, formatTokens } from "@/lib/utils";
import type { RunMeta } from "@/lib/types";

interface RunComparisonProps {
  runs: RunMeta[];
  currentRun: string;
  onRunChange?: (runId: string) => void;
}

export function RunComparison({ runs, currentRun, onRunChange }: RunComparisonProps) {
  const anyHasTokens = runs.some(
    (r) => r.total_tokens && (r.total_tokens.input > 0 || r.total_tokens.output > 0),
  );

  return (
    <div className="rounded-xl border bg-card overflow-hidden">
      <div className="px-4 py-3 border-b bg-muted/30">
        <h3 className="text-sm font-semibold">Five-Phase Convergence</h3>
        <p className="text-xs text-muted-foreground mt-0.5">
          Different runs converge on the same answer through varied exploration paths
        </p>
      </div>

      <div className="overflow-x-auto">
        <table className="w-full text-sm">
          <thead>
            <tr className="border-b bg-muted/20">
              <th className="px-4 py-2 text-left text-xs font-medium text-muted-foreground">
                Run
              </th>
              <th className="px-4 py-2 text-center text-xs font-medium text-muted-foreground">
                Iterations
              </th>
              {anyHasTokens && (
                <th className="px-4 py-2 text-center text-xs font-medium text-muted-foreground">
                  Tokens
                </th>
              )}
              <th className="px-4 py-2 text-left text-xs font-medium text-muted-foreground">
                Model
              </th>
              <th className="px-4 py-2 text-center text-xs font-medium text-muted-foreground">
                Status
              </th>
            </tr>
          </thead>
          <tbody>
            {runs.map((run) => {
              const total =
                run.total_tokens
                  ? run.total_tokens.input + run.total_tokens.output
                  : 0;
              const isCurrent = run.id === currentRun;
              const isClickable = !isCurrent && !!onRunChange;
              return (
                <tr
                  key={run.id}
                  onClick={isClickable ? () => onRunChange(run.id) : undefined}
                  className={cn(
                    "border-b last:border-b-0 transition-colors",
                    isCurrent && "bg-primary/5",
                    isClickable && "cursor-pointer hover:bg-muted/50",
                  )}
                >
                  <td className="px-4 py-2">
                    <span className="font-mono text-xs">{run.label}</span>
                  </td>
                  <td className="px-4 py-2 text-center">
                    <span className="font-mono">{run.iterations}</span>
                  </td>
                  {anyHasTokens && (
                    <td className="px-4 py-2 text-center">
                      <span className="font-mono text-xs tabular-nums">
                        {total > 0 ? formatTokens(total) : "\u2014"}
                      </span>
                    </td>
                  )}
                  <td className="px-4 py-2">
                    <span className="text-xs text-muted-foreground">{run.model}</span>
                  </td>
                  <td className="px-4 py-2 text-center">
                    {isCurrent ? (
                      <span className="inline-flex items-center rounded-full px-2 py-0.5 text-xs font-medium bg-primary/10 text-primary">
                        Viewing
                      </span>
                    ) : (
                      <span className={cn(
                        "inline-flex items-center rounded-full px-2 py-0.5 text-xs font-medium",
                        isClickable
                          ? "bg-primary/5 text-primary"
                          : "bg-muted text-muted-foreground",
                      )}>
                        {isClickable ? "Switch" : "Available"}
                      </span>
                    )}
                  </td>
                </tr>
              );
            })}
          </tbody>
        </table>
      </div>
    </div>
  );
}
