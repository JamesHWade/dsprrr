import { cn } from "@/lib/utils";
import type { RunMeta } from "@/lib/types";

interface RunComparisonProps {
  runs: RunMeta[];
  currentRun: string;
}

export function RunComparison({ runs, currentRun }: RunComparisonProps) {
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
              <th className="px-4 py-2 text-left text-xs font-medium text-muted-foreground">
                Model
              </th>
              <th className="px-4 py-2 text-center text-xs font-medium text-muted-foreground">
                Status
              </th>
            </tr>
          </thead>
          <tbody>
            {runs.map((run) => (
              <tr
                key={run.id}
                className={cn(
                  "border-b last:border-b-0 transition-colors",
                  run.id === currentRun && "bg-primary/5",
                )}
              >
                <td className="px-4 py-2">
                  <span className="font-mono text-xs">{run.label}</span>
                </td>
                <td className="px-4 py-2 text-center">
                  <span className="font-mono">{run.iterations}</span>
                </td>
                <td className="px-4 py-2">
                  <span className="text-xs text-muted-foreground">{run.model}</span>
                </td>
                <td className="px-4 py-2 text-center">
                  {run.id === currentRun ? (
                    <span className="inline-flex items-center rounded-full px-2 py-0.5 text-xs font-medium bg-primary/10 text-primary">
                      Viewing
                    </span>
                  ) : (
                    <span className="inline-flex items-center rounded-full px-2 py-0.5 text-xs font-medium bg-muted text-muted-foreground">
                      Available
                    </span>
                  )}
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </div>
  );
}
