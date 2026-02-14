import type { ContextVariable, Iteration } from "@/lib/types";
import { formatChars } from "@/lib/utils";
import { TokenBudget } from "./TokenBudget";

interface ContextPanelProps {
  contextVariables: ContextVariable[];
  iterations: Iteration[];
  currentIndex: number;
}

function extractAccessedVars(iterations: Iteration[], upTo: number): Set<string> {
  const accessed = new Set<string>();
  for (let i = 0; i <= Math.min(upTo, iterations.length - 1); i++) {
    const code = iterations[i].code;
    const matches = code.matchAll(/\.context\$(\w+)/g);
    for (const m of matches) {
      accessed.add(m[1]);
    }
  }
  return accessed;
}

function countToolCalls(iterations: Iteration[], upTo: number): Record<string, number> {
  const counts: Record<string, number> = { peek: 0, search: 0, llm_query: 0, SUBMIT: 0 };
  for (let i = 0; i <= Math.min(upTo, iterations.length - 1); i++) {
    const code = iterations[i].code;
    for (const tool of Object.keys(counts)) {
      const pattern = new RegExp(`\\b${tool}\\s*\\(`, "g");
      const matches = code.match(pattern);
      if (matches) counts[tool] += matches.length;
    }
  }
  return counts;
}

export function ContextPanel({
  contextVariables,
  iterations,
  currentIndex,
}: ContextPanelProps) {
  const accessedVars = extractAccessedVars(iterations, currentIndex);
  const toolCounts = countToolCalls(iterations, currentIndex);
  const totalChars = contextVariables.reduce((sum, v) => sum + v.size_chars, 0);

  return (
    <div className="sticky top-20 space-y-4">
      {/* Context variables */}
      <div className="rounded-xl border bg-card p-4 space-y-3">
        <h3 className="text-xs font-medium text-muted-foreground uppercase tracking-wider">
          Context Variables
        </h3>

        <div className="space-y-2">
          {contextVariables.map((cv) => {
            const isAccessed = accessedVars.has(cv.name);
            return (
              <div
                key={cv.name}
                className={`rounded-lg border p-3 transition-all ${isAccessed ? "border-primary/30 bg-primary/5" : "opacity-60"}`}
              >
                <div className="flex items-center justify-between mb-1">
                  <code className="text-xs font-mono font-medium">
                    .context${cv.name}
                  </code>
                  {isAccessed && (
                    <span className="inline-block w-2 h-2 rounded-full bg-primary" />
                  )}
                </div>
                <div className="flex gap-3 text-xs text-muted-foreground">
                  <span>{formatChars(cv.size_chars)}</span>
                  <span>{cv.n_files} files</span>
                </div>
              </div>
            );
          })}
        </div>
      </div>

      {/* Token budget comparison */}
      <TokenBudget
        totalContextChars={totalChars}
        iterations={iterations}
        currentIndex={currentIndex}
      />

      {/* Tool usage */}
      <div className="rounded-xl border bg-card p-4 space-y-3">
        <h3 className="text-xs font-medium text-muted-foreground uppercase tracking-wider">
          Tool Calls
        </h3>
        <div className="grid grid-cols-2 gap-2">
          {Object.entries(toolCounts).map(([tool, count]) => (
            <div key={tool} className="text-center p-2 rounded-lg bg-muted/50">
              <div className="text-lg font-semibold">{count}</div>
              <div className="text-xs text-muted-foreground font-mono">{tool}()</div>
            </div>
          ))}
        </div>
      </div>
    </div>
  );
}
