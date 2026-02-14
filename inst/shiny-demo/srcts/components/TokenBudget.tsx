import type { Iteration, TokenUsage } from "@/lib/types";
import { formatChars } from "@/lib/utils";

interface TokenBudgetProps {
  totalContextChars: number;
  iterations: Iteration[];
  currentIndex: number;
  totalTokens?: TokenUsage;
}

function formatTokens(n: number): string {
  if (n >= 1_000_000) return `${(n / 1_000_000).toFixed(1)}M`;
  if (n >= 1_000) return `${(n / 1_000).toFixed(1)}k`;
  return String(n);
}

export function TokenBudget({
  totalContextChars,
  iterations,
  currentIndex,
  totalTokens,
}: TokenBudgetProps) {
  // ---- Context window economy ----
  // Chars actually pulled into token space by peek/search
  const transferredChars = iterations
    .slice(0, currentIndex + 1)
    .reduce((sum, iter) => sum + iter.output.length + iter.code.length, 0);

  const contextRatio = totalContextChars > 0 ? transferredChars / totalContextChars : 0;
  const contextPct = (contextRatio * 100).toFixed(1);

  // Hypothetical: without an RLM, you'd need to dump search results and file
  // contents into the conversation. Estimate: each iteration's output averages
  // the data that WOULD have gone into the prompt as context. The RLM keeps it
  // out of the context window by running code in a separate R process.
  // The "full context" number represents what you'd need in the prompt to give
  // the LLM equivalent access — but no model's context window is that large.

  // ---- Actual token usage ----
  let inputSoFar = 0;
  let outputSoFar = 0;
  for (let i = 0; i <= Math.min(currentIndex, iterations.length - 1); i++) {
    const t = iterations[i].tokens;
    if (t) {
      inputSoFar += t.input;
      outputSoFar += t.output;
    }
  }

  let inputTotal = totalTokens?.input ?? 0;
  let outputTotal = totalTokens?.output ?? 0;
  if (!inputTotal && !outputTotal) {
    for (const iter of iterations) {
      if (iter.tokens) {
        inputTotal += iter.tokens.input;
        outputTotal += iter.tokens.output;
      }
    }
  }

  const totalSoFar = inputSoFar + outputSoFar;
  const grandTotal = inputTotal + outputTotal;
  const hasTokenData = grandTotal > 0;

  return (
    <div className="rounded-xl border bg-card overflow-hidden">
      <div className="px-4 py-3 border-b bg-muted/30">
        <h3 className="text-sm font-semibold">Token Economy</h3>
        <p className="text-xs text-muted-foreground mt-0.5">
          Code runs in a separate R process — only small slices enter the context window
        </p>
      </div>

      <div className="p-4 space-y-5">
        {/* Source Data Efficiency */}
        <div className="space-y-3">
          <div className="text-xs font-medium text-muted-foreground uppercase tracking-wider">
            Source Data Efficiency
          </div>

          {/* Two-row comparison */}
          <div className="space-y-2">
            <div className="space-y-1">
              <div className="flex items-baseline justify-between">
                <span className="text-xs text-muted-foreground">Available in R environment</span>
                <span className="text-xs font-mono font-medium tabular-nums">
                  {formatChars(totalContextChars)}
                </span>
              </div>
              <div className="h-5 rounded bg-foreground/15" />
            </div>
            <div className="space-y-1">
              <div className="flex items-baseline justify-between">
                <span className="text-xs text-muted-foreground">Transferred via peek/search</span>
                <span className="text-xs font-mono font-medium tabular-nums">
                  {formatChars(transferredChars)}
                </span>
              </div>
              <div className="flex items-center gap-2">
                <div
                  className="h-5 rounded bg-primary transition-all duration-300"
                  style={{ width: `${Math.max(3, Number(contextPct))}%` }}
                />
              </div>
            </div>
          </div>

          <div className="text-center">
            <span className="text-2xl font-bold tabular-nums text-primary">{contextPct}%</span>
            <span className="text-xs text-muted-foreground ml-1.5">of source data needed by the LLM</span>
          </div>
        </div>

        {/* Token consumption */}
        {hasTokenData && (
          <div className="space-y-3 pt-2 border-t">
            <div className="text-xs font-medium text-muted-foreground uppercase tracking-wider">
              Token Consumption
            </div>

            {/* Running total with progress */}
            <div className="flex items-baseline justify-between">
              <span className="text-2xl font-bold tabular-nums">{formatTokens(totalSoFar)}</span>
              <span className="text-xs text-muted-foreground">
                of {formatTokens(grandTotal)} total
              </span>
            </div>

            <div className="h-1.5 rounded-full bg-muted overflow-hidden">
              <div
                className="h-full rounded-full bg-primary transition-all duration-300"
                style={{ width: `${grandTotal > 0 ? Math.min((totalSoFar / grandTotal) * 100, 100) : 0}%` }}
              />
            </div>

            {/* Input / Output breakdown */}
            <div className="grid grid-cols-2 gap-2">
              <div className="text-center p-2 rounded-lg bg-muted/50">
                <div className="text-sm font-semibold tabular-nums">{formatTokens(inputSoFar)}</div>
                <div className="text-xs text-muted-foreground">input</div>
              </div>
              <div className="text-center p-2 rounded-lg bg-muted/50">
                <div className="text-sm font-semibold tabular-nums">{formatTokens(outputSoFar)}</div>
                <div className="text-xs text-muted-foreground">output</div>
              </div>
            </div>
          </div>
        )}

        {/* Context window insight */}
        {totalContextChars > 0 && (
          <div className="rounded-lg bg-blue-50 dark:bg-blue-950/30 border border-blue-200 dark:border-blue-900/50 p-3 space-y-1">
            <div className="text-xs font-medium text-blue-700 dark:text-blue-400">
              Why this matters
            </div>
            <p className="text-xs text-blue-600 dark:text-blue-500 leading-relaxed">
              {formatChars(totalContextChars)} of source code sits in the R environment as variables.
              The LLM writes code to peek() and search() what it needs — only
              those small fragments enter the prompt, leaving room for reasoning.
            </p>
          </div>
        )}
      </div>
    </div>
  );
}
