import type { Iteration, TokenUsage } from "@/lib/types";

interface TokenTallyProps {
  iterations: Iteration[];
  currentIndex: number;
  totalTokens?: TokenUsage;
}

function formatTokens(n: number): string {
  if (n >= 1_000_000) return `${(n / 1_000_000).toFixed(1)}M`;
  if (n >= 1_000) return `${(n / 1_000).toFixed(1)}k`;
  return String(n);
}

export function TokenTally({ iterations, currentIndex, totalTokens }: TokenTallyProps) {
  // Accumulate tokens up to current iteration
  let inputSoFar = 0;
  let outputSoFar = 0;
  for (let i = 0; i <= Math.min(currentIndex, iterations.length - 1); i++) {
    const t = iterations[i].tokens;
    if (t) {
      inputSoFar += t.input;
      outputSoFar += t.output;
    }
  }

  // Total tokens (from run-level data, or sum of all iterations)
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

  if (!hasTokenData) return null;

  const progress = grandTotal > 0 ? (totalSoFar / grandTotal) * 100 : 0;

  return (
    <div className="rounded-xl border bg-card p-4 space-y-3">
      <h3 className="text-xs font-medium text-muted-foreground uppercase tracking-wider">
        Token Usage
      </h3>

      {/* Running total */}
      <div className="text-center">
        <div className="text-2xl font-semibold tabular-nums">
          {formatTokens(totalSoFar)}
        </div>
        <div className="text-xs text-muted-foreground">
          of {formatTokens(grandTotal)} total
        </div>
      </div>

      {/* Progress bar */}
      <div className="h-1.5 rounded-full bg-muted overflow-hidden">
        <div
          className="h-full rounded-full bg-primary transition-all duration-300"
          style={{ width: `${Math.min(progress, 100)}%` }}
        />
      </div>

      {/* Input / Output breakdown */}
      <div className="grid grid-cols-2 gap-2 text-center">
        <div className="p-2 rounded-lg bg-muted/50">
          <div className="text-sm font-semibold tabular-nums">{formatTokens(inputSoFar)}</div>
          <div className="text-[10px] text-muted-foreground">input</div>
        </div>
        <div className="p-2 rounded-lg bg-muted/50">
          <div className="text-sm font-semibold tabular-nums">{formatTokens(outputSoFar)}</div>
          <div className="text-[10px] text-muted-foreground">output</div>
        </div>
      </div>
    </div>
  );
}
