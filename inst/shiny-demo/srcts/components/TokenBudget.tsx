import type { Iteration } from "@/lib/types";
import { formatChars } from "@/lib/utils";

interface TokenBudgetProps {
  totalContextChars: number;
  iterations: Iteration[];
  currentIndex: number;
}

export function TokenBudget({
  totalContextChars,
  iterations,
  currentIndex,
}: TokenBudgetProps) {
  // Estimate characters actually transferred to token space
  const transferredChars = iterations
    .slice(0, currentIndex + 1)
    .reduce((sum, iter) => {
      return sum + iter.output.length + iter.code.length;
    }, 0);

  const ratio = totalContextChars > 0 ? transferredChars / totalContextChars : 0;
  const percentage = (ratio * 100).toFixed(1);

  return (
    <div className="rounded-xl border bg-card p-4 space-y-3">
      <h3 className="text-xs font-medium text-muted-foreground uppercase tracking-wider">
        Token Economy
      </h3>

      <div className="space-y-2">
        <div className="flex justify-between text-sm">
          <span className="text-muted-foreground">Total context</span>
          <span className="font-mono font-medium">{formatChars(totalContextChars)}</span>
        </div>
        <div className="flex justify-between text-sm">
          <span className="text-muted-foreground">Actually transferred</span>
          <span className="font-mono font-medium text-primary">{formatChars(transferredChars)}</span>
        </div>
      </div>

      {/* Visual comparison bar */}
      <div className="space-y-1">
        <div className="h-3 rounded-full bg-muted overflow-hidden">
          <div
            className="h-full rounded-full bg-primary transition-all duration-500"
            style={{ width: `${Math.min(100, Number(percentage))}%` }}
          />
        </div>
        <div className="text-xs text-muted-foreground text-center">
          {percentage}% of context entered token space
        </div>
      </div>
    </div>
  );
}
