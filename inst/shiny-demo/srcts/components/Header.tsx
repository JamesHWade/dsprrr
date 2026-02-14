import type { AppMode, RunMeta } from "@/lib/types";
import { cn } from "@/lib/utils";

interface HeaderProps {
  mode: AppMode;
  onModeChange: (mode: AppMode) => void;
  selectedRun: string;
  onRunChange: (runId: string) => void;
  availableRuns: RunMeta[];
}

export function Header({
  mode,
  onModeChange,
  selectedRun,
  onRunChange,
  availableRuns,
}: HeaderProps) {
  return (
    <header className="sticky top-0 z-40 border-b bg-background/95 backdrop-blur supports-[backdrop-filter]:bg-background/60">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="flex items-center justify-between h-14">
          {/* Title */}
          <div className="flex items-center gap-3">
            <h1 className="text-lg font-semibold tracking-tight">
              How RLMs Work
            </h1>
            <span className="hidden sm:inline text-xs text-muted-foreground font-mono bg-muted px-2 py-0.5 rounded">
              dsprrr
            </span>
          </div>

          <div className="flex items-center gap-4">
            {/* Run selector (replay mode only) */}
            {mode === "replay" && availableRuns.length > 0 && (
              <select
                value={selectedRun}
                onChange={(e) => onRunChange(e.target.value)}
                className="h-8 rounded-md border border-input bg-background px-3 text-sm"
              >
                {availableRuns.map((run) => (
                  <option key={run.id} value={run.id}>
                    {run.label} ({run.iterations} iter)
                  </option>
                ))}
              </select>
            )}

            {/* Mode toggle */}
            <div className="flex items-center bg-muted rounded-lg p-0.5">
              <button
                onClick={() => onModeChange("replay")}
                className={cn(
                  "px-3 py-1 text-sm rounded-md transition-colors",
                  mode === "replay"
                    ? "bg-background text-foreground shadow-sm"
                    : "text-muted-foreground hover:text-foreground",
                )}
              >
                Replay
              </button>
              <button
                onClick={() => onModeChange("live")}
                className={cn(
                  "px-3 py-1 text-sm rounded-md transition-colors",
                  mode === "live"
                    ? "bg-background text-foreground shadow-sm"
                    : "text-muted-foreground hover:text-foreground",
                )}
              >
                Live
              </button>
            </div>
          </div>
        </div>
      </div>
    </header>
  );
}
