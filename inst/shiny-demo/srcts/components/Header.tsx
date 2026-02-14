import type { AppMode, RunMeta } from "@/lib/types";
import { cn } from "@/lib/utils";
import {
  Select,
  SelectContent,
  SelectGroup,
  SelectItem,
  SelectLabel,
  SelectSeparator,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select";

interface HeaderProps {
  mode: AppMode;
  onModeChange: (mode: AppMode) => void;
  selectedRun: string;
  onRunChange: (runId: string) => void;
  availableRuns: RunMeta[];
  currentQuestion?: string;
  onNavigateHome?: () => void;
  onNavigateToolkit?: () => void;
}

export function Header({
  mode,
  onModeChange,
  selectedRun,
  onRunChange,
  availableRuns,
  currentQuestion,
  onNavigateHome,
  onNavigateToolkit,
}: HeaderProps) {
  const replayRuns = availableRuns.filter((r) => !r.is_live);
  const liveRuns = availableRuns.filter((r) => r.is_live);

  return (
    <header className="sticky top-0 z-40 border-b bg-background/95 backdrop-blur supports-[backdrop-filter]:bg-background/60">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="flex items-center justify-between py-3">
          {/* Left: title + caption */}
          <div className="min-w-0">
            <div className="flex items-center gap-3">
              <h1 className="text-lg font-semibold tracking-tight">
                {onNavigateHome ? (
                  <button
                    onClick={onNavigateHome}
                    className="hover:text-muted-foreground transition-colors"
                  >
                    How RLMs Work
                  </button>
                ) : (
                  "How RLMs Work"
                )}
              </h1>
              <span className="hidden sm:inline text-xs text-muted-foreground font-mono bg-muted px-2 py-0.5 rounded">
                dsprrr
              </span>
              {onNavigateToolkit && (
                <button
                  onClick={onNavigateToolkit}
                  className="hidden sm:inline text-xs text-muted-foreground hover:text-foreground transition-colors"
                >
                  Toolkit
                </button>
              )}
            </div>
            {mode === "replay" && availableRuns.length > 1 && (
              <p className="hidden sm:block text-xs text-muted-foreground mt-0.5 italic">
                Same question, different exploration paths
              </p>
            )}
          </div>

          <div className="flex items-center gap-4">
            {/* Run selector (replay mode only) */}
            {mode === "replay" && availableRuns.length > 0 && (
              <Select value={selectedRun} onValueChange={onRunChange}>
                <SelectTrigger size="sm" className="max-w-[220px]">
                  <SelectValue />
                </SelectTrigger>
                <SelectContent>
                  {replayRuns.length > 0 && (
                    <SelectGroup>
                      <SelectLabel>Recorded Runs</SelectLabel>
                      {replayRuns.map((run) => (
                        <SelectItem key={run.id} value={run.id}>
                          <span className="flex items-center gap-2">
                            <span className="font-mono text-xs">{run.label}</span>
                            <span className="text-muted-foreground text-xs">
                              {run.iterations} iter
                            </span>
                          </span>
                        </SelectItem>
                      ))}
                    </SelectGroup>
                  )}
                  {liveRuns.length > 0 && (
                    <>
                      <SelectSeparator />
                      <SelectGroup>
                        <SelectLabel>Live Runs (this session)</SelectLabel>
                        {liveRuns.map((run) => (
                          <SelectItem key={run.id} value={run.id}>
                            <span className="flex items-center gap-2">
                              <span className="font-mono text-xs">{run.label}</span>
                              <span className="text-muted-foreground text-xs">
                                {run.iterations} iter &middot; {run.model}
                              </span>
                            </span>
                          </SelectItem>
                        ))}
                      </SelectGroup>
                    </>
                  )}
                </SelectContent>
              </Select>
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

        {/* Current question row */}
        {currentQuestion && (
          <div className="hidden sm:block pb-3 -mt-1">
            <p className="text-xs text-muted-foreground truncate">
              <span className="font-medium text-foreground">Investigating:</span>{" "}
              <span className="italic">&ldquo;{currentQuestion}&rdquo;</span>
            </p>
          </div>
        )}
      </div>
    </header>
  );
}
