import { cn } from "@/lib/utils";
import type { PlaybackState, PlaybackSpeed } from "@/lib/types";
import { Badge } from "@/components/ui/badge";

interface PlaybackControlsProps {
  state: PlaybackState;
  currentIndex: number;
  totalIterations: number;
  speed: PlaybackSpeed;
  runLabel?: string;
  onPlay: () => void;
  onPause: () => void;
  onStep: () => void;
  onStepBack: () => void;
  onReset: () => void;
  onSpeedChange: (speed: PlaybackSpeed) => void;
  onJumpTo: (index: number) => void;
}

export function PlaybackControls({
  state,
  currentIndex,
  totalIterations,
  speed,
  runLabel,
  onPlay,
  onPause,
  onStep,
  onStepBack,
  onReset,
  onSpeedChange,
  onJumpTo,
}: PlaybackControlsProps) {
  if (totalIterations === 0) return null;

  const progress =
    totalIterations > 0 ? ((currentIndex + 1) / totalIterations) * 100 : 0;
  const isDone = state === "done";

  return (
    <div className="fixed bottom-0 left-0 right-0 z-50 border-t bg-background/95 backdrop-blur supports-[backdrop-filter]:bg-background/80">
      {/* Progress bar */}
      <div className="h-1 bg-muted">
        <div
          className={cn(
            "h-full transition-all duration-300",
            isDone ? "bg-green-500" : "bg-primary",
          )}
          style={{ width: `${Math.max(0, progress)}%` }}
        />
      </div>

      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="flex items-center justify-between h-14">
          {/* Left: transport controls + run badge */}
          <div className="flex items-center gap-2">
            <div className="flex items-center gap-1">
              {/* Reset */}
              <button
                onClick={onReset}
                className="p-2 rounded-md text-muted-foreground hover:text-foreground hover:bg-muted transition-colors"
                title="Reset"
              >
                <svg className="w-4 h-4" fill="none" viewBox="0 0 24 24" strokeWidth={2} stroke="currentColor">
                  <path strokeLinecap="round" strokeLinejoin="round" d="M16.023 9.348h4.992v-.001M2.985 19.644v-4.992m0 0h4.992m-4.993 0 3.181 3.183a8.25 8.25 0 0 0 13.803-3.7M4.031 9.865a8.25 8.25 0 0 1 13.803-3.7l3.181 3.182" />
                </svg>
              </button>

              {/* Step back */}
              <button
                onClick={onStepBack}
                disabled={currentIndex <= 0}
                className="p-2 rounded-md text-muted-foreground hover:text-foreground hover:bg-muted transition-colors disabled:opacity-30"
                title="Previous"
              >
                <svg className="w-4 h-4" fill="none" viewBox="0 0 24 24" strokeWidth={2} stroke="currentColor">
                  <path strokeLinecap="round" strokeLinejoin="round" d="M21 16.811c0 .864-.933 1.406-1.683.977l-7.108-4.062a1.125 1.125 0 0 1 0-1.953l7.108-4.062A1.125 1.125 0 0 1 21 8.688v8.123ZM11.25 16.811c0 .864-.933 1.406-1.683.977l-7.108-4.062a1.125 1.125 0 0 1 0-1.953l7.108-4.062a1.125 1.125 0 0 1 1.683.977v8.123Z" />
                </svg>
              </button>

              {/* Play / Pause / Done */}
              {isDone ? (
                <button
                  onClick={onReset}
                  className="p-2.5 rounded-full bg-green-500 text-white hover:bg-green-600 transition-colors"
                  title="Complete — click to replay"
                >
                  <svg className="w-5 h-5" fill="none" viewBox="0 0 24 24" strokeWidth={2.5} stroke="currentColor">
                    <path strokeLinecap="round" strokeLinejoin="round" d="m4.5 12.75 6 6 9-13.5" />
                  </svg>
                </button>
              ) : (
                <button
                  onClick={state === "playing" ? onPause : onPlay}
                  className="p-2.5 rounded-full bg-primary text-primary-foreground hover:bg-primary/90 transition-colors"
                  title={state === "playing" ? "Pause" : "Play"}
                >
                  {state === "playing" ? (
                    <svg className="w-5 h-5" fill="currentColor" viewBox="0 0 24 24">
                      <rect x="6" y="4" width="4" height="16" rx="1" />
                      <rect x="14" y="4" width="4" height="16" rx="1" />
                    </svg>
                  ) : (
                    <svg className="w-5 h-5" fill="currentColor" viewBox="0 0 24 24">
                      <path d="M8 5.14v14l11-7-11-7z" />
                    </svg>
                  )}
                </button>
              )}

              {/* Step forward */}
              <button
                onClick={onStep}
                disabled={currentIndex >= totalIterations - 1}
                className="p-2 rounded-md text-muted-foreground hover:text-foreground hover:bg-muted transition-colors disabled:opacity-30"
                title="Next"
              >
                <svg className="w-4 h-4" fill="none" viewBox="0 0 24 24" strokeWidth={2} stroke="currentColor">
                  <path strokeLinecap="round" strokeLinejoin="round" d="M3 8.689c0-.864.933-1.406 1.683-.977l7.108 4.061a1.125 1.125 0 0 1 0 1.954l-7.108 4.061A1.125 1.125 0 0 1 3 16.811V8.69ZM12.75 8.689c0-.864.933-1.406 1.683-.977l7.108 4.061a1.125 1.125 0 0 1 0 1.954l-7.108 4.061a1.125 1.125 0 0 1-1.683-.977V8.69Z" />
                </svg>
              </button>
            </div>

            {/* Run badge */}
            {runLabel && (
              <Badge variant="secondary" className="hidden sm:inline-flex font-mono text-[10px]">
                {runLabel}
              </Badge>
            )}
          </div>

          {/* Center: iteration scrubber */}
          <div className="flex-1 mx-6 hidden sm:flex items-center gap-3">
            <input
              type="range"
              min={0}
              max={totalIterations - 1}
              value={Math.max(0, currentIndex)}
              onChange={(e) => onJumpTo(Number(e.target.value))}
              className="flex-1 h-1.5 bg-muted rounded-full appearance-none cursor-pointer accent-primary"
            />
            <span className="text-xs font-mono text-muted-foreground whitespace-nowrap text-right">
              {currentIndex < 0 ? "\u2014" : currentIndex + 1}/{totalIterations}
            </span>
          </div>

          {/* Right: speed control */}
          <div className="flex items-center gap-1">
            <span className="text-xs text-muted-foreground mr-1 hidden sm:inline">Speed</span>
            {([1, 2, 3] as PlaybackSpeed[]).map((s) => (
              <button
                key={s}
                onClick={() => onSpeedChange(s)}
                className={cn(
                  "px-2 py-1 text-xs rounded transition-colors",
                  s === speed
                    ? "bg-primary text-primary-foreground"
                    : "text-muted-foreground hover:text-foreground hover:bg-muted",
                )}
              >
                {s}x
              </button>
            ))}
          </div>
        </div>
      </div>
    </div>
  );
}
