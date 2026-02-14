import { useState, useCallback } from "react";
import { useShinyInput, useShinyOutput, useShinyMessageHandler } from "@posit/shiny-react";
import type { TraceData, RunMeta, AppMode, LiveConfig, LiveStatus } from "@/lib/types";
import { Header } from "./Header";
import { IntroPanel } from "./IntroPanel";
import { REPLTimeline } from "./REPLTimeline";
import { PhaseTimeline } from "./PhaseTimeline";
import { ContextPanel } from "./ContextPanel";
import { PlaybackControls } from "./PlaybackControls";
import { LiveModePanel } from "./LiveModePanel";
import { RunComparison } from "./RunComparison";
import { usePlayback } from "@/hooks/usePlayback";
import { usePhaseDetection } from "@/hooks/usePhaseDetection";

export function App() {
  const [showIntro, setShowIntro] = useState(true);
  const [liveTrace, setLiveTrace] = useState<TraceData | null>(null);
  const [liveStatus, setLiveStatus] = useState<LiveStatus>({ status: "idle" });

  // Shiny communication
  const [traceData] = useShinyOutput<TraceData>("trace_data", undefined);
  const [availableRuns] = useShinyOutput<RunMeta[]>("available_runs", []);
  const [selectedRun, setSelectedRun] = useShinyInput<string>("selected_run", "bslib-run-1");
  const [mode, setMode] = useShinyInput<AppMode>("mode", "replay");
  const [, setStartLiveRun] = useShinyInput<LiveConfig | null>("start_live_run", null, {
    priority: "event",
  });

  // Live mode message handlers
  useShinyMessageHandler("live_result", (data: TraceData) => {
    setLiveTrace(data);
  });

  useShinyMessageHandler("live_status", (status: LiveStatus) => {
    setLiveStatus(status);
  });

  // Active trace: either from replay or live mode
  const activeTrace = mode === "live" && liveTrace ? liveTrace : traceData;
  const iterations = activeTrace?.iterations ?? [];

  // Playback controls
  const playback = usePlayback(iterations.length);
  const phaseProgress = usePhaseDetection(iterations, playback.currentIndex);

  const handleStartExploring = useCallback(() => {
    setShowIntro(false);
    playback.play();
  }, [playback]);

  const handleRunChange = useCallback(
    (runId: string) => {
      setSelectedRun(runId);
      playback.reset();
    },
    [setSelectedRun, playback],
  );

  const handleStartLive = useCallback(
    (config: LiveConfig) => {
      setStartLiveRun(config);
      setLiveTrace(null);
      playback.reset();
    },
    [setStartLiveRun, playback],
  );

  if (showIntro) {
    return <IntroPanel onStart={handleStartExploring} />;
  }

  return (
    <div className="min-h-screen bg-background">
      <Header
        mode={mode}
        onModeChange={setMode}
        selectedRun={selectedRun}
        onRunChange={handleRunChange}
        availableRuns={availableRuns ?? []}
      />

      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 pb-24">
        {/* Phase progress bar */}
        <div className="py-4">
          <PhaseTimeline
            phases={phaseProgress.phases}
            currentPhase={phaseProgress.currentPhase}
            phaseTransitions={phaseProgress.phaseTransitions}
            totalIterations={iterations.length}
            currentIndex={playback.currentIndex}
          />
        </div>

        {mode === "live" ? (
          <LiveModePanel
            onStartRun={handleStartLive}
            liveStatus={liveStatus}
            liveTrace={liveTrace}
          />
        ) : null}

        {/* Main content grid */}
        <div className="grid grid-cols-1 lg:grid-cols-[1fr_380px] gap-6">
          <div className="space-y-4">
            {activeTrace ? (
              <REPLTimeline
                iterations={iterations}
                currentIndex={playback.currentIndex}
                question={activeTrace.question}
              />
            ) : (
              <div className="text-center py-12 text-muted-foreground">
                {mode === "replay"
                  ? "Select a run to begin..."
                  : "Configure and start a live run above."}
              </div>
            )}

            {/* Run comparison - show after completing a run */}
            {playback.state === "done" && availableRuns && availableRuns.length > 1 && (
              <RunComparison runs={availableRuns} currentRun={selectedRun} />
            )}
          </div>

          {/* Sidebar: context panel */}
          <div className="hidden lg:block">
            {activeTrace && (
              <ContextPanel
                contextVariables={activeTrace.context_variables}
                iterations={iterations}
                currentIndex={playback.currentIndex}
                totalTokens={activeTrace.total_tokens}
              />
            )}
          </div>
        </div>
      </div>

      {/* Sticky playback controls */}
      <PlaybackControls
        state={playback.state}
        currentIndex={playback.currentIndex}
        totalIterations={iterations.length}
        speed={playback.speed}
        runLabel={activeTrace?.run_id}
        onPlay={playback.play}
        onPause={playback.pause}
        onStep={playback.step}
        onStepBack={playback.stepBack}
        onReset={playback.reset}
        onSpeedChange={playback.setSpeed}
        onJumpTo={playback.jumpTo}
      />
    </div>
  );
}
