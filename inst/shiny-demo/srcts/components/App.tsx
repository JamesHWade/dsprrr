import { useState, useCallback, useRef } from "react";
import { useShinyInput, useShinyOutput, useShinyMessageHandler } from "@posit/shiny-react";
import type { TraceData, RunMeta, AppMode, LiveConfig, LiveStatus } from "@/lib/types";
import { Header } from "./Header";
import { IntroPanel } from "./IntroPanel";
import { ToolkitPanel } from "./ToolkitPanel";
import { REPLTimeline } from "./REPLTimeline";
import { PhaseTimeline } from "./PhaseTimeline";
import { ContextPanel } from "./ContextPanel";
import { PlaybackControls } from "./PlaybackControls";
import { LiveModePanel } from "./LiveModePanel";
import { RunComparison } from "./RunComparison";
import { usePlayback } from "@/hooks/usePlayback";
import { usePhaseDetection } from "@/hooks/usePhaseDetection";
import {
  Collapsible,
  CollapsibleContent,
  CollapsibleTrigger,
} from "@/components/ui/collapsible";

export function App() {
  const [showIntro, setShowIntro] = useState(true);
  const [showToolkit, setShowToolkit] = useState(false);
  const [showOrientation, setShowOrientation] = useState(true);
  const [liveTrace, setLiveTrace] = useState<TraceData | null>(null);
  const [liveStatus, setLiveStatus] = useState<LiveStatus>({ status: "idle" });

  // Session-local storage for completed live runs
  const sessionTracesRef = useRef<Map<string, TraceData>>(new Map());
  const [sessionRuns, setSessionRuns] = useState<RunMeta[]>([]);
  const liveRunCounterRef = useRef(0);

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

    // Store completed live trace as a session run
    liveRunCounterRef.current += 1;
    const sessionId = `live-${liveRunCounterRef.current}`;
    const traceWithId = { ...data, run_id: sessionId };
    sessionTracesRef.current.set(sessionId, traceWithId);

    const meta: RunMeta = {
      id: sessionId,
      label: `Live #${liveRunCounterRef.current}`,
      description: `${data.iterations?.length ?? 0} iterations, ${data.model}`,
      question: data.question ?? "",
      model: data.model ?? "",
      iterations: data.iterations?.length ?? 0,
      is_live: true,
      total_tokens: data.total_tokens,
    };
    setSessionRuns((prev) => [...prev, meta]);
  });

  useShinyMessageHandler("live_status", (status: LiveStatus) => {
    setLiveStatus(status);
  });

  // Merge server runs + session live runs
  const allRuns = [...(availableRuns ?? []), ...sessionRuns];

  // Active trace: session live run > current live > server replay
  // Note: sessionTracesRef is a useRef (not useState) for performance. Reading it
  // during render is safe here because the ref is only populated in the same handler
  // that calls setSessionRuns, which triggers the re-render that makes the matching
  // selectedRun available in the dropdown. Users can't select a session run before
  // the re-render that adds it to allRuns.
  const sessionTrace = sessionTracesRef.current.get(selectedRun);
  const activeTrace = sessionTrace
    ?? (mode === "live" && liveTrace ? liveTrace : traceData);
  const iterations = activeTrace?.iterations ?? [];

  // Playback controls
  const playback = usePlayback(iterations.length);
  const phaseProgress = usePhaseDetection(iterations, playback.currentIndex);

  const handleStartExploring = useCallback(() => {
    setShowIntro(false);
    setShowToolkit(true);
  }, []);

  const handleStartTraces = useCallback(() => {
    setShowIntro(false);
    setShowToolkit(false);
    playback.play();
  }, [playback]);

  const handleShowToolkit = useCallback(() => {
    setShowToolkit(true);
    playback.pause();
  }, [playback]);

  const handleNavigateHome = useCallback(() => {
    setShowIntro(true);
    setShowToolkit(false);
    playback.pause();
  }, [playback]);

  const handleRunChange = useCallback(
    (runId: string) => {
      setSelectedRun(runId);
      // Switch to replay mode so the run selector + trace render
      setMode("replay");
      playback.reset();
    },
    [setSelectedRun, setMode, playback],
  );

  const handleStartLive = useCallback(
    (config: LiveConfig) => {
      setStartLiveRun(config);
      setLiveTrace(null);
      playback.reset();
    },
    [setStartLiveRun, playback],
  );

  // Auto-hide orientation banner when playback completes
  const orientationVisible = showOrientation && playback.state !== "done";

  if (showIntro) {
    return (
      <IntroPanel
        onLearnToolkit={handleStartExploring}
        onJumpToTraces={handleStartTraces}
      />
    );
  }

  if (showToolkit) {
    return <ToolkitPanel onContinue={handleStartTraces} onBack={handleNavigateHome} />;
  }

  return (
    <div className="min-h-screen bg-background">
      <Header
        mode={mode}
        onModeChange={setMode}
        selectedRun={selectedRun}
        onRunChange={handleRunChange}
        availableRuns={allRuns}
        currentQuestion={activeTrace?.question}
        onNavigateHome={handleNavigateHome}
        onNavigateToolkit={handleShowToolkit}
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

        {/* Orientation banner */}
        {orientationVisible && (
          <div className="mb-4 rounded-lg border border-blue-200 dark:border-blue-900/50 bg-blue-50 dark:bg-blue-950/30 px-4 py-3 flex items-start gap-3">
            <div className="flex-1 text-sm text-blue-700 dark:text-blue-400">
              <span className="font-semibold">Watch the LLM think in code.</span>{" "}
              <span className="text-blue-600 dark:text-blue-500">
                Each card below is one REPL iteration &mdash; the model writes R code,
                executes it, reads the output, and decides what to do next. The sidebar
                tracks how little source data actually enters the context window.
              </span>
            </div>
            <button
              onClick={() => setShowOrientation(false)}
              className="shrink-0 p-1 rounded hover:bg-blue-200/50 dark:hover:bg-blue-800/30 text-blue-400 hover:text-blue-600 transition-colors"
              aria-label="Dismiss"
            >
              <svg className="w-4 h-4" fill="none" viewBox="0 0 24 24" strokeWidth={2} stroke="currentColor">
                <path strokeLinecap="round" strokeLinejoin="round" d="M6 18 18 6M6 6l12 12" />
              </svg>
            </button>
          </div>
        )}

        {mode === "live" ? (
          <LiveModePanel
            onStartRun={handleStartLive}
            liveStatus={liveStatus}
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

            {/* Mobile sidebar collapsible */}
            {activeTrace && (
              <div className="lg:hidden">
                <Collapsible>
                  <CollapsibleTrigger className="flex w-full items-center justify-between rounded-lg border bg-card px-4 py-3 text-sm font-medium hover:bg-muted/50 transition-colors">
                    <span>Token Economy &amp; Context</span>
                    <svg className="w-4 h-4 text-muted-foreground" fill="none" viewBox="0 0 24 24" strokeWidth={2} stroke="currentColor">
                      <path strokeLinecap="round" strokeLinejoin="round" d="m19.5 8.25-7.5 7.5-7.5-7.5" />
                    </svg>
                  </CollapsibleTrigger>
                  <CollapsibleContent className="mt-2">
                    <ContextPanel
                      contextVariables={activeTrace.context_variables}
                      iterations={iterations}
                      currentIndex={playback.currentIndex}
                      totalTokens={activeTrace.total_tokens}
                    />
                  </CollapsibleContent>
                </Collapsible>
              </div>
            )}

            {/* Run comparison - show after completing a run */}
            {playback.state === "done" && allRuns.length > 1 && (
              <RunComparison
                runs={allRuns}
                currentRun={selectedRun}
                onRunChange={handleRunChange}
              />
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
