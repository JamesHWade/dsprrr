export interface ContextVariable {
  name: string;
  size_chars: number;
  n_files: number;
}

export interface Iteration {
  iteration: number;
  reasoning: string;
  code: string;
  output: string;
  success: boolean;
  is_final: boolean;
  phase?: Phase;
}

export type Phase =
  | "orient"
  | "locate"
  | "trace_sass"
  | "cross_reference"
  | "identify_gap";

export interface TraceData {
  run_id: string;
  timestamp: string;
  question: string;
  model: string;
  context_variables: ContextVariable[];
  iterations: Iteration[];
  final_answer: string;
  iterations_used: number;
  llm_calls_used: number;
}

export interface RunMeta {
  id: string;
  label: string;
  question: string;
  model: string;
  iterations: number;
}

export interface LiveConfig {
  api_key: string;
  question: string;
  model: string;
}

export interface LiveStatus {
  status: "idle" | "running" | "complete" | "error";
  message?: string;
}

export type AppMode = "replay" | "live";

export type PlaybackState = "idle" | "playing" | "paused" | "done";

export type PlaybackSpeed = 1 | 2 | 3;

export const PHASE_INFO: Record<
  Phase,
  { label: string; color: string; description: string }
> = {
  orient: {
    label: "Orient",
    color: "var(--phase-orient)",
    description: "Scanning source structure and available variables",
  },
  locate: {
    label: "Locate",
    color: "var(--phase-locate)",
    description: "Finding specific code regions via peek() and search()",
  },
  trace_sass: {
    label: "Trace",
    color: "var(--phase-trace)",
    description: "Following Sass/CSS dependency chains",
  },
  cross_reference: {
    label: "Cross-ref",
    color: "var(--phase-cross-ref)",
    description: "Comparing across multiple source packages",
  },
  identify_gap: {
    label: "Identify",
    color: "var(--phase-identify)",
    description: "Pinpointing the specific gap or answer",
  },
};

export interface ConceptAnnotation {
  trigger_iteration: number;
  trigger_condition?: (iter: Iteration) => boolean;
  title: string;
  body: string;
}
