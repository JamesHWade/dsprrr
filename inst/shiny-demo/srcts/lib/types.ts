export interface ContextVariable {
  name: string;
  size_chars: number;
  n_files: number;
}

export interface TokenUsage {
  input: number;
  output: number;
}

export interface Iteration {
  iteration: number;
  reasoning: string;
  code: string;
  output: string;
  success: boolean;
  is_final: boolean;
  phase?: Phase;
  tokens?: TokenUsage;
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
  total_tokens?: TokenUsage;
}

export interface RunMeta {
  id: string;
  label: string;
  question: string;
  model: string;
  iterations: number;
  total_tokens?: TokenUsage;
}

export interface LiveConfig {
  provider: string;
  model: string;
  api_key?: string;
  question: string;
}

export interface ProviderOption {
  provider: string;
  label: string;
  models: { value: string; label: string }[];
  env_var: string;
  needs_api_key_input?: boolean;
}

export const PROVIDERS: ProviderOption[] = [
  {
    provider: "openai",
    label: "OpenAI",
    models: [
      { value: "gpt-5-mini", label: "GPT-5 Mini" },
      { value: "gpt-4o", label: "GPT-4o" },
      { value: "gpt-4o-mini", label: "GPT-4o Mini" },
      { value: "o3-mini", label: "o3-mini" },
    ],
    env_var: "OPENAI_API_KEY",
    needs_api_key_input: true,
  },
  {
    provider: "anthropic",
    label: "Anthropic",
    models: [
      { value: "claude-sonnet-4-5-20250929", label: "Claude Sonnet 4.5" },
      { value: "claude-haiku-4-5-20251001", label: "Claude Haiku 4.5" },
    ],
    env_var: "ANTHROPIC_API_KEY",
    needs_api_key_input: true,
  },
  {
    provider: "google",
    label: "Google Gemini",
    models: [
      { value: "gemini-2.0-flash", label: "Gemini 2.0 Flash" },
      { value: "gemini-2.0-pro", label: "Gemini 2.0 Pro" },
    ],
    env_var: "GOOGLE_API_KEY",
    needs_api_key_input: true,
  },
  {
    provider: "groq",
    label: "Groq",
    models: [
      { value: "llama-3.3-70b-versatile", label: "Llama 3.3 70B" },
      { value: "gemma2-9b-it", label: "Gemma 2 9B" },
    ],
    env_var: "GROQ_API_KEY",
    needs_api_key_input: true,
  },
  {
    provider: "deepseek",
    label: "DeepSeek",
    models: [
      { value: "deepseek-chat", label: "DeepSeek V3" },
      { value: "deepseek-reasoner", label: "DeepSeek R1" },
    ],
    env_var: "DEEPSEEK_API_KEY",
    needs_api_key_input: true,
  },
  {
    provider: "mistral",
    label: "Mistral",
    models: [
      { value: "mistral-large-latest", label: "Mistral Large" },
      { value: "mistral-small-latest", label: "Mistral Small" },
    ],
    env_var: "MISTRAL_API_KEY",
    needs_api_key_input: true,
  },
  {
    provider: "perplexity",
    label: "Perplexity",
    models: [
      { value: "llama-3.1-sonar-large-128k-online", label: "Sonar Large (online)" },
      { value: "llama-3.1-sonar-small-128k-online", label: "Sonar Small (online)" },
    ],
    env_var: "PERPLEXITY_API_KEY",
    needs_api_key_input: true,
  },
  {
    provider: "openrouter",
    label: "OpenRouter",
    models: [
      { value: "anthropic/claude-sonnet-4-5", label: "Claude Sonnet 4.5" },
      { value: "openai/gpt-4o", label: "GPT-4o" },
      { value: "google/gemini-2.0-flash-001", label: "Gemini 2.0 Flash" },
      { value: "meta-llama/llama-3.3-70b-instruct", label: "Llama 3.3 70B" },
    ],
    env_var: "OPENROUTER_API_KEY",
    needs_api_key_input: true,
  },
  {
    provider: "huggingface",
    label: "HuggingFace",
    models: [
      { value: "meta-llama/Llama-3.3-70B-Instruct", label: "Llama 3.3 70B" },
      { value: "mistralai/Mistral-7B-Instruct-v0.3", label: "Mistral 7B" },
    ],
    env_var: "HF_TOKEN",
    needs_api_key_input: true,
  },
  {
    provider: "github",
    label: "GitHub Models",
    models: [
      { value: "gpt-4o-mini", label: "GPT-4o Mini" },
      { value: "gpt-4o", label: "GPT-4o" },
    ],
    env_var: "GITHUB_PAT",
    needs_api_key_input: true,
  },
  {
    provider: "ollama",
    label: "Ollama (local)",
    models: [
      { value: "llama3.2", label: "Llama 3.2" },
      { value: "qwen2.5-coder", label: "Qwen 2.5 Coder" },
      { value: "mistral", label: "Mistral" },
    ],
    env_var: "OLLAMA_HOST",
  },
];

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
