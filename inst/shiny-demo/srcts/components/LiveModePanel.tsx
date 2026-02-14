import { useState, useMemo } from "react";
import type { LiveConfig, LiveStatus, ProviderOption } from "@/lib/types";
import { PROVIDERS } from "@/lib/types";
import { cn } from "@/lib/utils";

interface LiveModePanelProps {
  onStartRun: (config: LiveConfig) => void;
  liveStatus: LiveStatus;
}

export function LiveModePanel({
  onStartRun,
  liveStatus,
}: LiveModePanelProps) {
  const [providerKey, setProviderKey] = useState("openai");
  const [model, setModel] = useState("gpt-5-mini");
  const [apiKey, setApiKey] = useState("");
  const [question, setQuestion] = useState(
    "How does bslib process the foreground color from brand.yml?",
  );

  const provider = useMemo(
    () => PROVIDERS.find((p) => p.provider === providerKey) as ProviderOption,
    [providerKey],
  );

  const isRunning = liveStatus.status === "running";

  const handleProviderChange = (newProvider: string) => {
    setProviderKey(newProvider);
    setApiKey("");
    const p = PROVIDERS.find((pr) => pr.provider === newProvider);
    if (p && p.models.length > 0) {
      setModel(p.models[0].value);
    }
  };

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    if (!question.trim() || isRunning) return;
    onStartRun({
      provider: providerKey,
      model,
      api_key: apiKey || undefined,
      question,
    });
  };

  return (
    <div className="rounded-xl border bg-card p-6 mb-6">
      <h3 className="text-sm font-semibold mb-4">Live RLM Execution</h3>

      <form onSubmit={handleSubmit} className="space-y-4">
        <div className="grid sm:grid-cols-3 gap-4">
          <div className="space-y-1.5">
            <label className="text-xs font-medium text-muted-foreground" htmlFor="provider">
              Provider
            </label>
            <select
              id="provider"
              value={providerKey}
              onChange={(e) => handleProviderChange(e.target.value)}
              className="w-full h-9 rounded-md border border-input bg-background px-3 text-sm"
              disabled={isRunning}
            >
              {PROVIDERS.map((p) => (
                <option key={p.provider} value={p.provider}>
                  {p.label}
                </option>
              ))}
            </select>
          </div>

          <div className="space-y-1.5">
            <label className="text-xs font-medium text-muted-foreground" htmlFor="model">
              Model
            </label>
            <select
              id="model"
              value={model}
              onChange={(e) => setModel(e.target.value)}
              className="w-full h-9 rounded-md border border-input bg-background px-3 text-sm"
              disabled={isRunning}
            >
              {provider.models.map((m) => (
                <option key={m.value} value={m.value}>
                  {m.label}
                </option>
              ))}
            </select>
          </div>

          <div className="space-y-1.5">
            <label className="text-xs font-medium text-muted-foreground" htmlFor="api-key">
              API Key
              <span className="ml-1 text-[10px] opacity-60">
                or set {provider.env_var}
              </span>
            </label>
            <input
              id="api-key"
              type="password"
              value={apiKey}
              onChange={(e) => setApiKey(e.target.value)}
              placeholder={`${provider.env_var} or paste key...`}
              className="w-full h-9 rounded-md border border-input bg-background px-3 text-sm"
              disabled={isRunning}
            />
          </div>
        </div>

        <div className="space-y-1.5">
          <label className="text-xs font-medium text-muted-foreground" htmlFor="question">
            Question
          </label>
          <textarea
            id="question"
            value={question}
            onChange={(e) => setQuestion(e.target.value)}
            rows={2}
            className="w-full rounded-md border border-input bg-background px-3 py-2 text-sm resize-none"
            disabled={isRunning}
          />
        </div>

        <div className="flex items-center gap-3">
          <button
            type="submit"
            disabled={!question.trim() || isRunning}
            className={cn(
              "inline-flex items-center justify-center rounded-lg px-4 py-2 text-sm font-medium shadow-sm transition-colors",
              isRunning
                ? "bg-muted text-muted-foreground cursor-not-allowed"
                : "bg-primary text-primary-foreground hover:bg-primary/90",
            )}
          >
            {isRunning ? (
              <>
                <svg
                  className="animate-spin -ml-1 mr-2 h-4 w-4"
                  fill="none"
                  viewBox="0 0 24 24"
                >
                  <circle
                    className="opacity-25"
                    cx="12"
                    cy="12"
                    r="10"
                    stroke="currentColor"
                    strokeWidth="4"
                  />
                  <path
                    className="opacity-75"
                    fill="currentColor"
                    d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"
                  />
                </svg>
                Running...
              </>
            ) : (
              "Run RLM"
            )}
          </button>

          {liveStatus.status === "error" && (
            <span className="text-sm text-destructive">{liveStatus.message}</span>
          )}
          {liveStatus.status === "complete" && (
            <span className="text-sm text-green-600 dark:text-green-400">
              Complete! Results shown below.
            </span>
          )}
        </div>
      </form>

      <p className="mt-4 text-xs text-muted-foreground">
        Your API key is sent directly to the provider and is never stored.
        Alternatively, set the environment variable ({provider.env_var}) before launching the app and leave the key field empty.
        Powered by <a href="https://ellmer.tidyverse.org" className="underline" target="_blank" rel="noopener">ellmer</a> — any provider it supports works here.
      </p>
    </div>
  );
}
