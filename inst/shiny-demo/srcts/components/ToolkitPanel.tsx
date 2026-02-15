import { Tabs, TabsList, TabsTrigger, TabsContent } from "@/components/ui/tabs";
import { Badge } from "@/components/ui/badge";
import { CodeBlock } from "./CodeBlock";
import { Eye, Search, MessageCircle, CheckCircle2 } from "lucide-react";
import type { ComponentType } from "react";

interface ToolkitPanelProps {
  onContinue: () => void;
  onBack: () => void;
}

const actions = [
  {
    tab: "peek",
    name: "peek()",
    icon: Eye,
    signature: "peek(variable, start, end)",
    oneLiner: "Read a slice of a variable",
    example: 'peek(bslib_source, 100, 200)',
    description:
      "Returns lines 100\u2013200 of a variable. Only that slice enters the prompt \u2014 the rest stays in R memory.",
    note:
      "The full source can be millions of characters. peek() pulls in only what the model asks for.",
  },
  {
    tab: "search",
    name: "search()",
    icon: Search,
    signature: "search(variable, pattern)",
    oneLiner: "Regex search across a variable",
    example: 'search(bslib_source, "brand_color")',
    description:
      "Returns matching lines with surrounding context. The model doesn\u2019t scan the file \u2014 R does.",
    note:
      "Useful for locating symbols, function definitions, or patterns without reading everything.",
  },
  {
    tab: "ask",
    name: "llm_query()",
    icon: MessageCircle,
    signature: "llm_query(question, context)",
    oneLiner: "Send a sub-question to another LLM call",
    example: 'llm_query("What CSS class sets the foreground?", snippet)',
    description:
      "Delegates a focused question to a separate LLM call with a small context slice. Useful for summarising or interpreting code excerpts.",
    note:
      "The outer model stays on task while a cheaper inner call handles a detail question.",
  },
  {
    tab: "submit",
    name: "SUBMIT()",
    icon: CheckCircle2,
    signature: "SUBMIT(answer = ...)",
    oneLiner: "Return the final answer and stop",
    example: 'SUBMIT(answer = "The color propagates via ...")',
    description:
      "Terminates the REPL loop. The model calls this when it has enough evidence to answer.",
    note:
      "There is no fixed iteration limit. The model decides when to stop.",
  },
] as const;

const loopSteps = [
  { label: "Think", num: "1" },
  { label: "Write R", num: "2" },
  { label: "Execute", num: "3" },
  { label: "Read output", num: "4" },
] as const;

export function ToolkitPanel({ onContinue, onBack }: ToolkitPanelProps) {
  return (
    <div className="min-h-screen bg-background">
      <div className="max-w-3xl mx-auto px-4 sm:px-6 py-12 pb-32 space-y-12">
        {/* 1. Header */}
        <div className="space-y-3">
          <button
            onClick={onBack}
            className="text-sm text-muted-foreground hover:text-foreground transition-colors flex items-center gap-1"
          >
            <svg className="w-4 h-4" fill="none" viewBox="0 0 24 24" strokeWidth={2} stroke="currentColor">
              <path strokeLinecap="round" strokeLinejoin="round" d="M15.75 19.5 8.25 12l7.5-7.5" />
            </svg>
            Back
          </button>
          <h1 className="text-3xl sm:text-4xl font-bold tracking-tight">
            How the RLM explores
          </h1>
          <p className="text-sm text-muted-foreground max-w-xl">
            The LLM never sees all the source code at once. Instead, it writes
            R expressions in a REPL loop, using these four actions to pull in
            only what it needs.
          </p>
        </div>

        {/* 2. Tabbed action cards */}
        <Tabs defaultValue="peek" className="w-full">
          <TabsList className="w-full grid grid-cols-4">
            {actions.map((a) => {
              const Icon = a.icon as ComponentType<{ className?: string }>;
              return (
                <TabsTrigger key={a.tab} value={a.tab} className="text-xs sm:text-sm gap-1.5">
                  <Icon className="w-4 h-4 hidden sm:block" />
                  {a.tab === "ask" ? "Ask" : a.tab.charAt(0).toUpperCase() + a.tab.slice(1)}
                </TabsTrigger>
              );
            })}
          </TabsList>

          {actions.map((a) => {
            const Icon = a.icon as ComponentType<{ className?: string }>;
            return (
              <TabsContent key={a.tab} value={a.tab}>
                <div className="rounded-xl border bg-card p-6 space-y-4 mt-2">
                  <div className="flex items-center gap-3 flex-wrap">
                    <Icon className="w-5 h-5 text-muted-foreground" />
                    <Badge variant="secondary" className="font-mono text-xs">
                      {a.name}
                    </Badge>
                    <span className="text-sm text-muted-foreground">
                      {a.oneLiner}
                    </span>
                  </div>

                  <div className="font-mono text-xs text-muted-foreground bg-muted/50 rounded-md px-3 py-2">
                    {a.signature}
                  </div>

                  <p className="text-sm">{a.description}</p>

                  <CodeBlock code={a.example} className="text-xs" />

                  <p className="text-xs text-muted-foreground">{a.note}</p>
                </div>
              </TabsContent>
            );
          })}
        </Tabs>

        {/* 3. The loop */}
        <div className="space-y-4">
          <h2 className="text-lg font-semibold">The REPL loop</h2>
          <p className="text-sm text-muted-foreground max-w-lg">
            Each iteration follows the same cycle. Code executes in a
            separate R process. The model only sees the printed output.
          </p>

          <div className="flex items-center justify-center gap-0 py-4 overflow-x-auto">
            {loopSteps.map((step, i) => (
              <div key={step.label} className="flex items-center">
                <div className="flex flex-col items-center gap-1.5">
                  <div className="w-12 h-12 sm:w-14 sm:h-14 rounded-lg border bg-card flex items-center justify-center text-sm font-mono font-semibold text-muted-foreground">
                    {step.num}
                  </div>
                  <span className="text-[10px] sm:text-xs font-medium text-muted-foreground whitespace-nowrap">
                    {step.label}
                  </span>
                </div>
                {i < loopSteps.length - 1 && (
                  <svg
                    className="w-6 h-4 sm:w-8 text-muted-foreground/40 shrink-0 mx-1"
                    viewBox="0 0 24 12"
                    fill="none"
                    stroke="currentColor"
                    strokeWidth={2}
                  >
                    <path d="M2 6h16m0 0-4-4m4 4-4 4" strokeLinecap="round" strokeLinejoin="round" />
                  </svg>
                )}
              </div>
            ))}
          </div>

          <div className="flex justify-center">
            <div className="text-xs text-muted-foreground border border-dashed border-muted-foreground/30 rounded-full px-4 py-1.5">
              repeats until <code className="font-mono font-semibold text-foreground">SUBMIT()</code>
            </div>
          </div>
        </div>
      </div>

      {/* Sticky "Continue to traces" bar */}
      <div className="fixed bottom-0 inset-x-0 border-t bg-background/95 backdrop-blur supports-[backdrop-filter]:bg-background/80 z-30">
        <div className="max-w-3xl mx-auto px-4 sm:px-6 py-4 flex justify-center">
          <button
            onClick={onContinue}
            className="inline-flex items-center justify-center rounded-lg bg-primary px-8 py-3 text-sm font-medium text-primary-foreground shadow-sm hover:bg-primary/90 transition-colors"
          >
            Continue to traces
          </button>
        </div>
      </div>
    </div>
  );
}
