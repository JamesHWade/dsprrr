interface IntroPanelProps {
  onStart: () => void;
}

export function IntroPanel({ onStart }: IntroPanelProps) {
  return (
    <div className="min-h-screen flex items-center justify-center bg-background p-6">
      <div className="max-w-2xl w-full space-y-8">
        {/* Hero */}
        <div className="text-center space-y-4">
          <h1 className="text-4xl sm:text-5xl font-bold tracking-tight">
            Recursive Language Models
          </h1>
          <p className="text-xl text-muted-foreground">
            What if LLMs could <em>explore</em> instead of just <em>read</em>?
          </p>
        </div>

        {/* Comparison */}
        <div className="grid sm:grid-cols-2 gap-4">
          <div className="rounded-xl border bg-card p-6 space-y-3">
            <div className="text-sm font-medium text-muted-foreground uppercase tracking-wider">
              Traditional
            </div>
            <pre className="font-mono text-sm text-muted-foreground leading-relaxed whitespace-pre"><span className="text-foreground">llm</span>$<span className="text-foreground">chat</span>({"\n"}  <span className="text-orange-600 dark:text-orange-400">paste</span>({"\n"}    question,{"\n"}    <span className="text-destructive">all_source_code</span>{"\n"}  ){"\n"})</pre>
            <p className="text-sm text-muted-foreground mt-2">
              Stuff everything into the prompt. Hope the model finds the needle.
            </p>
          </div>

          <div className="rounded-xl border-2 border-primary/30 bg-card p-6 space-y-3">
            <div className="text-sm font-medium text-primary uppercase tracking-wider">
              RLM (dsprrr)
            </div>
            <pre className="font-mono text-sm leading-relaxed whitespace-pre"><span className="text-foreground">rlm</span>({"\n"}  <span className="text-muted-foreground">"question -&gt; answer"</span>,{"\n"}  question,{"\n"}  <span className="text-primary font-semibold">.llm</span>{"\n"})</pre>
            <p className="text-sm text-muted-foreground mt-2">
              Source code lives in <code className="text-xs bg-muted px-1 rounded">.context</code> variables.
              The model writes R to{" "}
              <code className="text-xs bg-muted px-1 rounded">search()</code> and{" "}
              <code className="text-xs bg-muted px-1 rounded">peek()</code> what it needs.
            </p>
          </div>
        </div>

        {/* Key insight */}
        <div className="rounded-xl bg-muted/50 p-6 text-center space-y-2">
          <p className="text-sm font-medium">
            4M characters of source code sit as R variables.
          </p>
          <p className="text-sm text-muted-foreground">
            The LLM only sees variable names and sizes. It writes code to transfer exactly what
            it needs from <strong>programmatic space</strong> to <strong>token space</strong>.
          </p>
        </div>

        {/* CTA */}
        <div className="text-center">
          <button
            onClick={onStart}
            className="inline-flex items-center justify-center rounded-lg bg-primary px-8 py-3 text-sm font-medium text-primary-foreground shadow-sm hover:bg-primary/90 transition-colors"
          >
            Start Exploring
          </button>
          <p className="mt-3 text-xs text-muted-foreground">
            Watch a real RLM investigate bslib issue #1123
          </p>
        </div>
      </div>
    </div>
  );
}
