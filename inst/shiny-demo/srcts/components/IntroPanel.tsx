interface IntroPanelProps {
  onLearnToolkit: () => void;
  onJumpToTraces: () => void;
}

export function IntroPanel({ onLearnToolkit, onJumpToTraces }: IntroPanelProps) {
  return (
    <div className="min-h-screen flex items-center justify-center bg-background p-6">
      <div className="max-w-2xl w-full space-y-8">
        {/* Hero */}
        <div className="text-center space-y-4">
          <h1 className="text-4xl sm:text-5xl font-bold tracking-tight">
            Recursive Language Models
          </h1>
          <p className="text-lg text-muted-foreground max-w-lg mx-auto">
            An RLM doesn&rsquo;t receive all the source code in its prompt.
            It writes R code to explore it incrementally.
          </p>
        </div>

        {/* Comparison */}
        <div className="grid sm:grid-cols-2 gap-4">
          <div className="rounded-xl border bg-card p-6 space-y-3">
            <div className="text-xs text-destructive/80 uppercase tracking-wider font-medium">
              Traditional Approach
            </div>
            <div className="text-sm font-medium text-muted-foreground uppercase tracking-wider">
              Paste everything
            </div>
            <pre className="font-mono text-sm text-muted-foreground leading-relaxed whitespace-pre"><span className="text-foreground">llm</span>$<span className="text-foreground">chat</span>({"\n"}  <span className="text-orange-600 dark:text-orange-400">paste</span>({"\n"}    question,{"\n"}    <span className="text-destructive">all_source_code</span>{"\n"}  ){"\n"})</pre>
            <p className="text-sm text-muted-foreground mt-2">
              The entire codebase goes into the prompt. The model gets one shot to find what matters.
            </p>
          </div>

          <div className="rounded-xl border-2 border-primary/30 bg-primary/5 p-6 space-y-3">
            <div className="text-xs text-primary uppercase tracking-wider font-medium">
              Recursive Language Model
            </div>
            <div className="text-sm font-medium text-muted-foreground uppercase tracking-wider">
              Explore incrementally
            </div>
            <pre className="font-mono text-sm leading-relaxed whitespace-pre"><span className="text-foreground">rlm</span>({"\n"}  <span className="text-muted-foreground">"question -&gt; answer"</span>,{"\n"}  question,{"\n"}  <span className="text-primary font-semibold">.llm</span>{"\n"})</pre>
            <p className="text-sm text-muted-foreground mt-2">
              Source code lives in{" "}
              <code className="text-xs bg-muted px-1 rounded">.context</code> variables.
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
            it needs into the context window.
          </p>
        </div>

        {/* Agents callout */}
        <div className="rounded-xl border border-dashed border-muted-foreground/25 p-6 space-y-3">
          <p className="text-sm font-medium">
            Isn&rsquo;t this just agents with tools?
          </p>
          <p className="text-sm text-muted-foreground leading-relaxed">
            Sort of. The difference is constraint. An agent framework gives the model
            an R interpreter and says &ldquo;figure it out.&rdquo; An RLM gives the model
            a small, fixed toolkit (<code className="text-xs bg-muted px-1 rounded">peek</code>,{" "}
            <code className="text-xs bg-muted px-1 rounded">search</code>,{" "}
            <code className="text-xs bg-muted px-1 rounded">llm_query</code>,{" "}
            <code className="text-xs bg-muted px-1 rounded">SUBMIT</code>) and a compiler that
            can optimize how those tools get used. Constraints make the program
            optimizable; optimizability is what separates a program from a script.
          </p>
        </div>

        {/* Khattab attribution */}
        <div className="text-sm text-muted-foreground space-y-2 px-2">
          <p className="leading-relaxed">
            The idea of <em>symbolic recursion</em> comes from{" "}
            <a
              href="https://x.com/lateinteraction"
              target="_blank"
              rel="noopener noreferrer"
              className="underline underline-offset-2 hover:text-foreground transition-colors"
            >
              Omar Khattab
            </a>
            &rsquo;s work on DSPy: instead of stuffing data into the prompt,
            externalize inputs as variables and let the model write code that
            references them. In R that looks like{" "}
            <code className="text-xs bg-muted px-1 rounded">lapply(chunks, llm_query, ...)</code>{" "}
            rather than pasting every chunk into the context. Each chunk is processed
            in a separate call that scales linearly, instead of quadratic attention
            over the concatenated text.
          </p>
        </div>

        {/* CTA */}
        <div className="text-center space-y-3">
          <div className="flex flex-col sm:flex-row items-center justify-center gap-3">
            <button
              onClick={onLearnToolkit}
              className="inline-flex items-center justify-center rounded-lg bg-primary px-8 py-3 text-sm font-medium text-primary-foreground shadow-sm hover:bg-primary/90 transition-colors"
            >
              How does this work?
            </button>
            <button
              onClick={onJumpToTraces}
              className="inline-flex items-center justify-center rounded-lg border px-8 py-3 text-sm font-medium hover:bg-muted/50 transition-colors"
            >
              Jump to traces
            </button>
          </div>
          <p className="text-xs text-muted-foreground">
            Traces show a real RLM investigating bslib issue #1123
          </p>
        </div>
      </div>
    </div>
  );
}
