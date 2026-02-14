import { cn } from "@/lib/utils";

interface CodeBlockProps {
  code: string;
  className?: string;
}

const RLM_TOOLS = ["SUBMIT", "peek", "search", "llm_query", "llm_query_batched"];

function highlightRlmTools(code: string): (string | JSX.Element)[] {
  const parts: (string | JSX.Element)[] = [];
  const pattern = new RegExp(`\\b(${RLM_TOOLS.join("|")})\\s*\\(`, "g");

  let lastIndex = 0;
  const matches = code.matchAll(pattern);

  for (const match of matches) {
    const start = match.index!;
    if (start > lastIndex) {
      parts.push(code.slice(lastIndex, start));
    }
    parts.push(
      <span key={start} className="rlm-tool">
        {match[1]}
      </span>,
    );
    parts.push("(");
    lastIndex = start + match[0].length;
  }

  if (lastIndex < code.length) {
    parts.push(code.slice(lastIndex));
  }

  return parts.length > 0 ? parts : [code];
}

export function CodeBlock({ code, className }: CodeBlockProps) {
  return (
    <div className={cn("code-block p-4 text-sm font-mono overflow-x-auto", className)}>
      <pre className="whitespace-pre-wrap break-words">
        <code>{highlightRlmTools(code)}</code>
      </pre>
    </div>
  );
}
