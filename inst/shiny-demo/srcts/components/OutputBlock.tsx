import { useState } from "react";
import { cn } from "@/lib/utils";

interface OutputBlockProps {
  output: string;
  success: boolean;
  className?: string;
}

export function OutputBlock({ output, success, className }: OutputBlockProps) {
  const [expanded, setExpanded] = useState(false);
  const isLong = output.length > 400;
  const displayOutput = isLong && !expanded ? output.slice(0, 400) + "..." : output;

  return (
    <div className={cn("rounded-lg border text-sm", className)}>
      {/* Status badge */}
      <div className="flex items-center gap-2 px-3 py-1.5 border-b bg-muted/50">
        <span
          className={cn(
            "inline-flex items-center rounded-full px-2 py-0.5 text-xs font-medium",
            success
              ? "bg-green-100 text-green-800 dark:bg-green-900/30 dark:text-green-400"
              : "bg-red-100 text-red-800 dark:bg-red-900/30 dark:text-red-400",
          )}
        >
          {success ? "OK" : "Error"}
        </span>
        <span className="text-xs text-muted-foreground">Output</span>
      </div>

      {/* Output content */}
      <pre className="p-3 text-xs font-mono whitespace-pre-wrap break-words text-muted-foreground overflow-x-auto max-h-64 overflow-y-auto">
        {displayOutput}
      </pre>

      {isLong && (
        <button
          onClick={() => setExpanded(!expanded)}
          className="w-full px-3 py-1 text-xs text-muted-foreground hover:text-foreground border-t bg-muted/30 transition-colors"
        >
          {expanded ? "Show less" : "Show more"}
        </button>
      )}
    </div>
  );
}
