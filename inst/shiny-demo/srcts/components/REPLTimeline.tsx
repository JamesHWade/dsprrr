import { useRef, useEffect } from "react";
import type { Iteration } from "@/lib/types";
import { IterationCard } from "./IterationCard";

interface REPLTimelineProps {
  iterations: Iteration[];
  currentIndex: number;
  question: string;
}

export function REPLTimeline({
  iterations,
  currentIndex,
  question,
}: REPLTimelineProps) {
  const bottomRef = useRef<HTMLDivElement>(null);
  const prevIndexRef = useRef(-1);

  useEffect(() => {
    if (currentIndex > prevIndexRef.current && bottomRef.current) {
      bottomRef.current.scrollIntoView({ behavior: "smooth", block: "end" });
    }
    prevIndexRef.current = currentIndex;
  }, [currentIndex]);

  const visibleIterations = iterations.slice(0, currentIndex + 1);

  return (
    <div className="space-y-6">
      {/* Question card */}
      <div className="rounded-xl border-2 border-primary/20 bg-primary/5 p-5">
        <div className="text-xs font-medium text-primary uppercase tracking-wider mb-2">
          Question
        </div>
        <p className="text-sm leading-relaxed">{question}</p>
      </div>

      {/* Timeline */}
      <div className="relative">
        {/* Vertical timeline line */}
        <div className="absolute left-[18px] top-0 bottom-0 w-px bg-border" />

        <div className="space-y-6">
          {visibleIterations.map((iter, idx) => (
            <div key={iter.iteration} className="relative pl-12">
              {/* Timeline dot */}
              <div className="absolute left-[12px] top-6 w-3 h-3 rounded-full border-2 border-background bg-primary z-10" />
              <IterationCard
                iteration={iter}
                isNew={idx === currentIndex}
              />
            </div>
          ))}
        </div>

        <div ref={bottomRef} />
      </div>

      {/* "Done" marker */}
      {currentIndex >= iterations.length - 1 && iterations.length > 0 && (
        <div className="text-center py-4">
          <span className="inline-flex items-center gap-2 rounded-full bg-green-100 dark:bg-green-900/30 px-4 py-2 text-sm font-medium text-green-800 dark:text-green-400">
            <svg className="w-4 h-4" fill="none" viewBox="0 0 24 24" strokeWidth={2} stroke="currentColor">
              <path strokeLinecap="round" strokeLinejoin="round" d="M9 12.75 11.25 15 15 9.75M21 12a9 9 0 1 1-18 0 9 9 0 0 1 18 0Z" />
            </svg>
            Run complete - {iterations.length} iterations
          </span>
        </div>
      )}
    </div>
  );
}
