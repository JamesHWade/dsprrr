interface ConceptCalloutProps {
  title: string;
  body: string;
}

export function ConceptCallout({ title, body }: ConceptCalloutProps) {
  return (
    <div className="callout-enter rounded-lg border border-primary/20 bg-primary/5 p-4 ml-8">
      <div className="flex items-start gap-3">
        <span className="mt-0.5 flex-shrink-0 w-5 h-5 rounded-full bg-primary/15 flex items-center justify-center">
          <svg
            className="w-3 h-3 text-primary"
            fill="none"
            viewBox="0 0 24 24"
            strokeWidth={2.5}
            stroke="currentColor"
          >
            <path
              strokeLinecap="round"
              strokeLinejoin="round"
              d="M12 18v-5.25m0 0a6.01 6.01 0 0 0 1.5-.189m-1.5.189a6.01 6.01 0 0 1-1.5-.189m3.75 7.478a12.06 12.06 0 0 1-4.5 0m3.75 2.383a14.406 14.406 0 0 1-3 0M14.25 18v-.192c0-.983.658-1.823 1.508-2.316a7.5 7.5 0 1 0-7.517 0c.85.493 1.509 1.333 1.509 2.316V18"
            />
          </svg>
        </span>
        <div className="space-y-1">
          <div className="text-sm font-medium text-primary">{title}</div>
          <p className="text-sm text-muted-foreground leading-relaxed">{body}</p>
        </div>
      </div>
    </div>
  );
}
