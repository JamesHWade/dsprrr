import { useState, useCallback, useRef, useEffect } from "react";
import type { PlaybackState, PlaybackSpeed } from "@/lib/types";

interface PlaybackControls {
  state: PlaybackState;
  currentIndex: number;
  speed: PlaybackSpeed;
  play: () => void;
  pause: () => void;
  step: () => void;
  stepBack: () => void;
  reset: () => void;
  jumpTo: (index: number) => void;
  setSpeed: (speed: PlaybackSpeed) => void;
}

export function usePlayback(totalIterations: number): PlaybackControls {
  const [state, setState] = useState<PlaybackState>("idle");
  const [currentIndex, setCurrentIndex] = useState(-1);
  const [speed, setSpeed] = useState<PlaybackSpeed>(1);
  const timerRef = useRef<ReturnType<typeof setTimeout> | null>(null);
  const stateRef = useRef(state);
  const indexRef = useRef(currentIndex);

  stateRef.current = state;
  indexRef.current = currentIndex;

  const clearTimer = useCallback(() => {
    if (timerRef.current !== null) {
      clearTimeout(timerRef.current);
      timerRef.current = null;
    }
  }, []);

  // Reset when total iterations changes (e.g., switching runs)
  const prevTotalRef = useRef(totalIterations);
  useEffect(() => {
    if (prevTotalRef.current !== totalIterations) {
      prevTotalRef.current = totalIterations;
      clearTimer();
      setCurrentIndex(-1);
      setState("idle");
    }
  }, [totalIterations, clearTimer]);

  const getDelay = useCallback(() => {
    const base = 3000;
    return base / speed;
  }, [speed]);

  const scheduleNext = useCallback(() => {
    clearTimer();
    timerRef.current = setTimeout(() => {
      if (stateRef.current !== "playing") return;

      const nextIdx = indexRef.current + 1;
      if (nextIdx >= totalIterations) {
        setState("done");
        setCurrentIndex(totalIterations - 1);
        return;
      }

      setCurrentIndex(nextIdx);
      scheduleNext();
    }, getDelay());
  }, [totalIterations, getDelay, clearTimer]);

  const play = useCallback(() => {
    if (totalIterations === 0) return;

    if (state === "done" || currentIndex >= totalIterations - 1) {
      setCurrentIndex(0);
      setState("playing");
    } else if (currentIndex < 0) {
      setCurrentIndex(0);
      setState("playing");
    } else {
      setState("playing");
    }
  }, [state, currentIndex, totalIterations]);

  useEffect(() => {
    if (state === "playing") {
      scheduleNext();
    } else {
      clearTimer();
    }
    return clearTimer;
  }, [state, currentIndex, scheduleNext, clearTimer]);

  const pause = useCallback(() => {
    clearTimer();
    setState("paused");
  }, [clearTimer]);

  const step = useCallback(() => {
    clearTimer();
    const nextIdx = currentIndex + 1;
    if (nextIdx >= totalIterations) {
      setState("done");
      return;
    }
    setCurrentIndex(nextIdx);
    setState("paused");
  }, [currentIndex, totalIterations, clearTimer]);

  const stepBack = useCallback(() => {
    clearTimer();
    const prevIdx = Math.max(0, currentIndex - 1);
    setCurrentIndex(prevIdx);
    setState("paused");
  }, [currentIndex, clearTimer]);

  const reset = useCallback(() => {
    clearTimer();
    setCurrentIndex(-1);
    setState("idle");
  }, [clearTimer]);

  const jumpTo = useCallback(
    (index: number) => {
      clearTimer();
      setCurrentIndex(Math.max(0, Math.min(index, totalIterations - 1)));
      setState("paused");
    },
    [totalIterations, clearTimer],
  );

  return {
    state,
    currentIndex,
    speed,
    play,
    pause,
    step,
    stepBack,
    reset,
    jumpTo,
    setSpeed,
  };
}
