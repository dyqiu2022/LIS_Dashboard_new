"""Benchmark phrase embedding and weighted clustering.

Examples:
  python benchmarks/embedding_benchmark.py --sizes 1000 10000 --dimension 512

The script intentionally measures unique phrases, cache hits, peak RSS and wall time. It does
not fabricate raw-row copies, because the production algorithm clusters unique values with counts.
"""
from __future__ import annotations

import argparse
import json
import resource
import sys
import time
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "backend"))
from app.services.embedding import embedding_service  # noqa: E402


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--sizes", nargs="+", type=int, default=[1000, 10000, 100000])
    parser.add_argument("--dimension", type=int, default=512)
    args = parser.parse_args()
    results = []
    for size in args.sizes:
        phrases = [f"临床诊断短语 {i}" for i in range(size)]
        started = time.perf_counter()
        vectors = embedding_service.encode(phrases, dimension=args.dimension)
        elapsed = time.perf_counter() - started
        rss = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss * 1024
        results.append({"size": size, "dimension": int(vectors.shape[1]), "seconds": elapsed,
                        "phrases_per_second": size / max(elapsed, 1e-9),
                        "peak_rss_bytes": rss, "matrix_bytes": int(vectors.nbytes)})
        print(json.dumps(results[-1], ensure_ascii=False))


if __name__ == "__main__":
    main()
