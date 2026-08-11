from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Iterable

import polars as pl

from app.schemas import KeywordDefinition


_SPLIT_RE = re.compile(r"[；，,;|()（）]")


def split_terms(value: str | None) -> list[str]:
    if not value:
        return []
    return [x.strip() for x in _SPLIT_RE.split(str(value)) if x.strip()]


def parse_and(definition: KeywordDefinition) -> list[list[str]]:
    groups = [split_terms(definition.and1), split_terms(definition.and2), split_terms(definition.and3)]
    return [group for group in groups if group]


def matches_text(value: object, definition: KeywordDefinition) -> bool:
    text = "" if value is None else str(value)
    segments = [part for part in _SPLIT_RE.split(text) if part]
    if not segments:
        return False
    conditions = parse_and(definition)
    not_words = split_terms(definition.not_words)
    not_limit = split_terms(definition.not_limit)
    for segment in segments:
        if not all("ALL" in group or any(term in segment for term in group) for group in conditions):
            continue
        if any(word in segment for word in not_limit):
            return True
        if any(word in segment for word in not_words):
            continue
        return True
    return False


def definition_sentence(definition: KeywordDefinition) -> str:
    conditions = parse_and(definition)
    if any("ALL" in group for group in conditions):
        text = "：全部数据"
    else:
        text = "".join(
            ("：" if index == 0 else ", 且") + "包含(" + "或".join(f'“{x}”' for x in group) + ")"
            for index, group in enumerate(conditions)
        )
    not_words = split_terms(definition.not_words)
    if not_words:
        text += ", 不包含(" + "或".join(f'“{x}”' for x in not_words)
        if split_terms(definition.not_limit):
            text += ", 除非包含" + "或".join(f'“{x}”' for x in split_terms(definition.not_limit))
        text += ")"
    if definition.exclude_groups:
        text += ", 与(" + "、".join(definition.exclude_groups) + ")互斥"
    return definition.class_name + text


def assign_keyword_classes(values: Iterable[object], definitions: list[KeywordDefinition]) -> list[str]:
    assignments: list[str] = []
    for value in values:
        matched = [d.class_name for d in definitions if matches_text(value, d)]
        assignments.append("|".join(matched) if matched else "未知")
    return assignments


def grouping_counts(frame: pl.DataFrame, grouping_col: str, category_col: str = "类别_关键词") -> pl.DataFrame:
    if grouping_col not in frame.columns:
        raise ValueError(f"不存在分组列: {grouping_col}")
    if category_col not in frame.columns:
        frame = frame.with_columns(pl.lit("").alias(category_col))
    return (
        frame.group_by([grouping_col, category_col])
        .len(name="临床诊断数量")
        .with_columns(pl.col(category_col).cast(pl.Utf8).fill_null(""))
        .sort(["临床诊断数量"], descending=True)
    )
