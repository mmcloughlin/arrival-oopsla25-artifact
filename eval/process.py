#!/usr/bin/env python3

import argparse
import sys
import math
import json
import re
import os
import pathlib
import logging
from enum import Enum, auto
from dataclasses import dataclass
from collections import Counter, namedtuple
from tabulate import tabulate
import matplotlib.pyplot as plt


EVAL_RUN_ID = "2025-03-25T15:40:31-t4hz3"
CI_RUN_ID = "2025-03-26T03:44:18-ci"


def self_dir():
    return pathlib.Path(__file__).parent.resolve()


def data_dir():
    return self_dir() / "data"


def eval_data_dir():
    """Path to checkout of https://github.com/mmcloughlin/arrival-data"""
    eval_data_dir = os.getenv("EVAL_DATA_DIR")
    assert eval_data_dir, "EVAL_DATA_DIR must be set"
    return pathlib.Path(eval_data_dir)


def run_report_path(run_id):
    return eval_data_dir() / "run" / run_id / "log" / "report.json"


def read_report(path):
    logging.info(f"reading report: {path}")
    with open(path, "r") as f:
        return json.load(f)


def assert_report_is_usable(report):
    # Release mode
    assert report["build_profile"] == "release"
    # Clean Git commit.
    assert not report["git_version"].endswith("-dirty")


@dataclass
class HexSegment:
    lit: str
    width: int


@dataclass
class SymbolicSegment:
    var: str
    width: int


def parse_encoding_template_segment_value(value):
    if value.startswith("0x"):
        return HexSegment(value[2:], 4 * (len(value) - 2))
    return SymbolicSegment(value, None)


def parse_encoding_template_segment(segment):
    match segment.split(":"):
        case [value, width]:
            segment = parse_encoding_template_segment_value(value)
            segment.width = int(width)
            return segment
        case [value]:
            return parse_encoding_template_segment_value(value)
        case _:
            raise ValueError(f"Invalid segment: {segment}")


class EncodingTemplate:
    def __init__(self, segments):
        self.segments = segments

    def width(self):
        return sum(segment.width for segment in self.segments)

    def encodings_count(self):
        n = 1
        for segment in self.segments:
            match segment:
                case HexSegment(_, width):
                    pass
                case SymbolicSegment(_, width):
                    n *= 2**width
                case _:
                    raise ValueError(f"Unexpected segment: {segment}")
        return n

    @classmethod
    def parse(cls, template):
        segment_strings = template.split("|")
        segments = [
            parse_encoding_template_segment(segment_string)
            for segment_string in segment_strings
        ]
        return cls(segments)


def read_isaspec_encoding_templates():
    # Parse encoding templates.
    encoding_template_strings = []
    with open(data_dir() / "isaspec_generate.txt", "r") as f:
        for line in f:
            if m := re.match(r"^Disassembling '(.+)'$", line.strip()):
                encoding_template_strings.append(m.group(1))

    # There are some limited cases of duplicates (eg. extend to self width)
    encoding_template_strings = list(set(encoding_template_strings))

    # Parse.
    encoding_templates = [
        EncodingTemplate.parse(template) for template in encoding_template_strings
    ]

    # Validate.
    for encoding_template in encoding_templates:
        assert encoding_template.width() == 32

    return encoding_templates


LineCountCategory = namedtuple("LineCountCategory", ["kind", "name"])
LineCount = namedtuple("LineCount", ["lines", "category", "file_name", "line"])


def parse_line_count_category(category):
    parts = category.split("/", 1)
    kind = parts[0]
    name = parts[1] if len(parts) == 2 else None
    return LineCountCategory(kind, name)


def read_line_counts():
    output = []
    with open(data_dir() / "spec_lines.tsv", "r") as f:
        for line in f:
            fields = line.strip().split("\t")
            assert fields[0] == "SPEC_LINES"
            output.append(
                LineCount(
                    int(fields[1]),
                    parse_line_count_category(fields[2]),
                    fields[3],
                    fields[4],
                )
            )
    return output


def read_generated_isle_files():
    with open(data_dir() / "generated_isle_files.txt", "r") as f:
        return [line.strip() for line in f]


def count_isaspec_config_lines():
    with open(data_dir() / "instructions.cloc", "r") as f:
        data = json.load(f)
        return data["data/instructions.rs"]["code"]


def duration_as_seconds(duration):
    return duration["secs"] + duration["nanos"] / 1e9


def all_chained_terms(report):
    chained = set()
    for expansion in report["expansions"]:
        for term in expansion["chained"]:
            chained.add(term)
    return chained


def all_expansion_terms(report):
    terms = set()
    for expansion in report["expansions"]:
        for term in expansion["terms"]:
            terms.add(term)
    return terms


def all_rules(report):
    rules = set()
    for expansion in report["expansions"]:
        for rule in expansion["rules"]:
            rules.add(rule)
    return rules


def expansion_has_timeout(expansion):
    for type_instantiation in expansion["type_instantiations"]:
        verdict = type_instantiation["verify"]["verdict"]
        if verdict == "unknown":
            return True
    return False


ExpansionTimings = namedtuple("ExpansionTimings", ["durations", "timeouts"])


def expansion_timings(expansion):
    durations = []
    timeouts = 0
    for type_instantiation in expansion["type_instantiations"]:
        verdict = type_instantiation["verify"]["verdict"]
        if verdict == "unknown":
            timeouts += 1
        elif verdict == "success":
            durations.append(duration_as_seconds(type_instantiation["duration"]))
        elif verdict == "inapplicable":
            # Note: inapplicable not included in duration list
            pass
        else:
            raise ValueError(f"Unexpected verdict: {verdict}")
    return ExpansionTimings(durations, timeouts)


def clif_instructions(expansion):
    instructions = set()
    for tag in expansion["tags"]:
        PREFIX = "clif_"
        if tag.startswith(PREFIX):
            instructions.add(tag[len(PREFIX) :])
    return instructions


def clif_instruction_category(instruction):
    CLIF_INSTRUCTION_CATEGORY = {
        "iadd": "int",
        "isub": "int",
        "ineg": "int",
        "iabs": "int",
        "imul": "int",
        "smulhi": "int",
        "umulhi": "int",
        "udiv": "int",
        "sdiv": "int",
        "urem": "int",
        "srem": "int",
        "uadd_overflow_trap": "int",
        "iconst": "int",
        "ishl": "int",
        "ushr": "int",
        "sshr": "int",
        "band": "int",
        "bxor": "int",
        "bor": "int",
        "bnot": "int",
        "rotl": "int",
        "rotr": "int",
        "bitselect": "int",
        "cls": "int",
        "clz": "int",
        "ctz": "int",
        "popcnt": "int",
        "ireduce": "int",
        "uextend": "int",
        "sextend": "int",
        "smin": "int",
        "umin": "int",
        "smax": "int",
        "umax": "int",
        "icmp": "int",
        "load": "mem",
        "uload8": "mem",
        "uload16": "mem",
        "uload32": "mem",
        "sload8": "mem",
        "sload16": "mem",
        "sload32": "mem",
        "store": "mem",
        "istore8": "mem",
        "istore16": "mem",
        "istore32": "mem",
        "f32const": "float",
        "f64const": "float",
        "fcmp": "float",
        "fadd": "float",
        "fsub": "float",
        "fmul": "float",
        "fdiv": "float",
        "fmin": "float",
        "fmax": "float",
        "fabs": "float",
        "fneg": "float",
        "sqrt": "float",
        "ceil": "float",
        "floor": "float",
        "trunc": "float",
        "nearest": "float",
        "fcopysign": "float",
        "fcvt_from_uint": "float",
        "fcvt_from_sint": "float",
        "fcvt_to_uint": "float",
        "fcvt_to_sint": "float",
        "fdemote": "float",
        "fpromote": "float",
        "bitcast": "cast",
    }
    return CLIF_INSTRUCTION_CATEGORY[instruction]


def clif_instruction_categories(expansion):
    return {
        clif_instruction_category(instruction)
        for instruction in clif_instructions(expansion)
    }


class SummaryStats(
    namedtuple(
        "SummaryStats",
        [
            "expansions",
            "type_instantiations",
            "success",
            "unknown",
            "inapplicable",
            "cvc5",
            "z3",
        ],
    )
):
    @property
    def applicable(self):
        return self.success + self.unknown


def summary_stats(report, category_filter=None):
    total_expansions = 0
    total_type_instantiations = 0
    type_instantiation_verdict_count = Counter()
    type_instantiation_solver_count = Counter()
    for expansion in report["expansions"]:
        # Filter by category
        categories = clif_instruction_categories(expansion)
        if category_filter and not category_filter(categories):
            continue

        total_expansions += 1
        for type_instantiation in expansion["type_instantiations"]:
            total_type_instantiations += 1
            verdict = type_instantiation["verify"]["verdict"]
            type_instantiation_verdict_count[verdict] += 1
            type_instantiation_solver_count[expansion["solver"]] += 1

    return SummaryStats(
        expansions=total_expansions,
        type_instantiations=total_type_instantiations,
        success=type_instantiation_verdict_count["success"],
        unknown=type_instantiation_verdict_count["unknown"],
        inapplicable=type_instantiation_verdict_count["inapplicable"],
        cvc5=type_instantiation_solver_count.get("cvc5", 0),
        z3=type_instantiation_solver_count.get("z3", 0),
    )


## Category Filters


# everything
def include_all(categories):
    return True


# everything w/o fp and memory
def exclude_float_mem(categories):
    return not ("mem" in categories or "float" in categories)


# anything including mem
def include_mem(categories):
    return "mem" in categories


# anything including fp
def include_float(categories):
    return "float" in categories


def is_expansion_fully_verified(expansion):
    for type_instantiation in expansion["type_instantiations"]:
        verdict = type_instantiation["verify"]["verdict"]
        if verdict == "unknown":
            return False
    return True


def compute_percent_fully_verified_rules(report):
    # Gather expansions by rule.
    rule_expansions = {}
    for expansion in report["expansions"]:
        for rule in expansion["rules"]:
            rule_expansions.setdefault(rule, []).append(expansion)

    # Count fully verified expansions.
    fully_verified_count = 0
    for rule, expansions in rule_expansions.items():
        if all(is_expansion_fully_verified(expansion) for expansion in expansions):
            fully_verified_count += 1
        else:
            logging.debug(f"rule '{rule}' is not fully verified.")

    return 100.0 * fully_verified_count / len(rule_expansions)


Row = namedtuple("Row", ["name", "stats"])


def build_coverage_table(report, *, compact=False):
    ROW_DEFINITIONS = [
        ("Memory", include_mem),
        ("Float", include_float),
        ("Rest" if compact else "Rest (Integer etc.)", exclude_float_mem),
        ("Total", include_all),
    ]
    return list(
        Row(name, summary_stats(report, category_filter))
        for name, category_filter in ROW_DEFINITIONS
    )


def write_coverage_table(rows):
    COLS = [
        ("expansions", "Expansions"),
        ("type_instantiations", "Type Inst."),
        ("success", "Verified"),
        ("unknown", "Timeout"),
        ("inapplicable", "Inapplicable"),
        ("cvc5", "cvc5"),
        ("z3", "Z3"),
    ]
    # Header
    print("\\begin{tabular}{r" + "r" * len(COLS) + "}")
    print("\\toprule")
    print(
        "\\multicolumn{3}{c}{} & \\multicolumn{3}{c}{Result} & \\multicolumn{2}{c}{Solver} \\\\"
    )
    print("\\cmidrule(lr){4-6} \\cmidrule(lr){7-8}")
    headings = ["Category"] + [heading for _, heading in COLS]
    print(" & ".join(headings) + " \\\\")
    print("\\midrule")

    # Rows
    for row in rows:
        stats = row.stats._asdict()
        if row.name == "Total":
            print("\\midrule")
        print(row.name, end="")
        for col, _ in COLS:
            print(f" & {stats[col]:,}", end="")
        print(" \\\\")

    # Footer
    print("\\bottomrule")
    print("\\end{tabular}")


def write_coverage_table_tabulate(rows):
    COLS = [
        ("expansions", "Expansions"),
        ("type_instantiations", "Type Inst."),
        ("success", "Verified"),
        ("unknown", "Timeout"),
        ("inapplicable", "Inapplicable"),
        ("cvc5", "cvc5"),
        ("z3", "Z3"),
    ]

    headers = ["Category"] + [heading for _, heading in COLS]
    table = []

    for row in rows:
        stats = row.stats._asdict()
        data_row = [row.name] + [f"{stats[col]:,}" for col, _ in COLS]
        table.append(data_row)

    # Print the table
    print(tabulate(table, headers=headers, tablefmt="grid"))


def command_coverage(report, opts):
    rows = build_coverage_table(report, compact=opts.ascii)
    if opts.ascii:
        write_coverage_table_tabulate(rows)
    else:
        write_coverage_table(rows)


def write_stat(name, value, unit=""):
    print(f"\\newcommand{{\\stat{name}}}{{{value:,}{unit}\\xspace}}")


def write_stat_percent(name, value):
    write_stat(name, value, "\\%")


def write_stat_lines(name, value):
    write_stat(name, value)
    write_stat(name + "K", round(value / 1000, 1), "K")


StatMacro = namedtuple("StatMacro", ["name", "value", "unit"], defaults=[""])


def clean_macro_name(name):
    REPLACEMENTS = [
        ("_", ""),
        ("0", "zero"),
        ("1", "one"),
        ("2", "two"),
        ("3", "three"),
        ("4", "four"),
        ("5", "five"),
        ("6", "six"),
        ("7", "seven"),
        ("8", "eight"),
        ("9", "nine"),
    ]
    for old, new in REPLACEMENTS:
        name = name.replace(old, new)
    return name


def write_stat_macro(macro):
    write_stat(macro.name, macro.value, unit=macro.unit)


def stat_macro_add_prefix(prefix, macro):
    return StatMacro(prefix + macro.name, macro.value, unit=macro.unit)


def stat_macro_percent(name, pcnt, ndigits=1):
    assert 0 <= pcnt <= 100
    value = round(pcnt, ndigits)
    return StatMacro(name, value, unit="\\%")


def stat_macro_ratio_percent(name, num, denom, ndigits=1):
    assert 0 <= num <= denom
    return stat_macro_percent(name, 100.0 * num / denom, ndigits=ndigits)


def round2(x):
    return round(x, 2)


def ceil_int(x):
    return int(math.ceil(x))


def duration_stats(name, duration, display=round2):
    UNITS = [("seconds", 1), ("minutes", 60), ("hours", 3600)]
    secs = duration_as_seconds(duration)
    stats = []
    for unit, factor in UNITS:
        stats.append(StatMacro(name + unit, display(secs / factor)))
    return stats


def line_count_category_groups(category):
    match category.kind:
        case "isle" | "rule":
            return ["isle"]
        case "spec":
            return ["spec"]
        case "model" | "attr" | "instantiation" | "macro" | "state" | "form":
            return ["support"]
        case _:
            raise ValueError(f"Undefined grouping for line count category: {category}")


def build_line_count_stats():
    line_counts = read_line_counts()
    generated_files = read_generated_isle_files()
    group_counts = Counter()
    for line_count in line_counts:
        if line_count.file_name in generated_files:
            continue
        groups = line_count_category_groups(line_count.category)
        # group_counts[f"kind{line_count.category.kind}"] += line_count.lines
        for group in groups:
            group_counts[f"group{group}"] += line_count.lines
    return group_counts


def write_line_count_stats(line_count_stats):
    for group, lines in line_count_stats.items():
        write_stat_lines(f"handlinecount{group}", lines)


def build_rule_line_count():
    line_counts = read_line_counts()
    rule_line_count = {}
    rules_by_root_term = {}
    for line_count in line_counts:
        category = line_count.category
        if category.kind != "rule":
            continue

        # Collect by root term.
        parts = category.name.split("/")
        root_term = parts[0]
        rules_by_root_term.setdefault(root_term, []).append(line_count)

        # If it has a name, use that.
        if len(parts) == 2:
            rule_name = parts[1]
            rule_line_count[rule_name] = line_count.lines

    for root_term, line_counts in rules_by_root_term.items():
        # If just one rule, it will be named by its root term.
        if len(line_counts) == 1:
            rule_line_count[root_term] = line_counts[0].lines
            continue

        # Otherwise, the name will be the source code location.
        for line_count in line_counts:
            loc = f"{line_count.file_name} line {line_count.line}"
            rule_line_count[loc] = line_count.lines

    return rule_line_count


def compute_all_rules_line_count(report):
    rule_line_count = build_rule_line_count()
    rules = all_rules(report)
    total = 0
    for rule in rules:
        total += rule_line_count[rule]
    return total


CrocusStats = namedtuple(
    "CrocusStats",
    [
        "total_rules",
        "total_type_instantiations",
        "total_specs",
    ],
)


def crocus_stats():
    # Crocus paper: https://doi.org/10.1145/3617232.3624862
    return CrocusStats(
        # Coverage reported in Table 1
        total_rules=98,
        total_type_instantiations=377,
        # Specs reported in section 4.1
        total_specs=136,
    )


def report_stats(report):
    stat_macros = []

    # Configured timout
    stat_macros.extend(duration_stats("timeout", report["timeout"], display=int))

    # Total time
    stat_macros.extend(duration_stats("totalduration", report["duration"]))
    stat_macros.extend(
        duration_stats("totaldurationround", report["duration"], display=ceil_int)
    )

    # Threads
    stat_macros.append(StatMacro("numthreads", report["num_threads"]))

    # Total number of rules.
    rules = all_rules(report)
    total_rules = len(rules)
    stat_macros.append(StatMacro("totalrules", total_rules))

    # Summary stats
    summary = summary_stats(report)
    stat_macros.append(StatMacro("totalexpansions", summary.expansions))
    stat_macros.append(StatMacro("totalinstantiations", summary.type_instantiations))
    stat_macros.append(StatMacro("totalapplicable", summary.applicable))
    stat_macros.append(StatMacro("totalverified", summary.success))
    stat_macros.append(StatMacro("totaltimeout", summary.unknown))
    stat_macros.append(StatMacro("totalcvcfive", summary.cvc5))
    stat_macros.append(StatMacro("totalzthree", summary.z3))

    stat_macros.append(
        stat_macro_ratio_percent(
            "percenttimeout", summary.unknown, summary.type_instantiations
        )
    )

    percent_fully_verified_rules = compute_percent_fully_verified_rules(report)
    stat_macros.append(
        stat_macro_percent("percentfullyverifiedrules", percent_fully_verified_rules)
    )

    # Comparison to prior work
    crocus = crocus_stats()
    # Ratio of rules encountered to total rules.
    stat_macros.append(
        StatMacro("vscrocusrules", round(total_rules / crocus.total_rules, 1))
    )
    # Ratio of expansions to rules.
    stat_macros.append(
        StatMacro(
            "vscrocusexpansionstorules",
            round(summary.expansions / crocus.total_rules, 1),
        )
    )
    # Ratio of instantiations to instantiations.
    stat_macros.append(
        StatMacro(
            "vscrocustypeinstantiations",
            round(summary.type_instantiations / crocus.total_type_instantiations, 1),
        )
    )

    return stat_macros


def command_stats(report, opts):
    stat_macros = []

    # Report stats.
    stat_macros.extend(report_stats(report))

    # Total number of chained terms.
    chained_terms = all_chained_terms(report)
    stat_macros.append(StatMacro("totalchained", len(chained_terms)))

    # Number of encoding templates.
    encoding_templates = read_isaspec_encoding_templates()
    stat_macros.append(StatMacro("totalopcodetemplates", len(encoding_templates)))

    # Write
    for stat_macro in stat_macros:
        write_stat_macro(stat_macro)

    # CI report.
    ci_report = read_report(run_report_path(opts.ci_run_id))
    assert_report_is_usable(ci_report)
    assert ci_report["git_version"] == report["git_version"]
    ci_stat_macros = report_stats(ci_report)
    for stat_macro in ci_stat_macros:
        write_stat_macro(stat_macro_add_prefix("ci", stat_macro))

    # Term spec category counts
    term_stats = build_term_stats(report)
    write_term_category_counts(term_stats.category_counts)
    write_term_class_counts(term_stats.class_counts)
    write_term_tag_counts(term_stats.tag_counts)
    write_term_category_summary(term_stats.category_counts)
    write_term_category_lines(term_stats.category_lines)
    write_term_spec_kind_lines(term_stats.spec_kind_lines)

    # ISA spec config lines
    isaspec_config_lines = count_isaspec_config_lines()
    write_stat_lines("isaspecconfiglines", isaspec_config_lines)
    isa_generated = TermCategory(TermKind.ISA, SpecKind.AUTO)
    isaspec_config_ratio = round(
        sum(term_stats.category_lines[isa_generated]) / isaspec_config_lines, 1
    )
    write_stat("isaspecconfigratio", isaspec_config_ratio)

    # Rule line counts
    rules_line_count = compute_all_rules_line_count(report)
    write_stat_lines("totalruleslines", rules_line_count)

    # Line count stats
    line_count_stats = build_line_count_stats()
    write_line_count_stats(line_count_stats)

    # Crocus stats
    crocus = crocus_stats()
    write_stat("crocustotalrules", crocus.total_rules)
    write_stat("crocustotaltypeinstantiations", crocus.total_type_instantiations)
    write_stat("specstotalcrocuspaper", crocus.total_specs)


def command_timings(report, opts):
    timeout_duration = duration_as_seconds(report["timeout"])

    # Collect type instantiaton durations
    durations = []
    timeouts = 0
    for expansion in report["expansions"]:
        timings = expansion_timings(expansion)
        durations.extend(timings.durations)
        timeouts += timings.timeouts

    # Compute CDF
    durations.sort()
    n = len(durations) + timeouts
    x = []
    y = []
    for i, duration in enumerate(durations):
        p = float(i + 1) / float(n)
        x.append(duration)
        y.append(p)

    # Style
    tick_font_size = 10
    label_font_size = tick_font_size + 1
    axis_color = "#777"

    # Plot
    fig, ax = plt.subplots(figsize=(4, 2), constrained_layout=True)
    # plt.subplots_adjust(top=0, bottom=0.22)
    ax.step(x, y, where="post")
    ax.set_xlabel("Verification times (seconds)", fontsize=label_font_size)
    ax.set_ylabel("CDF", fontsize=label_font_size)
    ax.tick_params(axis="both", which="major", labelsize=tick_font_size)
    ax.grid(linestyle="dotted")
    ax.set_xlim(0, min(8, timeout_duration))
    ax.set_ylim(0, 1)

    # Colors
    ax.spines[["bottom", "top", "left", "right"]].set_color(axis_color)
    [tick.set_markeredgecolor(axis_color) for tick in ax.xaxis.get_ticklines()]
    [tick.set_markeredgecolor(axis_color) for tick in ax.yaxis.get_ticklines()]

    fig.savefig(opts.output_path)


def command_timeouts(report, opts):
    timeout_count = Counter()
    for expansion in report["expansions"]:
        if expansion_has_timeout(expansion):
            timeout_count[expansion["description"]] += 1

    for description, count in timeout_count.items():
        print(f"{count}\t{description}")


def command_slow(report, opts):
    SLOW_THRESHOLD = 5
    for expansion in report["expansions"]:
        timings = expansion_timings(expansion)
        if timings.timeouts > 0:
            continue
        if len(timings.durations) == 0:
            continue
        slowest = max(timings.durations)
        if slowest >= SLOW_THRESHOLD:
            print(f"{slowest:.2f}\t{expansion['description']}")


class TermKind(Enum):
    ISA = auto()
    INTERNAL = auto()
    CLIF_IR = auto()
    EXTERNAL = auto()


def term_kind(term_meta):
    # ISA terms
    if term_meta["name"].startswith("MInst."):
        return TermKind.ISA

    # CLIF IR
    if any(tag.startswith("clif_") for tag in term_meta["tags"]):
        return TermKind.CLIF_IR

    # Derive from classification.
    CLASS_KIND = {
        "external": TermKind.EXTERNAL,
        "extractor": TermKind.INTERNAL,
        "constructor": TermKind.INTERNAL,
        "enum_variant": TermKind.INTERNAL,
    }
    return CLASS_KIND[term_meta["class"]]


class SpecKind(Enum):
    AUTO = auto()
    HAND = auto()


def term_spec_kind(term_meta):
    # ISA generated.
    if "isaspec_generated" in term_meta["tags"]:
        return SpecKind.AUTO
    # Internal derived specs (for ISLE types)
    if "internal_derived_spec" in term_meta["tags"]:
        return SpecKind.AUTO
    # Otherwise assume hand-written.
    return SpecKind.HAND


TermCategory = namedtuple("TermCategory", ["term_kind", "spec_kind"])


def term_category(term_meta):
    return TermCategory(term_kind(term_meta), term_spec_kind(term_meta))


def term_category_macro_name(category):
    name = category.term_kind.name + category.spec_kind.name
    return name.replace("_", "").lower()


def chained_term_category():
    return TermCategory(term_kind=TermKind.INTERNAL, spec_kind=SpecKind.AUTO)


def build_term_spec_line_counts():
    line_counts = read_line_counts()
    output = {}
    for line_count in line_counts:
        category = line_count.category
        if category.kind != "spec":
            continue
        assert category.name is not None, f"Missing spec term name: {line_count}"
        output[category.name] = line_count.lines
    return output


TermStats = namedtuple(
    "TermStats",
    [
        "category_counts",
        "category_lines",
        "spec_kind_lines",
        "class_counts",
        "tag_counts",
    ],
)


def build_term_stats(report):
    # Gather all terms present in the expansions.
    terms = all_expansion_terms(report)

    # Collect spec line counts
    term_spec_line_counts = build_term_spec_line_counts()

    # Also gather chained terms.
    chained_terms = all_chained_terms(report)

    category_counts = Counter()
    category_counts[chained_term_category()] = len(chained_terms)
    category_lines = Counter()
    spec_kind_lines = Counter()
    class_counts = Counter()
    tag_counts = Counter()
    for term_meta in report["terms"]:
        name = term_meta["name"]
        # Skip terms not present in any expansion.
        if name not in terms:
            continue
        # Every term in an expansion should have a spec.
        assert term_meta["has_spec"]
        # Confirm no overlap with chained.
        assert name not in chained_terms
        # Count
        category = term_category(term_meta)
        category_counts[category] += 1
        term_spec_line_count = term_spec_line_counts.get(name, 0)
        category_lines.setdefault(category, []).append(term_spec_line_count)
        spec_kind_lines[category.spec_kind] += term_spec_line_count
        # Class count
        class_counts[term_meta["class"]] += 1
        # Tag count
        for tag in term_meta["tags"]:
            tag_counts[tag] += 1

    return TermStats(
        category_counts, category_lines, spec_kind_lines, class_counts, tag_counts
    )


def table_cell_value(value, *, unit="", placeholder="-"):
    return f"{value:,}{unit}" if value > 0 else placeholder


def write_term_category_table(category_counts, category_lines):
    # Header
    print("\\newcounter{categoryrow}")
    print("\\setcounter{categoryrow}{0}")
    print("\\begin{tabular}{clrrrrrrrrr}")
    print("\\toprule")
    print(" &", end="")
    print(" & \multicolumn{3}{c}{Hand-Written}", end="")
    print(" & \multicolumn{4}{c}{Automated}", end="")
    print(" \\\\")
    print("\\cmidrule(lr){3-5} \\cmidrule(lr){6-9}")
    print("\\# & Category", end="")
    print(" & \multicolumn{2}{c}{Terms} & Lines", end="")
    print(" & \multicolumn{2}{c}{Terms} & Cases & Lines", end="")
    print(" \\\\")
    print("\\midrule")

    # Rows
    automatable = True
    for term_kind in TermKind:
        if term_kind == TermKind.CLIF_IR:
            print("\\midrule")
            automatable = False

        by_hand = TermCategory(term_kind, SpecKind.HAND)
        by_auto = TermCategory(term_kind, SpecKind.AUTO)

        total_count = category_counts[by_hand] + category_counts[by_auto]
        hand_percent_value = int(100.0 * category_counts[by_hand] / total_count)

        label = "\\termcategory" + term_kind.name.replace("_", "").lower()
        hand_count = table_cell_value(category_counts[by_hand])
        hand_percent = table_cell_value(hand_percent_value, unit="\%")
        hand_lines = table_cell_value(sum(category_lines.get(by_hand, [])))
        placeholder = "-" if automatable else ""
        auto_count = table_cell_value(category_counts[by_auto], placeholder=placeholder)
        auto_percent = table_cell_value(
            100 - hand_percent_value, unit="\%", placeholder=placeholder
        )
        cases = "\\stattotalopcodetemplates" if term_kind == TermKind.ISA else ""
        auto_lines = table_cell_value(
            sum(category_lines.get(by_auto, [])), placeholder=placeholder
        )

        print(f"\stepcounter{{categoryrow}}(\\arabic{{categoryrow}}) & {label}", end="")
        print(f" & {hand_count} & {hand_percent} & {hand_lines}", end="")
        print(f" & {auto_count} & {auto_percent} & {cases} & {auto_lines}", end="")
        print(" \\\\")

    # Footer
    print("\\bottomrule")
    print("\\end{tabular}")


def write_term_category_counts(category_counts):
    for category, count in category_counts.items():
        name = term_category_macro_name(category)
        write_stat(f"termkindcount{name}", count)


def write_term_class_counts(class_counts):
    for term_class, count in class_counts.items():
        name = clean_macro_name(term_class)
        write_stat(f"termclasscount{name}", count)


def write_term_tag_counts(tag_counts):
    for tag, count in tag_counts.items():
        name = clean_macro_name(tag)
        write_stat(f"termtagcount{name}", count)


def write_term_category_summary(category_counts):
    # Totals
    total_auto = 0
    total_hand = 0
    total_automatable = 0
    total = 0
    for category, count in category_counts.items():
        total += count
        if category.spec_kind == SpecKind.AUTO:
            total_auto += count
        elif category.spec_kind == SpecKind.HAND:
            total_hand += count
        else:
            raise ValueError(f"Unexpected spec kind: {category.spec_kind}")
        if category.term_kind in (TermKind.ISA, TermKind.INTERNAL):
            total_automatable += count

    write_stat("totalspecs", total)

    write_stat("totalspecsauto", total_auto)
    percent_auto = int(100.0 * total_auto / total)
    write_stat_percent("percentspecsauto", int(percent_auto))

    write_stat("totalspecshand", total_hand)
    percent_hand = 100 - percent_auto
    write_stat_percent("propspecshand", percent_hand)

    write_stat("totalspecsautomatable", total_automatable)
    percent_auto_of_automatable = int(100.0 * total_auto / total_automatable)
    write_stat_percent("percentspecsautoofautomatable", percent_auto_of_automatable)

    # Comparison to prior work.
    # Ratio of total to hand-written, since Crocus would require all hand-written.
    write_stat("specstotalhandratio", round(total / total_hand, 1))
    # How many more than Crocus
    crocus = crocus_stats()
    assert total_hand > crocus.total_specs
    write_stat("specstotalmorethancrocus", total_hand - crocus.total_specs)

    # By kind.
    for term_kind in TermKind:
        by_hand = TermCategory(term_kind, SpecKind.HAND)
        by_auto = TermCategory(term_kind, SpecKind.AUTO)
        total_count = category_counts[by_hand] + category_counts[by_auto]
        hand_percent = int(100.0 * category_counts[by_hand] / total_count)
        auto_percent = 100 - hand_percent
        name = clean_macro_name(term_kind.name).lower()
        write_stat_percent(f"percentspecs{name}hand", hand_percent)
        write_stat_percent(f"percentspecs{name}auto", auto_percent)


def write_term_category_lines(category_lines):
    for category, lines in category_lines.items():
        name = term_category_macro_name(category)
        write_stat_lines(f"totalspeclines{name}", sum(lines))


def write_term_spec_kind_lines(spec_kind_lines):
    for spec_kind, lines in spec_kind_lines.items():
        name = spec_kind.name.lower()
        write_stat_lines(f"totalspeckindlines{name}", lines)


def command_specs(report, opts):
    term_stats = build_term_stats(report)
    write_term_category_table(term_stats.category_counts, term_stats.category_lines)


def command_terms(report, opts):
    # Gather all terms present in the expansions.
    terms = all_expansion_terms(report)

    # Collect spec line counts
    term_spec_line_counts = build_term_spec_line_counts()

    for term_meta in report["terms"]:
        name = term_meta["name"]
        # Skip terms not present in any expansion.
        if name not in terms:
            continue
        assert term_meta["has_spec"]
        # Print
        category = term_category(term_meta)
        term_kind = category.term_kind.name.lower()
        spec_kind = category.spec_kind.name.lower()
        lines = term_spec_line_counts.get(name, 0)
        print(f"{name}\t{term_kind}\t{spec_kind}\t{term_meta['class']}\t{lines}")


def command_lint(report, opts):
    # Baseline checks.
    assert_report_is_usable(report)

    # Any expansions with no instantiations?
    total_instantiations = Counter()
    for expansion in report["expansions"]:
        n = len(expansion["type_instantiations"])
        if n == 0:
            description = expansion["description"]
            print(f"warn: expansion '{description}' has no instantiations")
        total_instantiations[expansion["description"]] += n

    for description, n in total_instantiations.items():
        if n == 0:
            print(f"warn: rule '{description}' has no instantiations")


def main():
    # Options.
    parser = argparse.ArgumentParser(description='Process evaluation data', fromfile_prefix_chars='@', allow_abbrev=False)
    parser.add_argument('--run-id', help="id for full verification run", default=EVAL_RUN_ID)
    parser.add_argument('--ci-run-id', help="id for ci verification run", default=CI_RUN_ID)
    parser.add_argument('--log-level', default="info")
    parser.add_argument('--ascii', action='store_true', help="output table in ASCII format instead of LaTeX")

    subparsers = parser.add_subparsers(required=True)

    parser_coverage = subparsers.add_parser("coverage")
    parser_coverage.set_defaults(command=command_coverage)

    parser_timings = subparsers.add_parser("timings")
    parser_timings.add_argument("output_path", help="output file for timings plot")
    parser_timings.set_defaults(command=command_timings)

    parser_timeouts = subparsers.add_parser("timeouts")
    parser_timeouts.set_defaults(command=command_timeouts)

    parser_slow = subparsers.add_parser("slow")
    parser_slow.set_defaults(command=command_slow)

    parser_specs = subparsers.add_parser("specs")
    parser_specs.set_defaults(command=command_specs)

    parser_stats = subparsers.add_parser("stats")
    parser_stats.set_defaults(command=command_stats)

    parser_terms = subparsers.add_parser("terms")
    parser_terms.set_defaults(command=command_terms)

    parser_lint = subparsers.add_parser("lint")
    parser_lint.set_defaults(command=command_lint)

    # Parse
    opts = parser.parse_args()
    logging.basicConfig(level=opts.log_level.upper())

    # Load
    report_path = run_report_path(opts.run_id)
    report = read_report(report_path)

    # Run command
    opts.command(report, opts)


if __name__ == "__main__":
    main()
