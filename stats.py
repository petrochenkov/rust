import argparse
import pandas as pd
import re
from collections import defaultdict
from ordered_enum import OrderedEnum
from pathlib import Path

kStatsRegex = (
    rf"caller_parent: (\w+), "
    + rf"stmts_before: (\w+), "
    + rf"arg0_match: (\w+), "
    + rf"arg0_preproc: (\w+), "
    + rf"args_match: (\w+), "
    + rf"args_preproc: (\w+), "
    + rf"ret_match: (\w+), "
    + rf"has_self: (\w+), "
    + rf"caller_has_self: (\w+), "
    + rf"same_name: (\w+), "
    + rf"ret_postproc: (\w+), "
    + rf"source: (\w+)"
)
kParentStatRegex = rf"delegation count: (\d+), parent count: (\d+)"
kLocationRegex = rf"--> (.+)$"


class CallerParent(OrderedEnum):
    InherentImpl = "InherentImpl"
    TraitImpl = "TraitImpl"
    Trait = "Trait"
    Other = "Other"

    def descr():
        return "caller parent"


class StmtsBefore(OrderedEnum):
    TRUE = "true"
    FALSE = "false"

    def descr():
        return "stmts before"


class Arg0Match(OrderedEnum):
    Same = "Same"
    SameUpToSelfType = "SameUpToSelfType"
    Coerced = "Coerced"
    Different = "Different"

    def descr():
        return "arg0 match"


class Arg0Preproc(OrderedEnum):
    No = "No"
    Field = "Field"
    Getter = "Getter"
    Other = "Other"

    def descr():
        return "arg0 preproc"


class ArgsMatch(OrderedEnum):
    Same = "Same"
    SameUpToSelfType = "SameUpToSelfType"
    Coerced = "Coerced"
    SameCount = "SameCount"
    DifferentCount = "DifferentCount"

    def descr():
        return "args match"


class ArgsPreproc(OrderedEnum):
    No = "No"
    Field = "Field"
    Getter = "Getter"
    Other = "Other"

    def descr():
        return "args preproc"


class RetMatch(OrderedEnum):
    Same = "Same"
    SameUpToSelfType = "SameUpToSelfType"
    Coerced = "Coerced"
    Different = "Different"

    def descr():
        return "ret match"


class HasSelf(OrderedEnum):
    TRUE = "true"
    FALSE = "false"

    def descr():
        return "has self"


class CallerHasSelf(OrderedEnum):
    TRUE = "true"
    FALSE = "false"

    def descr():
        return "caller has self"


class SameName(OrderedEnum):
    TRUE = "true"
    FALSE = "false"

    def descr():
        return "same name"


class RetPostproc(OrderedEnum):
    FALSE = "false"
    TRUE = "true"

    def descr():
        return "ret postproc"


class Source(OrderedEnum):
    User = "User"
    Lang = "Lang"
    Bang = "Bang"
    Attr = "Attr"
    Derive = "Derive"

    def descr():
        return "source"


def is_delegation_impl(
    caller_parent,
    stmts_before,
    arg0_preproc,
    arg0_match,
    args_match,
    args_preproc,
    ret_match,
    has_self,
    caller_has_self,
    ret_postproc,
    source,
):
    sig_known = caller_parent == CallerParent.TraitImpl
    arg0_sig_known = caller_has_self == CallerHasSelf.TRUE

    ret_ok = (
        (ret_match == RetMatch.Same and ret_postproc == RetPostproc.FALSE)
        or (ret_match == RetMatch.SameUpToSelfType)
        or (
            ret_match == RetMatch.Coerced
            and ret_postproc == RetPostproc.FALSE
            and sig_known
        )
    )
    stmts_before_ok = (
        stmts_before == StmtsBefore.FALSE
        or arg0_preproc == Arg0Preproc.Other
        or args_preproc == Arg0Preproc.Other
    )
    arg0_ok = (
        (arg0_match == Arg0Match.Same)
        or (arg0_match == Arg0Match.SameUpToSelfType)
        or arg0_sig_known
    )
    args_ok = (
        (args_match == ArgsMatch.Same and args_preproc == ArgsPreproc.No)
        or (args_match == ArgsMatch.SameUpToSelfType)
        or (
            args_match == ArgsMatch.Coerced
            and args_preproc == ArgsPreproc.No
            and sig_known
        )
    )
    has_self_ok = (
        (has_self == HasSelf.TRUE and caller_has_self == CallerHasSelf.TRUE)
        or (has_self == HasSelf.FALSE and caller_has_self == CallerHasSelf.FALSE)
        or (caller_parent == CallerParent.Other)
    )
    source_ok = source != Source.Lang and source != Source.Derive

    return (
        ret_ok and stmts_before_ok and arg0_ok and args_ok and has_self_ok and source_ok
    )


def is_delegation(item):
    return is_delegation_impl(
        item["caller parent"],
        item["stmts before"],
        item["arg0 preproc"],
        item["arg0 match"],
        item["args match"],
        item["args preproc"],
        item["ret match"],
        item["has self"],
        item["caller has self"],
        item["ret postproc"],
        item["source"],
    )


class Stats:
    def __init__(self, streamFilename: Path):
        self.stream = streamFilename.open(encoding="utf-8")

    def getStatClasses(self):
        return [
            CallerParent,
            StmtsBefore,
            Arg0Match,
            Arg0Preproc,
            ArgsMatch,
            ArgsPreproc,
            RetMatch,
            HasSelf,
            CallerHasSelf,
            SameName,
            RetPostproc,
            Source,
        ]

    def getColumnOrder(self):
        return [stat_class.descr() for stat_class in self.getStatClasses()]

    def parse(self):
        data = defaultdict(list)
        delegation_count_descr = "delegation count"
        parent_count_descr = "parent count"
        parentData = {delegation_count_descr: [], parent_count_descr: []}

        stats_parsed = False
        for line in self.stream.readlines():
            if stats_match := re.search(kStatsRegex, line):
                for i, stat_class in enumerate(self.getStatClasses()):
                    stat = stat_class(f"{stats_match.group(i + 1)}")
                    data[stat_class.descr()].append(stat)

                stats_parsed = True
                continue

            if stats_parsed:
                if location_match := re.search(kLocationRegex, line):
                    location = location_match.group(1)
                else:
                    location = "<unknown>"
                data["location"].append(location)
                stats_parsed = False
                continue

            if parent_stat_match := re.search(kParentStatRegex, line):
                delegation_count = int(parent_stat_match.group(1))
                parent_count = int(parent_stat_match.group(2))
                parentData[delegation_count_descr].append(delegation_count)
                parentData[parent_count_descr].append(parent_count)

        self.df = pd.DataFrame(data)
        self.pdf = pd.DataFrame(parentData)

    def dumpCounts(self, resultFolder):
        counts = pd.DataFrame(self.df.value_counts())
        countsFile = open(f"{resultFolder}/counts-all.html", "w")
        counts.to_html(countsFile)

        for descr in self.getColumnOrder():
            counts = pd.DataFrame(self.df[descr].value_counts())
            countsFile = open(f"{resultFolder}/counts-{descr}.html", "w")
            counts.to_html(countsFile)

    def dumpParentCounts(self, resultFolder):
        parent_counts = pd.DataFrame(
            self.pdf.groupby("delegation count")["parent count"].sum()
        )
        parent_counts = parent_counts.sort_values("parent count", ascending=False)
        parentCountsFile = open(f"{resultFolder}/parent-counts.html", "w")
        parent_counts.to_html(parentCountsFile)

    def dumpResults(self, resultFolder):
        filtered_rows = []
        for _, row in self.df.iterrows():
            if is_delegation(row):
                filtered_rows.append(row)
        self.df = pd.DataFrame(filtered_rows)
        self.df = self.df.reset_index(drop=True)

        dataframeFile = open(f"{resultFolder}/dataframe.html", "w")
        self.df.to_html(dataframeFile)
        dataframeFileParent = open(f"{resultFolder}/parent-dataframe.html", "w")
        self.pdf.to_html(dataframeFileParent)

        self.df = self.df.drop("location", axis=1)

        self.dumpCounts(resultFolder)
        self.dumpParentCounts(resultFolder)


def main():
    parser = argparse.ArgumentParser(description="Delegation")
    parser.add_argument(
        "--logs", type=Path, required=True, help="Path to compiler logs"
    )
    args = parser.parse_args()

    resultFolder = "stats"
    df_path = Path(f"{resultFolder}/df.pkl")
    pdf_path = Path(f"{resultFolder}/pdf.pkl")

    stats = Stats(args.logs)
    if df_path.exists():
        stats.df = pd.read_pickle(df_path)
        stats.pdf = pd.read_pickle(pdf_path)
    else:
        stats.parse()
        stats.df.to_pickle(df_path)
        stats.pdf.to_pickle(pdf_path)

    stats.dumpResults(resultFolder)


if __name__ == "__main__":
    main()
