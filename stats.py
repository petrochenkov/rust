import argparse
import pandas as pd
import re
from collections import defaultdict
from ordered_enum import OrderedEnum
from pathlib import Path

kNumRegex = r"[0-9]+"
kWordRegex = r"[a-zA-Z]+"
kStatsRegex = (
    r"dstats. "
    + rf"caller_parent: ({kWordRegex}), "
    + rf"stmts: ({kWordRegex}), "
    + rf"arg0_match: ({kWordRegex}), "
    + rf"arg0_preproc: ({kWordRegex}), "
    + rf"args_match: ({kWordRegex}), "
    + rf"args_preproc: ({kWordRegex}), "
    + rf"ret_match: ({kWordRegex}), "
    + rf"has_self: ({kWordRegex}), "
    + rf"caller_has_self: ({kWordRegex}), "
    + rf"same_name: ({kWordRegex}), "
    + rf"ret_postproc: ({kWordRegex})"
)

kMethodsRegex = (
    rf"warning: delegation methods stats. methods_count: ({kNumRegex}), ({kNumRegex})."
)

kPathRegex = r"[a-zA-z\/.:0-9]+"
kPrimapySpanRegex = rf"--> ({kPathRegex})"


class CallerParent(OrderedEnum):
    InherentImpl = "InherentImpl"
    TraitImpl = "TraitImpl"
    Trait = "Trait"
    Other = "Other"

    def descr():
        return "caller parent"


class StmtsCount(OrderedEnum):
    ZeroWithTail = "ZeroWithTail"
    OneWithoutTail = "OneWithoutTail"
    OneWithTail = "OneWithTail"
    Other = "Other"

    def descr():
        return "stmt count"


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


class Stats:
    def __init__(self, streamFilename: Path):
        self.stream = streamFilename.open(encoding="utf-8")

    def getStatsList(self):
        return [
            CallerParent,
            StmtsCount,
            Arg0Match,
            Arg0Preproc,
            ArgsMatch,
            ArgsPreproc,
            RetMatch,
            HasSelf,
            CallerHasSelf,
            SameName,
            RetPostproc,
        ]

    def getHistOrder(self):
        return [stat_class.descr() for stat_class in self.getStatsList()]

    def parse(self):
        data = defaultdict(list)
        locations = []
        delegation_count_descr = "delegation count"
        parent_count_descr = "parent count"
        methodsData = {delegation_count_descr: [], parent_count_descr: []}

        stats_parsed = False
        for line in self.stream.readlines():
            if stats_match := re.search(kStatsRegex, line):
                for i, stat_class in enumerate(self.getStatsList()):
                    stat = stat_class(f"{stats_match.group(i + 1)}")
                    data[stat_class.descr()].append(stat)

                stats_parsed = True
                continue

            if stats_parsed and (location_match := re.search(kPrimapySpanRegex, line)):
                locations.append(location_match.group(1))
                stats_parsed = False
                continue

            if methods_match := re.search(kMethodsRegex, line):
                delegation_count = int(methods_match.group(1))
                parent_count = int(methods_match.group(2))
                try:
                    index = methodsData[delegation_count_descr].index(delegation_count)
                    methodsData[parent_count_descr][index] += parent_count
                except:
                    methodsData[delegation_count_descr].append(delegation_count)
                    methodsData[parent_count_descr].append(parent_count)

        self.df = pd.DataFrame(data=data, index=locations)
        self.mdf = pd.DataFrame(data=methodsData)

    def dumpHist(self, resultFolder):
        hist = pd.DataFrame(self.df.value_counts())
        hist = hist.sort_values(by=self.getHistOrder())

        histFile = open(f"{resultFolder}/hist.html", "w+")
        hist.to_html(histFile)

    def dumpMethodsStat(self, resultFolder):
        methodsFile = open(f"{resultFolder}/methods.html", "w+")
        self.mdf.sort_values(by=["parent count"], ascending=False).to_html(methodsFile)

    def dumpResults(self):
        resultFolder = "stats"
        dataframeFile = open(f"{resultFolder}/dataframe.html", "w+")
        self.df.to_html(dataframeFile)
        self.dumpHist(resultFolder)
        self.dumpMethodsStat(resultFolder)


def main():
    parser = argparse.ArgumentParser(description="Delegation")
    parser.add_argument(
        "--logs", type=Path, required=True, help="Path to compiler logs"
    )
    args = parser.parse_args()

    stats = Stats(args.logs)
    stats.parse()
    stats.dumpResults()


if __name__ == "__main__":
    main()
