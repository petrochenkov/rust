import argparse
import re
import pandas as pd
from ordered_enum import OrderedEnum
from pathlib import Path

kNumRegex = r"[0-9]+"
kWordRegex = r"[a-zA-Z]+"
kStatsRegex = (
    r"dstats. "
    rf"(caller_parent: ({kWordRegex}), )*"
    + rf"(stmts: ({kWordRegex}), )*"
    + rf"(arg0_match: ({kWordRegex}), )"
    + rf"(arg0_preproc: ({kWordRegex}), )"
    + rf"(args_match: ({kWordRegex}), )"
    + rf"(args_preproc: ({kWordRegex}), )"
    + rf"(ret_match: ({kWordRegex}), )"
    + rf"(has_self: ({kWordRegex}), )"
    + rf"(caller_has_self: ({kWordRegex}), )"
    + rf"(same_name: ({kWordRegex}), )*"
    + rf"(ret_postproc: ({kWordRegex}))"
)

kMethodsRegex = (
    rf"warning: delegation methods stats. methods_count: ({kNumRegex}), ({kNumRegex})."
)

kPathRegex = r"[a-zA-z\/.:0-9]+"
kPrimapySpanRegex = r"--> ({})".format(kPathRegex)


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
    def __init__(self, streamFilename: Path, compressed: bool):
        self.stream = streamFilename.open(encoding="utf-8")
        self.compressed = compressed

    def getStatsList(self):
        if not self.compressed:
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
        else:
            return [ArgsMatch, RetMatch, RetPostproc]

    def getHistOrder(self):
        if not self.compressed:
            return [
                CallerParent.descr(),
                StmtsCount.descr(),
                Arg0Match.descr(),
                Arg0Preproc.descr(),
                ArgsMatch.descr(),
                ArgsPreproc.descr(),
                RetMatch.descr(),
                HasSelf.descr(),
                CallerHasSelf.descr(),
                SameName.descr(),
                RetPostproc.descr(),
            ]
        else:
            return [RetPostproc.descr(), ArgsMatch.descr(), RetMatch.descr()]

    def getEmptyDfMap(self):
        res = {}
        for stat in self.getStatsList():
            res[stat.descr()] = []

        return res

    def parse(self):
        lines = self.stream.readlines()

        data = self.getEmptyDfMap()
        index = []  # spans

        methodsData = {"methods delegation count": [], "impl count": []}

        access = False
        for line in lines:
            match = re.search(kStatsRegex, line)
            if match != None:
                i = 2
                for stat in self.getStatsList():
                    while match.group(i) == None:
                        i += 2

                    s = f"{match.group(i)}"
                    elem = stat(s)
                    data[stat.descr()].append(elem)
                    i += 2

                access = True
                continue

            match = re.search(kPrimapySpanRegex, line)
            if match != None and access:
                index.append(match.group(1).strip())
                access = False
                continue

            match = re.search(kMethodsRegex, line)
            if match != None:
                key_descr = "methods delegation count"
                val_descr = "impl count"
                key = int(match.group(1))
                val = int(match.group(2))
                methods_count = key
                found = False
                for i in range(0, len(methodsData[key_descr])):
                    if methodsData[key_descr][i] == methods_count:
                        methodsData[val_descr][i] += val
                        found = True
                        break

                if not found:
                    methodsData[key_descr].append(key)
                    methodsData[val_descr].append(val)

        self.df = pd.DataFrame(data=data, index=index)
        self.mdf = pd.DataFrame(data=methodsData)

    def dumpHist(self, resultFolder):
        order = self.getHistOrder()

        hist = self.df.reindex(columns=order)
        hist = hist.value_counts()
        hist = pd.DataFrame(hist)

        hist = hist.sort_values(by=order)
        histFile = open(f"{resultFolder}/hist.html", "w+")
        hist.to_html(histFile)

    def dumpMethodsStat(self, resultFolder):
        methodsFile = open(f"{resultFolder}/methods.html", "w+")
        self.mdf.sort_values(by=["impl count"], ascending=False).to_html(methodsFile)

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
    parser.add_argument("--compressed", action=argparse.BooleanOptionalAction)
    args = parser.parse_args()

    stats = Stats(args.logs, args.compressed)
    stats.parse()
    stats.dumpResults()


if __name__ == "__main__":
    main()
