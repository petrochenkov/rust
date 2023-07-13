import argparse
import re
import copy
import typing
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
import collections
from ordered_enum import OrderedEnum
from pathlib import Path

kNumRegex = r"[0-9]+"
kWordRegex = r"[a-zA-Z]+"
kStatsRegex = r"dstats. " \
    fr"(stmts: ({kWordRegex}), )*" + \
    fr"(args_match: ({kWordRegex}), )" + \
    fr"(ret_match: ({kWordRegex}), )" + \
    fr"(self_arg: ({kWordRegex}), )" + \
    fr"(same_name: ({kWordRegex}), )*" + \
    fr"(has_expr_after: ({kWordRegex}))"

kMethodsRegex = fr"warning: delegation methods stats. methods_count: ({kNumRegex}), ({kNumRegex})."

kPathRegex = r"[a-zA-z\/.:0-9]+"
kPrimapySpanRegex = r"--> ({})".format(kPathRegex)

class StmtsCount(OrderedEnum):
    ZeroWithTail = "ZeroWithTail"
    OneWithoutTail = "OneWithoutTail"
    OneWithTail = "OneWithTail"
    Other = "Other"

    def descr():
        return "stmt count"

class ArgsMatch(OrderedEnum):
    Same = "Same"
    SameUpToSelfType = "SameUpToSelfType"
    SameNumber = "SameNumber"
    Different = "Different"

    def descr():
        return "arguments match"

class RetMatch(OrderedEnum):
    Same = "Same"
    SameUpToSelfType = "SameUpToSelfType"
    Different = "Different"

    def descr():
        return "ret match"

class HasSelf(OrderedEnum):
    Value = "Value"
    Type = "Type"
    Other = "Other"

    def descr():
        return "self argument"

class VisibilityCmp(OrderedEnum):
    TRUE = "true"
    FALSE = "false"

    def descr():
        return "visibility cmp"

class SameName(OrderedEnum):
    TRUE = "true"
    FALSE = "false"

    def descr():
        return "same name"

class HasExprAfter(OrderedEnum):
    FALSE = "false"
    TRUE = "true"

    def descr():
        return "has expr after call"

class ParentIsImpl(OrderedEnum):
    TRUE = "true"
    FALSE = "false"

    def descr():
        return "parent is impl"

KReplaceMap = {
    StmtsCount.descr(): {StmtsCount.ZeroWithTail: 1, StmtsCount.OneWithoutTail: 2, StmtsCount.OneWithTail: 3, StmtsCount.Other: 4},
    ArgsMatch.descr(): {ArgsMatch.Same: 1, ArgsMatch.SameUpToSelfType: 2, ArgsMatch.Different: 3, ArgsMatch.SameNumber: 4},
    RetMatch.descr(): {RetMatch.Same: 1, RetMatch.SameUpToSelfType: 2, RetMatch.Different: 3},
    HasSelf.descr(): {HasSelf.Value: 1, HasSelf.Type: 2, HasSelf.Other: 3},
    # VisibilityCmp.descr(): {VisibilityCmp.TRUE: 1, VisibilityCmp.FALSE: 2},
    SameName.descr(): {SameName.TRUE: 1, SameName.FALSE: 2},
    HasExprAfter.descr(): {HasExprAfter.TRUE: 1, HasExprAfter.FALSE: 2},
    # sParentIsImpl.descr(): {ParentIsImpl.TRUE: 1, ParentIsImpl.FALSE: 2},
}

class Stats:
    def __init__(self, streamFilename: Path, compressed: bool):
        self.stream = streamFilename.open()
        self.compressed = compressed

    def getStatsList(self):
        if not self.compressed:
            return [StmtsCount, ArgsMatch, RetMatch, HasSelf, SameName, HasExprAfter]
        else:
            return [ArgsMatch, RetMatch, HasSelf, HasExprAfter]

    def getHistOrder(self):
        if not self.compressed:
            return [SameName.descr(), StmtsCount.descr(), HasExprAfter.descr(),
                HasSelf.descr(), RetMatch.descr(), ArgsMatch.descr()]
        else:
            return [HasExprAfter.descr(), HasSelf.descr(), ArgsMatch.descr(), RetMatch.descr()]

    def getEmptyDfMap(self):
        res = {}
        for stat in self.getStatsList():
            res[stat.descr()] = []

        return res

    def parse(self):
        lines = self.stream.readlines()

        data = self.getEmptyDfMap()
        index = [] # spans

        methodsData = {
            "methods delegation count": [],
            "impl count": []
        }

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

        self.df = pd.DataFrame(data = data, index = index)
        self.mdf = pd.DataFrame(data = methodsData)
        return

    def dumpSingleHist(self, folderName, name):
        plt.figure(figsize=(16,9))
        histFile = open(f"{folderName}/{name}.pdf", "wb+")
        new_df = self.df[name]
        hist = new_df.value_counts()
        hist.plot(kind='bar')
        plt.savefig(histFile, format="pdf")
        return

    def dumpHeatMap(self, resultFolder):
        heatMapFile = open(f"{resultFolder}/heatmap.pdf", "wb+")
        df = self.df.replace(KReplaceMap).astype(int)
        f, ax = plt.subplots(figsize=(16, 9))
        corr = df.corr()
        sns.heatmap(round(corr,2), annot=True, ax=ax, cmap="coolwarm",fmt='.2f', \
            linewidths=.05)
        plt.savefig(heatMapFile, format="pdf")

    def dumpHist(self, resultFolder):
        order = self.getHistOrder()

        hist = self.df.reindex(columns=order)
        hist = hist.value_counts()
        hist = pd.DataFrame(hist)

        def comparator(lhs, rhs):
            lhs = lhs.split(" ", 1)[0]
            rhs = rhs.split(" ", 1)[0]

            match (lhs, rhs)

        hist = hist.sort_values(by=order)
        histFile = open(f"{resultFolder}/hist.html", "w+")
        hist.to_html(histFile)

    def dumpMethodsStat(self, resultFolder):
        methodsFile = open(f"{resultFolder}/methods.html", "w+")
        self.mdf.sort_values(by=['impl count'], ascending = False).to_html(methodsFile)

    def dumpResults(self):
        resultFolder = "stats"

        dataframeFile = open(f"{resultFolder}/dataframe.html", "w+")
        self.df.to_html(dataframeFile)
        for stat in self.getStatsList():
            self.dumpSingleHist(resultFolder, stat.descr())

        self.dumpHeatMap(resultFolder)
        self.dumpHist(resultFolder)
        self.dumpMethodsStat(resultFolder)
        return

def main():
    parser = argparse.ArgumentParser(description="Cosimulation")
    parser.add_argument(
        "--logs", type=Path, required=True, help="Path to compiler logs"
    )
    parser.add_argument(
        "--compressed", action=argparse.BooleanOptionalAction
    )
    args = parser.parse_args()

    stats = Stats(args.logs, args.compressed)
    stats.parse()
    stats.dumpResults()

if __name__ == "__main__":
    main()
