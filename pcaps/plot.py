import sys
import matplotlib.pyplot as plt
from matplotlib.patches import Patch
import pandas as pd
import numpy as np

c1 = "red"
c2 = "skyblue"
c3 = "forestgreen"
c4 = "lightsalmon"
c5 = "dodgerblue"
c6 = "darkseagreen"
c7 = "orchid"

colors = {
    "A" : c1,
    "B" : c2,
    "C" : c3,
    "D" : c4,
    "E" : c5,
    "F" : c6,
    "G" : c7
}

legend_elements_basic = [
    Patch(color=c1, label="A"),
    Patch(color=c2, label="B"),
    Patch(color=c3, label="C"),
]

legend_elements_five = legend_elements_basic + [Patch(color=c5, label="E")]

legend_elements_seven = legend_elements_five + [
    Patch(color=c6, label="F"),
    Patch(color=c7, label="G"),
]


def flesh_out_plot(f, df, name):
    # Setting Y-axis limits, ticks
    f.set_ylim(0, len(df))
    f.axes.yaxis.set_visible(False)
    f.set_xticks(np.arange(df["pushed"].min(), df["popped"].max() + 1, 1))

    # Setting labels and the legend
    # f.set_xlabel('seconds since start')

    for index, row in df.iterrows():
        # Declaring a bar in schedule
        # [(start, stride)] in x-axis
        # (start, stride) in y-axis
        treetime = row["popped"] - row["pushed"]
        color = colors[row["flow"]]
        f.broken_barh([(row["pushed"], treetime)], (index, 1), facecolors=color)
        if "rate_limit" in name:
            f.text(x=row["popped"] + 0.2, 
                   y=index + 0.7, 
                   s=row["length"], 
                   color='black', 
                   fontsize="x-small")
    f.invert_yaxis()


def make_plot(df, subplt, name):
    fig, f1 = subplt.subplots(1, 1)
    fig.set_size_inches(20, 10, forward=True)
    df1 = df.sort_values("pushed")
    df1 = df1.reset_index()
    flesh_out_plot(f1, df1, name)
    subplt.savefig("pcaps/graphs/" + name, bbox_inches="tight")


def plot():
    for i in [
        "fcfs",
        "rr",
        "wfq",
        "strict"
    ]:
        df = pd.read_csv(f"dsl/_build/{i}.csv")
        make_plot(df, plt, i)


def plot_extension():
    for i in ["extension", "extension_ternary"]:
        df = pd.read_csv(f"dsl/_build/output{i}.csv")
        make_plot(df, plt, i)


if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "--ext":
        plot_extension()
    else:
        plot()
