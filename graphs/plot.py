import os
import matplotlib.pyplot as plt
import matplotlib.patches as patches
import pandas as pd


colors = {
    "A" : "red",
    "B" : "skyblue",
    "C" : "forestgreen",
    "D" : "lightsalmon",
    "E" : "dodgerblue",
    "F" : "darkseagreen",
    "G" : "orchid"
}


script_dir = os.path.dirname(os.path.realpath(__file__))


def make_plot(file, name):
    df = pd.read_csv(file)
    df.sort_values("popped", inplace=True)
    df.reset_index(inplace=True)

    fig, ax = plt.subplots(1, 1)
    fig.set_size_inches(20, 10, forward=True)
    ax.set_ylim(0, len(df))
    ax.axes.yaxis.set_visible(False)

    used_flows = []
    for index, row in df.iterrows():
        treetime = row["popped"] - row["pushed"]
        flow = row["flow"]
        color = colors[flow]
        if flow not in used_flows:
            used_flows.append(flow)
        ax.broken_barh([(row["pushed"], treetime)], (index, 1), facecolor=color)
    
    used_flows.sort()
    handles = []
    for flow in used_flows:
        patch = patches.Patch(color=colors[flow], label=flow)
        handles.append(patch)
    ax.invert_yaxis()
    ax.legend(handles=handles)

    plt.savefig(name, bbox_inches="tight")


if __name__ == "__main__":
    for file in os.listdir(script_dir):
        if ".csv" in file:
            name = os.path.splitext(os.path.basename(file))[0]
            png = os.path.join(script_dir, name)
            file = os.path.join(script_dir, file)
            make_plot(file, png)
