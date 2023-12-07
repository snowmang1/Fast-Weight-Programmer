import matplotlib.pyplot as plt

def results():
    fig, axs = plt.subplots(1, 2, sharey=True, tight_layout=True)

    # We can set the number of bins with the *bins* keyword argument.
    axs[0].hist(bins=1, 4)
    axs[1].hist(bins=1, 3.75)
    plt.title("average hours to game completion")
    plt.show()

results()
