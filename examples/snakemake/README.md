# Running mikRopML with snakemake

1. Create a conda environment and activate it:
    ```sh
    conda env create -f config/environment.yml
    conda activate smk-ML
    ```
    (Recommend [miniconda](https://docs.conda.io/en/latest/miniconda.html) if you don't already have conda installed.)

    Alternatively, you can [install snakemake](https://snakemake.readthedocs.io/en/stable/getting_started/installation.html) and the other dependencies listed in [`config/environment.yml`](config/environment.yml) however you like.
1. Install the mikRopML R package: [mikRopML install instructions](https://github.com/SchlossLab/mikRopML#installation)

    e.g.
    ``` sh
    R -e 'devtools::install_github("SchlossLab/mikRopML")'
    ```
1. Clone or download this repo and go to the example snakemake directory:
    ``` sh
    git clone https://github.com/SchlossLab/mikRopML
    cd mikRopML/examples/snakemake
    ```
1. Do a dry run to make sure the snakemake workflow works:
    ``` sh
    snakemake -n
    ```
1. Run the workflow:

    Run it locally with:
    ``` sh
    snakemake
    ```

    You can specify multiple cores with:
    ``` sh
    snakemake -j 2
    ```

    To run the workflow on an HPC with SLURM:

    1. Edit your email (`YOUR_EMAIL_HERE`) and SLURM account (`YOUR_ACCOUNT_HERE`) in:
        - [`code/submit_slurm.sh`](code/submit_slurm.sh)
        - [`config/cluster.json`](config/cluster.json)
    1. Submit the snakemake workflow with:
        ``` µsh
        sbatch code/submit_slurm.sh
        ```
        The main job will then submit other snakemake jobs.


See the [snakemake docs](https://snakemake.readthedocs.io/en/stable) for more details and a helpful snakemake tutorial.