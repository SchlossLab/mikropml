# Running mikRopML with snakemake

1. Install the mikRopML R package: [mikRopML install instructions](https://github.com/SchlossLab/mikRopML#installation)
1. Install snakemake: [snakemake install instructions](https://snakemake.readthedocs.io/en/stable/getting_started/installation.html)
1. Clone or download this repo and go to the example snakemake directory:
    ```
    git clone https://github.com/SchlossLab/mikRopML
    cd mikRopML/examples/snakemake
    ```
1. Do a dry run to make sure the snakemake workflow works:
    ```
    snakemake -n
    ```
1. Run the workflow:

    Run it locally with:
    ```
    snakemake
    ```

    You can specify multiple cores with:
    ```
    snakemake -j 2
    ```

    To run the workflow on an HPC with SLURM:

    1. Edit your email (`YOUR_EMAIL_HERE`) and SLURM account (`YOUR_ACCOUNT_HERE`) in:
        - [`code/submit_slurm.sh`](code/submit_slurm.sh)
        - [`config/cluster.json`](config/cluster.json)
    1. Submit the snakemake workflow with:
        ```
        sbatch code/submit_slurm.sh
        ```
        The main job will then submit other snakemake jobs.


See the [snakemake docs](https://snakemake.readthedocs.io/en/stable) for more details and a helpful snakemake tutorial.