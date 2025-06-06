# Executable Environment for OSF Project [bkuwa](https://osf.io/bkuwa/)

This repository was automatically generated as part of a project to test the reproducibility of open science projects hosted on the Open Science Framework (OSF).

**Project Title:** Exploring the Effect of Social Distance on Medium Preferences in Interpersonal Communication: A Replication of Amit, Wakslak and Trope (2013, Experiment 2)

**Project Description:**
> Amit et al. (2013) concluded that social distance can influence communication preferences: People prefer communicating with closer others using pictures (which are more concrete) and more distant others using words (which are more abstract). We conducted a high-powered (N = 988) preregistered replication of Amit et al. (2013, Experiment 2) and extended the design by manipulating the presence of a potential confound we detected when examining the original instructions. The original effect successfully replicated using the original instructions but did not replicate after the removal of the confound. Moreover, we demonstrate that the effect obtained with the original instructions likely relies on a different mechanism (comfort with sending personal pictures to close and distant contacts) than that posited in the original study (preference for concrete and abstract communication). These results cast doubt on the original interpretation and highlight the importance of transparent reporting standards in research.

**Original OSF Page:** [https://osf.io/bkuwa/](https://osf.io/bkuwa/)

---

**Important Note:** The contents of the `bkuwa_src` folder were cloned from the OSF project on **12-03-2025**. Any changes made to the original OSF project after this date will not be reflected in this repository.

The `DESCRIPTION` file was automatically added to make this project Binder-ready. For more information on how R-based OSF projects are containerized, please refer to the `osf-to-binder` GitHub repository: [https://github.com/Code-Inspect/osf-to-binder](https://github.com/Code-Inspect/osf-to-binder)

## flowR Integration

This version of the repository has the **[flowR Addin](https://github.com/flowr-analysis/rstudio-addin-flowr)** preinstalled. flowR allows visual design and execution of data analysis workflows within RStudio, supporting better reproducibility and modular analysis pipelines.

To use flowR, open the project in RStudio and go to `Addins` > `flowR`.

## How to Launch:

**Launch in your Browser:**

🚀 **MyBinder:** [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/code-inspect-binder/osf_bkuwa-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment in your web browser.
   * Please note that Binder may take a few minutes to build the environment.

🚀 **NFDI JupyterHub:** [![NFDI](https://nfdi-jupyter.de/images/nfdi_badge.svg)](https://hub.nfdi-jupyter.de/r2d/gh/code-inspect-binder/osf_bkuwa-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment on the NFDI JupyterHub platform.

**Access Downloaded Data:**
The downloaded data from the OSF project is located in the `bkuwa_src` folder.

## Run via Docker for Long-Term Reproducibility

In addition to launching this project using Binder or NFDI JupyterHub, you can reproduce the environment locally using Docker. This is especially useful for long-term access, offline use, or high-performance computing environments.

### Pull the Docker Image

```bash
docker pull meet261/repo2docker-bkuwa-f:latest
```

### Launch RStudio Server

Run the container (with a name, e.g. `rstudio-dev`):
```bash
docker run -it --name rstudio-dev --platform linux/amd64 -p 8888:8787 --user root meet261/repo2docker-bkuwa-f bash
```

Inside the container, start RStudio Server with no authentication:
```bash
/usr/lib/rstudio-server/bin/rserver --www-port 8787 --auth-none=1
```

Then, open your browser and go to: [http://localhost:8888](http://localhost:8888)

> **Note:** If you're running the container on a remote server (e.g., via SSH), replace `localhost` with your server's IP address.
> For example: `http://<your-server-ip>:8888`

## Looking for the Base Version?

For the original Binder-ready repository **without flowR**, visit:
[osf_bkuwa](https://github.com/code-inspect-binder/osf_bkuwa)

