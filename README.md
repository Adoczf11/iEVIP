# iEVIP

This repository contains the analysis code associated with the study:

**iEVIP: A Dual-inspired Intelligent Platform for Smart Response Extracellular Vesicle Isolation and Single-Vesicle Profiling**

The code is organized into two main modules: extracellular vesicle isolation-related analysis and single-vesicle profiling-related analysis. The scripts are provided to support reproducibility of the computational analyses presented in the manuscript.

## Repository structure

/code
├── code_isolation
│   └── Scripts for extracellular vesicle isolation-related analysis
│
└── code_profiling
    └── Scripts for single-vesicle profiling, feature analysis, and classification


In the Code Ocean capsule, the recommended directory structure is:

/code      Source code and analysis scripts
/data      Input data or example data
/results   Output files generated after running the code


## Code description

### code_isolation

This folder contains scripts used for the analysis related to extracellular vesicle isolation, including data processing and result generation associated with the isolation module of the iEVIP platform.

### code_profiling

This folder contains scripts used for single-vesicle profiling analysis, including feature extraction, statistical analysis, visualization, and machine-learning-based classification where applicable.

## Running the code in Code Ocean

The code should be placed under the `/code` directory of the Code Ocean capsule.

If some original clinical or raw imaging data cannot be publicly shared because of privacy, ethical, or file-size restrictions, representative example data or processed feature tables are provided where applicable to demonstrate the analysis workflow.

## Output files

The scripts generate analysis outputs such as processed tables, statistical results, classification results, and figures. These files should be written to the `/results` directory.


## Software environment

The code is intended to be run in the Code Ocean computational environment associated with this capsule. Required software packages and dependencies are specified in the capsule environment.

For local use, users should install the required packages according to the programming language and package information provided in each script or environment file.

## Notes on reproducibility

This repository is intended to reproduce the computational analyses associated with the manuscript. Due to privacy restrictions, some clinical raw data may not be publicly released. Where applicable, processed or representative data are provided to enable verification of the analysis workflow.

## License

Please refer to the license file or the corresponding manuscript information for terms of use.

## Contact

For questions about the code or analysis workflow, please contact the corresponding authors of the manuscript.
