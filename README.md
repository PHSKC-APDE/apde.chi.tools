# APDE CHI Tools

## Purpose

apde.chi.tools is an R library that provides easy access and centralized versioning of convenience tools for the CHI/CHNA update process.

The are meant to support our epidemiologist in various steps along the pipeline, including:

+ planning
+ batch processing
+ validation
+ upload

## Installation

To install the latest version: [`apde.chi.tools`](https://github.com/PHSKC-APDE/apde.chi.tools) ... `remotes::install_github("PHSKC-APDE/apde.chi.tools", auth_token = NULL)`

To install a particular version, append the version tag to the repository name. You can see a list of recent tags on the right of this page, and click [`Releases`](https://github.com/PHSKC-APDE/apde.chi.tools/releases) to view details ... `remotes::install_github("PHSKC-APDE/apde.chi.tools@v2025.0.0", auth_token = NULL)`. Note, you will want to use the lateset version for the year of work of the CHI project (so work performed in 2025 should use the 2025 version)

To install a particular branch, (for example, if participating in testing or needing a feature still in development) specify it with the 'ref' argument, e.g., `remotes::install_github("PHSKC-APDE/apde.chi.tools", ref = "dev", auth_token = NULL)`

## Loading a package

Load [`apde.chi.tools`](https://github.com/PHSKC-APDE/apde.chi.tools) ... `library(apde.chi.tools)`


## New for version 2025.0.0 (and 2025.0.1)

Our first full release!

Versioning scheme, expect a release each year with number for that year 

CHI functions in the CHI repository disabled. Use apde.chi.tools going forward 

If something doesn’t work or appears broken, let Danny or Ronald know, and submit an [issue](https://github.com/PHSKC-APDE/apde.chi.tools/issues).

Review documentation using '?[function_name]()' in R studio, as well as the [wiki](https://github.com/PHSKC-APDE/apde.chi.tools/wiki).

Version 2025.0.1 includes minor patches and will require that you have RADS version 1.3.4 or later installed.

## Best Practices

If you have code that uses these functions from last year, you will want to confirm that the function contained in this package conforms to the expectations of the previous version. You can review the manual for any function by typing '?[function_name]()'. There you will see the expected parameters and examples of usage. If these are insufficient, please reach out!

## Problems?

-   If you come across a bug or have specific suggestions for improvement, please click on ["Issues"](https://github.com/PHSKC-APDE/apde.chi.tools/issues) at the top of this page and then click ["New Issue"](https://github.com/PHSKC-APDE/apde.chi.tools/issues/new/choose) and provide the necessary details.
