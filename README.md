# P3 Template
Welcome to this a template repository! In the top right corner you can 'Use this template'

P3 stands for:
- Project
- Process
- Presentation


## Project
A project as in a complete collection of code for a specific purpose. In both R and Python a collection of code is called a project. In Python characterised with a pyproject.toml file and in R with a .Rproj file. The goal of this template is to go one step further. Not only some generic project settings are included, but more best practices regarding package selection, package management and folder strucure are taken into account. In addition, this is done within the project, but in the background. This enables beginners to use these tools without complete understanding, while more expert users can adapt everything.

Concrete elements are:
- Folder structure following standard package structure, thereby enabling automatated checks and documentation
- Default folders for synthetic data and metadata
- Config.yaml for adapting the defaults 
- A set-up script that runs everything needed
- Renv for external package management, without the need to initialize or update renv manually
- Filled and commented .Rprofile, .Rbuildignore and .gitignore
- A Quarto how-to that illustrates the content of the project (see Process below)

## Process
The current structure is opinionated. It works particularly well when combined with a certain process. Within the research community and (early) data science there was a preference for tools like R Markdown (in R) and Jupyter Notebooks (in Python). Both tools enable the combination of natural language (text) and programming language (code). This enables so-called 'literate programming'. As defined by Knuth. 

Quarto is a new tool that facilitates both R and Python code blocks (among others). In addition, it enables diverse forms of presentation, like pdf, powerpoint, sites and even books (see Presentation below).

A user can simple load the quarto (qmd) file and
1. run a code block
2. inspect it's output
3. either modify the code (and run again) or run the next code block

Within the code blocks there is not 'raw' R code, but there are functions run that can be found in the R folder. Experienced users can also adapt these functions. Run load_all() and run the code block again. Putthing functions with detailed R code within the R-folder enables the qmd file to become very clean. 



Focuses on workflow
Pipeline becomes a tool rather than a concept
Emphasizes "how" you work


## Presentation
The visuals that are produced with the quarto file are also saved for later usage.

More specific than "Product"
Directly references outputs (reports/viz)
Quarto fits naturally here
Clear deliverable focus
