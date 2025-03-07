<p align="center"><img src="src/assets/p3_logo.png" alt="Instroomanalyse MBO"></p> <h1 align="center">Instroomanalyse CAMBO</h1> <div align="center"> <strong>🚀  🛠️</strong> <br> Een aanpasbare analyse en demo repository ondersteund door LLMs. <br> <sub>Gebaseerd op het R P3 Template, ideaal voor data-analisten, onderzoekers en developers op zoek naar gestandaardiseerde structuur</sub> 

</div>


## 📋 Table of Contents

- [Motivation](#-motivation)
- [What P3 Stans For](#-what-p3-stands-for)
- [Quick Start](#-quick-start)
- [Features](#-features)
- [Project Structure](#-project-structure)
- [Contributing](#-contributing)
- [License](#-license)
- [Additional Resources](#-additional-resources)


## 💡 Motivation

The P3 Template addresses common challenges in R project start-up, organization and reproducibility. It provides a standardized structure that:
- Enables secure and high-performance LLM-usage from the IDE.
- Improves project maintainability
- Streamlines the transition from explorative analysis to final presentation

### What P3 Stands For:

- Project: A complete, well-organized codebase with good practices incorporated
- Process: A way-of-work and guide to help you being more productive
- Presentation: Professional, adaptable and beautiful outputs

## 🚀 Quick Start

1. Ensure you have R (>= 4.0.0) and RStudio installed.

2. Click "Code" in the top right corner
3. Clone this repository with git or git clients like Github Desktop or Smartgit, or download as zip.
4. Open the project in RStudio and run the setup script by clicking Enter:
```
Setup script detected. Run 00_setup.R? (press ENTER to run, ESC to skip): 
```
5. Depending on your local R packages, this may take a few minutes.
6. Run the exploratory analysis by opening instroomprognose_prototype.qmd and click Render
7. Investigate the report in html or pdf

### Troubleshooting:
- If you encounter package installation issues, try updating R and RStudio to their latest versions.
- For renv-related problems, refer to the [renv documentation](https://rstudio.github.io/renv/articles/renv.html).

## 📁 Project Structure

🚧 The P3 Template follows a well-organized directory structure to enhance productivity and maintainability. Here's an overview of the key directories: 

```
project-root/
├── data/
│   └── synthetic/                   # Synthetic data for testing and examples
│   └── reference/       
│   └── metadata/      
├── man/                             # Auto-generated documentation
├── R/                               # R functions and scripts
└── utils/               
    ├── pal_prompts/     
├── instroomprognose_prototype.qmd   # The analysis file that demonstrates built-in functionality
├── 00_set_up.R                      # Ensuring all basic steps are done and project is ready-for-analysis
├── config.yml                       # Configuration settings, like which data folder to use (by default synthetic)
├── CLAUDE.md                        # Instructions that help LLMs to support you appropriately, can be configured

```

## 🤝 Contributing

- Thanks to [Npuls](https://npuls.nl/) for providing the opportunity to develop this package
- Thank you to CEDA-colleagues for stimulating conversations and feedback Bram, Tomer, Amir, Tony, Theo, Ash, Steven, Caspar, Shirley and Martine
- Thanks to SURF Developer Program for sparking interest in templates


Contribute as well! Please see our Contributing Guide for details. 

Key ways to contribute:

- Report bugs or suggest features by opening an issue
- Submit pull requests for bug fixes or new features
- Improve documentation or add usage examples


## 📄 License

This project is licensed under the [Apache License](LICENSE.md).

## 📚 Additional Resources

- See P3 Template
- Stuff regarding CAMBO

