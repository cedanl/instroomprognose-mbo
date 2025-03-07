
## Ease-of-use

Back in the old days of 2022, data science and programming was hard work, but no longer!

1. Open instroomprognose-prototype.qmd, read what happens before inspecting and running a code block
2. Change some code a little (texts, colors, etc). Run the code again to see the effect.
3. Manually changing code is hard work. select a code-block you want to change and hit Control-Shift-G or Command-Shift-G.
4. Explain in a sentence or two what you want, hit ENTER and wait.
5. Tell people about the amazing things that just happened on your screen.

Quarto, with it's combination of text and code, is an example of 'literate programming'. It is very easy to follow what happens because the intentions are readable in natural language.

Since code is also text, LLMs can help excellent with this. Especially if all the intentions are present in the file.

## Collaboration

Working with LLMs is nice, but working with people even more so! However, when working with privacy-sensitive data, this can be a challenge..

1. What setting do you need to adapt to run this code with real data instead of synthetic (hint: look at the README.md).
2. If you change this code will the instroomprognose.qmd still run? (hint: look at config.yml and think about it and maybe look some more).
3. If you have no experience with git, ask someone that does. Otherwise, inspect .gitignore and look there for 'data'-references.

A setup file, a config file and gitignore help to ensure reproducability across users and environments. The use of synthetic data is literally privacy-by-default. This makes it easy to share working code with others and develop together with external collaborators.

## Structure

Back in the old days of 2023, ChatGPT could help with small code changes, but a codebase was still difficult to maintain. 

1. This quarto file with all these code blocks it has getting large. Copy a code block.
2. Open 'data_processing.R' in the R folder and paste the earlier code block
3. Hit Control-Shift-G and ask it to make a proper function of it.
4. run ```check()``` in the console to validate the function technically.
5. If check fails, talk with someone about it. How to solve this?
6. To use this function with the .qmd we need to load it. Run ```load_all()```
7. Now go back to the original code-block. Adapt it to run the function correctly and ensure the output is either printed or assinged to the same object.

All the code is very modular and constantly validated. This ensures there is no technical debt.  
# TODO testing with pal

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
