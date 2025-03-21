# Course Website Template <a href='https://your-course-url.com'><img src='files/icon-512.png' align="right" height="139" /></a>

[Course Number • Term Year](https://your-course-url.com)  
[Instructor Name](https://instructor-website.com) • Department Name • Institution Name

---

**[Quarto](https://quarto.org/) + [{targets}](https://docs.ropensci.org/targets/) + [{renv}](https://rstudio.github.io/renv/) + [{xaringan}](https://github.com/yihui/xaringan) = magic! 🪄**

---

## How to use this template

1. Clone or download this repository
2. Modify the configuration files:
   - `_quarto.yml`: Main site configuration
   - `_variables.yml`: Course-specific variables
   - Update content in `content/`, `assignment/`, `resource/`, etc.
3. Add your own content and customize as needed

## How to build the site

1. Install [RStudio](https://www.rstudio.com/products/rstudio/download/#download) version 2022.07.1 or later since it has a [Quarto](https://quarto.org/) installation embedded in it. Otherwise, download and install [Quarto](https://quarto.org/) separately.
2. Open the `.Rproj` file to open an [RStudio Project](https://r4ds.had.co.nz/workflow-projects.html).
3. If it's not installed already, R *should* try to install the [{renv} package](https://rstudio.github.io/renv/) when you open the RStudio Project for the first time. If you don't see a message about package installation, install it yourself by running `install.packages("renv")` in the R console.
4. Run `renv::restore()` in the R console to install all the required packages for this project.
5. Run `targets::tar_make()` in the R console to build everything.
6. 🎉 All done! 🎉 The complete website will be in a folder named `_site/`.

## {targets} pipeline

This template uses the [{targets} package](https://docs.ropensci.org/targets/) to build the site and all its supporting files. The complete pipeline is defined in [`_targets.R`](_targets.R) and can be run in the R console with:

```r
targets::tar_make()
```

The pipeline does several major tasks:

- **Create supporting data files**: Functions in [`R/tar_data.R`](R/tar_data.R) save and/or generate datasets used in the course.

- **Compress project folders**: Compresses all folders in [`/projects/`](/projects/) so students can download self-contained RStudio Projects as `.zip` files.

- **Render xaringan slides**: Converts [{xaringan}](https://github.com/yihui/xaringan) slides to HTML and PDF format.

- **Build Quarto website**: Compiles and stitches together all the `.qmd` files based on the settings in [`_quarto.yml`](_quarto.yml).

## Directory Structure

- `content/`: Course content files
- `assignment/`: Assignment instructions and resources
- `example/`: Example code and materials
- `resource/`: General course resources
- `slides/`: Course presentation slides
- `projects/`: RStudio projects for assignments
- `files/`: Static files (images, PDFs, etc.)
- `data/`: Datasets used in the course

## Licenses

**Text and figures:** All prose and images are licensed under Creative Commons ([CC-BY-NC 4.0](https://creativecommons.org/licenses/by-nc/4.0/))

**Code:** All code is licensed under the [MIT License](LICENSE.md).
