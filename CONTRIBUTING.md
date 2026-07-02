# Contribution Guidelines

Thanks for your interest in contributing to `roxyreqs`. Feedback, bug
reports, and pull requests from the R community are welcome.

## Versioning

The `main` branch is protected and contains released versions as tagged
commits. We follow [semantic versioning](https://semver.org/)
(`major.minor.patch`), reflected in `DESCRIPTION`. Notable changes are
recorded in `NEWS.md`.

## Pull Requests

1. Fork the repository and create a branch from `main`.
2. Use a descriptive, hyphen-separated branch name with one of these
   prefixes: `feature/`, `bugfix/`, `hotfix/`, or `docs/`
   (e.g., `feature/new-login`).
3. Open a pull request against `main`, referencing any related issue
   (e.g., `Closes #9`).

## Commit Messages

Follow a simplified [Conventional Commits](https://www.conventionalcommits.org/)
style: start with a category (`feat`, `fix`, `refactor`, `chore`) followed
by a colon and an imperative description.

```bash
git commit -m 'feat: add new login component'
git commit -m 'fix: correct header alignment issue'
git commit -m 'chore: update README with usage examples'
```

## Coding Style

Follow the [tidyverse style guide](https://style.tidyverse.org/): lowercase
names with underscores, two-space indentation, spaces after commas. Enforce
style and lint locally:

```r
styler::style_pkg(exclude_dirs = c("inst"))
devtools::load_all()
lintr::lint_package()
```

## Documentation

Document all functions with [roxygen2](https://roxygen2.r-lib.org/). Exported
functions need `@export` and `@return` tags; internal functions use `@noRd`.
Add `@meta` tags for traceability where useful. Regenerate docs after
changing roxygen comments:

```r
devtools::document()
```

## Testing

Write unit tests for exported functions, with `@meta` tags describing author,
reviewer, review date, and purpose. Run the suite and metadata checks before
opening a pull request:

```r
devtools::test()
roxyreqs::check_meta(print_error = TRUE)
```
