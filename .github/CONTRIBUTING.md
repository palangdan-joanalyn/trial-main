# Contributing to this Erlang Project

Thank you for contributing to the Erlang Project! By following these guidelines, the project stays organized, maintainable and easy to work with.

## Table of Contents
 - [How to Contribute](#how-to-contribute)
 - [Coding Standards](#coding-standards)
 - [Reporting Issues](#reporting-issues)
 - [Pull Request Guidelines](#pull-request-guidelines)

---

## How to Contribute

 1. **Clone the repository** - Create your local copy of the repository
 2. **Create a new branch** - Always create a new branch for each issue or feature you are working on:
	git checkout -b feature/<feature-name>
 3. **Make your changes** - Implement your feature or fix.
 4. **Test your changes** - Make sure to test your changes thoroughly.
 5. **Commit your changes** - Use clear, concise commit messages following the commit message guidelines.
 6. **Push your changes** - Push your branch:
	git push origin feature/your-feature-name
 7. **Open a pull request** - Go to the repository and create a pull request.

---

## Coding Standards

### 1. **General guidelines**
     - Stick to 80 characters per line maximum. If a line becomes too long due to complex logic or lengthy variable names, break the line into multiple shorter lines.
     - Use 4-space indentation for all code. Do not use tabs.
     - Separate functions with 2 blank lines.
     - Place module-level attributes (e.g., -module, -author, -vsn) at the top of your Erlang files, followed by other attributes like -export and -include.
     - Group functions logically. Always list exported functions first, followed by unexported ones.
     - Avoid deep nesting (no more than 3 levels).
     - Place types at the beginning of the file, before functions.
     - Functions with a single and short implementation should be placed on a single line.
### 2. **Naming Conventions**
     - Module names: Use lowercase with words separated by underscores (_). Module names must be the same as the filename.
     - Function names: Use lowercase with words separated by underscores (_).
     - Variables: Use CamelCase. Variables starting with an underscore (_) should be used for ignored variables.
### 3. **Commenting and Documentation**
     - Use `%%` for single-line comments. Avoid inline comments.
     - Use `%%%` for formal documentation.
     - Document modules and functions with a brief description of their purpose and functionality.
     - Use EDoc syntax for function documentation:
          - `@doc` for function description
          - `@spec` for the function’s type signature
          - `@param` for input parameters
          - `@return` for return values
### 4. **Import statements**
     - Avoid using the `-import` directive. Always use the full module name when calling functions from other modules.
### 5. **Function constructs**
     - Prefer pattern matching over case expressions.
     - Write functions to either express a workflow or perform a single task.
     - Avoid many arguments in functions; instead, group related parameters in a tuple or record.
### 6. **Control construct**
     - Indent case clauses with 4 spaces.
     - Use a fallback clause in if statements (e.g., true ->).
     - Avoid inline actions within case or if statements.

---

## Reporting Issues

   If any issues or any violations of the coding conventions are encountered, report them by creating a new issue. Contributors are encouraged to:
     - Open an issue if inconsistencies are spotted in the code style.
     - Suggest improvements to the coding standards.
     - Flag areas where the code might violate the guidelines.

   ### To report an issue:
    1. Go to the Issues page.
    2. Click on New Issue.
    3. Select the appropriate issue template, if available (e.g., "Coding Standards Violation").
    4. Provide a clear description of the issue, including what coding convention or guideline was violated and the specific lines of code (if applicable).

---

## Issue Template for Coding Violations

   When reporting a violation of the coding standards, use this template:

---
name: Coding Standards Violation
about: Report a violation of coding standards or conventions.
title: '[Coding Standards Violation] - <Brief Description>'
labels: 'coding standards'
assignees: ''

---

### Description of the violation

Provide a clear description of the violation (which coding standard or guideline was violated).

### Suggested fix

If you have a suggestion for fixing the issue, please provide it here.

### Affected Code (if applicable)

Provide the lines of code where the violation occurs or attach a screenshot.

---

## Pull Request Guidelines

 - **Link to Issues**: When submitting a pull request, make sure to reference any issues it addresses (e.g., Fixes #42).
 - **Small Changes**: Keep pull requests small and focused on a single issue or feature.
 - **Clear Commit Messages**: Follow this format for commit messages:
   ```plaintext
   type(scope): brief description of changes

 For example:
     git commit -m "feat(authentication): add JWT-based authentication system"

### Commit Types:

 - **feat**: A new feature
 - **fix**: A bug fix
 - **docs**: Documentation updates
 - **style**: Code style changes (formatting, white space, etc.)
 - **refactor**: Refactoring code (no functional changes)
 - **test**: Adding or modifying tests
 - **chore**: Other changes (dependencies, tooling, etc.)

