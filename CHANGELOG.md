# Changelog

All notable changes to MIAnex are documented in this file.

## 0.0.1 (unreleased)

### Added:
- Command `list` to list all open imports
- Command `activate IMPORT` to switch to a current import
- Command `deactivate` to go back to main
    - Flag `--force` to discard uncommitted changes and go back to main
- Command `import [IMAGES]` to create a new import
    - Flag `--identifier TEXT` adds an optional identifier
    - Flag `--current` to use the current branch for the import
- XDG based config file
