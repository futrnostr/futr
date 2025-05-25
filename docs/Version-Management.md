# Automated Version Management

This document explains the automated version management system implemented to solve the problem of having to manually update version information across multiple files.

## Problem

Previously, updating the version required manually editing version information in at least 12 different locations:

- `futr.cabal` - Package version
- `src/Types.hs` - Runtime version string
- `docs/Setup.md` - Build paths containing version
- `flatpak/com.futrnostr.futr.yml` - Installation paths
- `platform/windows/copy-dlls.sh` - Build directory paths
- `platform/windows/futr.rc` - Windows resource file (multiple locations)
- `platform/windows/innosetup.iss` - Installer paths

## Solution

The new system implements a **single source of truth** approach where:

1. **Version is defined only in `futr.cabal`**
2. **All other files are generated automatically** from templates during build
3. **Template Haskell provides compile-time version injection** for Haskell code

## Architecture

### 1. Version Module (`src/Version.hs`)
- Uses Template Haskell to read version from `futr.cabal` at compile time
- Provides `versionString` and `runtimeVersion` for use in Haskell code
- Automatically rebuilds when `futr.cabal` changes

### 2. Enhanced Setup.hs
- Reads version from `futr.cabal` during build
- Generates platform-specific files from templates
- Integrates with existing Qt resource compilation

### 3. Template Files
All version-dependent files now have `.template` counterparts:
- `platform/windows/futr.rc.template`
- `platform/windows/copy-dlls.sh.template`
- `platform/windows/innosetup.iss.template`
- `flatpak/com.futrnostr.futr.yml.template`

Templates use placeholders like `@VERSION@` that get replaced during build.

### 4. Build Integration
- Custom setup hooks in `Setup.hs` handle template processing
- All generation happens automatically during `cabal build`
- No manual intervention required

## Usage

### Updating Version

**Old way** (12 files to edit manually):
```bash
# Edit futr.cabal
# Edit src/Types.hs
# Edit platform/windows/futr.rc
# Edit platform/windows/copy-dlls.sh
# Edit platform/windows/innosetup.iss
# Edit flatpak/com.futrnostr.futr.yml
# Edit docs/Setup.md (multiple places)
# ... and more
```

**New way** (1 file to edit):
```bash
# Edit only futr.cabal, then build
sed -i 's/version:.*/version: 0.4.0.0/' futr.cabal
cabal build  # All files generated automatically
```

### Using the Helper Script
```bash
./update-version.sh 0.4.0.0
```

This script:
1. Updates `futr.cabal`
2. Runs `cabal build` to generate all files
3. Reports completion

### Manual Build Process
```bash
cabal build  # Setup.hs automatically generates version files
```

## Template Placeholders

Templates support these placeholders:
- `@VERSION@` - Version in dot notation (e.g., "0.3.0.0")
- `@VERSION_STRING@` - Same as @VERSION@ but in string format
- `@VERSION_COMMA@` - Version in comma notation (e.g., "0,3,0,0") for Windows resources

## File Status

### Source Files (edit these):
- ✅ `futr.cabal` - **Single source of truth**
- ✅ Template files (`*.template`) - Edit these instead of generated files

### Generated Files (do not edit directly):
- ⚠️ `platform/windows/futr.rc` - Generated from template
- ⚠️ `platform/windows/copy-dlls.sh` - Generated from template
- ⚠️ `platform/windows/innosetup.iss` - Generated from template
- ⚠️ `flatpak/com.futrnostr.futr.yml` - Generated from template

### Migrated Files:
- ✅ `src/Types.hs` - Now uses Version module

## Benefits

1. **Single Edit Point**: Change version in one place only
2. **Automatic Consistency**: No risk of version mismatches
3. **Build Integration**: Works with existing build system
4. **Developer Friendly**: Clear separation of templates vs generated files
5. **CI/CD Ready**: Automated generation works in build pipelines

## Migration Notes

When upgrading to this system:

1. Existing version-dependent files will be overwritten on first build
2. Any manual changes to generated files will be lost
3. Make changes to template files instead
4. Remove generated files from version control (add to .gitignore)

## Troubleshooting

### Template Not Found
```
Warning: Template not found: platform/windows/futr.rc.template
```
Solution: Ensure all template files exist, or the system will skip generation.

### Build Errors
If build fails with Template Haskell errors, ensure:
- `futr.cabal` is valid and parseable
- Version format follows semantic versioning
- Template files have correct placeholder syntax

### Version Mismatch
All generated files should have identical version numbers. If not:
- Clean build: `cabal clean && cabal build`
- Check template placeholders are correct
- Verify `futr.cabal` has the expected version
