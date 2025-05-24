# Windows Platform Support

This directory contains Windows-specific resources and build configurations for the futr application.

## Windows Resources

### Files

- `futr.rc` - Windows resource file containing version information and application icon
- `futr_res.o` - Compiled resource object file (generated during build)

### Build Requirements

For Windows builds, you need:

1. **windres** - Windows Resource Compiler (part of MinGW or MSYS2)
2. **Icon file** - The application icon (`resources/icons/nostr-purple.ico`)

### How it Works

When building on Windows (detected by `os == "mingw32"`), the build system will:

1. Check for the existence of `platform/windows/futr.rc`
2. Compile it using `windres` to create `platform/windows/futr_res.o`
3. Link the compiled resource object file into the final executable

This adds:
- Application icon to the executable
- Version information (visible in Windows file properties)
- Company/product metadata

### Manual Resource Compilation

If you need to manually compile the resources:

```bash
# Navigate to platform/windows directory
cd platform/windows

# Compile the resource file
windres futr.rc -o futr_res.o
```

### Updating Version Information

The version information in `futr.rc` should be kept in sync with the version in `futr.cabal`. Currently set to version 0.3.0.0.

### Icon Requirements

- Icon file must be in ICO format
- Located at `../../resources/icons/nostr-purple.ico` (relative to the RC file)

### Troubleshooting

If resource compilation fails:
- Ensure `windres` is in your PATH
- Check that the icon file exists and is a valid ICO format
- Verify the RC file syntax is correct

The build will continue without resources if compilation fails, but you'll get a warning message.
