#!/usr/bin/env bash

# Fix GHC settings for local Windows environment
# This script detects your local MSYS2/MinGW installation and configures GHC accordingly

set -e

echo "=== Fixing GHC Settings for Local Windows Environment ==="

# Detect MSYS2 installation paths
MSYS2_PATHS=(
    "C:/ghcup/msys64"
    "C:/msys64"
    "C:/msys32"
    "C:/tools/msys64"
    "C:/tools/msys32"
)

MINGW64_PATH=""
for path in "${MSYS2_PATHS[@]}"; do
    if [[ -d "$path/mingw64" ]]; then
        MINGW64_PATH="$path/mingw64"
        echo "Found MSYS2 MinGW64 at: $MINGW64_PATH"
        break
    fi
done

if [[ -z "$MINGW64_PATH" ]]; then
    echo "Error: Could not find MSYS2 MinGW64 installation"
    echo "Please ensure MSYS2 is installed and MinGW64 is available"
    echo "Expected paths: ${MSYS2_PATHS[*]}"
    exit 1
fi

# Get GHC library directory
GHC_LIBDIR=$(ghc --print-libdir)
SETTINGS_FILE="$GHC_LIBDIR/settings"

if [[ ! -f "$SETTINGS_FILE" ]]; then
  echo "Error: GHC settings file not found at $SETTINGS_FILE"
  exit 1
fi

echo "GHC Library Directory: $GHC_LIBDIR"
echo "Settings File: $SETTINGS_FILE"

# Make a backup
cp "$SETTINGS_FILE" "$SETTINGS_FILE.bak"
echo "Backup created: $SETTINGS_FILE.bak"

echo "Replacing GHC settings with MinGW64 configuration..."

# Create settings file with detected paths
cat > "$SETTINGS_FILE" << EOF
[("GCC extra via C opts", "")
,("C compiler command", "$MINGW64_PATH/bin/gcc.exe")
,("C compiler flags", "-I$MINGW64_PATH/include")
,("C++ compiler command", "$MINGW64_PATH/bin/g++.exe")
,("C++ compiler flags", "-I$MINGW64_PATH/include")
,("C compiler link flags", "-L$MINGW64_PATH/lib -L$MINGW64_PATH/x86_64-w64-mingw32/lib")
,("C compiler supports -no-pie", "NO")
,("Haskell CPP command", "$MINGW64_PATH/bin/gcc.exe")
,("Haskell CPP flags", "-E -undef -traditional -Wno-invalid-pp-token -Wno-unicode -Wno-trigraphs -I$MINGW64_PATH/include")
,("ld command", "$MINGW64_PATH/bin/ld.exe")
,("ld flags", "")
,("ld supports compact unwind", "NO")
,("ld supports filelist", "NO")
,("ld is GNU ld", "YES")
,("ld supports single module", "NO")
,("Merge objects command", "")
,("Merge objects flags", "")
,("ar command", "$MINGW64_PATH/bin/ar.exe")
,("ar flags", "qcs")
,("ar supports at file", "YES")
,("ar supports -L", "NO")
,("ranlib command", "$MINGW64_PATH/bin/ranlib.exe")
,("otool command", "otool")
,("install_name_tool command", "install_name_tool")
,("touch command", "\$topdir/bin/touchy.exe")
,("dllwrap command", "$MINGW64_PATH/bin/dllwrap.exe")
,("windres command", "$MINGW64_PATH/bin/windres.exe")
,("unlit command", "\$topdir/bin/unlit")
,("cross compiling", "NO")
,("target platform string", "x86_64-unknown-mingw32")
,("target os", "OSMinGW32")
,("target arch", "ArchX86_64")
,("target word size", "8")
,("target word big endian", "NO")
,("target has GNU nonexec stack", "NO")
,("target has .ident directive", "NO")
,("target has subsections via symbols", "NO")
,("target has RTS linker", "YES")
,("target has libm", "YES")
,("Unregisterised", "NO")
,("LLVM target", "x86_64-unknown-windows")
,("LLVM llc command", "llc")
,("LLVM opt command", "opt")
,("LLVM clang command", "clang")
,("Use inplace MinGW toolchain", "NO")
,("Use interpreter", "YES")
,("Support SMP", "YES")
,("RTS ways", "v thr thr_debug thr_debug_p thr_p debug debug_p p")
,("Tables next to code", "YES")
,("Leading underscore", "NO")
,("Use LibFFI", "NO")
,("RTS expects libdw", "NO")
]
EOF

echo "GHC settings patched successfully!"

# Verify the changes
echo ""
echo "=== Verification ==="
echo "C compiler command:"
grep "C compiler command" "$SETTINGS_FILE" || echo "Not found"
echo ""
echo "C++ compiler command:"
grep "C++ compiler command" "$SETTINGS_FILE" || echo "Not found"
echo ""
echo "ld command:"
grep "ld command" "$SETTINGS_FILE" || echo "Not found"
echo ""
echo "ar command:"
grep "ar command" "$SETTINGS_FILE" || echo "Not found"

echo ""
echo "=== Fix Complete ==="
echo "GHC should now use MinGW64 GCC instead of clang"
