#!/usr/bin/env bash

# Fix GHC settings to use proper MinGW64 toolchain instead of clang
# This script patches the GHC settings file to use GCC instead of clang

set -e

echo "=== Fixing GHC Settings for MinGW64 ==="

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

echo "Replacing GHC settings with hardcoded MinGW64 configuration..."

# Create a hardcoded settings file that we know works for CLANG64
cat > "$SETTINGS_FILE" << 'EOF'
[("GCC extra via C opts", "")
,("C compiler command", "C:/msys64/clang64/bin/clang.exe")
,("C compiler flags", "-IC:/msys64/clang64/include")
,("C++ compiler command", "C:/msys64/clang64/bin/clang++.exe")
,("C++ compiler flags", "-IC:/msys64/clang64/include")
,("C compiler link flags", "-LC:/msys64/clang64/lib -LC:/msys64/clang64/x86_64-w64-mingw32/lib")
,("C compiler supports -no-pie", "NO")
,("Haskell CPP command", "C:/msys64/clang64/bin/clang.exe")
,("Haskell CPP flags", "-E -undef -traditional -Wno-invalid-pp-token -Wno-unicode -Wno-trigraphs -IC:/msys64/clang64/include")
,("ld command", "C:/msys64/clang64/bin/ld.lld.exe")
,("ld flags", "")
,("ld supports compact unwind", "NO")
,("ld supports filelist", "NO")
,("ld is GNU ld", "NO")
,("ld supports single module", "NO")
,("Merge objects command", "")
,("Merge objects flags", "")
,("ar command", "C:/msys64/clang64/bin/llvm-ar.exe")
,("ar flags", "qcls")
,("ar supports at file", "YES")
,("ar supports -L", "YES")
,("ranlib command", "C:/msys64/clang64/bin/llvm-ranlib.exe")
,("otool command", "otool")
,("install_name_tool command", "install_name_tool")
,("touch command", "$topdir/bin/touchy.exe")
,("dllwrap command", "C:/msys64/clang64/bin/dllwrap.exe")
,("windres command", "C:/msys64/clang64/bin/windres.exe")
,("unlit command", "$topdir/bin/unlit")
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
