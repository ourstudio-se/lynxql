#!/bin/bash

# Install script for Lynx Language Server and VS Code Extension

set -e

echo "🔧 Installing Lynx Language Server and VS Code Extension..."

# Build and install the LSP server
echo "📦 Building Lynx Language Server..."
cargo build --release --bin lynx-lsp

echo "🚀 Installing LSP server to system..."
cargo install --path . --bin lynx-lsp

# Verify installation
echo "✅ Verifying LSP server installation..."
if command -v lynx-lsp &> /dev/null; then
    echo "✅ lynx-lsp is installed and available in PATH"
else
    echo "❌ lynx-lsp not found in PATH. Please ensure ~/.cargo/bin is in your PATH"
    exit 1
fi

# Build VS Code extension
echo "📦 Building VS Code extension..."
cd vscode-lynx-extension
npm install
npm run compile

echo "🎯 VS Code extension compiled successfully"

# Create VSIX package
echo "📦 Creating VSIX package..."
if command -v vsce &> /dev/null; then
    vsce package
    echo "✅ VSIX package created successfully"
else
    echo "⚠️  vsce not found. Install with: npm install -g vsce"
    echo "   Then run: vsce package"
fi

echo ""
echo "🎉 Installation complete!"
echo ""
echo "📋 Next steps:"
echo "1. Install the VS Code extension:"
echo "   code --install-extension lynx-language-4.0.0.vsix"
echo ""
echo "2. Or open vscode-lynx-extension folder in VS Code and press F5 for development"
echo ""
echo "3. Open any .lynx file to see real-time type checking in action!"
echo ""
echo "🔧 Troubleshooting:"
echo "- Ensure lynx-lsp is in your PATH: which lynx-lsp"
echo "- Check VS Code settings for lynx.server.path"
echo "- Use Command Palette: 'Lynx: Show Server Info' for diagnostics"