# GitHub Copilot Instructions for lzma-conduit

## Project Overview

lzma-conduit is a Haskell library that provides a Conduit interface for LZMA/XZ compression and decompression. It wraps the `lzma` package to provide streaming compression/decompression capabilities through the Conduit abstraction.

## Architecture

- **Main Module**: `Data.Conduit.Lzma` - Single module exposing four core functions
- **Key APIs**:
  - `compress` - Compress ByteString into XZ format with optional compression level (0-9, default 6)
  - `compressWith` - Compress with custom compression parameters
  - `decompress` - Decompress from LZMA/XZ format with optional memory limit
  - `decompressWith` - Decompress with custom decompression parameters

## Build & Development Setup

### Prerequisites
```bash
# Install system dependencies
sudo apt-get install liblzma-dev

# Cabal and GHC are required (see .cabal file for tested versions)
```

### Build Commands
```bash
# Configure project
cabal configure --enable-tests

# Build library
cabal build all

# Run tests
cabal test all

# Build with specific GHC version (see haskell-ci.yml for tested versions)
cabal build --with-ghc=ghc-9.2.8
```

### Testing
- Test suite uses QuickCheck for property-based testing
- Test framework: test-framework with HUnit and QuickCheck2
- Main test file: `tests/Main.hs`
- Tests cover: compression, decompression, chaining, error handling

## Coding Style & Conventions

### General Guidelines
- Use explicit imports from dependencies
- Follow Haddock documentation style for public APIs
- Maintain compatibility with GHC 9.2.8 through 9.10.1
- Use `-Wall` for strict compilation warnings

### Formatting
- 2-space indentation
- Align type signatures and record fields
- Use qualified imports for external modules (e.g., `qualified Codec.Compression.Lzma as Lzma`)
- Use unqualified imports for standard Prelude-like modules

### Error Handling
- Use `MonadThrow` for streaming errors
- Provide descriptive error messages with context (e.g., "Data.Conduit.Lzma.decompress: error: ...")
- Use `prettyRet` helper to convert LZMA error codes to human-readable strings

### Conduit Patterns
- Always handle empty ByteString chunks explicitly (skip/ignore them)
- Use `await` for input, `yield` for output, `leftover` for unconsumed data
- Use `liftIO` for IO operations within Conduit transformers
- Implement state machines using recursive `go` functions

## Key Dependencies

- **lzma** (0.0.0.3 - 0.1): Core LZMA compression bindings
- **conduit** (1.1.0 - 1.4): Streaming data interface
- **bytestring** (0.9.1 - 0.12): Binary data handling
- **resourcet** (1.1.0 - 1.3): Resource management
- **transformers** (0.2 - 0.6): Monad transformers

## Testing Approach

### Property-Based Testing
- Use QuickCheck generators for random ByteString data
- Test compression/decompression round-trips
- Verify error handling with corrupt/invalid data
- Test boundary conditions (empty input, split data)

### Test Categories
1. **Compress Tests**: Validate compression works and produces reasonable output
2. **Decompress Tests**: Test decompression error handling and edge cases
3. **Chained Tests**: Verify compress→decompress round-trips preserve data

### Running Specific Tests
```bash
# Run all tests
cabal test

# Run with detailed output
cabal test --test-show-details=direct
```

## Common Tasks

### Adding New Compression Parameters
1. Update `compress` or `decompress` function signatures
2. Pass new parameters to `Lzma.defaultCompressParams` or `Lzma.defaultDecompressParams`
3. Document new parameters with Haddock comments
4. Add tests for new parameter combinations

### Modifying Stream Processing
1. Changes typically go in the `go` helper functions
2. Maintain state machine pattern: handle input required, output available, stream end, and errors
3. Always test with various chunk sizes and empty chunks
4. Ensure `leftover` is used for unconsumed data

### Adding Error Handling
1. Use `throwM` with `userError` for user-facing errors
2. Add new error cases to `prettyRet` if needed
3. Test error paths with QuickCheck properties

## CI/CD

- GitHub Actions workflow: `.github/workflows/haskell-ci.yml`
- Tested on: GHC 9.2.8, 9.4.8, 9.6.6, 9.8.2, 9.10.1
- Platform: Ubuntu (latest)
- All PRs must pass tests across all GHC versions

## Important Notes

- This library provides a thin streaming wrapper around the `lzma` package
- Memory limits are important for decompression to prevent resource exhaustion
- Empty ByteString chunks must be filtered/skipped to avoid confusing the underlying LZMA API
- The library supports both legacy LZMA and modern XZ formats
- Concatenated streams are supported by default in decompression
