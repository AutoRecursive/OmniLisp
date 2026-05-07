# Contributing to OmniLisp

Thank you for your interest in contributing to OmniLisp! This guide will help you get started.

## How to Contribute

Contributions are welcome in several forms:

- **Bug reports**: Open an issue describing the problem, expected behavior, and steps to reproduce.
- **Feature requests**: Propose new backends, language features, or improvements via issues.
- **Code contributions**: Submit pull requests for bug fixes, new backends, or enhancements.
- **Documentation**: Improve guides, examples, or inline comments.

## Getting Started

1. Fork the repository and clone your fork:
   ```bash
   git clone <your-fork-url> OmniLisp
   cd OmniLisp
   ```

2. Install Racket from <https://racket-lang.org/>.

3. Verify the installation:
   ```bash
   racket omnilisp/main.rkt --list-targets
   ```

4. Run the test suite to ensure everything works:
   ```bash
   racket tests/transpiler.rkt
   ```

## Code Style

- **Racket conventions**: Follow standard Racket style with 2-space indentation.
- **Naming**: Use kebab-case for functions and variables (e.g., `emit-program`, `backend-registry`).
- **Documentation**: Add docstrings for public functions and structs.
- **Comments**: Explain non-obvious logic, especially in code generation.
- **Match expressions**: Use pattern matching for IR node dispatch in backend emitters.

## Testing

All code changes should include tests:

1. **Run existing tests** before making changes:
   ```bash
   racket tests/transpiler.rkt
   ```

2. **Add tests** for new features or bug fixes in `tests/transpiler.rkt`.

3. **Backend tests**: When adding a new backend, include tests that verify:
   - Correct code generation for all IR node types
   - Generated code compiles (if applicable)
   - Generated code produces expected output

4. **Example programs**: Add sample programs to `examples/` demonstrating your backend's features.

## Commit Format

Use clear, descriptive commit messages following this style:

- **Format**: `<action>: <brief description>`
- **Actions**: `add`, `update`, `fix`, `refactor`, `docs`, `test`
- **Examples**:
  - `add: JavaScript backend with ES6 support`
  - `fix: handle nested lambda expressions in Python backend`
  - `docs: update BACKEND_GUIDE with optimization tips`

Keep the first line under 70 characters. Add details in the commit body if needed.

## Pull Request Process

1. **Create a feature branch** from `main`:
   ```bash
   git checkout -b feature/your-feature-name
   ```

2. **Make your changes** following the code style and testing guidelines above.

3. **Commit your changes** with clear commit messages.

4. **Push your branch** and open a pull request:
   ```bash
   git push -u origin feature/your-feature-name
   ```

5. **PR description** should include:
   - Summary of changes (1-3 bullet points)
   - Test plan or verification steps
   - Related issues (if applicable)

6. **Address review feedback** promptly and push updates to your branch.

## Adding a New Backend

The most common contribution is adding a new target language backend. See [BACKEND_GUIDE.md](BACKEND_GUIDE.md) for detailed instructions. In summary:

1. Create `omnilisp/<language>-backend.rkt` with an `emit-program` function.
2. Register your backend in `omnilisp/backends.rkt`.
3. Add tests to `tests/transpiler.rkt`.
4. Add example programs to `examples/`.
5. Update documentation if your backend introduces special forms.

## Questions or Issues?

- Open an issue for bugs, feature requests, or questions.
- Check existing issues before creating a new one.
- Provide minimal reproducible examples for bug reports.

Thank you for contributing to OmniLisp!
