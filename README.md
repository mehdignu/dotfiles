# Emacs Configuration

modular, language-focused Emacs configuration centered around **LSP**, **project-based workflows**, and **modern tooling**.  
This setup is optimized for Go, Python, Java, Vue (Vue 3), Docker, and YAML-heavy infrastructure work.

## Core Features

### UI / UX
- Doom One theme
- Dashboard startup screen
- Line numbers enabled globally
- Pixel-precision scrolling
- Which-Key hints
- Vertico + Orderless completion

### Project & Navigation
- Projectile project management
- Treemacs project explorer
- Consult + Ripgrep search
- Magit + Forge for Git workflows

---

## Language Support

### Go (`init-go.el`)
- `go-mode` + `lsp-mode`
- `gofumpt` formatting
- Format and imports on save
- Protobuf support via `buf`
- Project commands: `go run .`, `go test ./...`, `go build ./...`

**Key bindings**
- `C-c i` — organize imports
- `C-c r` — go run
- `C-c b` — go build

---

### Python (`init-python.el`)
- `python-mode` + Pyright
- Automatic `.venv` activation
- Ruff formatting on save
- Ruff linting via Flycheck
- pytest integration
- DAP debugging via `debugpy`

**Key bindings**
- `C-c C-t a` — pytest all
- `C-c C-t f` — pytest file
- `C-c C-r` — send region to REPL
- `C-c C-b` — send buffer to REPL

---

### Java (`init-java.el`)
- `lsp-java` (jdtls)
- Maven/Gradle/monorepo-safe root detection
- Format-on-save with timeout protection
- Compile + run via `compile` for clickable errors

**Key bindings**
- `C-c j r` — compile and run
- `C-c j R` — recompile
- `C-c j c` — prompt for compile command
- `C-c i` — organize imports

---

### Vue 3 (`init-vue.el`)
- `web-mode` derived `vue-web-mode`
- Volar + TypeScript LSP
- Monorepo-safe project roots
- Format-on-save with safeguards
- Blank-line normalization

---

### Docker (`init-docker.el`)
- `dockerfile-mode`
- `docker-compose-mode`
- Docker CLI UI
- Project-root-aware Compose commands

**Key bindings (prefix `C-c D`)**
- `D` — Docker UI
- `u` — docker compose up
- `b` — docker compose up --build
- `D` — docker compose down
- `l` — docker compose logs -f

---

### YAML / Infrastructure
- `yaml-mode` + YAML Language Server
- Kubernetes schema validation
- GitHub Actions schema support

---

## Global Key Bindings

| Keybinding | Action |
|-----------|--------|
| `C-x g` | Magit status |
| `C-c p` | Projectile commands |
| `C-c e` | Treemacs |
| `C-c s` | Ripgrep search |
| `M-←/→/↑/↓` | Window navigation |

---

## External Dependencies

Install these tools and ensure they are in your `PATH`:

- `go`, `gopls`
- `python3`, `ruff`, `debugpy`
- `javac`, `java`, `jdtls`
- `node` (Volar / TypeScript tooling)
- `docker`, `docker compose`
- `ripgrep`
- `yaml-language-server`
- `buf` (optional, for protobuf)

---

## License

Personal configuration. Use, fork, or adapt as needed. feel free to contribute :)
