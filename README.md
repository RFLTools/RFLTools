# RFLTools

**AutoCAD / Civil 3D Alignment Design Toolkit**  
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![AutoCAD](https://img.shields.io/badge/AutoCAD-2013–2027-red)](https://www.autodesk.com)

---

## Overview

RFLTools is a comprehensive set of civil engineering alignment design tools for **AutoCAD** and **Civil 3D**, developed in the field since the early 1990s. It covers the full lifecycle of roadway geometry — from horizontal and vertical alignment definition through spirals, superelevation, sight distance analysis, cross sections, and roundabout design.

The toolkit consists of over **60 AutoCAD commands** plus a library of supporting functions, packaged as a single `.VLX` file for zero-dependency deployment. A compiled **VB.NET (ObjectARX)** component is available for performance-critical iterative calculations.

---

## Quick Start

1. **Download** `RFLTools.VLX` from the repository root
2. In AutoCAD, type `APPLOAD` and select `RFLTools.VLX`
3. All commands are now available

For faster performance on spiral and cross-section calculations, copy the appropriate `.DLL` from the `DLL/` folder to a trusted AutoCAD support path. See [Installation](#installation) for details.

---

## Modules

The repository is organized into the following major areas:

| Module | Description |
|--------|-------------|
| **Horizontal Alignment** | Curve geometry, station-offset, alignment read/write/draw, Civil 3D integration |
| **Vertical Profiles** | Profile definition, drawing, labelling, BC & Alberta standard formats |
| **Spirals** | Clothoid transition curve fitting and geometry |
| **Superelevation** | Banked curve design per TAC/BCMoTI standards |
| **Sight Distance** | Stopping sight distance analysis along alignments and profiles |
| **Roundabouts** | Roundabout geometry design |
| **Cross Sections** | Quick section and longitudinal section generation (QSection / QLSection) |
| **Barrier Warrants** | Traffic barrier placement analysis |
| **Utility Commands** | 60+ general drafting and annotation tools (polyline editing, best-fit geometry, ditch design, annotation, layer management, coordinate conversion, etc.) |
| **Grid Drawing** | Custom grid generation for profile sheets |

Each module's specific commands are documented in the `README.md` within its subdirectory.

---

## Installation

### Basic (VLX — no compilation required)

1. Download `RFLTools.VLX`
2. In AutoCAD: `APPLOAD` → select `RFLTools.VLX` → **Load**
3. Optionally add to the Startup Suite for automatic loading

The `.VLX` includes all AutoLISP code. No DLLs are required for basic functionality.

### Faster Calculations (DLL)

Iterative computations (spirals, cross-section training) run significantly faster with the compiled VB.NET DLL.

Copy the appropriate file(s) from the `DLL/` folder to a trusted AutoCAD support path:

- `RFLTools_Acad20XXx64.dll` (for 64-bit AutoCAD)
- `RFLTools_Acad20XXx86.dll` (for 32-bit AutoCAD)
- `System.Data.SQLite.dll` (required for QLSection tools)

---

## Building from Source

### Development Workflow

The tools are developed as individual `.lsp` files organized by function across the repository's subdirectories. When a new routine is written or an existing one modified, it's placed in the appropriate folder. The build process then assembles everything into a single distributable package:

1. **Run `RunMe.cmd`** — this script concatenates every individual `.lsp` file into a single `LoadRFLTools.lsp` (~205,000 lines, 8 MB). This is the master loader that includes all tools and utilities.

2. **Open in VLIDE** — load `LoadRFLTools.lsp` into AutoCAD's Visual LISP IDE (type `VLIDE` in AutoCAD).

3. **Compile to VLX** — use VLIDE to compile `LoadRFLTools.lsp` into a single `RFLTools.VLX` file (~3.7 MB). The `.VLX` is a self-contained, packaged application that loads all tools at once without external file dependencies.

The pre-compiled `RFLTools.VLX` in the repository root is the result of this process — most users can skip straight to [Installation](#installation).

### VB.NET DLL (Visual Studio)

The repository contains two Visual Studio solutions:

- **`RFLTools_Solution/`** — targets AutoCAD 2013 through 2024
- **`RFLTools_Solution_2025/`** — targets AutoCAD 2025 and later (Autodesk changed their .NET development platform starting with AutoCAD 2025)

To compile either solution, you will need the **AutoCAD ObjectARX SDK** for each target version. These libraries are provided by Autodesk and are not included in this repository. Create a folder structure such as:

```
Autodesk_Libraries/
├── Autodesk_ObjectARX_2013_Win_64_and_32Bit/
├── Autodesk_ObjectARX_2014_Win_64_and_32Bit/
├── Autodesk_ObjectARX_2015_Win_64_and_32_Bit/
├── Autodesk_ObjectARX_2016_Win_64_and_32_Bit/
├── Autodesk_ObjectARX_2017_Win_64_and_32_Bit/
├── Autodesk_ObjectARX_2018_Win_64_and_32_Bit/
├── Autodesk_ObjectARX_2019_Win_64_and_32_Bit/
├── Autodesk_ObjectARX_2020_Win_64_and_32_Bit/
└── ... (any additional target versions)
```

Then open the solution in Visual Studio (2019 or later recommended) and build for your target configuration.

---

## AutoCAD Version Support

The AutoLISP `.VLX` works on all supported AutoCAD versions. The compiled DLLs are version-specific:

| AutoCAD | DLL Support |
|---------|-------------|
| 2013–2024 | ✓ x86 + x64 |
| 2025–2027 | ✓ x64 |

---

## Project Structure (High-Level)

| Directory | Contents |
|-----------|----------|
| `Align/` | Core alignment design modules |
| `Commands/` | General-purpose utility commands |
| `Common/` | Shared library functions |
| `Math/` | Custom math library |
| `DLL/` | Pre-compiled VB.NET DLLs |
| `RFLTools_Solution/` | VB.NET solution (AutoCAD 2013–2024) |
| `RFLTools_Solution_2025/` | VB.NET solution (AutoCAD 2025+) |
| `DrawGrid/` | Grid drawing tools |
| `Lessons/` | Training materials |
| `Slides/` | Presentation materials |

See individual subdirectory `README.md` files for detailed command listings.

---

## License

GNU General Public License v3.0 — see [LICENSE](LICENSE).

---

## About the Author

**Robert Livingston** — Civil engineer with over 30 years of experience in roadway alignment design. These tools have been developed and refined in professional practice since the early 1990s.

---

# Discussion Notes — Proposed Changes

## What changed from the current README

The current README opens with "Purpose: Master loader of RFL Tools" and jumps straight into ObjectARX build dependencies. The proposed version:

- **Adds** a clear project overview, quick start guide, installation instructions, and high-level module descriptions
- **Adds** reference to Autodesk development libraries as a build prerequisite (organized into a clean folder structure example)
- **Expands** the VLX build process into a clear 3-step workflow (RunMe.cmd → VLIDE → .VLX) so contributors understand how the tool is packaged
- **Keeps** the detailed command listings where they belong — in each subdirectory's `README.md`
- **De-emphasizes** the build process (moved below installation) since most users just need the VLX

---

*Proposed rewrite generated June 4, 2026 — v2*
