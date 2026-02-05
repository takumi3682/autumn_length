# JMA/project_paths.py
from __future__ import annotations
from pathlib import Path
import os

def _project_root() -> Path:
    v = os.environ.get("PROJECT_ROOT")
    if v:
        return Path(v).expanduser().resolve()
    return Path(__file__).resolve().parent

ROOT = _project_root()

def _data_root() -> Path:
    v = os.environ.get("METDATA_ROOT")
    if v:
        return Path(v).expanduser().resolve()
    return (ROOT / "data").resolve()   # ←プロジェクト配下 data をデフォルトに

PROJECT = ROOT  # 互換エイリアス

# 入力データ（外部）
#DATA_ROOT = _data_root()
#JMA_DATA  = DATA_ROOT / "JMA"
#ERA5_DATA = DATA_ROOT / "ERA5"

# プロジェクト配下（成果物など）
DATA = ROOT / "data"
OUT  = ROOT / "outputs"
FIG  = OUT / "figures"
TAB  = OUT / "tables"
REP  = OUT / "reports"
CFG  = ROOT / "configs"
