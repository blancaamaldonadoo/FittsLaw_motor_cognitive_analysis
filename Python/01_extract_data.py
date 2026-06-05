# =========================================================================
# 01_extract_data.py
# Purpose:  Parse FittsStudy .txt output files and export to Excel
# Input:    Folder with .txt results organized by condition subfolders
# Output:   Tablas_TFG_con_Fallos.xlsx (one sheet per condition)
# Note:     The ERRORES column is left empty — filled manually afterwards
#           This script was used for the first batch of participants (n=19)
# =========================================================================

import os
import re
import pandas as pd
import numpy as np

# -------------------------------------------------------------------------
# 1. CONFIGURATION
# -------------------------------------------------------------------------

ROOT_PATH = '/Users/blancamaldonado/Library/Mobile Documents/com~apple~CloudDocs/Blanca/Uni/4º Carrera/TFG/BLANCA'

DAYS       = ['1', '2', '3', '4']
CONDITIONS = ['D', 'ND', 'Carga D', 'Carga ND']

OUTPUT_FILE = 'Tablas_TFG_con_Fallos.xlsx'

# -------------------------------------------------------------------------
# 2. HELPER FUNCTION
# Extracts a numeric value from text using a regex pattern
# Handles comma-as-decimal-separator formatting
# -------------------------------------------------------------------------

def extract_number(pattern, text):
    match = re.search(pattern, text)
    if match:
        try:
            return float(match.group(1).replace(',', '.'))
        except ValueError:
            return None
    return None

# -------------------------------------------------------------------------
# 3. PARSE .TXT FILES
# Classifies each file by condition based on its folder path
# Extracts bivariate Fitts' Law metrics from the relevant block
# -------------------------------------------------------------------------

records = []

for root, dirs, files in os.walk(ROOT_PATH):
    for filename in files:
        if not filename.endswith('.txt'):
            continue

        filepath       = os.path.join(root, filename)
        filepath_upper = filepath.upper()

        # Classify condition from folder name
        if 'CARGA COGNITIVA ND' in filepath_upper:
            condition = 'Carga ND'
        elif 'CARGA COGNITIVA D' in filepath_upper:
            condition = 'Carga D'
        elif 'NO DOMINANTE' in filepath_upper:
            condition = 'ND'
        elif 'DOMINANTE' in filepath_upper:
            condition = 'D'
        else:
            continue

        day = next((d for d in DAYS if d in filepath_upper), 'UNKNOWN')

        with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
            content = f.read()

        bivariate_match = re.search(r"Fitts' Law - Bivariate.*", content, re.DOTALL)
        if not bivariate_match:
            continue

        block = bivariate_match.group(0)
        records.append({
            'Sujeto'    : filename.replace('.txt', '').strip().upper(),
            'Dia'       : day,
            'Condicion' : condition,
            'MTavg'     : extract_number(r'MTavg\s*=\s*([\d\.,-]+)', block),
            'TP_avg_2d' : extract_number(r'TP_avg\(2d\)\s*=\s*([\d\.,-]+)', block),
            'r_2d'      : extract_number(r'r\(2d\)\s*=\s*([\d\.,-]+)', block),
            'Error_pct' : extract_number(r'Error%\s*=\s*([\d\.,-]+)', block),
            'ERRORES'   : np.nan   # To be filled manually (cognitive errors)
        })

print(f"{len(records)} records extracted from .txt files.")

# -------------------------------------------------------------------------
# 4. BUILD COMPLETE STRUCTURE
# Creates all Subject × Day × Condition combinations
# Missing entries are filled with NaN
# -------------------------------------------------------------------------

if not records:
    print("No data found. Check folder structure and .txt content.")
else:
    df_existing = pd.DataFrame(records)
    subjects    = df_existing['Sujeto'].unique()
    days_found  = [d for d in DAYS if d in df_existing['Dia'].unique()]

    full_index = pd.MultiIndex.from_product(
        [subjects, days_found, CONDITIONS],
        names=['Sujeto', 'Dia', 'Condicion']
    )
    df_template = pd.DataFrame(index=full_index).reset_index()
    df_final    = pd.merge(df_template, df_existing,
                           on=['Sujeto', 'Dia', 'Condicion'], how='left')

    # -------------------------------------------------------------------------
    # 5. EXPORT TO EXCEL (one sheet per condition)
    # -------------------------------------------------------------------------

    COLUMNS = ['Sujeto', 'Dia', 'Condicion', 'MTavg', 'TP_avg_2d', 'r_2d', 'Error_pct', 'ERRORES']

    with pd.ExcelWriter(OUTPUT_FILE) as writer:
        for cond in CONDITIONS:
            df_cond        = df_final[df_final['Condicion'] == cond].copy()
            df_cond['Dia'] = pd.Categorical(df_cond['Dia'],
                                            categories=DAYS, ordered=True)
            df_cond        = df_cond.sort_values(['Dia', 'Sujeto'])[COLUMNS]
            df_cond.to_excel(writer, sheet_name=cond[:31], index=False)

    print(f"Done. File saved: {OUTPUT_FILE}")
    print("Open the dual-task sheets and fill in the ERRORES column manually.")
