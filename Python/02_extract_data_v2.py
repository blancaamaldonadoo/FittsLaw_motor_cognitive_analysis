# =========================================================================
# 02_extract_data_v2.py
# Purpose:  Parse FittsStudy .txt files for the second batch of participants
# Input:    Folder with .txt results organized by condition subfolders
# Output:   Resultados_Tanda_2.xlsx (one sheet per condition)
# Note:     Improved version of 01_extract_data.py — automatically extracts
#           cognitive errors from "ERRORES EN LA CUENTA" field in .txt files
#           Used for the final dataset (n=22, 3 participants added)
# =========================================================================

import os
import re
import pandas as pd

# -------------------------------------------------------------------------
# 1. CONFIGURATION
# -------------------------------------------------------------------------

ROOT_PATH = '/Users/blancamaldonado/Library/Mobile Documents/com~apple~CloudDocs/Blanca/Uni/4º Carrera/TFG/RESULTADOS'

DAYS       = ['1', '2', '3', '4']
CONDITIONS = ['D', 'ND', 'Carga D', 'Carga ND']

OUTPUT_FILE = 'Resultados_Tanda_2.xlsx'

# -------------------------------------------------------------------------
# 2. HELPER FUNCTION
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
# -------------------------------------------------------------------------

records = []

if not os.path.exists(ROOT_PATH):
    print(f"Error: path not found — {ROOT_PATH}")
else:
    for root, dirs, files in os.walk(ROOT_PATH):
        for filename in files:
            if not filename.endswith('.txt'):
                continue

            filepath       = os.path.join(root, filename)
            filepath_upper = filepath.upper()

            # Classify condition from folder name
            if 'CARGA COGNITIVA NO DOMINANTE' in filepath_upper:
                condition = 'Carga ND'
            elif 'CARGA COGNITIVA DOMINANTE' in filepath_upper:
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

            bivariate_match = re.search(r"Fitts' Law - Bivariate.*",
                                        content, re.DOTALL)
            if not bivariate_match:
                continue

            block          = bivariate_match.group(0)
            cognitive_errors = extract_number(
                r'ERRORES EN LA CUENTA:\s*([\d\.,-]+)', content
            )

            records.append({
                'Sujeto'        : filename.replace('.txt', '').strip().upper(),
                'Dia'           : day,
                'Condicion'     : condition,
                'MTavg'         : extract_number(r'MTavg\s*=\s*([\d\.,-]+)', block),
                'TP_avg_2d'     : extract_number(r'TP_avg\(2d\)\s*=\s*([\d\.,-]+)', block),
                'r_2d'          : extract_number(r'r\(2d\)\s*=\s*([\d\.,-]+)', block),
                'Error_pct'     : extract_number(r'Error%\s*=\s*([\d\.,-]+)', block),
                'ERRORES'       : cognitive_errors
            })
            print(f"Extracted: {filename} | Condition: {condition} | "
                  f"Day: {day} | Errors: {cognitive_errors}")

# -------------------------------------------------------------------------
# 4. EXPORT TO EXCEL
# -------------------------------------------------------------------------

if not records:
    print("No data found. Check folder structure and .txt content.")
else:
    df = pd.DataFrame(records).drop_duplicates(
        subset=['Sujeto', 'Dia', 'Condicion']
    )

    COLUMNS = ['Sujeto', 'Dia', 'Condicion', 'MTavg',
               'TP_avg_2d', 'r_2d', 'Error_pct', 'ERRORES']

    with pd.ExcelWriter(OUTPUT_FILE) as writer:
        for cond in CONDITIONS:
            df_cond = df[df['Condicion'] == cond].copy()
            if df_cond.empty:
                continue
            df_cond['Dia'] = pd.Categorical(df_cond['Dia'],
                                            categories=DAYS, ordered=True)
            df_cond = df_cond.sort_values(['Dia', 'Sujeto'])[COLUMNS]
            df_cond.to_excel(writer, sheet_name=cond[:31], index=False)

    print(f"\nDone. File saved: {OUTPUT_FILE} | {len(df)} records total.")
