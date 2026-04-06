import pandas as pd

file_1 = "liste patients V5 - TCI complet.xlsx"
file_2 = "logs_predistim_paulo.xlsx"

df1 = pd.read_excel(file_1)
df2 = pd.read_excel(file_2)

subjects = df1["Subject"].dropna().astype(str).str.strip()

def convert_code(code):
    code = str(code)
    return f"{code[:2]}-{code[2:5]}"

df2["Subject"] = df2["code_patient"].apply(convert_code)

cols = ["left", "right", "left_v", "right_v"]

df2_filtered = df2[
    df2[cols].notna().all(axis=1) & ~(df2[cols] == "-").any(axis=1)
]

matches = df2_filtered[df2_filtered["Subject"].isin(subjects)]

print("Number of matching patients:", len(matches))
print("\nMatching IDs:")
print(matches["Subject"].unique())