# fitts-law-motor-cognitive-analysis

R and Python scripts for the analysis of motor-cognitive learning and control using Fitts' Law. Includes descriptive statistics, linear mixed-effects models (LMMs), and visualization of throughput, movement time, and error rate across single- and dual-task conditions.

---

## Overview

This repository contains all the code developed for the Bachelor's Thesis *"Motor-cognitive control and learning applying Fitts' Law"* (Biomedical Engineering, CEU San Pablo University, 2026).

The study evaluates motor and cognitive learning in healthy young adults (18–35 years) using a touchscreen-based Fitts' Law paradigm. Performance is assessed across four conditions combining hand laterality (dominant vs. non-dominant) and cognitive load (single-task vs. dual-task), over four experimental sessions.

---

## Repository Structure

```
fitts-law-motor-cognitive-analysis/
│
├── data/
│   └── ...                  # Raw and processed data files (Excel/CSV)
│
├── R/
│   ├── descriptive.R        # Descriptive statistics and quality check
│   ├── transformations.R    # Variable transformations (log, arcsine)
│   ├── lmm_tp.R             # Linear mixed model for Throughput (TP)
│   ├── lmm_er.R             # Linear mixed model for Error Rate (ER)
│   ├── lmm_errors.R         # Linear mixed model for cognitive Errors
│   └── plots.R              # All figures (trend charts, boxplots, trajectories, costs)
│
├── Python/
│   └── preprocessing.py     # Data cleaning and preprocessing pipeline
│
└── README.md
```

---

## Methods

- **Task:** ISO 9241-9 multi-directional pointing task (2D circular layout) using FittsStudy software
- **Device:** ASUS Zenbook 14 OLED (UX3405MA), 14-inch OLED touchscreen, 2880×1800 px, 120 Hz
- **Participants:** 21 healthy adults, mean age 23 ± 2.80 years
- **Conditions:** Dominant (D), Non-Dominant (ND), Dominant + Load (D+L), Non-Dominant + Load (ND+L)
- **Sessions:** 3 consecutive training days + 1 retention session (5–7 days later)
- **Cognitive task:** Backward counting in steps of 3

### Key variables

| Variable | Description |
|---|---|
| TP | Throughput (bps) — primary outcome |
| MT | Mean movement time (ms) |
| ER | Error rate (proportion of missed targets) |
| Errors | Count of cognitive errors during dual-task |

---

## Statistical Analysis

All statistical analyses were performed in **R**. The pipeline includes:

1. **Quality check** — verification of N = 304 observations (21 subjects × 4 sessions × 4 conditions)
2. **Descriptive statistics** — mean, SD, normality check (Shapiro-Wilk)
3. **Variable transformations** — log(MT), arcsine(ER), Fisher's Z for correlation coefficients
4. **Linear Mixed-Effects Models (LMMs)** — fitted with `lme4` and `lmerTest`; ANOVA Type III with Satterthwaite correction; post-hoc pairwise comparisons with `emmeans`
5. **Model comparison** — null vs. interaction model via likelihood ratio test (AIC, χ², p-value)

### R packages used

```r
lme4, lmerTest, emmeans, performance, insight, ggplot2
```

---

## Results Summary

| Model | AIC (null) | AIC (interaction) | χ² | p-value |
|---|---|---|---|---|
| TP | 1273.18 | 686.11 | 617.08 | < 2.2e-16 |
| ER | −1827.3 | −1914.5 | 117.16 | < 2.2e-16 |
| Errors | 769.81 | 711.32 | 72.49 | 4.63e-13 |

Key findings:
- **TP increased** significantly across training sessions (H1 ✓)
- **MT decreased** with practice (H2 ✓)
- **Cognitive load** was the strongest predictor of performance degradation (H3 ✓)
- **Dominant hand** showed higher TP and faster learning, but more cognitive errors under dual-task (H4 ✓)
- **ND + Load** condition showed the greatest susceptibility to forgetting after the rest interval (H5 ✓)

---

## Reference

> Maldonado Pérez-Hidalgo, B. (2026). *Motor-cognitive control and learning applying Fitts' Law*. Bachelor's Thesis, Biomedical Engineering, CEU San Pablo University. Supervisor: C. Sánchez López de Pablo.

---

## License

This repository is shared for academic and reproducibility purposes.
