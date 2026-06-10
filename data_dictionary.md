# Data Dictionary: Operational Triage Dataset
**File:** `dataset_operational_triage.csv`  
**Description:** Synthetic clinical cohort (N=165,500) generated for the 2026 FIFA World Cup epidemiological triage model. This operational dataset includes stochastic noise simulating field-laboratory conditions, instrument variance, and recall bias expected during an acute healthcare surge.

## 1. Identifiers & Targets
| Variable Name | Data Type | Description | Values / Range |
| :--- | :--- | :--- | :--- |
| `patient_id` | Integer | Unique primary key identifying the simulated patient. | `1` to `165500` |
| `pathogen` | Factor | Ground-truth etiological agent for the infection. | `Ebola_Bundibugyo`, `Influenza_H5N1` |
| `is_ebola` | Factor | Explicit Machine Learning binary target classification. | `Ebola`, `Influenza` |

## 2. Temporal & Epidemiological Dynamics
| Variable Name | Data Type | Description | Values / Range |
| :--- | :--- | :--- | :--- |
| `infection_date` | Date | Initial exposure/infection event mapping. | `2026-05-27` to `2026-08-25` |
| `triage_arrival_date` | Date | Clinical presentation date (infection date + incubation). | Calculated Date |
| `incubation_days` | Integer | Pathogen latency period.<br> *Note: Includes ±3 days stochastic recall bias in this operational dataset.* | `1` to `~25` (Right-skewed) |
| `silent_detection_phase` | Binary | Flag indicating early-phase infection prior to systemic epidemiological alert (June 10, 2026). | `0` (No), `1` (Yes) |
| `presymptomatic_shedding` | Binary | Identifies infectiousness prior to clinical symptom onset. | `0` (No), `1` (Yes) |
| `arrival_day` | Integer | Re-mapped arrival day for the discrete-time hospital simulation, compressed into a 60-day acute surge via Beta distribution. | `1` to `60` |

## 3. Demographics & Behavioral Profiling
| Variable Name | Data Type | Description | Values / Range |
| :--- | :--- | :--- | :--- |
| `age` | Integer | Patient age (truncated normal distribution). | `16` to `80` |
| `tourism_mobility` | Factor | Spatiotemporal classification of the patient's presence during the mass gathering. | `Host_City`, `Tourist_Corridor` |
| `exposure_route` | Factor | Suspected transmission vector behavior. | `0` (Indirect/Zoonotic), `1` (Direct Human Contact) |
| `care_avoidance` | Binary | Psychosocial variable capturing healthcare hesitancy or infrastructure access barriers. | `0` (Standard Seeking), `1` (Avoidance) |

## 4. Physiological Biomarkers (With Operational Noise)
*Note: In the operational dataset, these values include a 10% to 15% standard deviation injection to simulate triage field conditions.*

| Variable Name | Data Type | Description | Values / Range |
| :--- | :--- | :--- | :--- |
| `platelets_count` | Integer | Thrombocyte count. Highly relevant for modelling Ebola-induced thrombocytopenia. | Continuous (Log-normal derived) |
| `white_blood_cells` | Integer | Leukocyte count. Differentiates broad systemic viral responses. | Continuous (Log-normal derived) |

## 5. Clinical Outcomes & Infrastructure Impact
| Variable Name | Data Type | Description | Values / Range |
| :--- | :--- | :--- | :--- |
| `clinical_outcome` | Factor | Terminal case disposition (Case Fatality Rate). | `0` (Survived), `1` (Deceased) |
| `hospital_stay_days` | Integer | Total length of stay (LOS) in High-Level Isolation Units, conditional on survival outcome. | `1` to `~30` |
| `household_sar` | Integer | Secondary Attack Rate (SAR); localized community transmission multiplier per untreated case. | Normal distribution bounds |