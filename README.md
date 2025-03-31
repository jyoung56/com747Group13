# COM747 GROUP 13 COURSEWORK 2  
## Hephzibah, Joseph, Mooed & Arslan  

### Chosen Dataset  
**Cardiovascular Disease Dataset**  
[Link to Dataset](https://www.kaggle.com/datasets/sulianova/cardiovascular-disease-dataset/data)  

| Feature | Type | Column Name | Description |
|---------|------|------------|-------------|
| Age | Objective Feature | `age` | Age in days (int) |
| Height | Objective Feature | `height` | Height in cm (int) |
| Weight | Objective Feature | `weight` | Weight in kg (float) |
| Gender | Objective Feature | `gender` | Categorical (binary) |
| Systolic Blood Pressure | Examination Feature | `ap_hi` | Blood pressure (int) |
| Diastolic Blood Pressure | Examination Feature | `ap_lo` | Blood pressure (int) |
| Cholesterol | Examination Feature | `cholesterol` | 1 = Normal, 2 = Above Normal, 3 = Well Above Normal |
| Glucose | Examination Feature | `gluc` | 1 = Normal, 2 = Above Normal, 3 = Well Above Normal |
| Smoking | Subjective Feature | `smoke` | Binary (0 = No, 1 = Yes) |
| Alcohol Intake | Subjective Feature | `alco` | Binary (0 = No, 1 = Yes) |
| Physical Activity | Subjective Feature | `active` | Binary (0 = No, 1 = Yes) |
| Cardiovascular Disease | Target Variable | `cardio` | Binary (0 = No, 1 = Yes) |

---

### Order to Run Scripts  
1. `load_data.R`  
2. `Data_Cleaning.R`  
3. `feature_engineering.R`  
4. `Cardiovascular_EDA_Report.R`  
5. `split_data.R`  
6. ...  
