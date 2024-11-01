## File Format 

* Type : Tab-delimited text file
* Decimal point : Use point `"."` as the decimal point. 

## Data Structure

### First 4 lines
The first four lines contain 
`"Class"`,
`"File type"`,
`"Injection order"` and
`"Batch ID"`
data. 
These lines are in a wide format and begin with a series of tabs representing empty columns for chemical data.

Restriction in the MS-Dial software for those 4 lines is as below. 

* `"Class"`: Any values
* `"File type"`: Only `"Standard"`, `"QC"`, `"Sample"` or `"Blank"` are allowed.
* `"Injection order"`: Any integer values
* `"Batch ID"`: Any integer values

### Chemical Data
From the 5th line onward, the file includes measurements and information about chemicals starting with a header line. 
Sample IDs are in the header line (5th line).
There are some columns of stats at the right end of the table.
Those extra columns can be identified by `"NA"` in the `"File type"` row. 

### Chemical Information Columns
The initial set of columns contains information about chemicals until the sample information columns.

* **Required Columns**:
  `"Alignment ID"`,
  `"Average Rt(min)"`,
  `"Average Mz"`,
  `"Metabolite name"`,
  `"S/N average"` and
  `"Comment"`

* **Purpose**:
  - These 5 columns, `Alignment ID`, `Average Rt(min)`, `Average Mz`, `Metabolite name`, and `S/N average` will be read and copied to an output file, concentration table. 
  - The `Comment` column will be read and used for data processing but **not** copied to the output file.

* **Column Names**:
  These six columns must have exact names as specified. All other columns will be ignored.

* **Comment Column Values**:
  The `Comment` column contains information about the type of standards and can have one of the following values: 
  `"Quant"` (standard for calibration curve),`"IS"` (Internal standard), or `""` (non-targeted chemical).

### Sample Information

#### Calibration curve samples
  * Class: `"CalCurve"`
  * File type: `"Standard"`
  * Sample ID: "[numbers]**\_Cal\_[concentration]\_**[…]"; regex: `"[[:digit:]]+_Cal_[[:digit:]-]+_.*"` 
  * Concentration Formatting: Replace decimal point in concentration numbers with a dash `"-"` , e.g. 0.01 becomes `"0-01"`, 0.5 -> `"0-5"`, 0 -> `"0"`, 10 -> `"10"`. 
  * Zero Concentration Samples: There must be at least two samples with zero concentration.

#### Quality Control Samples
  * The samples with identical "QC sample ID" are replicates of one sample. They are supposed to have the identical measurement. For each "QC sample ID", there must be multiple loaded samples. 
  * Class: `"QC"`
  * File type: `"QC"`
  * Sample ID: "[numbers]\_QC[QC sample ID]\_[…]"; regex: `"[[:digit:]]+_QC[[:alnum:]]*_.*"`
  * Examples: `"230613_QCsp500ngL_GWb1__PFPaQ_BEHC18_1mL_ESIpos_20"` and `"22_QC1_GWb1"`.

#### Instrumental Blanks
  * Class: `"Blank"`
  * File type: `"Blank"`
  * Sample ID: "[numbers]\_Blank[numbers]\_[…]"; regex: `"[[:digit:]]+_Blank[[:digit:]]*_.*"`

#### Procedural Blanks
  * The samples for blank substraction
  * Class: `"ProcBlank"`
  * File type: `"Blank"`
  * Sample ID: "[numbers]\_Blank[numbers]\_[…]"; regex: `"[[:digit:]]+_Blank[[:digit:]]*_.*"`
