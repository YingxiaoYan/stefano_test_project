# File Format 

* Type : Tab-delimited text file
* Decimal point : Use point `"."` as the decimal point. 

# Table Structure

The input file contains the data in a table format described as below. 

| Lines    |      1st      |       2nd        | 3rd section |
| :------- | :-----------: | :--------------: | :---------: |
| 1st-4th  |    *empty*    |   Sample info    |   *empty*   |
| From 5th | Chemical info | Measurement data | Extra stats |

## First 4 lines

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

## Starting from the 5th line

From the 5th line onward, the file contains measurement data and information about chemicals beginning with a header line. 
The columns in these lines are organized into 3 sections. 
* The first section contains chemical information. 
* The second includes all measurement data.
* The third section provides some statistical summaries of the measurement data. Those extra columns can be identified by `"NA"` in the `"File type"` row. 

### Chemical Information Columns (1st section)

The initial set of columns contains information about chemicals.

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

### Sample Information Columns (2nd section)

* The first line of this section (or the 5th line of the file) contains sample IDs.
* General form of **Sample ID**: 
  * The sample type or sample ID followed by other information. Optionally a number in various lengths can be added ahead of the sample type/ID with an underline. 
  * [number\_]\[sample type/ID\]\_[…]"
  * In regular expression: `"([[:digit:]]+_)?[[:alnum:]-]+_.*"` 

#### Calibration curve samples

  * Class: `"CalCurve"`
  * File type: `"Standard"`

##### Samples to fit the curve

  * Sample ID: "[numbers\_]**Cal\_[concentration]\_**[…]"; regex: `"([[:digit:]]+_)?Cal_[[:digit:]-]+_.*"` 
  * Concentration Formatting: Replace decimal point in concentration numbers with a dash `"-"` , e.g. 0.01 becomes `"0-01"`, 0.5 -> `"0-5"`, 10 -> `"10"`. 

##### Zero concentration

  * There must be at least two samples with zero concentration.
  * Sample ID: "[numbers\_]**Cal\_0\_**[…]"; regex: `"([[:digit:]]+_)?Cal_0_.*"` 

#### Quality Control Samples

  * The samples with identical "QC sample ID" are replicates of one sample. They are supposed to have the identical measurement. For each "QC sample ID", there must be multiple loaded samples. 
  * Class: `"QC"`
  * File type: `"QC"`
  * Sample ID: "[numbers\_]**QC[QC sample ID]**\_[…]"; regex: `"([[:digit:]]+_)?QC[[:alnum:]]*_.*"`
  * Examples: `"230613_QCsp500ngL_GWb1__PFPaQ_BEHC18_1mL_ESIpos_20"` and `"22_QC1_GWb1"`.

#### Blanks

  * The data of all of these samples are used for blank substraction. 
  * File type: `"Blank"`
