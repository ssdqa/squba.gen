# Create a summary concept reference table

For several SQUBA modules, this function is used to create a summary
reference table that displays a more detailed breakdown of concept usage

## Usage

``` r
generate_ref_table(tbl, id_col, name_col, denom, time = FALSE)
```

## Arguments

- tbl:

  *tabular input* \|\| **required**

  A table, typically generated within an \*\_output function, that
  contains the concepts of interest to be displayed in the reference
  table

- id_col:

  *string* \|\| **required**

  The name of the column with the concepts to be summarized in the
  reference table

- name_col:

  *string* \|\| **required**

  The name of the column with the concept name associated with the
  concept in `id_col`

- denom:

  *string* \|\| **required**

  The name of the column with a denominator count (or any numerical
  value) associated with `id_col` to be displayed in the reference table

- time:

  *boolean* \|\| defaults to `FALSE`

  A boolean indicating whether the input data is stratified by time. For
  over time analyses, the provided denom column will be summed across
  the entire time period and an all-time value will be displayed.

## Value

A gt reference table with summary information about the concepts
included in the provided table. This is typically included as a
complement to other graphical output.

## Examples

``` r
# generate reference table for non-time dependent concept summary

input_tbl_notime <- dplyr::tibble('concept_id' = c(1, 2, 3, 4),
                                  'concept_name' = c('test1', 'test2',
                                                     'test3', 'test4'),
                                  'ct_concept' = c(100, 200, 300, 400),
                                  'site' = c('Site A', 'Site A', 'Site A',
                                  'Site A'))

generate_ref_table(tbl = input_tbl_notime,
                   id_col = 'concept_id',
                   name_col = 'concept_name',
                   denom = 'ct_concept',
                   time = FALSE)


  
    Concept Reference Table
    
  
  
  {"x":{"tag":{"name":"Reactable","attribs":{"data":{"site":["Site A","Site A","Site A","Site A"],"concept_id":[1,2,3,4],"concept_name":["test1","test2","test3","test4"],"denom_col":[100,200,300,400]},"columns":[{"id":"site","name":"site","type":"character","na":"NA","minWidth":125,"style":"function(rowInfo, colInfo) {\nconst rowIndex = rowInfo.index + 1\nif (colInfo.id === 'site' & rowIndex === 1) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\nif (colInfo.id === 'site' & rowIndex === 2) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\nif (colInfo.id === 'site' & rowIndex === 3) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\nif (colInfo.id === 'site' & rowIndex === 4) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\n}","html":true,"align":"left"},{"id":"concept_id","name":"concept_id","type":"numeric","na":"NA","minWidth":125,"style":"function(rowInfo, colInfo) {\nconst rowIndex = rowInfo.index + 1\nif (colInfo.id === 'site' & rowIndex === 1) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\nif (colInfo.id === 'site' & rowIndex === 2) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\nif (colInfo.id === 'site' & rowIndex === 3) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\nif (colInfo.id === 'site' & rowIndex === 4) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\n}","html":true,"align":"right"},{"id":"concept_name","name":"concept_name","type":"character","na":"NA","minWidth":125,"style":"function(rowInfo, colInfo) {\nconst rowIndex = rowInfo.index + 1\nif (colInfo.id === 'site' & rowIndex === 1) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\nif (colInfo.id === 'site' & rowIndex === 2) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\nif (colInfo.id === 'site' & rowIndex === 3) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\nif (colInfo.id === 'site' & rowIndex === 4) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\n}","html":true,"align":"left"},{"id":"denom_col","name":"Total Count","type":"numeric","na":"NA","minWidth":125,"style":"function(rowInfo, colInfo) {\nconst rowIndex = rowInfo.index + 1\nif (colInfo.id === 'site' & rowIndex === 1) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\nif (colInfo.id === 'site' & rowIndex === 2) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\nif (colInfo.id === 'site' & rowIndex === 3) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\nif (colInfo.id === 'site' & rowIndex === 4) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\n}","cell":["100","200","300","400"],"html":true,"align":"right"}],"searchable":true,"defaultPageSize":10,"showPageSizeOptions":false,"pageSizeOptions":[10,25,50,100],"paginationType":"numbers","showPagination":true,"showPageInfo":true,"minRows":1,"height":"auto","theme":{"color":"#333333","backgroundColor":"#FFFFFF","stripedColor":"rgba(128,128,128,0.05)","style":{"font-family":"system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif","fontSize":"16px"},"tableStyle":{"borderTopStyle":"solid","borderTopWidth":"2px","borderTopColor":"#D3D3D3"},"headerStyle":{"fontWeight":"normal","backgroundColor":"transparent","borderBottomStyle":"solid","borderBottomWidth":"2px","borderBottomColor":"#D3D3D3"},"groupHeaderStyle":{"fontWeight":"normal","backgroundColor":"transparent","borderBottomStyle":"solid","borderBottomWidth":"2px","borderBottomColor":"#D3D3D3"},"cellStyle":{"fontWeight":"normal"}},"elementId":"xombwkkeul","dataKey":"66e6f3e0b075909f7a0ff8278820cf2c"},"children":[]},"class":"reactR_markup"},"evals":["tag.attribs.columns.0.style","tag.attribs.columns.1.style","tag.attribs.columns.2.style","tag.attribs.columns.3.style"],"jsHooks":[]}


# generate reference table for time dependent concept summary

input_tbl_time <- dplyr::tibble('concept_id' = c(1, 2, 3, 4, 1, 2, 3, 4),
                                'time_start' = c('2012-01-01', '2012-01-01',
                                                 '2012-01-01', '2012-01-01',
                                                 '2013-01-01', '2013-01-01',
                                                 '2013-01-01', '2013-01-01'),
                                'time_increment' = c('year','year','year',
                                                     'year','year','year',
                                                     'year','year'),
                                'concept_name' = c('test1', 'test2', 'test3',
                                                   'test4', 'test1', 'test2',
                                                   'test3', 'test4'),
                                'ct_concept' = c(100, 200, 300, 400, 200,
                                                 300, 400, 500),
                                'site' = c('Site A', 'Site A', 'Site A',
                                           'Site A', 'Site A', 'Site A',
                                           'Site A', 'Site A'))

generate_ref_table(tbl = input_tbl_time,
                   id_col = 'concept_id',
                   name_col = 'concept_name',
                   denom = 'ct_concept',
                   time = TRUE)


  
    Concept Reference Table
    
  
  
  {"x":{"tag":{"name":"Reactable","attribs":{"data":{"site":["Site A","Site A","Site A","Site A"],"concept_id":[1,2,3,4],"concept_name":["test1","test2","test3","test4"],"denom_col":[300,500,700,900]},"columns":[{"id":"site","name":"site","type":"character","na":"NA","minWidth":125,"style":"function(rowInfo, colInfo) {\nconst rowIndex = rowInfo.index + 1\nif (colInfo.id === 'site' & rowIndex === 1) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\nif (colInfo.id === 'site' & rowIndex === 2) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\nif (colInfo.id === 'site' & rowIndex === 3) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\nif (colInfo.id === 'site' & rowIndex === 4) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\n}","html":true,"align":"left"},{"id":"concept_id","name":"concept_id","type":"numeric","na":"NA","minWidth":125,"style":"function(rowInfo, colInfo) {\nconst rowIndex = rowInfo.index + 1\nif (colInfo.id === 'site' & rowIndex === 1) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\nif (colInfo.id === 'site' & rowIndex === 2) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\nif (colInfo.id === 'site' & rowIndex === 3) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\nif (colInfo.id === 'site' & rowIndex === 4) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\n}","html":true,"align":"right"},{"id":"concept_name","name":"concept_name","type":"character","na":"NA","minWidth":125,"style":"function(rowInfo, colInfo) {\nconst rowIndex = rowInfo.index + 1\nif (colInfo.id === 'site' & rowIndex === 1) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\nif (colInfo.id === 'site' & rowIndex === 2) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\nif (colInfo.id === 'site' & rowIndex === 3) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\nif (colInfo.id === 'site' & rowIndex === 4) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\n}","html":true,"align":"left"},{"id":"denom_col","name":"Total Count (All Time Points)","type":"numeric","na":"NA","minWidth":125,"style":"function(rowInfo, colInfo) {\nconst rowIndex = rowInfo.index + 1\nif (colInfo.id === 'site' & rowIndex === 1) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\nif (colInfo.id === 'site' & rowIndex === 2) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\nif (colInfo.id === 'site' & rowIndex === 3) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\nif (colInfo.id === 'site' & rowIndex === 4) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\n}","cell":["300","500","700","900"],"html":true,"align":"right"}],"searchable":true,"defaultPageSize":10,"showPageSizeOptions":false,"pageSizeOptions":[10,25,50,100],"paginationType":"numbers","showPagination":true,"showPageInfo":true,"minRows":1,"height":"auto","theme":{"color":"#333333","backgroundColor":"#FFFFFF","stripedColor":"rgba(128,128,128,0.05)","style":{"font-family":"system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif","fontSize":"16px"},"tableStyle":{"borderTopStyle":"solid","borderTopWidth":"2px","borderTopColor":"#D3D3D3"},"headerStyle":{"fontWeight":"normal","backgroundColor":"transparent","borderBottomStyle":"solid","borderBottomWidth":"2px","borderBottomColor":"#D3D3D3"},"groupHeaderStyle":{"fontWeight":"normal","backgroundColor":"transparent","borderBottomStyle":"solid","borderBottomWidth":"2px","borderBottomColor":"#D3D3D3"},"cellStyle":{"fontWeight":"normal"}},"elementId":"bfdpbcyvkg","dataKey":"5ab9510eda248171e34526cc29e6c585"},"children":[]},"class":"reactR_markup"},"evals":["tag.attribs.columns.0.style","tag.attribs.columns.1.style","tag.attribs.columns.2.style","tag.attribs.columns.3.style"],"jsHooks":[]}


```
