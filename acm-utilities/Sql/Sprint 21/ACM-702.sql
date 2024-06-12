  UPDATE [ACM].[dbo].[ACM_UDF_FIELD]
  SET
      [FIELD_MASC] = '^(1)+\d{9}$'

  WHERE ID_ABACUS_UDF_FIELD = 3
  GO
  
_____________________________________________
  

  UPDATE [ACM].[dbo].[ACM_UDF_FIELD]
  SET
      [FIELD_MASC] = '^(2)+\d{9}$'

  WHERE ID_ABACUS_UDF_FIELD = 4
  GO