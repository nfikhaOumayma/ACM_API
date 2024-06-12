CREATE FUNCTION [dbo].[CheckID]
(
	@CUSTIDNUMBER varchar(50)	, --1111438865
	@CustIDTYPE   varchar(10)  --'TB' , 'IQ' 
)
RETURNS INT
AS
BEGIN

Declare @Result varchar(10)
DECLARE @IDNUMBER VARCHAR (15)
DECLARE @IDTYPE VARCHAR (2)
DECLARE @I INT
DECLARE @TEMP VARCHAR (30)
DECLARE @TEMP1 VARCHAR (30)
DECLARE @TOTAL INT
DECLARE @FIN  VARCHAR (20)
SET @I = 0
SET @TOTAL = 0

--Parameters 
--1-Guest ID number
--2-Guest ID type : --'TB' , 'IQ'
--[TB] 1111438865 --Saudi
--[IQ] 2222288942 --NON Saudi

Set @Result = ''
SET @IDTYPE = @CustIDTYPE;
SET @IDNUMBER = @CUSTIDNUMBER

IF  ((@IDTYPE ='IQ' OR @IDTYPE = 'TB') AND (ISNUMERIC(@IDNUMBER) = 1) AND (LEN(@IDNUMBER) = 10))
BEGIN 
      IF ( (@IDTYPE = 'IQ' AND @IDNUMBER LIKE '2%') OR (@IDTYPE = 'TB' AND @IDNUMBER LIKE '1%') )
      BEGIN 
            WHILE (@I < 10)
            BEGIN
                  IF (@I % 2 = 0) BEGIN
                        SET @TEMP1 = '0' + CAST (SUBSTRING(@IDNUMBER,@I+1,1) AS INT) * 2
                        SET @TEMP = SUBSTRING(@TEMP1 ,LEN(@TEMP1)-1,2)
                        SET @TOTAL = @TOTAL + CAST (SUBSTRING(@TEMP,1,1) AS INT) + case when LEN(@TEMP) =2 then CAST (SUBSTRING(@TEMP,LEN(@TEMP),1)AS INT) ELSE 0 END
                  END 
                  ELSE BEGIN 
                  SET @TOTAL = @TOTAL + CONVERT(INT,SUBSTRING(@IDNUMBER,@I+1,1))
                  END   
                  SET @I=@I+1
            END
			IF(@TOTAL % 10 = 0)
				Set @Result = 1
			ELSE
				Set @Result = 0
      END
      ELSE
            Set @Result = 0
END
ELSE 
Set @Result = 0   
       return @Result
END
