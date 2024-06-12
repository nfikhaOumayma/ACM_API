--  Update Branche Name Description on ACM_COSTUMER, ACM_LOAN and ACM_USER
BEGIN

Declare @jsonval NVarChar(2048)
Declare @branchID int
Declare @branchName varchar(50)
Declare @branchDesc varchar(200)
Declare cur_json_values cursor for Select value_json 
		from ACM_SETTING_LIST_VALUES where TABLE_ABACUS_NAME = 'Branches';

OPEN cur_json_values
	FETCH cur_json_values INTO @jsonval
	WHILE @@FETCH_STATUS = 0
	BEGIN	
		select @branchID= JSON_VALUE(@jsonval,'$.branchID') ;
		select @branchName= JSON_VALUE(@jsonval,'$.name');
		select @branchDesc= JSON_VALUE(@jsonval,'$.description') ;

		PRINT '' + convert(char(2),@branchID) + '  ' + @branchName + '  ' +@branchDesc;
	
		----- ACM_CUSTOMER
		update ACM_CUSTOMER SET BRANCHE_NAME = @branchName, BRANCHE_DESCRIPTION = @branchDesc
		where BRANCH_ID = @branchID and 
		(BRANCHE_NAME != @branchName OR BRANCHE_DESCRIPTION != @branchDesc);
	
		PRINT 'UPDATE ACM_CUSTOMER : updated rows ' + convert(char(10),@@ROWCOUNT) ;

		----- ACM_LOAN
		update ACM_LOAN SET BRANCHE_NAME = @branchName, BRANCHE_DESCRIPTION = @branchDesc
		where BRANCHID = @branchID and 
		(BRANCHE_NAME != @branchName OR BRANCHE_DESCRIPTION != @branchDesc);

		PRINT 'UPDATE ACM_LOAN : updated rows ' + convert(char(10),@@ROWCOUNT) ;

		----- ACM_USERS
		update ACM_USERS SET BRANCHE_NAME = @branchName, BRANCHE_DESCRIPTION = @branchDesc
		where BRANCHID = @branchID and 
		(BRANCHE_NAME != @branchName OR BRANCHE_DESCRIPTION != @branchDesc);

		PRINT 'UPDATE ACM_USERS : updated rows ' + convert(char(10),@@ROWCOUNT) ;

		FETCH cur_json_values INTO @jsonval
	END
	CLOSE cur_json_values
	DEALLOCATE cur_json_values

END;