   
   ------------------------------------
   -- USE DB Name --------
	USE ACM_BRJMF
    GO
	------ Enable CDC FOR DB ----------
	EXEC sys.sp_cdc_enable_db
    GO
	-- DISABLE CDC FOR DB------------
	EXEC sys.sp_cdc_disable_db
    GO
	-- CHECK ENABLED 1 / DISABLED 0 CDC FOR DB---------
	SELECT name, is_cdc_enabled
    FROM sys.databases

	--check enabled cdc tables with details
	SELECT * FROM cdc.change_tables

	------------------ACM_LOAN-----------------------
	-- Activate CDC for table
	EXEC sys.sp_cdc_enable_table
	@source_schema = 'dbo',
	@source_name = 'ACM_LOAN',
	@capture_instance = NULL,
	@role_name = NULL,
	@supports_net_changes = 1
	GO 

	-- disabled cdc table 
	EXEC sys.sp_cdc_disable_table
	@source_schema = 'dbo',
	@source_name = 'ACM_LOAN',
	@capture_instance = 'dbo_ACM_LOAN'


	 --check whether cdc is enabled on the table
	SELECT name, is_tracked_by_cdc
    FROM sys.tables
    where name = 'ACM_LOAN'

	-- check operations for ACM_LOAN table 
	SELECT * FROM cdc.dbo_ACM_LOAN_CT
	

	------------------ ACM-ADDRESS----------------

	-- Activate CDC for table
	EXEC sys.sp_cdc_enable_table
	@source_schema = 'dbo',
	@source_name = 'ACM_ADDRESS',
	@capture_instance = NULL,
	@role_name = NULL,
	@supports_net_changes = 1
	GO 


	-- check operations for ACM_ADDRESS table 
    SELECT * FROM cdc.dbo_ACM_ADDRESS_CT

	
	-- disabled cdc table 
	EXEC sys.sp_cdc_disable_table
	@source_schema = 'dbo',
	@source_name = 'ACM_ADDRESS',
	@capture_instance = 'dbo_ACM_ADDRESS'
	-------------ACM_CUSTOMER-------------

	EXEC sys.sp_cdc_enable_table
	@source_schema = 'dbo',
	@source_name = 'ACM_CUSTOMER',
	@capture_instance = NULL,
	@role_name = NULL,
	@supports_net_changes = 1
	GO 
	SELECT * FROM cdc.dbo_ACM_CUSTOMER_CT

	-- disabled cdc table 
	EXEC sys.sp_cdc_disable_table
	@source_schema = 'dbo',
	@source_name = 'ACM_CUSTOMER',
	@capture_instance = 'dbo_ACM_CUSTOMER'

	------------ACM_LOAN_INSTANCE------------

	EXEC sys.sp_cdc_enable_table
	@source_schema = 'dbo',
	@source_name = 'ACM_LOAN_INSTANCE',
	@capture_instance = NULL,
	@role_name = NULL,
	@supports_net_changes = 1
	GO 

	SELECT * FROM cdc.dbo_ACM_LOAN_INSTANCE_CT


	-- disabled cdc table 
	EXEC sys.sp_cdc_disable_table
	@source_schema = 'dbo',
	@source_name = 'ACM_LOAN_INSTANCE',
	@capture_instance = 'dbo_ACM_LOAN_INSTANCE'


	------------------ACM_LOAN_PARTICIPANTS------------------
	EXEC sys.sp_cdc_enable_table
	@source_schema = 'dbo',
	@source_name = 'ACM_LOAN_PARTICIPANTS',
	@capture_instance = NULL,
	@role_name = NULL,
	@supports_net_changes = 1
	GO 

	SELECT * FROM cdc.dbo_ACM_LOAN_PARTICIPANTS_CT

	-- disabled cdc table 
	EXEC sys.sp_cdc_disable_table
	@source_schema = 'dbo',
	@source_name = 'ACM_LOAN_PARTICIPANTS',
	@capture_instance = 'dbo_ACM_LOAN_PARTICIPANTS'
	
	-----------ACM_LINK_RELATIONSHIPS--------------
	EXEC sys.sp_cdc_enable_table
	@source_schema = 'dbo',
	@source_name = 'ACM_LINKS_RELATIONSHIPS',
	@capture_instance = NULL,
	@role_name = NULL,
	@supports_net_changes = 1
	GO 

	SELECT * FROM cdc.dbo_ACM_LINKS_RELATIONSHIPS_CT

	-- disabled cdc table 
	EXEC sys.sp_cdc_disable_table
	@source_schema = 'dbo',
	@source_name = 'ACM_LINKS_RELATIONSHIPS',
	@capture_instance = 'dbo_ACM_LINKS_RELATIONSHIPS'

	-----------ACM_UDF_LINK-----------------
	EXEC sys.sp_cdc_enable_table
	@source_schema = 'dbo',
	@source_name = 'ACM_UDF_LINK',
	@capture_instance = NULL,
	@role_name = NULL,
	@supports_net_changes = 1
	GO 
	SELECT * FROM cdc.dbo_ACM_UDF_LINK_CT

	-- disabled cdc table 
	EXEC sys.sp_cdc_disable_table
	@source_schema = 'dbo',
	@source_name = 'ACM_UDF_LINK',
	@capture_instance = 'dbo_ACM_UDF_LINK'