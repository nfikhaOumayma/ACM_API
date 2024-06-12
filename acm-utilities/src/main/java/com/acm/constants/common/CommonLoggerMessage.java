/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */

package com.acm.constants.common;

/**
 * The Class CommonLoggerMessage.
 * 
 * @author HaythemBenizid
 * @since 0.1.0
 */
public final class CommonLoggerMessage implements java.io.Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -217984601683150473L;

	/** The Constant RESPONSE_SENT_BY_SERVER. Controller Message Response to be sent to Server. */
	public static final String RESPONSE_SENT_BY_SERVER =
			"############# Response to be sent by Server ... #############";

	/**
	 * The Constant RECEIVING_DELETE_REQUEST. Controller Message to be displayed before delete
	 * request.
	 */
	public static final String RECEIVING_DELETE_REQUEST =
			"############# Receiving Delete Request of {} #############";

	/**
	 * The Constant RECEIVING_UPDATE_REQUEST. Controller Message to be displayed before update
	 * request.
	 */
	public static final String RECEIVING_UPDATE_REQUEST =
			"############# Receiving Update Request of {} #############";

	/**
	 * The Constant RECEIVING_CREATION_REQUEST. Controller Message to be displayed before creation
	 * request.
	 */
	public static final String RECEIVING_CREATION_REQUEST =
			"############# Receiving Creation Request of {} #############";

	/**
	 * The Constant RECEIVING_REQUEST_DATA_BY_ID. Controller Message to be displayed after a get
	 * request using one Parameter.
	 */
	public static final String RECEIVING_REQUEST_DATA_BY_ID =
			"Requesting elements from Server using ID {} ";

	/**
	 * The Constant RECEIVING_REQUEST_DATA. Controller Message to be displayed before a get request.
	 */
	public static final String RECEIVING_REQUEST_DATA =
			"############# Requesting elements of {} from Server #############";

	/** The Constant RETURNING_DATA. */
	public static final String RETURNING_DATA = "############# Returning data ... #############";

	/** The Constant RETRIEVING_DATA. */
	public static final String RETRIEVING_DATA = "############# Retrieving data ... #############";

	/** The Constant RETURNING_DATA_FROM_CLIENT. */
	public static final String RETURNING_DATA_FROM_CLIENT =
			"############# Returning data From service client #############";

	/** The Constant BEFORE_EXECUTION. */
	public static final String BEFORE_EXECUTION =
			"############# Start execution of {} #############";

	/** The Constant BEFORE_EXECUTION_FEIGN_CLIENTS. */
	public static final String BEFORE_EXECUTION_FEIGN_CLIENTS =
			"############# Start reciving response from clients #############";

	/** The Constant DELETE_DATA. */
	public static final String DELETE_DATA = "############# Delete DATA #############";

	/**
	 * The Constant NOT_FOUND_OBJECT. No Object has been found used for Logging. object will be
	 * passed as param.
	 */
	public static final String NOT_FOUND_OBJECT = "No {} has been found ";

	/** The Constant CREATED_OBJECT. */
	public static final String CREATED_OBJECT = "Created Object ";

	/** The Constant UPDATED_OBJECT. */
	public static final String UPDATED_OBJECT = "Updated Object ";

	/** The Constant DATA_INTEGRITY_CHECK1. */
	public static final String DATA_INTEGRITY_CHECK1 = "Performing Data integrity Check";

	/** The Constant ERROR_WHILE_SAVING. */
	public static final String ERROR_WHILE_SAVING = "Error while Saving : {} ";

	/** The Constant PERSISTING_OBJECT. */
	public static final String PERSISTING_OBJECT = "Persisting {} object in DB";

	/** The Constant ERROR_WHILE_DELETING. */
	public static final String ERROR_WHILE_DELETING = "Error while Deleting ";

	/** The Constant CHECKING_HABILITATION. */
	public static final String CHECKING_HABILITATION = "Checking Habilitation for User";

	/** The Constant SUCCESFULL_DELETE. */
	public static final String SUCCESFULL_DELETE = "{} has been successfully removed from DB";

	/** The Constant SUCCESFULL_CREATE. */
	public static final String SUCCESFULL_CREATE = "{} has been successfully created in DB";

	/** The Constant SUCCESFULL_UPDATE. */
	public static final String SUCCESFULL_UPDATE = "{} has been successfully updated in DB";

	/** The Constant USER_CONNECTED. */
	public static final String USER_CONNECTED = "Get User Connected : {}";

	/** The Constant ERROR_WHILE_GETTING_CONNECTED_USER. */
	public static final String ERROR_WHILE_GETTING_CONNECTED_USER =
			"Error while getting connected user";

	/** The Constant ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE. */
	public static final String ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE =
			"Error while connecting to remote service";

	/** The Constant SUCCESFULL_UPLOAD_FILE_TO_GED. */
	public static final String SUCCESFULL_UPLOAD_FILE_TO_GED =
			"{} File(s) has been successfully uploaded to GED";

	/**
	 * No Object has been found used for Logging. object will be passed as param
	 */
	public static final String FILE_EXIST_ALREADY = "File {} already exist";

	/**
	 * DOCUMENT_NOT_FOUND document not found.
	 */
	public static final String DOCUMENT_NOT_FOUND = "Document not found";

	/** The Constant ERROR_WHILE_FINDING_DATA_FROM_ABACUS. */
	public static final String ERROR_WHILE_FINDING_DATA_FROM_ABACUS =
			" ### FAILED QUERY ### Error while executing query on ABCUS DB.";

	/** The Constant CODE_SETTING_EXIST. */
	public static final String CODE_SETTING_EXIST = "Code Setting is Already Exist";

	/** The Constant LEVEL_PROCESS_AMOUNT_NOT_SORTED. */
	public static final String LEVEL_PROCESS_AMOUNT_NOT_SORTED = "Amount not sorted";

	/** The Constant ERROR_WHILE_EXECUTING_WORKFLOW. */
	public static final String ERROR_WHILE_EXECUTING_WORKFLOW =
			" ### FAILED ### Error while executing the workflow for the given LOAN.";
	
	/** The Constant ERROR_WHILE_DELETING_JOURNAL_ENTRY_TYPE. */
	public static final String ERROR_WHILE_DELETING_JOURNAL_ENTRY_TYPE =
			" ### FAILED ### Error while deleting journal entry type";
	
}
