/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */

package com.acm.constants.common;

/**
 * The Class {@link CommonLoggerMessage}.
 * 
 * @author HaythemBenizid
 * @since 0.1.0
 */
public final class CommonExceptionsMessage implements java.io.Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 7647922489903789987L;

	/** The Constant EXCEPTIONS_ID_NULL. */
	public static final String EXCEPTIONS_ID_NULL = "ID is Null";

	/** The Constant EXCEPTIONS_OBJECT_NULL. */

	public static final String EXCEPTIONS_OBJECT_NULL = "Object is Null";
	/** The Constant EXCEPTIONS_SERVER_UNAVAILABLE. */
	public static final String EXCEPTIONS_SERVER_UNAVAILABLE = "Server temporarily unavailable";

	/** The Constant EXCEPTIONS_FAILED_AUTHENTICATION. */
	public static final String EXCEPTIONS_FAILED_AUTHENTICATION =
			"Authentication failed. Please check your username or password.";

	/** The Constant EXCEPTIONS_FAILED_CREDENTIALS. */
	public static final String EXCEPTIONS_FAILED_CREDENTIALS =
			"Wordpass invalid. Please check password.";

	/** The Constant EXCEPTIONS_FAILED_LOAD_BALANCER. */
	public static final String EXCEPTIONS_FAILED_LOAD_BALANCER =
			"Load balancer does not have available server.";

	/** The Constant EXCEPTIONS_NO_ROUTE_TO_HOST. */
	public static final String EXCEPTIONS_NO_ROUTE_TO_HOST = "No route to host (Host unreachable).";

	/** The Constant EXCEPTIONS_ILLEGAL_ARGUMENT. */
	public static final String EXCEPTIONS_ILLEGAL_ARGUMENT = "Illegal argument";

	/** The Constant NOT_FOUND. */
	public static final String NOT_FOUND = "Not found ";

	/** The Constant WITH_ID. */
	public static final String WITH_ID = " with ID = ";

	/** The Constant DOCUMENT_NOT_FOUND. */
	public static final String DOCUMENT_NOT_FOUND = "Document not found";

	/** The Constant FILE_EXIST_ALREADY. */
	public static final String FILE_EXIST_ALREADY = "File exist ";

	/** The Constant EXCEPTIONS_CONVERSION. */
	public static final String EXCEPTIONS_CONVERSION = "Cannot convert file";

	/** The Constant EXCEPTIONS_FAILED_DELETE. */
	public static final String EXCEPTIONS_FAILED_DELETE = "failed to delete document.";

	/** The Constant EXCEPTIONS_SITE_NOT_FOUND. */
	public static final String EXCEPTIONS_SITE_NOT_FOUND = "Site is Null";

	/** The Constant CODE_SETTING_EXIST. */
	public static final String CODE_SETTING_EXIST = "Code Setting is already Exist";

	/** The Constant LEVEL_PROCESS_AMOUNT_NOT_SORTED. */
	public static final String LEVEL_PROCESS_AMOUNT_NOT_SORTED = "Amount not sorted";

	/** The Constant EXCEPTION_EXECUTING_WORKFLOW. */
	public static final String EXCEPTION_EXECUTING_WORKFLOW =
			"Error while executing the workflow for.";

	/** The Constant CUSTOMER_EXIST_ALREADY. */
	public static final String CUSTOMER_EXIST_ALREADY =
			"Customer with the same given informations exist in DB ";

	/** The Constant APPROVAL_PROCESS_FAILED. */
	public static final String APPROVAL_PROCESS_FAILED = "Failed to approve loan in ABACUS.";

	/** The Constant API_ABACUS_FAILED. */
	public static final String API_ABACUS_FAILED = "Failed to Execute API in ABACUS.";

	/** The Constant CUSTOMER_MAX_ACTIVE_ACCOUNT. */
	public static final String CUSTOMER_MAX_ACTIVE_ACCOUNT = "Limit Customer Max Active Account.";

	/** The Constant LOGIN_INVALID. */
	public static final String LOGIN_INVALID = "Login invalid ";

	/** The Constant ERROR_API_ABACUS. */
	public static final String ERROR_API_ABACUS = "Error API Abacus";

	/** The Constant CUSTOMER_INVALID_DATE_BIRTH. */
	public static final String CUSTOMER_INVALID_DATE_BIRTH = "Can't be born in the future";

	/** The Constant CUSTOMER_INVALID_AGE. */
	public static final String CUSTOMER_INVALID_AGE =
			"Customer age is not acceptable for this product";

	/** The Constant FAILED_GENERATE_REPORT. */
	public static final String FAILED_GENERATE_REPORT =
			"Failed to generate Report : Error has been occurred.";

	/** The Constant COMPLETE_CUSTOMER_DATA_EXCEPTION. */
	public static final String COMPLETE_CUSTOMER_DATA_EXCEPTION = "Customer data incomplet";

	/** The Constant COMPLETE_LOAN_DATA_EXCEPTION. */
	public static final String COMPLETE_LOAN_DATA_EXCEPTION = "Loan data incomplet";

	/** The Constant UNABLE_TO_CONTACT_CUSTOMER. */
	public static final String UNABLE_TO_CONTACT_CUSTOMER =
			"Unable to communicate with customer , customer doesn't have an email nor an IB account";

	/** The Constant SAVE_FILE_EXECPTION. */
	public static final String SAVE_FILE_EXECPTION = "Save file exception";

	/** The Constant INCENTIVE_SETTING_EXIST_ALREADY. */
	public static final String INCENTIVE_SETTING_EXIST_ALREADY = "This setting already exist in DB";

	/** The Constant ISCORE_INVALID_EXPIRY_DATE. */
	public static final String ISCORE_INVALID_EXPIRY_DATE = "I-SCORE Expiry Date not reached";

	/** The Constant ISCORE_INVALID_EXPIRY_DATE_ERROR_FAILED. */
	public static final String ISCORE_INVALID_EXPIRY_DATE_ERROR_FAILED =
			"I-SCORE Expiry Date not reached for Error and Failed Reponse";

	/** The Constant ISCORE_PRODUCT_CONFIGURATION. */
	public static final String ISCORE_PRODUCT_CONFIGURATION =
			"Check your I-SCORE/PRODUCT Configuration.";

	/** The Constant AML_POURCENTAGE_CONFIGURATION. */
	public static final String AML_POURCENTAGE_CONFIGURATION =
			"Check your AML/Pourcentage Configuration.";
	/** The Constant LOAN_ALREADY_ASSIGNED. */
	public static final String LOAN_ALREADY_ASSIGNED = "Loan has been assigned by another user ";

	/** The Constant CARDS_EXIST_IN_DATABASE. */
	public static final String CARDS_EXIST_IN_DATABASE = "Cards Already Exist In Database";

	/** The Constant LOAN_ALREADY_EXIST_IN_ACM. */
	public static final String LOAN_ALREADY_EXIST_IN_ACM =
			"Loan already exist in ACM by this Account Number";
	/** The Constant CANNOT_CANCEL_ISSUED_LOAN. */
	public static final String CANNOT_CANCEL_ISSUED_LOAN =
			"Cannot cancel loan, the loan already issued in abacus";
	/** The Constant RENEWAL_CONDITION_SETTING_NOT_FOUND. */
	public static final String RENEWAL_CONDITION_SETTING_NOT_FOUND =
			"No Renewal condition setting found for this customer";
	/** The Constant GROUP_CONNECTED_NOT_ALLOWED. */
	public static final String GROUP_CONNECTED_NOT_ALLOWED =
			"Connected user has not the right to update Exception Request , please check the value of APPROVE_EXCEPTIONS in ACM_ENVIRONMENT table";
	/** The Constant ENABLE_CRITICAL_DATA. */
	public static final String ENABLE_CRITICAL_DATA = "Failed to enable critical data for customer";
	/** The Constant WITH_ID_EXTERN. */
	public static final String WITH_ID_EXTERN = " with EXTERN ID = ";
	/** The Constant ERROR_WHILE_UPDATE_CUSTOMERS_BRANCHES. */
	public static final String ERROR_WHILE_UPDATE_CUSTOMERS_BRANCHES =
			"error while update customers branches";
	/** The Constant ERROR_WHILE_UPDATE_CUSTOMERS_PORTFOLIOS. */
	public static final String ERROR_WHILE_UPDATE_CUSTOMERS_PORTFOLIOS =
			"error while update customers portfolios";
	/** The Constant ERROR_GET_PRODUCT_FROM_ABACUS. */
	public static final String ERROR_GET_PRODUCT_FROM_ABACUS =
			"Can't Execute Query to get product or there is no product in Abacus";

	/** The Constant WORKFLOW_SETTING_EXCEPTION. */
	public static final String WORKFLOW_SETTING_EXCEPTION = "WorkFlow Setting Error";

	/** The Constant COLLECTION_ALREADY_ASSIGNED. */
	public static final String COLLECTION_ALREADY_ASSIGNED =
			"Collection has been assigned by another user ";

	/** The Constant ERROR_PRODUCT_NOT_FOUND. */
	public static final String ERROR_PRODUCT_NOT_FOUND = "Product Not Found";

	/** The Constant WORKFLOW_SETTING_NOT_FOUND. */
	public static final String WORKFLOW_SETTING_NOT_FOUND = "WorkFlow Setting Not Found";

	/** The Constant WORKFLOW_INSTANCE_NOT_FOUND. */
	public static final String WORKFLOW_INSTANCE_NOT_FOUND = "WorkFlow Instance Not Found";
	/** The Constant UNIQUE_CODE_EXPENSES_TYPE. */
	public static final String UNIQUE_CODE_EXPENSES_TYPE =
			"Code Expenses Type Already Exist In Database";
	/** The Constant CR_OR_DR_ACCOUNTS_ARE_EMPTY. */
	public static final String CR_OR_DR_ACCOUNTS_ARE_EMPTY = "Credit Or Debit Accounts Are Empty";
	/** The Constant SUPPLIER_EXIST_ALREADY. */
	public static final String SUPPLIER_EXIST_ALREADY =
			"Supplier with the same given informations exist in DB ";

	/** The Constant SUPPLIER_ERROR_MSG. */
	public static final String SUPPLIER_ERROR_MSG =
			"The identity and RNE you entered refer to two different customers, please verify.";

	/** The Constant CUSTOMER_ERROR_MSG. */
	public static final String CUSTOMER_ERROR_MSG =
			"The identity and RNE you entered refer to two different suppliers, please verify.";
}
