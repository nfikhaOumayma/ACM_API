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
public final class CommonErrorCode implements java.io.Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 7216844388130044475L;

	/*
	 * COMMON ERROR CODE Range : ACM-00001 -> ACM-00500
	 */
	/** The Constant CODE_USER_NOT_ALLOWED. */
	public static final String CODE_USER_NOT_ALLOWED = "ACM-00001";

	/** The Constant CODE_LOAD_BALANCER_NOT_AVAILABLE. */
	public static final String CODE_LOAD_BALANCER_NOT_AVAILABLE = "ACM-00002";

	/** The Constant CODE_PARAM_CONFIGURATION_NOT_FOUND. */
	public static final String CODE_PARAM_CONFIGURATION_NOT_FOUND = "ACM-00003";

	/** The Constant CODE_USER_NOT_FOUND. */
	public static final String CODE_USER_NOT_FOUND = "ACM-00004";

	/** The Constant CODE_USER_EXIST. */
	public static final String CODE_USER_EXIST = "ACM-00005";

	/** The Constant FEIGN_CONNECTION_ERROR. */
	public static final String FEIGN_CONNECTION_ERROR = "ACM-00006";

	/** The Constant CODE_SETTING_EXIST. */
	public static final String CODE_SETTING_EXIST = "ACM-00007";

	/** The Constant LEVEL_PROCESS_AMOUNT_NOT_SORTED. */
	public static final String LEVEL_PROCESS_AMOUNT_NOT_SORTED = "ACM-00008";

	/** The Constant NO_ROUTE_TO_HOST. */
	public static final String NO_ROUTE_TO_HOST = "ACM-00009";

	/*
	 * TECHNICAL ERROR CODE EXCEPTIONS Range : ACM-00501 -> ACM-01000
	 */
	/** The Constant CREATION_FAILED. */
	public static final String CREATION_FAILED = "ACM-00501";

	/** The Constant UPDATE_FAILED. */
	public static final String UPDATE_FAILED = "ACM-00502";

	/** The Constant DELETE_FAILED. */
	public static final String DELETE_FAILED = "ACM-00503";

	/** The Constant CODE_DATA_NOT_FOUND. */
	public static final String CODE_DATA_NOT_FOUND = "ACM-00504";

	/** The Constant CODE_INVALID_DATA. */
	public static final String CODE_INVALID_DATA = "ACM-00505";

	/** The Constant CODE_INVALID_QUERY. */
	public static final String CODE_INVALID_QUERY = "ACM-00506";

	/** The Constant UPLOAD_FAILED. */
	public static final String UPLOAD_FAILED = "ACM-00507";

	/** The Constant API_ABACUS. */
	public static final String API_ABACUS = "ACM-00508";

	/** The Constant LOGIN_INVALID. */
	public static final String LOGIN_INVALID = "ACM-00509";

	/** The Constant PWD_INVALID. */
	public static final String PWD_INVALID = "ACM-00510";

	/** The Constant PWD_DONT_MATCH. */
	public static final String PWD_DONT_MATCH = "ACM-00511";

	/*
	 * PARAMETRAGE-SERVICE Range : ACM-01001 -> ACM-01500
	 */
	/** The Constant PARAMETRAGE_RANGE. */
	@SuppressWarnings("unused")
	private static final String PARAMETRAGE_RANGE = "ACM-01001 -> ACM-01500";

	/** The Constant CODE_INVALID_P_W_D. */
	public static final String CODE_INVALID_P_W_D = "ACM-01001";

	/*
	 * REPORTING-SERVICE Range : ACM-02001 -> ACM-02500
	 */
	/** The Constant REPORTING_RANGE. */
	@SuppressWarnings("unused")
	private static final String REPORTING_RANGE = "ACM-02001 -> ACM-02500";

	/** The Constant FAILED_GENERATE_REPORT. */
	public static final String FAILED_GENERATE_REPORT = "ACM-02001";

	/*
	 * TRANSVERS-SERVICE Range : ACM-03001 -> ACM-03500
	 */
	/** The Constant REPORTING_RANGE. */
	@SuppressWarnings("unused")
	private static final String TRANSVERS_SERVICE = "ACM-03001 -> ACM-03500";
	/** The Constant API_ABACUS_UNREACHABLE. */
	public static final String API_ABACUS_UNREACHABLE = "ACM-03001";

	/*
	 * GED-SERVICE Range : ACM-06001 -> ACM-06500
	 */
	/** The Constant SOUSCRIPTION_RANGE. */
	@SuppressWarnings("unused")
	private static final String GED_RANGE = "ACM-06001 -> ACM-06500";

	/** The Constant DOCUMENT_NOT_FOUND. */
	public static final String DOCUMENT_NOT_FOUND = "ACM-06001";

	/** The Constant DOCUMENT_ERROR. */
	public static final String DOCUMENT_ERROR = "ACM-06002";

	/** The Constant SCAN_FAIL. */
	public static final String SCAN_FAIL = "ACM-06003";

	/** The Constant SAVE_FILE_FAILED. */
	public static final String SAVE_FILE_FAILED = "ACM-06004";

	/** The Constant GED_FILE_NOT_FOUND. */
	public static final String GED_FILE_NOT_FOUND = "ACM-06008";

	/** The Constant SAVE_FILE_MAX_UPLOAD_SIZE. */
	public static final String SAVE_FILE_MAX_UPLOAD_SIZE = "ACM-06009";

	/** The Constant SAVE_FILE_ALREADY_EXIST. */
	public static final String SAVE_FILE_ALREADY_EXIST = "ACM-06010";

	/** The Constant GED_CONNECTION_ERROR. */
	public static final String GED_CONNECTION_ERROR = "ACM-06011";

	/*
	 * CRM-SERVICE Range : ACM-06501 -> ACM-07000
	 */
	/** The Constant CRM_RANGE. */
	@SuppressWarnings("unused")
	private static final String CRM_RANGE = "ACM-06501 -> ACM-07000";

	/*
	 * CREDIT-SERVICE Range : ACM-07001 -> ACM-07500
	 */
	/** The Constant CREDIT_RANGE. */
	@SuppressWarnings("unused")
	private static final String CREDIT_RANGE = "ACM-07001 -> ACM-07500";
	/** The Constant INITIAL_CHECK_CODE_DATA_NOT_FOUND. */
	public static final String INITIAL_CHECK_CODE_DATA_NOT_FOUND = "ACM-07001";
	/** The Constant FIELD_VISIT_CODE_DATA_NOT_FOUND. */
	public static final String FIELD_VISIT_CODE_DATA_NOT_FOUND = "ACM-07002";
	/** The Constant GUARANTOR_CODE_DATA_NOT_FOUND. */
	public static final String GUARANTOR_CODE_DATA_NOT_FOUND = "ACM-07003";
	/** The Constant COLLATEROL_CODE_DATA_NOT_FOUND. */
	public static final String COLLATEROL_CODE_DATA_NOT_FOUND = "ACM-07004";
	/** The Constant UPLOAD_DOCUMENT_CODE_DATA_NOT_FOUND. */
	public static final String UPLOAD_DOCUMENT_CODE_DATA_NOT_FOUND = "ACM-07005";
	/** The Constant FINANCIAL_ANALYSIS_CODE_DATA_NOT_FOUND. */
	public static final String FINANCIAL_ANALYSIS_CODE_DATA_NOT_FOUND = "ACM-07006";
	/** The Constant CHECK_APP_L1_CODE_DATA_NOT_FOUND. */
	public static final String CHECK_APP_L1_CODE_DATA_NOT_FOUND = "ACM-07007";
	/** The Constant INFORM_CUSTOMER_CODE_DATA_NOT_FOUND. */
	public static final String INFORM_CUSTOMER_CODE_DATA_NOT_FOUND = "ACM-07008";
	/** The Constant INFORM_CUSTOMER_CODE_DATA_NOT_FOUND. */
	public static final String UPLOAD_SIGNED_DOCUMENT_CODE_DATA_NOT_FOUND = "ACM-07009";
	/** The Constant CHECK_APP_L2_CODE_DATA_NOT_FOUND. */
	public static final String CHECK_APP_L2_CODE_DATA_NOT_FOUND = "ACM-07010";
	/** The Constant CHECK_APP_L3_CODE_DATA_NOT_FOUND. */
	public static final String CHECK_APP_L3_CODE_DATA_NOT_FOUND = "ACM-07011";
	/** The Constant CHECK_APP_L4_CODE_DATA_NOT_FOUND. */
	public static final String CHECK_APP_L4_CODE_DATA_NOT_FOUND = "ACM-07012";
	/** The Constant GUARANTOR_COLLATEROL_CODE_DATA_NOT_FOUND. */
	public static final String GUARANTOR_COLLATEROL_CODE_DATA_NOT_FOUND = "ACM-07013";
	/** The Constant LOAN_INFORMATION_CHANGE. */
	public static final String LOAN_INFORMATION_CHANGE = "ACM-07014";
	/** The Constant USER_FOUND. */
	public static final String USER_FOUND = "ACM-07015";
	/** The Constant CUSTOMER_EXIST. */
	public static final String CUSTOMER_EXIST = "ACM-07016";
	/** The Constant CUSTOMER_LIMIT_MAX_ACTIVE_ACCOUNT. */
	public static final String CUSTOMER_LIMIT_MAX_ACTIVE_ACCOUNT = "ACM-07017";
	/** The Constant CUSTOMER_INVALID_DATE_BIRTH. */
	public static final String CUSTOMER_INVALID_DATE_BIRTH = "ACM-07018";
	/** The Constant ACTIVITI_EXCEPTION. */
	public static final String ACTIVITI_EXCEPTION = "ACM-07019";
	/** The Constant COMPLETE_CUSTOMER_DATA_EXCEPTION. */
	public static final String COMPLETE_CUSTOMER_DATA_EXCEPTION = "ACM-07020";
	/** The Constant COMPLETE_LOAN_DATA_EXCEPTION. */
	public static final String COMPLETE_LOAN_DATA_EXCEPTION = "ACM-07021";
	/** The Constant NO_CUSTOMER_CONTACT. */
	public static final String NO_CUSTOMER_CONTACT = "ACM-07023";
	/** The Constant CHECK_FIELD_CONFIGURATION. */
	public static final String CHECK_FIELD_CONFIGURATION = "ACM-07022";
	/** The Constant ISCORE_INVALID_EXPIRY_DATE. */
	public static final String ISCORE_INVALID_EXPIRY_DATE = "ACM-07024";

	/** The Constant ISCORE_PRODUCT_CONFIGURATION. */
	public static final String ISCORE_PRODUCT_CONFIGURATION = "ACM-07025";
	/** The Constant CUSTOMER_EXIST_PHONE_NUMBER. */
	public static final String CUSTOMER_EXIST_PHONE_NUMBER = "ACM-07026";
	/** The Constant CUSTOMER_EXIST_MOBILE_NUMBER. */
	public static final String CUSTOMER_EXIST_MOBILE_NUMBER = "ACM-07027";
	/** The Constant AML_POURCENTAGE_CONFIGURATION. */
	public static final String AML_POURCENTAGE_CONFIGURATION = "ACM-07028";
	/** The Constant LOAN_ALREADY_ASSIGNED. */
	public static final String LOAN_ALREADY_ASSIGNED = "ACM-07029";
	/** The Constant INCENTIVE_SETTING_EXIST. */
	public static final String INCENTIVE_SETTING_EXIST = "ACM-07030";
	/** The Constant INCENTIVE_OPERATION_SETTING_EXIST. */
	public static final String INCENTIVE_OPERATION_SETTING_EXIST = "ACM-07031";
	/** The Constant INCENTIVE_REGISTRATION_SETTING_EXIST. */
	public static final String INCENTIVE_REGISTRATION_SETTING_EXIST = "ACM-07032";
	/** The Constant MEZA_CARD_EXIST. */
	public static final String MEZA_CARD_EXIST = "ACM-07033";
	/** The Constant CARDS_EXIST_IN_DATABASE. */
	public static final String CARDS_EXIST_IN_DATABASE = "ACM-07034";
	/** The Constant LOAN_ALREADY_EXIST_IN_ACM. */
	public static final String LOAN_ALREADY_EXIST_IN_ACM = "ACM-07042";
	/** The Constant CANNOT_CANCEL_ISSUED_LOAN. */
	public static final String CANNOT_CANCEL_ISSUED_LOAN = "ACM-07035";
	/** The Constant INCENTIVE_INCENTIVE_AUTHENTIFICATION_RESIGNING_DATE. */
	public static final String INCENTIVE_AUTHENTIFICATION_RESIGNING_DATE = "ACM-07036";
	/** The Constant INCENTIVE_AUTHENTIFICATION_LOGIN. */
	public static final String INCENTIVE_AUTHENTIFICATION_LOGIN = "ACM-07037";
	/** The Constant INCENTIVE_AUTHENTIFICATION_LOGIN_OR_PASSWORD. */
	public static final String INCENTIVE_AUTHENTIFICATION_LOGIN_OR_PASSWORD = "ACM-07038";
	/** The Constant INCENTIVE_AUTHENTIFICATION_ENABLED. */
	public static final String INCENTIVE_AUTHENTIFICATION_USER_ENABLED = "ACM-07039";
	/** The Constant INCENTIVE_AUTHENTIFICATION_ATTACHED_GROUP. */
	public static final String INCENTIVE_AUTHENTIFICATION_ATTACHED_GROUP = "ACM-07040";
	/** The Constant RENEWAL_CONDITION_SETTING_NOT_FOUND. */
	public static final String RENEWAL_CONDITION_SETTING_NOT_FOUND = "ACM-07043";
	/** The Constant ISCORE_INVALID_EXPIRY_DATE_FAILED_ERROR. */
	public static final String ISCORE_INVALID_EXPIRY_DATE_FAILED_ERROR = "ACM-07041";
	/** The Constant OPENED_REQUEST_ALREADY_EXIST. */
	public static final String OPENED_REQUEST_ALREADY_EXIST = "ACM-07044";
	/** The Constant LOAN_ALREADY_ISSUED. */
	public static final String LOAN_ALREADY_ISSUED = "ACM-07042";
	/** The Constant NATIONAL_ID_NOT_FOUND. */
	public static final String NATIONAL_ID_NOT_FOUND = "ACM-07045";
	/** The Constant CUSTOMER_INVALID_AGE. */
	public static final String CUSTOMER_INVALID_AGE = "ACM-07046";
	/** The Constant ENABLE_CRITICAL_DATA. */
	public static final String ENABLE_CRITICAL_DATA = "ACM-07047";
	/** The Constant ERROR_WHILE_UPDATE_CUSTOMERS_BRANCHES. */
	public static final String ERROR_WHILE_UPDATE_CUSTOMERS_BRANCHES = "ACM-07048";
	/** The Constant ERROR_WHILE_UPDATE_CUSTOMERS_PORTFOLIOS. */
	public static final String ERROR_WHILE_UPDATE_CUSTOMERS_PORTFOLIOS = "ACM-07049";
	/** The Constant ERROR_QUERY_GET_PRODUCT_FROM_ABACUS. */
	public static final String ERROR_QUERY_GET_PRODUCT_FROM_ABACUS = "ACM-07050";

	/** The Constant PRODUCT_NOT_FOUND. */
	public static final String PRODUCT_NOT_FOUND = "ACM-07051";
	/** The Constant UNIQUE_CODE_EXPENSES_TYPE. */
	public static final String UNIQUE_CODE_EXPENSES_TYPE = "ACM-07052";
	/** The Constant DrAndCrAccountsEmpty. */
	public static final String DrAndCrAccountsEmpty = "ACM-07053";
	/** The Constant EXPENSES_LIMIT_NOT_FOUND. */
	public static final String EXPENSES_LIMIT_NOT_FOUND = "ACM-07054";

	/** COLLECTION_ALREADY_ASSIGNED. */
	public static final String COLLECTION_ALREADY_ASSIGNED = "ACM-07052";

	/*
	 * Setting workflow Exceptions
	 */
	/** The Constant WORKFLOW_SETTING_EXCEPTION. */
	public static final String WORKFLOW_SETTING_EXCEPTION = "ACM-07053";
	/** The Constant APPROVAL_EXCEPTION. */
	public static final String APPROVAL_EXCEPTION = "ACM-07060";
	/** The Constant JOURNAL_ENTRY_EXCEPTION. */
	public static final String JOURNAL_ENTRY_EXCEPTION = "ACM-07056";
	/** The Constant WORKFLOW_SETTING_EXCEPTION. */
	public static final String CHECK_MEZA_CARD_EXCEPTION = "ACM-07057";
	/** The Constant WORKFLOW_SETTING_EXCEPTION. */
	public static final String CHECK_MEZA_CARD_EXCEPTION_UNTRUST = "ACM-07058";

	/** The Constant CHECK_FEES_EXCEPTION. */
	public static final String CHECK_FEES_EXCEPTION = "ACM-07059";

	/** The Constant SUPPLIER_EXIST. */
	public static final String SUPPLIER_EXIST = "ACM-07061";

	/** The Constant SUPPLIER_ERROR. */
	public static final String SUPPLIER_ERROR = "ACM-07062";

	/** The Constant CUSTOMER_ERROR. */
	public static final String CUSTOMER_ERROR = "ACM-07063";

	/** The Constant CHECK_DISBURSE_EXCEPTION. */
	public static final String CHECK_DISBURSE_EXCEPTION = "ACM-07064";
	/** The Constant FETCH_LOANS_SCHEDULES. */
	public static final String FETCH_LOANS_SCHEDULES = "ACM-07065";
	
	/** The Constant JOURNAL_ENTRY_WORKFLOW_STEP. */
	public static final String JOURNAL_ENTRY_WORKFLOW_STEP = "ACM-07066";
}
