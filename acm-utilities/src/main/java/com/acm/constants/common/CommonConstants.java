/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */

package com.acm.constants.common;

/**
 * { @link CommonConstants } class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
public final class CommonConstants implements java.io.Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -3253575284165307215L;

	/** The Constant APP_NAME. */
	public static final String APP_NAME = "ACM";

	/** The Constant APP_CLIENT. */
	public static final String APP_CLIENT = "ACM";

	/** The Constant DEFAULT_USER. */
	public static final String DEFAULT_USER = "Super.Admin";

	/** The Constant DEFAULT_DECIMAL_NUMBER. */
	public static final Integer DEFAULT_DECIMAL_NUMBER = 3;

	/** The Constant ANONYMOUS_USER. */
	public static final String ANONYMOUS_USER = "anonymousUser";

	/** The Constant AUTHORIZED_GROUPS. */
	public static final String AUTHORIZED_GROUPS = "AUTHORIZED_GROUPS";

	/** no replay email. */
	public static final String NO_REPLAY_EMAIL = "devops@talys.digital";

	/** The Constant NO_MATCH. */
	public static final String NO_MATCH = "No Match";

	/** The Constant INTERNET_BANKING. */
	public static final String INTERNET_BANKING = "INTERNET_BANKING";

	/** The Constant KO. */
	public static final String KO = "KO";

	/** The Constant OK. */
	public static final String OK = "OK";

	/** The Constant ADD. */
	public static final String ADD = "ADD";

	/** The Constant UPDATE. */
	public static final String UPDATE = "UPDATE";

	/** The Constant COMMON_YEAR_NUMBER_OF_DAYS. */
	public static final String COMMON_YEAR_NUMBER_OF_DAYS = "365";

	/** The Constant PATTREN_DATE. */
	public static final String PATTREN_DATE = "dd/MM/yyyy";

	/** The Constant DEFAULT_UDF_GROUP_NATIONALITY. */
	public static final Long DEFAULT_UDF_GROUP_NATIONALITY = 2L;

	/*
	 * PARMS for ACTIVITI WORKFLOW
	 */
	/** The Constant PROCESS_OBJECT_LOAN. */
	public static final String PROCESS_OBJECT_LOAN = "loanDTO";

	/** The Constant CONST_LOAN_ID. */
	public static final String CONST_LOAN_ID = "loanId";

	/** The Constant TIMER_TASK_REMAINDER_KEY. */
	public static final String TIMER_TASK_REMAINDER_KEY = "duration";

	/** The Constant TIMER_TASK_REMAINDER_VALUE. */
	public static final String TIMER_TASK_REMAINDER_VALUE = "PT24H";

	/*
	 * ACM_SETTING_MOTIFS_REJET_CATEGORIE
	 */
	/** The Constant ACM_SETTING_MOTIFS_REJET_CATEGORIE_REJET. */
	public static final String ACM_SETTING_MOTIFS_REJET_CATEGORIE_REJET = "REJET";

	/** The Constant ACM_SETTING_MOTIFS_REJET_CATEGORIE_CORRECTIFS. */
	public static final String ACM_SETTING_MOTIFS_REJET_CATEGORIE_CORRECTIFS = "CORRECTIFS";

	/*
	 * ACM_SETTING_DOCUMENTS_TYPE
	 */
	/** The Constant ACM_SETTING_DOCS_TYPE_LOAN. */
	public static final String ACM_SETTING_DOCS_TYPE_LOAN = "0 , LOAN";

	/** The Constant ACM_SETTING_DOCS_TYPE_CLIENT. */
	public static final String ACM_SETTING_DOCS_TYPE_CLIENT = "1 , CLIENT";

	/** The Constant ACM_SETTING_DOCS_ASSIGN_DOCUMENT_AUTRE. */
	public static final String ACM_SETTING_DOCS_ASSIGN_DOCUMENT_AUTRE = "2 , ASSIGN_DOCUMENT";
	/** The Constant ACM_SETTING_DOCS_COLLECTION. */
	public static final String ACM_SETTING_DOCS_COLLECTION = "4 , COLLECTION";

	/*
	 * ACM_SETTING_GUARANTOR_COLLATERAL
	 */
	/** The Constant ACM_SETTING_GUARANTOR_COLLATERAL_GUARANTOR. */
	public static final String ACM_SETTING_GUARANTOR_COLLATERAL_GUARANTOR = "GUARANTOR";

	/** The Constant ACM_SETTING_GUARANTOR_COLLATERAL_COLLATERAL. */
	public static final String ACM_SETTING_GUARANTOR_COLLATERAL_COLLATERAL = "COLLATERAL";

	/*
	 * ACM_SETTING_REQUIRED_STEP
	 */
	/** The Constant ACM_SETTING_REQUIRED_STEP_FIELD_VISIT. */
	public static final String ACM_SETTING_REQUIRED_STEP_FIELD_VISIT = "FIELD_VISIT";

	/** The Constant ACM_SETTING_REQUIRED_STEP_AUDIT_REVIEW. */
	public static final String ACM_SETTING_REQUIRED_STEP_AUDIT_REVIEW = "AUDIT_REVIEW";

	/** The Constant ACM_SETTING_REQUIRED_STEP_RISK_REVIEW. */
	public static final String ACM_SETTING_REQUIRED_STEP_RISK_REVIEW = "RISK_REVIEW";

	/*
	 * ACM_HABILITATION_IHM_ROUTE CODE
	 */
	/** The Constant ACM_HOME. */
	public static final String ACM_HOME = "HOME";

	/** The Constant ACM_IHM_LOAN_DETAILS. */
	public static final String ACM_IHM_LOAN_DETAILS = "IHM_LOAN_DETAILS";

	/** The Constant ACM_IHM_FIELD_VISIT. */
	public static final String ACM_IHM_FIELD_VISIT = "IHM_FIELD_VISIT";

	/** The Constant ACM_IHM_CHECK_GUARANTOR. */
	public static final String ACM_IHM_CHECK_GUARANTOR = "IHM_CHECK_GUARANTOR";

	/** The Constant ACM_IHM_CHECK_COLLATERAL. */
	public static final String ACM_IHM_CHECK_COLLATERAL = "IHM_CHECK_COLLATERAL";

	/** The Constant ACM_IHM_FINANCIAL_ANALYSIS. */
	public static final String ACM_IHM_FINANCIAL_ANALYSIS = "IHM_FINANCIAL_ANALYSIS";

	/** The Constant ACM_IHM_CALENDAR. */
	public static final String ACM_IHM_CALENDAR = "IHM_CALENDAR";

	/** The Constant ACM_IHM_CUSTOMER_DECISION. */
	public static final String ACM_IHM_CUSTOMER_DECISION = "IHM_CUSTOMER_DECISION";

	/** The Constant ACM_IHM_UPLOAD_DOCUMENT. */
	public static final String ACM_IHM_UPLOAD_DOCUMENT = "IHM_UPLOAD_DOCUMENT";

	/** The Constant ACM_IHM_TASK. */
	public static final String ACM_IHM_TASK = "IHM_TASK";

	/** The Constant ACM_IHM_LOAN_APPROVAL. */
	public static final String ACM_IHM_LOAN_APPROVAL = "IHM_LOAN_APPROVAL";

	/** The Constant ACM_IHM_ADD_DOCUMENT. */
	public static final String ACM_IHM_ADD_DOCUMENT = "IHM_ADD_DOCUMENT";

	/** The Constant ACM_IHM_CUSTOMER_NOTES. */
	public static final String ACM_IHM_CUSTOMER_NOTES = "IHM_CUSTOMER_NOTES";

	/** The Constant ACM_IHM_UPLOAD_SIGNED_AGREEMENT. */
	public static final String ACM_IHM_UPLOAD_SIGNED_AGREEMENT = "IHM_UPLOAD_SIGNED_AGREEMENT";

	/** The Constant ACM_IHM_NOTIFICATIONS. */
	public static final String ACM_IHM_NOTIFICATIONS = "IHM_NOTIFICATIONS";

	/** The Constant ACM_IHM_FIND_DOCUMENT. */
	public static final String ACM_IHM_FIND_DOCUMENT = "IHM_FIND_DOCUMENT";

	/** The Constant ACM_IHM_LOAN_REVIEW. */
	public static final String ACM_IHM_LOAN_REVIEW = "IHM_LOAN_REVIEW";

	/** The Constant ACM_IHM_LOAN_ASSIGN. */
	public static final String ACM_IHM_LOAN_ASSIGN = "IHM_LOAN_ASSIGN";

	/** The Constant ACM_IHM_SETTING. */
	public static final String ACM_IHM_SETTING = "IHM_SETTING";

	/** The Constant ACM_IHM_ONLINE_APPLICATION. */
	public static final String ACM_IHM_ONLINE_APPLICATION = "IHM_ONLINE_APPLICATION";

	/** The Constant ACM_IHM_ONLINE_APPLICATION_INFO. */
	public static final String ACM_IHM_ONLINE_APPLICATION_INFO = "IHM_ONLINE_APPLICATION_INFO";

	/** The Constant ACM_IHM_CUSTOMER_MANAGEMENT. */
	public static final String ACM_IHM_CUSTOMER_MANAGEMENT = "IHM_CUSTOMER_MANAGEMENT";

	/** The Constant ACM_IHM_ANALYTICS. */
	public static final String ACM_IHM_ANALYTICS = "IHM_ANALYTICS";

	/** The Constant ACM_IHM_LOAN_MANAGEMENT. */
	public static final String ACM_IHM_LOAN_MANAGEMENT = "IHM_LOAN_MANAGEMENT";

	/** The Constant ACM_IHM_SCREENING. */
	public static final String ACM_IHM_SCREENING = "IHM_SCREENING";

	/*
	 * NOTIFICATION
	 */
	/** The Constant ACM_NOTIFICATION_ACTION_ADD. */
	public static final String ACM_NOTIFICATION_ACTION_ADD = "NEW APPLICATION";

	/** The Constant ACM_NOTIFICATION_ACTION_SUBMITTED. */
	public static final String ACM_NOTIFICATION_ACTION_SUBMITTED = "SUBMITTED FOR APPROVAL";

	/** The Constant ACM_NOTIFICATION_ACTION_APPROVE. */
	public static final String ACM_NOTIFICATION_ACTION_APPROVE = "APPROVED";

	/** The Constant ACM_NOTIFICATION_ACTION_REJECT. */
	public static final String ACM_NOTIFICATION_ACTION_REJECT = "REJECT";

	/** The Constant ACM_NOTIFICATION_ACTION_DECLINE. */
	public static final String ACM_NOTIFICATION_ACTION_DECLINE = "DECLINE";

	/** The Constant ACM_NOTIFICATION_ACTION_CANCEL. */
	public static final String ACM_NOTIFICATION_ACTION_CANCEL = "CANCEL";

	/** The Constant ACM_NOTIFICATION_ACTION_REVEIW. */
	public static final String ACM_NOTIFICATION_ACTION_REVEIW = "REVIEW";

	/** The Constant ACM_NOTIFICATION_ACTION_AUDIT. */
	public static final String ACM_NOTIFICATION_ACTION_AUDIT = "AUDIT";

	/** The Constant ACM_NOTIFICATION_ACTION_RISK. */
	public static final String ACM_NOTIFICATION_ACTION_RISK = "RISK";

	/** The Constant ACM_NOTIFICATION_READY_FOR_DISBURSMENT. */
	public static final String ACM_NOTIFICATION_READY_FOR_DISBURSMENT = "READY FOR DISBURSMENT";

	/** The Constant ACM_NOTIFICATION_CLIENT_ASK_REVIEW. */
	public static final String ACM_NOTIFICATION_CLIENT_ASK_REVIEW = "CLIENT ASKED FOR REVIEW";

	/** The Constant ACM_NOTIFICATION_ACTION_REASSIGN. */
	public static final String ACM_NOTIFICATION_ACTION_REASSIGN = "REASSIGN";

	/** The Constant ACM_NOTIFICATION_ACTION_ASSIGN_IB. */
	public static final String ACM_NOTIFICATION_ACTION_ASSIGN_IB = "ASSIGN_IB";

	/** The Constant ACM_NOTIFICATION_ACTION_ISSUED. */
	public static final String ACM_NOTIFICATION_ACTION_ISSUED = "ISSUED";

	/** The Constant ACM_NOTIFICATION_ACTION_ASSIGN_EXPENSES. */
	public static final String ACM_NOTIFICATION_ACTION_ASSIGN_EXPENSES = "ASSIGN EXPENSES";

	/** The Constant ACM_NOTIFICATION_ACTION_REJECT_EXPENSES. */
	public static final String ACM_NOTIFICATION_ACTION_REJECT_EXPENSES = "REJECT EXPENSES";

	/** The Constant ACM_NOTIFICATION_ACTION_ACCEPT_EXPENSES. */
	public static final String ACM_NOTIFICATION_ACTION_ACCEPT_EXPENSES = "ACCEPT EXPENSES";

	/** The Constant ACM_NOTIFICATION_ACTION_UPLOAD_SIGNED_AGREEMENT. */
	public static final String ACM_NOTIFICATION_ACTION_UPLOAD_SIGNED_AGREEMENT =
			"UPLOAD SIGNED AGREEMENT";

	/** The Constant ACM_NOTIFICATION_ACTION_CENTRAL_REVISION_2. */
	public static final String ACM_NOTIFICATION_ACTION_CENTRAL_REVISION_2 = "CENTRAL REVISION 2";

	/** The Constant ACM_NOTIFICATION_ACTION_CUSTOMER_DECISION. */
	public static final String ACM_NOTIFICATION_ACTION_CUSTOMER_DECISION = "CUSTOMER DECISION";

	/** The Constant ACM_NOTIFICATION_CONDITIONNAL_APPROVE. */
	public static final String ACM_NOTIFICATION_CONDITIONNAL_APPROVE = "CONDITIONNAL APPROVE";

	/** The Constant ACM_NOTIFICATION_SENT_MEZA_CARD. */
	public static final String ACM_NOTIFICATION_SENT_MEZA_CARD = "MEZA CARD SENT";

	/** The Constant ACM_NOTIFICATION_ACTION_COMPLIANCE. */
	public static final String ACM_NOTIFICATION_ACTION_COMPLIANCE = "COMPLIANCE";

	/** The Constant ACM_NOTIFICATION_EXCEPTION_REQUEST_NEW. */
	public static final String ACM_NOTIFICATION_EXCEPTION_REQUEST_NEW = "NEW";

	/** The Constant ACM_NOTIFICATION_EXCEPTION_REQUEST_CANCELLED. */
	public static final String ACM_NOTIFICATION_EXCEPTION_REQUEST_CANCELLED = "CANCELLED";

	/** The Constant ACM_NOTIFICATION_EXCEPTION_REQUEST_ACCEPTED. */
	public static final String ACM_NOTIFICATION_EXCEPTION_REQUEST_ACCEPTED = "ACCEPTED";

	/** The Constant ACM_NOTIFICATION_EXCEPTION_REQUEST_REJECTED. */
	public static final String ACM_NOTIFICATION_EXCEPTION_REQUEST_REJECTED = "REJECTED";

	/*
	 * TIME REMINDER / LOAN CATEGORY : INSTANCE / WARNING / ALERT
	 */
	/** The Constant CATEGORY_INSTANCE. */
	public static final String CATEGORY_INSTANCE = "INSTANCE";

	/** The Constant CATEGORY_WARNING. */
	public static final String CATEGORY_WARNING = "WARNING";

	/** The Constant CATEGORY_ALTERT. */
	public static final String CATEGORY_ALTERT = "ALERT";

	/** The Constant CATEGORY_REMINDER_GENERIC. */
	public static final String CATEGORY_REMINDER_GENERIC = "REMINDER_GENERIC";

	/*
	 * KEY
	 */
	/** The Constant key license . */
	public static final String ACM_KEY = "KEY_LICENCE";

	/** The Constant TRANSFORMATION. */
	public static final String TRANSFORMATION = "RSA/ECB/OAEPWithSHA-256AndMGF1Padding";

	/** The Constant EXPIRYDATE. */
	public static final String EXPIRYDATE = "EXPIRYDATE";

	/** The Constant MAX_ACTIVE_USER. */
	public static final String MAX_ACTIVE_USER = "MAXACTIVEUSER";

	/** The Constant MAX_ACTIVE_USER. */
	public static final String OWNER_LICENCE = "OWNER";

	/** The Constant SIMULTANIOUS_USER. */
	public static final String SIMULTANIOUS_USER = "SIMULTANIOUSUSER";

	/** The Constant MAC. */
	public static final String MAC = "MAC";

	/** The Constant DEFAULT_MAC. */
	public static final String DEFAULT_MAC = "11-22-33-44-55-60";

	/** The Constant CODE_HABILITATION_SETTING. */
	public static final String CODE_HABILITATION_SETTING = "IHM_SETTING";

	/*
	 * Notification Setting
	 */
	/** The Constant REMINDER. */
	public static final String REMINDER = "REMINDER";

	/** The Constant WORKFLOW. */
	public static final String WORKFLOW = "WORKFLOW";

	/** The Constant EMAIL. */
	public static final String EMAIL = "EMAIL";

	/** The Constant SMS. */
	public static final String SMS = "SMS";

	/** The Constant NOTIF. */
	public static final String NOTIF = "NOTIF";

	/*
	 * EMAIL
	 */
	/** The Constant LINK_LABEL. */
	public static final String LINK_LABEL = "Access to Loan";

	/** The Constant DEFAULT_GROUPE. */
	public static final String DEFAULT_GROUPE = "ACM_DEFAULT_GROUP";

	/** The Constant RELATION_GUARANTOR. */
	public static final String RELATION_GUARANTOR = "GUARANTOR";

	/*
	 * IB lOAN STATUS
	 */
	/** The Constant STATUT_IB_INIT. */
	public static final Integer STATUT_IB_INIT = 0;

	/** The Constant STATUT_IB_REJECT. */
	public static final Integer STATUT_IB_REJECT = -1;

	/** The Constant STATUT_IB_ACCEPT. */
	public static final Integer STATUT_IB_ACCEPT = 1;

	/*
	 * ROUND TYPE
	 */
	/** The Constant ROUND_UP. */
	public static final String ROUND_UP = "UP";

	/** The Constant ROUND_DOWN. */
	public static final String ROUND_DOWN = "DOWN";

	/** The Constant USER_GROUPE_BRANCH_OPERATION. */
	public static final String USER_GROUPE_BRANCH_OPERATION = "BRANCH_OPERATION";

	/** The Constant ACTION_INSERT. */
	public static final String ACTION_INSERT = "I";

	/** The Constant ACTION_UPDATE. */
	public static final String ACTION_UPDATE = "U";

	/** The Constant ACTION_DELETE. */
	public static final String ACTION_DELETE = "D";

	/** The Constant SEAT_ADDRESS. */
	public static final String SEAT_ADDRESS = "SEAT_ADDRESS";

	/** The Constant I_SCORE_HEADER_1. */
	public static final String I_SCORE_HEADER_1 = "HDHD|MF00180001|0000000000|";

	/** The Constant I_SCORE_HEADER_2. */
	public static final String I_SCORE_HEADER_2 = "|000000|001\r\n";

	/** The Constant HEAD_OFFICE. */
	public static final String HEAD_OFFICE = "Head Office";

	/** The Constant BRANCH_OPERATION. */
	public static final String BRANCH_OPERATION = "BRANCH_OPERATION";

	/** The Constant CENTRAL_REVISION. */
	public static final String CENTRAL_REVISION = "CENTRAL_REVISION";

	/** The Constant BRANCH_AUDITOR. */
	public static final String BRANCH_AUDITOR = "BRANCH_AUDITOR";

	/** The Constant COMPLIANCE_GROUP. */
	public static final String COMPLIANCE_GROUP = "COMPLIANCE_GROUP";

	/** The Constant RISK_MANAGER. */
	public static final String RISK_MANAGER = "RISK_MANAGER";

	/*
	 * UDF Constante
	 */
	/** The Constant UDF_MEZA_CARD_INTERNAL. */
	public static final String UDF_MEZA_CARD_INTERNAL = "MezaCard Internal";

	/** The Constant UDF_CODE_NATIONALITY. */
	public static final String UDF_CODE_NATIONALITY = "Customer Nationality";

	/** The Constant APPROVE_EXCEPTIONS. */
	public static final String APPROVE_EXCEPTIONS = "APPROVE_EXCEPTIONS";

	/** The Constant CUSTOMER_CATEGORY. */
	public static final String CUSTOMER_CATEGORY = "CUSTOMER";

	/** The Constant COLLATERAL_CATEGORY. */
	public static final String COLLATERAL_CATEGORY = "COLLATERAL";

	/** The Constant SUPPLIER_CATEGORY. */
	public static final String SUPPLIER_CATEGORY = "SUPPLIER";

	/** The Constant ERROR_RESPONSE. */
	public static final String ERROR_RESPONSE = "ErrorResponse";

	/*
	 * SYNCHRONIZE BRANCHES AND PORTFOLIOS
	 */
	/** The Constant LAST_BRANCH_CHANGED_ID_SYNCHRONIZED. */
	public static final String LAST_BRANCH_CHANGED_ID_SYNCHRONIZED =
			"LAST_BRANCH_CHANGED_ID_SYNCHRONIZED";
	/*
	 * LOAN APPLICATION STATUS constantes
	 */
	/** The Constant TOPUP. */
	public static final String TOPUP = "TOPUP";

	/** The Constant REFINANCE. */
	public static final String REFINANCE = "REFINANCE";

	/** The Constant NEW_APPLICATION. */
	public static final String NEW_APPLICATION = "NEW_APPLICATION";

	/** The Constant LAST_CUACCOUNT_PORTFOLIO_TRANSFERID. */
	public static final String LAST_CUACCOUNT_PORTFOLIO_TRANSFERID =
			"LAST_CUACCOUNT_PORTFOLIO_TRANSFERID";

	/** The Constant SYNCHRONIZE_PORTFOLIOS. */
	public static final String SYNCHRONIZE_PORTFOLIOS = "SYNCHRONIZE_PORTFOLIOS";

	/** The Constant SYNCHRONIZE_BRANCHES. */
	public static final String SYNCHRONIZE_BRANCHES = "SYNCHRONIZE_BRANCHES";

	/** The Constant UDF_JOB. */
	public static final String UDF_JOB = "Job";

	/** The Constant UDF_FAMILY_SITUATION. */
	public static final String UDF_FAMILY_SITUATION = "Family situation";

	/** The Constant UDF_PLACE_OF_ISSUE. */
	public static final String UDF_PLACE_OF_ISSUE = "Place of issue";

	/** The Constant UDF_ISSUE_DATE. */
	public static final String UDF_ISSUE_DATE = "Add Issue Date";

	/** The Constant DUE_DATE. */
	public static final String DUE_DATE = "Due Date";

	/** The Constant PREVIOUS_ACTION_COMPLETE_DATE. */
	public static final String PREVIOUS_ACTION_COMPLETE_DATE = "Pervious action complete date";

	/*
	 * Collection & Legal constants
	 */

	/** The Constant COLLECTION_CATEGORY. */
	public static final String COLLECTION_CATEGORY = "COLLECTION";

	/** The Constant LEGAL_CATEGORY. */
	public static final String LEGAL_CATEGORY = "LEGAL";

	/*
	 * Calendar type event
	 */

	/** The Constant STEP_TASK_TYPE. */
	public static final String STEP_TASK_TYPE = "step_task";

	/** The Constant LOAN. */
	public static final String LOAN_CATEGORY = "LOAN";

	/** The Constant LOAN. */
	public static final String ITEM_CATEGORY = "ITEM";

	/** The Constant PROSPECT_CATEGORY. */
	public static final String PROSPECT_CATEGORY = "PROSPECT";

	/** The Constant UNASSIGNED. */
	public static final String UNASSIGNED = "UNASSIGNED";

	/** The Constant REMINDER_COLLECTION_CATEGORY. */
	public static final String REMINDER_COLLECTION_CATEGORY = "REMINDER_COLLECTION";

	/** The Constant REMINDER_SUP_COLLECTION_CATEGORY. */
	public static final String REMINDER_SUP_COLLECTION_CATEGORY = "REMINDER_SUP_COLLECTION";

	/** The Constant GLOBAL_SETTING_NOTIF. */
	public static final String GLOBAL_SETTING_NOTIF = "GLOBAL";

	/** The Constant SYNCHRO_CALENDAR_SETTING. */
	public static final String SYNCHRO_CALENDAR_SETTING = "CALENDAR";

	/** The Constant YAKEEN_API. */
	public static final String YAKEEN_API = "Yakeen";

	/** The Constant DAKHLI_API. */
	public static final String DAKHLI_API = "Dakhli";

	/** The Constant ABSHER. */
	public static final String ABSHER = "Absher";

	/** The Constant NAFITH. */
	public static final String NAFITH = "Nafith";

	/** The Constant SCORE_RIES_API. */
	public static final String SCORE_RIES_API = "ScoreRies";

	/** The Constant MOURABHA_API. */
	public static final String MOURABHA_API = "Mourabha";

	/** The Constant MOBISHASTRA_API. */
	public static final String MOBISHASTRA_API = "Mobishastra";

	/** The Constant E_SIGNATURE_SANAD_EL_AMER_API. */
	public static final String E_SIGNATURE_SANAD_EL_AMER_API = "E-Signature Sanad El Amer";

	/** The Constant E_SIGNATURE_AGREEMENT_API. */
	public static final String E_SIGNATURE_AGREEMENT_API = "E-Signature Agreement";

	/** The Constant NAFEDH_API. */
	public static final String NAFEDH_API = "Nafedh";

	/** The Constant SIMAH_API. */
	public static final String SIMAH_API = "Simah";

	/** The Constant AML_API. */
	public static final String AML_API = "Aml";

	/** The Constant PAYMENT_API. */
	public static final String PAYMENT_API = "Payment";

	/** The Constant MOURABHA_SELL_API. */
	public static final String MOURABHA_SELL_API = "Mourabha Sell";

	/** The Constant MOURABHA_BUY_API. */
	public static final String MOURABHA_BUY_API = "Mourabha Buy";

	/** The Constant IB_IHM_ROOT_SIGN_CONTRACT. */
	public static final String IB_IHM_ROOT_SIGN_CONTRACT = "sign-contract";

	/** The Constant MOURABHA_TRANSFER_NOTICE_API. */
	public static final String MOURABHA_TRANSFER_NOTICE_API = "Mourabha Transfer Notice";

	/** The Constant GENERIC_WORKFLOW_WORKFLOW_PROCESS. */
	public static final String GENERIC_WORKFLOW_WORKFLOW_PROCESS = "GENERIC_WORKFLOW";
	/** The Constant CUSTOMER_STATUS_REIS. */
	public static final String CUSTOMER_STATUS_REIS = "BLOCKED";

	/** The Constant MOFEED_API. */
	public static final String MOFEED_API = "Masdr Mofeed";

	/** The Constant MANUAL_ENTRY. */
	public static final String MANUAL_ENTRY = "Manual entry";

	/** The Constant PERSONNAL_CONTRIBUTION. */
	public static final String PERSONNAL_CONTRIBUTION = "Personnal contribution";

	/** The Constant LOAN_AMOUNT. */
	public static final String LOAN_AMOUNT = "Loan Amount";

	/** The Constant FIXED_AMOUNT. */
	public static final String FIXED_AMOUNT = "Fixed Amount";

}
