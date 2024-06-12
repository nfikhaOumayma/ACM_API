/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */

package com.acm.constants.common;

/**
 * {@link ACMConstantWorkflowStatuts} class. Annexe Workflow, statuts et Situations
 * 
 * @author HaythemBenizid
 * @since 0.6.0
 */
public final class ACMConstantWorkflowStatuts implements java.io.Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -6970022578295420043L;

	/*
	 * ### PROCESS WORKFLOW NAME ###
	 */
	/** The Constant PROCESS_VALIDATION_DEMANDE. */
	public static final String PROCESS_VALIDATION_LOAN = "process_loan_final";

	/** The Constant PROCESS_VALIDATION_LOAN for client TAMKEEN. */
	public static final String PROCESS_VALIDATION_LOAN_TAMKEEN = "process_loan_TAMKEEN_new";

	/*
	 * ### LOAN SATATUS ###
	 */
	/** The Constant INITIAL_CHECK. */
	public static final String INITIAL_CHECK = "1 , Initial Check";

	/** The Constant FIELD_VISIT. */
	public static final String FIELD_VISIT = "2 , Field visit";

	/** The Constant GUARANTOR. */
	public static final String GUARANTOR = "3 , Guarantor";

	/** The Constant COLLATERAL. */
	public static final String COLLATERAL = "4 , Collateral";

	/** The Constant ADD_DOCUMENTS. */
	public static final String ADD_DOCUMENTS = "5 , Add Documents";

	/** The Constant FINANCIAL_ANALYSIS. */
	public static final String FINANCIAL_ANALYSIS = "6 , Financial Analysis";

	/** The Constant APPROVAL_L1. */
	public static final String APPROVAL_L1 = "7 , Approval L1";

	/** The Constant APPROVAL_L2. */
	public static final String APPROVAL_L2 = "8 , Approval L2";

	/** The Constant APPROVAL_L3. */
	public static final String APPROVAL_L3 = "9 , Approval L3";

	/** The Constant APPROVAL_L4. */
	public static final String APPROVAL_L4 = "10 , Approval L4";

	/** The Constant CUSTOMER_DECISION. */
	public static final String CUSTOMER_DECISION = "11 , Customer Decision";

	/** The Constant UPLOAD_SIGNED_AGREEMENT. */
	public static final String UPLOAD_SIGNED_AGREEMENT = "12 , Docs Signing";

	/** The Constant DISBURSEMENT_CASE_CLOSURE. */
	public static final String DISBURSEMENT_CASE_CLOSURE = "13 , Disbursement & Case Closure";

	/** The Constant REJECTED. */
	public static final String REJECTED = "14 , Rejected";

	/** The Constant CANCELLED. */
	public static final String CANCELLED = "15 , Cancelled";

	/** The Constant REVIEW. */
	public static final String REVIEW = "16 , Review";

	/** The Constant REVIEW. */
	public static final String DECLINE = "17 , Declined";

	/** The Constant SCREENING. */
	public static final String SCREENING = "18 , Screening";

	/** The Constant AUDIT. */
	public static final String AUDIT = "19 , Audit";

	/** The Constant RISK. */
	public static final String RISK = "20 , Risk";

	/** The Constant UPDATE_LOAN_DATA. */
	public static final String UPDATE_LOAN_DATA = "21 , Complete Loan Data";

	/** The Constant ISSUED. */
	public static final String ISSUED = "22 , Issued";

	/** The Constant CENTRAL_REVISION. */
	public static final String CENTRAL_REVISION = "23 , Central Revision";

	/*
	 * ### LOAN SATATUS ###
	 */
	/** The Constant STATUS_TAB_NEW. */
	public static final String STATUS_TAB_NEW = "1 , New";
	/** The Constant STATUS_TAB_DRAFTS. */
	public static final String STATUS_TAB_DRAFTS = "2 , Drafts";
	/** The Constant STATUS_TAB_PENDING_APPROVAL. */
	public static final String STATUS_TAB_PENDING_APPROVAL = "3 , PendingApproval";
	/** The Constant STATUS_TAB_APPROVED. */
	public static final String STATUS_TAB_APPROVED = "4 , Approved";
	/** The Constant STATUS_TAB_REJECTED. */
	public static final String STATUS_TAB_REJECTED = "5 , Rejected";
	/** The Constant STATUS_TAB_CANCELLED. */
	public static final String STATUS_TAB_CANCELLED = "6 , Cancelled";
	/** The Constant STATUS_TAB_CORRECTIFS. */
	public static final String STATUS_TAB_REVIEW = "7 , Review";
	/** The Constant STATUS_TAB_ISSUED. */
	public static final String STATUS_TAB_ISSUED = "8 , Issued";
	/*
	 * ### WORKFLOW SCHEMA ACTION ###
	 */
	/** The Constant ACTION_ACCEPTED. */
	public static final String ACTION_ACCEPTED = "accepted";
	/** The Constant ACTION_REJECTED. */
	public static final String ACTION_REJECTED = "rejected";
	/** The Constant ACTION_FIELD_VISIT. */
	public static final String ACTION_FIELD_VISIT = "field_visit";
	/** The Constant ACTION_GUARANTOR_ONLY. */
	public static final String ACTION_GUARANTOR_ONLY = "guarantor_only";
	/** The Constant ACTION_GUARANTOR_COLLATERAL. */
	public static final String ACTION_GUARANTOR_COLLATERAL = "guarantor_collateral";
	/** The Constant ACTION_COLLATERAL_ONLY. */
	public static final String ACTION_COLLATERAL_ONLY = "collateral_only";
	/** The Constant ACTION_NOT_GUARANTOR_COLLATERAL. */
	public static final String ACTION_NOT_GUARANTOR_COLLATERAL = "not_guarantor_collateral";
	/** The Constant ACTION_CHECK_DOC_NO. */
	public static final String ACTION_CHECK_DOC_NO = "check_Doc_no";
	/** The Constant ACTION_CHECK_DOC_YES. */
	public static final String ACTION_CHECK_DOC_YES = "check_Doc_yes";
	/** The Constant ACTION_REVIEW. */
	public static final String ACTION_REVIEW = "review";
	/** The Constant ACTION_APPROVED. */
	public static final String ACTION_APPROVED = "approved";
	/** The Constant ACTION_NEXT_APPROVE_LEVEL. */
	public static final String ACTION_NEXT_APPROVE_LEVEL = "next_approve_level";
	/** The Constant ACTION_MISSING_APPROVEL_PROCESS_CONFIG. */
	public static final String ACTION_MISSING_APPROVEL_PROCESS_CONFIG = "missing_approvel_process";
	/** The Constant ACTION_CUSTOMER_AGREED_YES. */
	public static final String ACTION_CUSTOMER_AGREED_YES = "customer_agreed_yes";
	/** The Constant ACTION_CUSTOMER_AGREED_NO. */
	public static final String ACTION_CUSTOMER_AGREED_NO = "customer_agreed_no";
	/** The Constant ACTION_DATA_VALID_YES. */
	public static final String ACTION_DATA_VALID_YES = "data_valid_yes";
	/** The Constant ACTION_DATA_VALID_NO. */
	public static final String ACTION_DATA_VALID_NO = "data_valid_no";
	/** The Constant ACTION_CHECK_SCREENING_NO. */
	public static final String ACTION_CHECK_SCREENING_NO = "check_screening_no";
	/** The Constant ACTION_CHECK_SCREENING_YES. */
	public static final String ACTION_CHECK_SCREENING_YES = "check_screening_yes";
	/** The Constant ACTION_CHECK_SCREENING. */
	public static final String ACTION_CHECK_SCREENING = "check_screening";
	/** The Constant REQUIRED_FIELD_VISIT. */
	public static final String ACTION_REQUIRED_FIELD_VISIT = "required_field_visit";
	/** The Constant NOT_REQUIRED_FIELD_VISIT. */
	public static final String ACTION_NOT_REQUIRED_FIELD_VISIT = "not_required_field_visit";
	/** The Constant AUDIT_ONLY. */
	public static final String ACTION_AUDIT_ONLY = "audit_only";
	/** The Constant NOT_AUDIT_RISK. */
	public static final String ACTION_NOT_AUDIT_RISK = "not_audit_risk";
	/** The Constant AUDIT_RISK. */
	public static final String ACTION_AUDIT_RISK = "audit_risk";
	/** The Constant RISK_ONLY. */
	public static final String ACTION_RISK_ONLY = "risk_only";
	/** The Constant CHECK_AUDIT_RISK. */
	public static final String ACTION_CHECK_AUDIT_RISK = "check_audit_risk";
	/** The Constant ACTION_BACK_BY_STEP. */
	public static final String ACTION_BACK_BY_STEP = "back_by_step";
	/** The Constant ACTION_BACK_BY_STEP. */
	public static final String ACTION_APPROVE_UPLOAD_SIGNED_AGREEMENTS =
			"approve_upload_signed_agreements";

	/*
	 * ### WORKFLOW REQUEST ACTION (AND USED TO BUTTON HABILITATION)###
	 */
	/** The Constant WORKFLOW_REQUEST_ACTION_PASS. */
	public static final String WORKFLOW_REQUEST_ACTION_PASS = "PASS";
	/** The Constant WORKFLOW_REQUEST_ACTION_NEXT. */
	public static final String WORKFLOW_REQUEST_ACTION_NEXT = "NEXT";
	/** The Constant WORKFLOW_REQUEST_ACTION_REJECT. */
	public static final String WORKFLOW_REQUEST_ACTION_REJECT = "REJECT";
	/** The Constant WORKFLOW_REQUEST_ACTION_ASK_FOR_REVIEW. */
	public static final String WORKFLOW_REQUEST_ACTION_ASK_FOR_REVIEW = "ASK_FOR_REVIEW";
	/** The Constant WORKFLOW_REQUEST_ACTION_AGREED. */
	public static final String WORKFLOW_REQUEST_ACTION_AGREED = "AGREED";
	/** The Constant WORKFLOW_REQUEST_ACTION_DECLINED. */
	public static final String WORKFLOW_REQUEST_ACTION_DECLINED = "DECLINED";
	/** The Constant WORKFLOW_REQUEST_ACTION_SUBMIT. */
	public static final String WORKFLOW_REQUEST_ACTION_SUBMIT = "SUBMIT";
	/** The Constant WORKFLOW_REQUEST_ACTION_REVIEW. */
	public static final String WORKFLOW_REQUEST_ACTION_REVIEW = "REVIEW";
	/** The Constant WORKFLOW_REQUEST_ACTION_RECOMMEND_AUDIT. */
	public static final String WORKFLOW_REQUEST_ACTION_RECOMMEND_AUDIT = "RECOMMEND_AUDIT";
	/** The Constant WORKFLOW_REQUEST_ACTION_RECOMMEND_RISK. */
	public static final String WORKFLOW_REQUEST_ACTION_RECOMMEND_RISK = "RECOMMEND_RISK";
	/** The Constant WORKFLOW_REQUEST_ACTION_AGREED_UPLOAD_SIGNED_AGREEMENT. */
	public static final String WORKFLOW_REQUEST_ACTION_AGREED_UPLOAD_SIGNED_AGREEMENT = "AGREED_UPLOAD_SIGNED_AGREEMENT";
	/** The Constant WORKFLOW_REQUEST_CUSTOMER_DECISION. */
	public static final String WORKFLOW_REQUEST_CUSTOMER_DECISION = "CUSTOMER_DECISION";
	/** The Constant WORKFLOW_REQUEST_ACTION_APPROVE. */
	public static final String WORKFLOW_REQUEST_ACTION_APPROVE = "APPROVE";
	/** The Constant WORKFLOW_REQUEST_ACTION_CANCELED. */
	public static final String WORKFLOW_REQUEST_ACTION_CANCELED = "CANCELED";
	/** The Constant WORKFLOW_REQUEST_ACTION_SCREENING. */
	public static final String WORKFLOW_REQUEST_ACTION_SCREENING = "SCREENING";
	/** The Constant WORKFLOW_REQUEST_ACTION_AUDIT. */
	public static final String WORKFLOW_REQUEST_ACTION_AUDIT = "AUDIT";
	/** The Constant WORKFLOW_REQUEST_ACTION_RISK. */
	public static final String WORKFLOW_REQUEST_ACTION_RISK = "RISK";
	/** The Constant WORKFLOW_REQUEST_ACTION_BACK. */
	public static final String WORKFLOW_REQUEST_ACTION_REVIEW_AGREEMENT = "REVIEW_AGREEMENT";
	/** The Constant WORKFLOW_REQUEST_ACTION_ISSUED. */
	public static final String WORKFLOW_REQUEST_ACTION_ISSUED = "ISSUED";

	/*
	 * ### CUSTOMER DESICION STATUS ###
	 */
	/** The Constant CUSTOMER_DESICION_STATUS_ASK_REVIEW. */
	public static final String CUSTOMER_DESICION_STATUS_ASK_REVIEW = "1 , Ask for Review";

	/** The Constant CUSTOMER_DESICION_STATUS_DECLIEND. */
	public static final String CUSTOMER_DESICION_STATUS_DECLIEND = "2 , Declined";

	/** The Constant CUSTOMER_DESICION_STATUS_AGREED. */
	public static final String CUSTOMER_DESICION_STATUS_AGREED = "3 , Agreed";

	/** The Constant CUSTOMER_DESICION_STATUS_NOTE. */
	public static final String CUSTOMER_DESICION_STATUS_NOTE = "4 , Note";

	/** The Constant CUSTOMER_DESICION_REVIEW_AGREEMENTS. */
	public static final String CUSTOMER_DESICION_REVIEW_AGREEMENTS = "5 , Review agreements";

	/*
	 * ### LOAN APPROVAL HISTORIQUE STATUS ###
	 */
	/** The Constant LOAN_APPROVAL_STATUS_REVIEW. */
	public static final String LOAN_APPROVAL_STATUS_REVIEW = "1 , Review";

	/** The Constant LOAN_APPROVAL_STATUS_APPROVED. */
	public static final String LOAN_APPROVAL_STATUS_APPROVED = "2 , Approved";

	/** The Constant LOAN_APPROVAL_STATUS_REJECTED. */
	public static final String LOAN_APPROVAL_STATUS_REJECTED = "3 , Rejected";

	/** The Constant LOAN_RISK_AUDIT_STATUS_RECOMMEND. */
	public static final String LOAN_RISK_AUDIT_STATUS_RECOMMEND = "4, Recommend";

	/** The Constant UPDATE_ASSIGNED_TO_CUSTOMER. */
	public static final String UPDATE_ASSIGNED_TO_CUSTOMER = "5 , Update Assign to customer";

	/** The Constant REASSIGN. */
	public static final String LOAN_REASSIGN = "6, Reassign";

	/** The Constant LOAN_APPROVAL_STATUS_CANCELLED. */
	public static final String LOAN_APPROVAL_STATUS_CANCELLED = "7, Cancelled";

	/** The Constant LOAN_REVERSER. */
	public static final String LOAN_REVERSER = "8, Reverser";
	/*
	 * ### STATUS (Setting Loan Document) REQUIRED ###
	 */
	/** The Constant STATUS_REQUIRED. */
	public static final String STATUS_REQUIRED = "1 , Required";

	/** The Constant STATUS_NOT_REQUIRED. */
	public static final String STATUS_NOT_REQUIRED = "2 , Not Required";

	/*
	 * ### LOAN ABACUS STATUS ###
	 */
	/** The Constant LOAN_ABACUS_STATUS_APPLIED : (demande de crédit). */
	public static final String LOAN_ABACUS_STATUS_APPLIED = "1 , Applied";

	/** The Constant LOAN_ABACUS_STATUS_APPROVED : (Crédit approuvé). */
	public static final String LOAN_ABACUS_STATUS_APPROVED = "2 , Approved";

	/** The Constant LOAN_ABACUS_STATUS_ISSUED : (Crédit Décaissé Normale). */
	public static final String LOAN_ABACUS_STATUS_ISSUED = "4 , Issued";

	/**
	 * The Constant LOAN_ABACUS_STATUS_CHARGED_OFF : (Transfert du crédit vers un produit de
	 * créances douteuses).
	 */
	public static final String LOAN_ABACUS_STATUS_CHARGED_OFF = "8 , Charged off";

	/** The Constant LOAN_ABACUS_STATUS_BAD_DEBT : (Créance douteuse). */
	public static final String LOAN_ABACUS_STATUS_BAD_DEBT = "16 , Bad debt";

	/** The Constant LOAN_ABACUS_STATUS_TRANSFERRED : (Transféré vers un autre crédit). */
	public static final String LOAN_ABACUS_STATUS_TRANSFERRED = "32 , Transferred";

	/** The Constant LOAN_ABACUS_STATUS_CANCELLED : (Crédit Annulé). */
	public static final String LOAN_ABACUS_STATUS_CANCELLED = "64 , Cancelled";

	/*
	 * ### CUSTOMER ACCOUNT SCHEDULE STATUS ###
	 */
	/** The Constant CUSTOMER_ACCOUNT_SCHEDULE_STATUS_PAID. */
	public static final String CUSTOMER_ACCOUNT_SCHEDULE_STATUS_PAID = "1 , Payé Totalement";

	/** The Constant CUSTOMER_ACCOUNT_SCHEDULE_STATUS_PARTIALLY_PAID. */
	public static final String CUSTOMER_ACCOUNT_SCHEDULE_STATUS_PARTIALLY_PAID =
			"2 , Payé Partiellement";

	/** The Constant CUSTOMER_ACCOUNT_SCHEDULE_STATUS_NOT_PAID. */
	public static final String CUSTOMER_ACCOUNT_SCHEDULE_STATUS_NOT_PAID = "3 , Non Payé";

	/*
	 * ### CRM : Task STATUS ###
	 */
	/** The Constant TASK_STATUS_DONE. */
	public static final String TASK_STATUS_DONE = "Done";

	/** The Constant TASK_STATUS_NEW. */
	public static final String TASK_STATUS_NEW = "New";

	/** The Constant TASK_STATUS_CANCELED. */
	public static final String TASK_STATUS_CANCELED = "Canceled";
	/** The Constant TASK_STATUS_CLOSED. */
	public static final String TASK_STATUS_CLOSED = "CLOSED";

	/** The Constant EVENT_TYPE_APPOINTEMENT. */
	public static final String EVENT_TYPE_APPOINTEMENT = "appointement";

	/** The Constant EVENT_TYPE_TASK. */
	public static final String EVENT_TYPE_TASK = "task";

	/** The Constant NEXT_ACTION_TASK. */
	public static final String NEXT_ACTION_TASK = "nextActionTask";

	/*
	 * ### Level APPROVAL NUMBER ###
	 */
	/** The Constant LEVEL1. */
	public static final String LEVEL1 = "1 , Approval L1";

	/** The Constant LEVEL2. */
	public static final String LEVEL2 = "2 , Approval L2";

	/** The Constant LEVEL3. */
	public static final String LEVEL3 = "3 , Approval L3";

	/** The Constant LEVEL4. */
	public static final String LEVEL4 = "4 , Approval L4";

	/*
	 * Statut Loan IB
	 */
	/** The Constant INITIAL_ONLINE_APPLICATION. */
	public static final String INITIAL_ONLINE_APPLICATION = "0 , Initial Online Application";

	/** The Constant REJECT_ONLINE_APPLICATION. */
	public static final String REJECT_ONLINE_APPLICATION = "-1 , Reject Online Application";

	/** The Constant ACCEPT_ONLINE_APPLICATION. */
	public static final String ACCEPT_ONLINE_APPLICATION = "1 , Accept Online Application";

	/*
	 * Statut REQUEST EXCEPTION
	 */
	/** The Constant NEW_STATUT_REQUEST. */
	public static final String NEW_STATUT_REQUEST = "0 , NEW";

	/** The Constant ACCEPTED_STATUT_REQUEST. */
	public static final String ACCEPTED_STATUT_REQUEST = "1 , ACCEPTED";

	/** The Constant REJECTED_STATUT_REQUEST. */
	public static final String REJECTED_STATUT_REQUEST = "-1 , REJECTED";

	/** The Constant CANCELLED_STATUT_REQUEST. */
	public static final String CANCELLED_STATUT_REQUEST = "-2 , CANCELLED";

	/** The Constant CLOSED_STATUT_REQUEST. */
	public static final String CLOSED_STATUT_REQUEST = "2, CLOSED";

}
