/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link LegalActionDetail} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class LegalActionDetail {

	/** The bank name DESC. */
	@JsonProperty("BankName_DESC")
	public String bankName_DESC;

	/** The bank name. */
	@JsonProperty("BankName")
	public String bankName;

	/** The branch DESC. */
	@JsonProperty("Branch_DESC")
	public String branch_DESC;

	/** The branch. */
	@JsonProperty("Branch")
	public String branch;

	/** The approval date. */
	@JsonProperty("ApprovalDate")
	public String approvalDate;

	/** The balance. */
	@JsonProperty("Balance")
	public String balance;

	/** The main action DESC. */
	@JsonProperty("MainAction_DESC")
	public String mainAction_DESC;

	/** The main action. */
	@JsonProperty("MainAction")
	public String mainAction;

	/** The sub action DESC. */
	@JsonProperty("SubAction_DESC")
	public String subAction_DESC;

	/** The sub action. */
	@JsonProperty("SubAction")
	public String subAction;

	/** The main action code. */
	@JsonProperty("MainActionCode")
	public String mainActionCode;

	/** The sub action code. */
	@JsonProperty("SubActionCode")
	public String subActionCode;

	/** The l EGA L STATEMEN T date. */
	@JsonProperty("LEGAL_STATEMENT_date")
	public String lEGAL_STATEMENT_date;

	/** The reporting date. */
	@JsonProperty("ReportingDate")
	public String reportingDate;

	/** The l A severity ID. */
	@JsonProperty("LASeverityID")
	public String lASeverityID;

	/**
	 * Instantiates a new legal action detail.
	 */
	public LegalActionDetail() {

		// EMPTY
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "LegalActionDetail ["
				+ (bankName_DESC != null ? "bankName_DESC=" + bankName_DESC + ", " : "")
				+ (bankName != null ? "bankName=" + bankName + ", " : "")
				+ (branch_DESC != null ? "branch_DESC=" + branch_DESC + ", " : "")
				+ (branch != null ? "branch=" + branch + ", " : "")
				+ (approvalDate != null ? "approvalDate=" + approvalDate + ", " : "")
				+ (balance != null ? "balance=" + balance + ", " : "")
				+ (mainAction_DESC != null ? "mainAction_DESC=" + mainAction_DESC + ", " : "")
				+ (mainAction != null ? "mainAction=" + mainAction + ", " : "")
				+ (subAction_DESC != null ? "subAction_DESC=" + subAction_DESC + ", " : "")
				+ (subAction != null ? "subAction=" + subAction + ", " : "")
				+ (mainActionCode != null ? "mainActionCode=" + mainActionCode + ", " : "")
				+ (subActionCode != null ? "subActionCode=" + subActionCode + ", " : "")
				+ (lEGAL_STATEMENT_date != null
						? "lEGAL_STATEMENT_date=" + lEGAL_STATEMENT_date + ", "
						: "")
				+ (reportingDate != null ? "reportingDate=" + reportingDate + ", " : "")
				+ (lASeverityID != null ? "lASeverityID=" + lASeverityID : "") + "]";
	}

}
