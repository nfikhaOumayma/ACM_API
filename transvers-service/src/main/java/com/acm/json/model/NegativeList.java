/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link NegativeList} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class NegativeList {

	/** The bank name DESC. */
	@JsonProperty("BankName_DESC")
	public String bankName_DESC;

	/** The bank name. */
	@JsonProperty("BankName")
	public String bankName;

	/** The branch name DESC. */
	@JsonProperty("BranchName_DESC")
	public String branchName_DESC;

	/** The branch name. */
	@JsonProperty("BranchName")
	public String branchName;

	/** The approval date. */
	@JsonProperty("ApprovalDate")
	public String approvalDate;

	/** The facility type DESC. */
	@JsonProperty("FacilityType_DESC")
	public String facilityType_DESC;

	/** The facility type. */
	@JsonProperty("FacilityType")
	public String facilityType;

	/** The limit. */
	@JsonProperty("Limit")
	public String limit;

	/** The balance. */
	@JsonProperty("Balance")
	public String balance;

	/** The customer type DESC. */
	@JsonProperty("CustomerType_DESC")
	public String customerType_DESC;

	/** The customer type. */
	@JsonProperty("CustomerType")
	public String customerType;

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

	/** The last payment date. */
	@JsonProperty("LastPaymentDate")
	public String lastPaymentDate;

	/** The r EPORTE D DATE. */
	@JsonProperty("REPORTED_DATE")
	public String rEPORTED_DATE;

	/**
	 * Instantiates a new negative list.
	 */
	public NegativeList() {

		// EMPTY
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "NegativeList ["
				+ (bankName_DESC != null ? "bankName_DESC=" + bankName_DESC + ", " : "")
				+ (bankName != null ? "bankName=" + bankName + ", " : "")
				+ (branchName_DESC != null ? "branchName_DESC=" + branchName_DESC + ", " : "")
				+ (branchName != null ? "branchName=" + branchName + ", " : "")
				+ (approvalDate != null ? "approvalDate=" + approvalDate + ", " : "")
				+ (facilityType_DESC != null ? "facilityType_DESC=" + facilityType_DESC + ", " : "")
				+ (facilityType != null ? "facilityType=" + facilityType + ", " : "")
				+ (limit != null ? "limit=" + limit + ", " : "")
				+ (balance != null ? "balance=" + balance + ", " : "")
				+ (customerType_DESC != null ? "customerType_DESC=" + customerType_DESC + ", " : "")
				+ (customerType != null ? "customerType=" + customerType + ", " : "")
				+ (mainAction_DESC != null ? "mainAction_DESC=" + mainAction_DESC + ", " : "")
				+ (mainAction != null ? "mainAction=" + mainAction + ", " : "")
				+ (subAction_DESC != null ? "subAction_DESC=" + subAction_DESC + ", " : "")
				+ (subAction != null ? "subAction=" + subAction + ", " : "")
				+ (lastPaymentDate != null ? "lastPaymentDate=" + lastPaymentDate + ", " : "")
				+ (rEPORTED_DATE != null ? "rEPORTED_DATE=" + rEPORTED_DATE : "") + "]";
	}

}
