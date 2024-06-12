/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link CompanySME} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class CompanySME {

	/** The d PID. */
	@JsonProperty("DPID")
	public String dPID;

	/** The category DESC. */
	@JsonProperty("Category_DESC")
	public String category_DESC;

	/** The category. */
	@JsonProperty("Category")
	public String category;

	/** The bussiness activity 1 DESC. */
	@JsonProperty("BussinessActivity1_DESC")
	public String bussinessActivity1_DESC;

	/** The bussiness activity 1. */
	@JsonProperty("BussinessActivity1")
	public String bussinessActivity1;

	/** The bussiness activity 2 DESC. */
	@JsonProperty("BussinessActivity2_DESC")
	public String bussinessActivity2_DESC;

	/** The bussiness activity 2. */
	@JsonProperty("BussinessActivity2")
	public String bussinessActivity2;

	/** The paid capital. */
	@JsonProperty("PaidCapital")
	public String paidCapital;

	/** The turn over. */
	@JsonProperty("TurnOver")
	public String turnOver;

	/** The total assets. */
	@JsonProperty("TotalAssets")
	public String totalAssets;

	/** The no of employee. */
	@JsonProperty("NoOfEmployee")
	public String noOfEmployee;

	/** The established date. */
	@JsonProperty("EstablishedDate")
	public String establishedDate;

	/** The b A start date. */
	@JsonProperty("BAStartDate")
	public String bAStartDate;

	/** The b A end date. */
	@JsonProperty("BAEndDate")
	public String bAEndDate;

	/** The financial statement date. */
	@JsonProperty("FinancialStatementDate")
	public String financialStatementDate;

	/** The exdng max M turnover date. */
	@JsonProperty("ExdngMaxMTurnoverDate")
	public String exdngMaxMTurnoverDate;

	/** The date reported. */
	@JsonProperty("DateReported")
	public String dateReported;

	/**
	 * Instantiates a new company SME.
	 */
	public CompanySME() {

		// EMPTY
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "CompanySME [" + (dPID != null ? "dPID=" + dPID + ", " : "")
				+ (category_DESC != null ? "category_DESC=" + category_DESC + ", " : "")
				+ (category != null ? "category=" + category + ", " : "")
				+ (bussinessActivity1_DESC != null
						? "bussinessActivity1_DESC=" + bussinessActivity1_DESC + ", "
						: "")
				+ (bussinessActivity1 != null ? "bussinessActivity1=" + bussinessActivity1 + ", "
						: "")
				+ (bussinessActivity2_DESC != null
						? "bussinessActivity2_DESC=" + bussinessActivity2_DESC + ", "
						: "")
				+ (bussinessActivity2 != null ? "bussinessActivity2=" + bussinessActivity2 + ", "
						: "")
				+ (paidCapital != null ? "paidCapital=" + paidCapital + ", " : "")
				+ (turnOver != null ? "turnOver=" + turnOver + ", " : "")
				+ (totalAssets != null ? "totalAssets=" + totalAssets + ", " : "")
				+ (noOfEmployee != null ? "noOfEmployee=" + noOfEmployee + ", " : "")
				+ (establishedDate != null ? "establishedDate=" + establishedDate + ", " : "")
				+ (bAStartDate != null ? "bAStartDate=" + bAStartDate + ", " : "")
				+ (bAEndDate != null ? "bAEndDate=" + bAEndDate + ", " : "")
				+ (financialStatementDate != null
						? "financialStatementDate=" + financialStatementDate + ", "
						: "")
				+ (exdngMaxMTurnoverDate != null
						? "exdngMaxMTurnoverDate=" + exdngMaxMTurnoverDate + ", "
						: "")
				+ (dateReported != null ? "dateReported=" + dateReported : "") + "]";
	}

}
