/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link Datum} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class Datum {

	/** The d ISCLAIME R CONTENT. */
	@JsonProperty("DISCLAIMER_CONTENT")
	public String dISCLAIMER_CONTENT;

	/** The n AME. */
	@JsonProperty("NAME")
	public String nAME;

	/** The i D. */
	@JsonProperty("ID")
	public String iD;

	/** The d OB. */
	@JsonProperty("DOB")
	public String dOB;

	/** The u SE R ID. */
	@JsonProperty("USER_ID")
	public String uSER_ID;

	/** The n ATIONALITY. */
	@JsonProperty("NATIONALITY")
	public String nATIONALITY;

	/** The n ATIONALIT Y DESC. */
	@JsonProperty("NATIONALITY_DESC")
	public String nATIONALITY_DESC;

	/** The g ENDER. */
	@JsonProperty("GENDER")
	public String gENDER;

	/** The g ENDE R DESC. */
	@JsonProperty("GENDER_DESC")
	public String gENDER_DESC;

	/** The i D TYPE. */
	@JsonProperty("ID_TYPE")
	public String iD_TYPE;

	/** The i D TYP E DESC. */
	@JsonProperty("ID_TYPE_DESC")
	public String iD_TYPE_DESC;

	/** The t ICKE T ID. */
	@JsonProperty("TICKET_ID")
	public String tICKET_ID;

	/** The c REATE D DATE. */
	@JsonProperty("CREATED_DATE")
	public String cREATED_DATE;

	/** The identifier details. */
	@JsonProperty("IdentifierDetails")
	public List<IdentifierDetail> identifierDetails;

	/** The address history. */
	@JsonProperty("AddressHistory")
	public List<AddressHistory> addressHistory;

	/** The credit facilities. */
	@JsonProperty("CreditFacilities")
	public List<CreditFacility> creditFacilities;

	/** The written off facilities. */
	@JsonProperty("WrittenOffFacilities")
	public List<WrittenOffFacility> writtenOffFacilities;

	/** The dishonored cheques. */
	@JsonProperty("DishonoredCheques")
	public List<DishonoredCheque> dishonoredCheques;

	/** The unearned interest. */
	@JsonProperty("UnearnedInterest")
	public List<UnearnedInterest> unearnedInterest;

	/** The security indicator. */
	@JsonProperty("SecurityIndicator")
	public List<SecurityIndicator> securityIndicator;

	/** The perf overdue graph. */
	@JsonProperty("PerfOverdueGraph")
	public List<PerfOverdueGraph> perfOverdueGraph;

	/** The utilization graph. */
	@JsonProperty("UtilizationGraph")
	public List<UtilizationGraph> utilizationGraph;

	/** The legal actions. */
	@JsonProperty("LegalActions")
	public List<LegalAction> legalActions;

	/** The max NDPD history graph. */
	@JsonProperty("MaxNDPDHistoryGraph")
	public List<MaxNDPDHistoryGraph> maxNDPDHistoryGraph;

	/** The negative list. */
	@JsonProperty("NegativeList")
	public List<NegativeList> negativeList;

	/** The c R data. */
	@JsonProperty("CRData")
	public List<CRData> cRData;

	/** The closed account details. */
	@JsonProperty("ClosedAccountDetails")
	public List<ClosedAccountDetail> closedAccountDetails;

	/** The written off account details. */
	@JsonProperty("WrittenOffAccountDetails")
	public List<WrittenOffAccountDetail> writtenOffAccountDetails;

	/** The profile summary. */
	@JsonProperty("ProfileSummary")
	public List<ProfileSummary> profileSummary;

	/** The open account details. */
	@JsonProperty("OpenAccountDetails")
	public List<OpenAccountDetail> openAccountDetails;

	/** The d OR. */
	@JsonProperty("DOR")
	public Object dOR;

	/** The company SME. */
	@JsonProperty("CompanySME")
	public List<CompanySME> companySME;

	/** The relationship details. */
	@JsonProperty("RelationshipDetails")
	public List<RelationshipDetail> relationshipDetails;

	/** The data provider. */
	@JsonProperty("DataProvider")
	public String dataProvider;

	/** The bank code. */
	@JsonProperty("BankCode")
	public String bankCode;

	/** The bank branch code. */
	@JsonProperty("BankBranchCode")
	public String bankBranchCode;

	/** The guarantee type. */
	@JsonProperty("Guarantee_Type")
	public String guarantee_Type;

	/** The accepted. */
	@JsonProperty("Accepted")
	public String accepted;

	/** The used. */
	@JsonProperty("Used")
	public String used;

	/** The date reported. */
	@JsonProperty("DateReported")
	public String dateReported;

	/**
	 * Instantiates a new datum.
	 */
	public Datum() {

		// EMPTY
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "Datum ["
				+ (dISCLAIMER_CONTENT != null ? "dISCLAIMER_CONTENT=" + dISCLAIMER_CONTENT + ", "
						: "")
				+ (nAME != null ? "nAME=" + nAME + ", " : "")
				+ (iD != null ? "iD=" + iD + ", " : "") + (dOB != null ? "dOB=" + dOB + ", " : "")
				+ (uSER_ID != null ? "uSER_ID=" + uSER_ID + ", " : "")
				+ (nATIONALITY != null ? "nATIONALITY=" + nATIONALITY + ", " : "")
				+ (nATIONALITY_DESC != null ? "nATIONALITY_DESC=" + nATIONALITY_DESC + ", " : "")
				+ (gENDER != null ? "gENDER=" + gENDER + ", " : "")
				+ (gENDER_DESC != null ? "gENDER_DESC=" + gENDER_DESC + ", " : "")
				+ (iD_TYPE != null ? "iD_TYPE=" + iD_TYPE + ", " : "")
				+ (iD_TYPE_DESC != null ? "iD_TYPE_DESC=" + iD_TYPE_DESC + ", " : "")
				+ (tICKET_ID != null ? "tICKET_ID=" + tICKET_ID + ", " : "")
				+ (cREATED_DATE != null ? "cREATED_DATE=" + cREATED_DATE + ", " : "")
				+ (identifierDetails != null ? "identifierDetails=" + identifierDetails + ", " : "")
				+ (addressHistory != null ? "addressHistory=" + addressHistory + ", " : "")
				+ (creditFacilities != null ? "creditFacilities=" + creditFacilities + ", " : "")
				+ (writtenOffFacilities != null
						? "writtenOffFacilities=" + writtenOffFacilities + ", "
						: "")
				+ (dishonoredCheques != null ? "dishonoredCheques=" + dishonoredCheques + ", " : "")
				+ (unearnedInterest != null ? "unearnedInterest=" + unearnedInterest + ", " : "")
				+ (securityIndicator != null ? "securityIndicator=" + securityIndicator + ", " : "")
				+ (perfOverdueGraph != null ? "perfOverdueGraph=" + perfOverdueGraph + ", " : "")
				+ (utilizationGraph != null ? "utilizationGraph=" + utilizationGraph + ", " : "")
				+ (legalActions != null ? "legalActions=" + legalActions + ", " : "")
				+ (maxNDPDHistoryGraph != null ? "maxNDPDHistoryGraph=" + maxNDPDHistoryGraph + ", "
						: "")
				+ (negativeList != null ? "negativeList=" + negativeList + ", " : "")
				+ (cRData != null ? "cRData=" + cRData + ", " : "")
				+ (closedAccountDetails != null
						? "closedAccountDetails=" + closedAccountDetails + ", "
						: "")
				+ (writtenOffAccountDetails != null
						? "writtenOffAccountDetails=" + writtenOffAccountDetails + ", "
						: "")
				+ (profileSummary != null ? "profileSummary=" + profileSummary + ", " : "")
				+ (openAccountDetails != null ? "openAccountDetails=" + openAccountDetails + ", "
						: "")
				+ (dOR != null ? "dOR=" + dOR + ", " : "")
				+ (companySME != null ? "companySME=" + companySME + ", " : "")
				+ (relationshipDetails != null ? "relationshipDetails=" + relationshipDetails + ", "
						: "")
				+ (dataProvider != null ? "dataProvider=" + dataProvider + ", " : "")
				+ (bankCode != null ? "bankCode=" + bankCode + ", " : "")
				+ (bankBranchCode != null ? "bankBranchCode=" + bankBranchCode + ", " : "")
				+ (guarantee_Type != null ? "guarantee_Type=" + guarantee_Type + ", " : "")
				+ (accepted != null ? "accepted=" + accepted + ", " : "")
				+ (used != null ? "used=" + used + ", " : "")
				+ (dateReported != null ? "dateReported=" + dateReported : "") + "]";
	}

}
