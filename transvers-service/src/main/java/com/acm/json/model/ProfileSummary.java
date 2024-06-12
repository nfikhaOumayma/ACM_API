/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link ProfileSummary} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class ProfileSummary {

	/** The gender DESC. */
	@JsonProperty("Gender_DESC")
	public String gender_DESC;

	/** The gender. */
	@JsonProperty("Gender")
	public String gender;

	/** The d OB. */
	@JsonProperty("DOB")
	public String dOB;

	/** The marital status DESC. */
	@JsonProperty("MaritalStatus_DESC")
	public String maritalStatus_DESC;

	/** The marital status. */
	@JsonProperty("MaritalStatus")
	public String maritalStatus;

	/** The citizenship DESC. */
	@JsonProperty("Citizenship_DESC")
	public String citizenship_DESC;

	/** The citizenship. */
	@JsonProperty("Citizenship")
	public String citizenship;

	/** The occupation DESC. */
	@JsonProperty("Occupation_DESC")
	public String occupation_DESC;

	/** The occupation. */
	@JsonProperty("Occupation")
	public String occupation;

	/** The full name. */
	@JsonProperty("FullName")
	public String fullName;

	/** The i D value. */
	@JsonProperty("IDValue")
	public String iDValue;

	/** The contact value. */
	@JsonProperty("ContactValue")
	public String contactValue;

	/** The full address. */
	@JsonProperty("FullAddress")
	public String fullAddress;

	/** The s CORE. */
	@JsonProperty("SCORE")
	public int sCORE;

	/** The r EASONCODE 1 DESC. */
	@JsonProperty("REASONCODE1_DESC")
	public String rEASONCODE1_DESC;

	/** The r EASONCODE 1. */
	@JsonProperty("REASONCODE1")
	public String rEASONCODE1;

	/** The r EASONCODE 2 DESC. */
	@JsonProperty("REASONCODE2_DESC")
	public String rEASONCODE2_DESC;

	/** The r EASONCODE 2. */
	@JsonProperty("REASONCODE2")
	public String rEASONCODE2;

	/** The w ORS T LA. */
	@JsonProperty("WORST_LA")
	public String wORST_LA;

	/** The w ORS T L A DESC. */
	@JsonProperty("WORST_LA_DESC")
	public String wORST_LA_DESC;

	/** The business activity start date. */
	@JsonProperty("Business_Activity_Start_Date")
	public String business_Activity_Start_Date;

	/** The legal form DESC. */
	@JsonProperty("Legal_Form_DESC")
	public String legal_Form_DESC;

	/** The legal form. */
	@JsonProperty("Legal_Form")
	public String legal_Form;

	/** The ownership classification DESC. */
	@JsonProperty("Ownership_Classification_DESC")
	public String ownership_Classification_DESC;

	/** The ownership classification. */
	@JsonProperty("Ownership_Classification")
	public String ownership_Classification;

	/** The contact type DESC. */
	@JsonProperty("ContactType_DESC")
	public String contactType_DESC;

	/** The contact type. */
	@JsonProperty("ContactType")
	public String contactType;

	/**
	 * Instantiates a new profile summary.
	 */
	public ProfileSummary() {

		// EMPTY
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "ProfileSummary [" + (gender_DESC != null ? "gender_DESC=" + gender_DESC + ", " : "")
				+ (gender != null ? "gender=" + gender + ", " : "")
				+ (dOB != null ? "dOB=" + dOB + ", " : "")
				+ (maritalStatus_DESC != null ? "maritalStatus_DESC=" + maritalStatus_DESC + ", "
						: "")
				+ (maritalStatus != null ? "maritalStatus=" + maritalStatus + ", " : "")
				+ (citizenship_DESC != null ? "citizenship_DESC=" + citizenship_DESC + ", " : "")
				+ (citizenship != null ? "citizenship=" + citizenship + ", " : "")
				+ (occupation_DESC != null ? "occupation_DESC=" + occupation_DESC + ", " : "")
				+ (occupation != null ? "occupation=" + occupation + ", " : "")
				+ (fullName != null ? "fullName=" + fullName + ", " : "")
				+ (iDValue != null ? "iDValue=" + iDValue + ", " : "")
				+ (contactValue != null ? "contactValue=" + contactValue + ", " : "")
				+ (fullAddress != null ? "fullAddress=" + fullAddress + ", " : "") + "sCORE="
				+ sCORE + ", "
				+ (rEASONCODE1_DESC != null ? "rEASONCODE1_DESC=" + rEASONCODE1_DESC + ", " : "")
				+ (rEASONCODE1 != null ? "rEASONCODE1=" + rEASONCODE1 + ", " : "")
				+ (rEASONCODE2_DESC != null ? "rEASONCODE2_DESC=" + rEASONCODE2_DESC + ", " : "")
				+ (rEASONCODE2 != null ? "rEASONCODE2=" + rEASONCODE2 + ", " : "")
				+ (wORST_LA != null ? "wORST_LA=" + wORST_LA + ", " : "")
				+ (wORST_LA_DESC != null ? "wORST_LA_DESC=" + wORST_LA_DESC + ", " : "")
				+ (business_Activity_Start_Date != null
						? "business_Activity_Start_Date=" + business_Activity_Start_Date + ", "
						: "")
				+ (legal_Form_DESC != null ? "legal_Form_DESC=" + legal_Form_DESC + ", " : "")
				+ (legal_Form != null ? "legal_Form=" + legal_Form + ", " : "")
				+ (ownership_Classification_DESC != null
						? "ownership_Classification_DESC=" + ownership_Classification_DESC + ", "
						: "")
				+ (ownership_Classification != null
						? "ownership_Classification=" + ownership_Classification + ", "
						: "")
				+ (contactType_DESC != null ? "contactType_DESC=" + contactType_DESC + ", " : "")
				+ (contactType != null ? "contactType=" + contactType : "") + "]";
	}

}
