/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link CRDATAFields} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class CRDATAFields {

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
	 * Instantiates a new CRDATA fields.
	 */
	public CRDATAFields() {

		// EMPTY
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "CRDATAFields ["
				+ (dataProvider != null ? "dataProvider=" + dataProvider + ", " : "")
				+ (bankCode != null ? "bankCode=" + bankCode + ", " : "")
				+ (bankBranchCode != null ? "bankBranchCode=" + bankBranchCode + ", " : "")
				+ (guarantee_Type != null ? "guarantee_Type=" + guarantee_Type + ", " : "")
				+ (accepted != null ? "accepted=" + accepted + ", " : "")
				+ (used != null ? "used=" + used + ", " : "")
				+ (dateReported != null ? "dateReported=" + dateReported : "") + "]";
	}

}
