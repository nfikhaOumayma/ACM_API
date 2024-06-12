/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link RelationshipDetail} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class RelationshipDetail {

	/** The ullname. */
	@JsonProperty("FULLNAME")
	public String fULLNAME;

	/** The i D TYP E DESC. */
	@JsonProperty("ID_TYPE_DESC")
	public String iD_TYPE_DESC;

	/** The i D TYPE. */
	@JsonProperty("ID_TYPE")
	public String iD_TYPE;

	/** The i D VALUE. */
	@JsonProperty("ID_VALUE")
	public String iD_VALUE;

	/** The s UBJEC T KEY. */
	@JsonProperty("SUBJECT_KEY")
	public String sUBJECT_KEY;

	/** The n ATUR E O F RELATIONSHI P DESC. */
	@JsonProperty("NATURE_OF_RELATIONSHIP_DESC")
	public String nATURE_OF_RELATIONSHIP_DESC;

	/** The n ATUR E O F RELATIONSHIP. */
	@JsonProperty("NATURE_OF_RELATIONSHIP")
	public String nATURE_OF_RELATIONSHIP;

	/** The r ELATIONSHI P TYP E DESC. */
	@JsonProperty("RELATIONSHIP_TYPE_DESC")
	public String rELATIONSHIP_TYPE_DESC;

	/** The r ELATIONSHI P TYPE. */
	@JsonProperty("RELATIONSHIP_TYPE")
	public String rELATIONSHIP_TYPE;

	/** The r ELATIONSHI P STATU S DESC. */
	@JsonProperty("RELATIONSHIP_STATUS_DESC")
	public String rELATIONSHIP_STATUS_DESC;

	/** The r ELATIONSHI P STATUS. */
	@JsonProperty("RELATIONSHIP_STATUS")
	public String rELATIONSHIP_STATUS;

	/** The d AT E O F LEAVING. */
	@JsonProperty("DATE_OF_LEAVING")
	public String dATE_OF_LEAVING;

	/**
	 * Instantiates a new relationship detail.
	 */
	public RelationshipDetail() {

		// EMPTY
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "RelationshipDetail [" + (fULLNAME != null ? "fULLNAME=" + fULLNAME + ", " : "")
				+ (iD_TYPE_DESC != null ? "iD_TYPE_DESC=" + iD_TYPE_DESC + ", " : "")
				+ (iD_TYPE != null ? "iD_TYPE=" + iD_TYPE + ", " : "")
				+ (iD_VALUE != null ? "iD_VALUE=" + iD_VALUE + ", " : "")
				+ (sUBJECT_KEY != null ? "sUBJECT_KEY=" + sUBJECT_KEY + ", " : "")
				+ (nATURE_OF_RELATIONSHIP_DESC != null
						? "nATURE_OF_RELATIONSHIP_DESC=" + nATURE_OF_RELATIONSHIP_DESC + ", "
						: "")
				+ (nATURE_OF_RELATIONSHIP != null
						? "nATURE_OF_RELATIONSHIP=" + nATURE_OF_RELATIONSHIP + ", "
						: "")
				+ (rELATIONSHIP_TYPE_DESC != null
						? "rELATIONSHIP_TYPE_DESC=" + rELATIONSHIP_TYPE_DESC + ", "
						: "")
				+ (rELATIONSHIP_TYPE != null ? "rELATIONSHIP_TYPE=" + rELATIONSHIP_TYPE + ", " : "")
				+ (rELATIONSHIP_STATUS_DESC != null
						? "rELATIONSHIP_STATUS_DESC=" + rELATIONSHIP_STATUS_DESC + ", "
						: "")
				+ (rELATIONSHIP_STATUS != null ? "rELATIONSHIP_STATUS=" + rELATIONSHIP_STATUS + ", "
						: "")
				+ (dATE_OF_LEAVING != null ? "dATE_OF_LEAVING=" + dATE_OF_LEAVING : "") + "]";
	}

}
