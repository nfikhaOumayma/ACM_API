/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link Inquiry}.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class Inquiry {

	/** The s UBSCRIBE R ID. */
	@JsonProperty("SUBSCRIBER_ID")
	public String sUBSCRIBER_ID;

	/** The r EASO N DESC. */
	@JsonProperty("REASON_DESC")
	public String rEASON_DESC;

	/** The r EASON. */
	@JsonProperty("REASON")
	public String rEASON;

	/** The l OA N TYP E DESC. */
	@JsonProperty("LOAN_TYPE_DESC")
	public String lOAN_TYPE_DESC;

	/** The l OA N TYPE. */
	@JsonProperty("LOAN_TYPE")
	public String lOAN_TYPE;

	/** The i NQUIR Y DATE. */
	@JsonProperty("INQUIRY_DATE")
	public String iNQUIRY_DATE;

	/** The c LIEN T CATEGOR Y DESC. */
	@JsonProperty("CLIENT_CATEGORY_DESC")
	public String cLIENT_CATEGORY_DESC;

	/** The c LIEN T CATEGORY. */
	@JsonProperty("CLIENT_CATEGORY")
	public String cLIENT_CATEGORY;

	/**
	 * Instantiates a new inquiry.
	 */
	public Inquiry() {

		// EMPTY
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "Inquiry [" + (sUBSCRIBER_ID != null ? "sUBSCRIBER_ID=" + sUBSCRIBER_ID + ", " : "")
				+ (rEASON_DESC != null ? "rEASON_DESC=" + rEASON_DESC + ", " : "")
				+ (rEASON != null ? "rEASON=" + rEASON + ", " : "")
				+ (lOAN_TYPE_DESC != null ? "lOAN_TYPE_DESC=" + lOAN_TYPE_DESC + ", " : "")
				+ (lOAN_TYPE != null ? "lOAN_TYPE=" + lOAN_TYPE + ", " : "")
				+ (iNQUIRY_DATE != null ? "iNQUIRY_DATE=" + iNQUIRY_DATE + ", " : "")
				+ (cLIENT_CATEGORY_DESC != null
						? "cLIENT_CATEGORY_DESC=" + cLIENT_CATEGORY_DESC + ", "
						: "")
				+ (cLIENT_CATEGORY != null ? "cLIENT_CATEGORY=" + cLIENT_CATEGORY : "") + "]";
	}

}
