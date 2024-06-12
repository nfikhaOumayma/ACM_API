/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link DisputeRef} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class DisputeRef {

	/** The d ISPUT E ID. */
	@JsonProperty("DISPUTE_ID")
	public String dISPUTE_ID;

	/** The r EGISTRATIO N DATE. */
	@JsonProperty("REGISTRATION_DATE")
	public String rEGISTRATION_DATE;

	/** The d AT A CORRECTIO N TYPE. */
	@JsonProperty("DATA_CORRECTION_TYPE")
	public String dATA_CORRECTION_TYPE;

	/** The n OTE. */
	@JsonProperty("NOTE")
	public String nOTE;

	/** The s TATU S NAME. */
	@JsonProperty("STATUS_NAME")
	public String sTATUS_NAME;

	/** The r ESOLVE D DATE. */
	@JsonProperty("RESOLVED_DATE")
	public String rESOLVED_DATE;

	/**
	 * Instantiates a new dispute ref.
	 */
	public DisputeRef() {

		// EMPTY
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "DisputeRef [" + (dISPUTE_ID != null ? "dISPUTE_ID=" + dISPUTE_ID + ", " : "")
				+ (rEGISTRATION_DATE != null ? "rEGISTRATION_DATE=" + rEGISTRATION_DATE + ", " : "")
				+ (dATA_CORRECTION_TYPE != null
						? "dATA_CORRECTION_TYPE=" + dATA_CORRECTION_TYPE + ", "
						: "")
				+ (nOTE != null ? "nOTE=" + nOTE + ", " : "")
				+ (sTATUS_NAME != null ? "sTATUS_NAME=" + sTATUS_NAME + ", " : "")
				+ (rESOLVED_DATE != null ? "rESOLVED_DATE=" + rESOLVED_DATE : "") + "]";
	}

}
