/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link IdentifierDetail}.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class IdentifierDetail {

	/** The i D TYP E DESC. */
	@JsonProperty("ID_TYPE_DESC")
	public String iD_TYPE_DESC;

	/** The i D TYPE. */
	@JsonProperty("ID_TYPE")
	public String iD_TYPE;

	/** The i D CODE. */
	@JsonProperty("ID_CODE")
	public String iD_CODE;

	/**
	 * Instantiates a new identifier detail.
	 */
	public IdentifierDetail() {

		// EMPTY
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "IdentifierDetail ["
				+ (iD_TYPE_DESC != null ? "iD_TYPE_DESC=" + iD_TYPE_DESC + ", " : "")
				+ (iD_TYPE != null ? "iD_TYPE=" + iD_TYPE + ", " : "")
				+ (iD_CODE != null ? "iD_CODE=" + iD_CODE : "") + "]";
	}

}
