/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link CRData} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class CRData {

	/** The c R DATA. */
	@JsonProperty("CR_DATA")
	public List<CRDATAFields> cR_DATA;

	/** The data. */
	@JsonProperty("Data")
	public List<Datum> data;

	/**
	 * Instantiates a new CR data.
	 */
	public CRData() {

		// EMPTY
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "CRData [" + (cR_DATA != null ? "cR_DATA=" + cR_DATA + ", " : "")
				+ (data != null ? "data=" + data : "") + "]";
	}

}
