/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link AddressHistory}.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class AddressHistory {

	/** The address. */
	@JsonProperty("Address")
	public String address;

	/** The type DESC. */
	@JsonProperty("Type_DESC")
	public String type_DESC;

	/** The type. */
	@JsonProperty("Type")
	public String type;

	/** The reporting date. */
	@JsonProperty("ReportingDate")
	public String reportingDate;

	/**
	 * Instantiates a new address history.
	 */
	public AddressHistory() {

		// EMPTY
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "AddressHistory [" + (address != null ? "address=" + address + ", " : "")
				+ (type_DESC != null ? "type_DESC=" + type_DESC + ", " : "")
				+ (type != null ? "type=" + type + ", " : "")
				+ (reportingDate != null ? "reportingDate=" + reportingDate : "") + "]";
	}
}
