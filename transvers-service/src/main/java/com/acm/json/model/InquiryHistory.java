/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link InquiryHistory}.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class InquiryHistory {

	/** The inquiry. */
	@JsonProperty("Inquiry")
	public List<Inquiry> inquiry;

	/**
	 * Instantiates a new inquiry history.
	 */
	public InquiryHistory() {

		// EMPTY
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "InquiryHistory [" + (inquiry != null ? "inquiry=" + inquiry : "") + "]";
	}

}
