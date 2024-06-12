/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link CompanyRequest} class. ObjectMapper om = new ObjectMapper(); Root root =
 * om.readValue(myJsonString), Root.class);
 * 
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class CompanyRequest {

	/** The company live request. */
	@JsonProperty("LIVE_REQUEST")
	public CompanyLiveRequest companyLiveRequest;

	/**
	 * Instantiates a new company request.
	 */
	public CompanyRequest() {

		// EMPTY
	}

	/**
	 * Instantiates a new company request.
	 *
	 * @param companyLiveRequest the company live request
	 */
	public CompanyRequest(CompanyLiveRequest companyLiveRequest) {

		this.companyLiveRequest = companyLiveRequest;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "CompanyRequest ["
				+ (companyLiveRequest != null ? "companyLiveRequest=" + companyLiveRequest : "")
				+ "]";
	}

}
