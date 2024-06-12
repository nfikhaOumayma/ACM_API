/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link CustomerRequest} class. ObjectMapper om = new ObjectMapper(); Root root =
 * om.readValue(myJsonString), Root.class);
 * 
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class CustomerRequest {

	/** The customer live request. */
	@JsonProperty("LIVE_REQUEST")
	public CustomerLiveRequest customerLiveRequest;

	/**
	 * Instantiates a new customer request.
	 */
	public CustomerRequest() {

		// EMPTY
	}

	/**
	 * Instantiates a new customer request.
	 *
	 * @param customerLiveRequest the customer live request
	 */
	public CustomerRequest(CustomerLiveRequest customerLiveRequest) {

		this.customerLiveRequest = customerLiveRequest;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "CustomerRequest ["
				+ (customerLiveRequest != null ? "customerLiveRequest=" + customerLiveRequest : "")
				+ "]";
	}

}
