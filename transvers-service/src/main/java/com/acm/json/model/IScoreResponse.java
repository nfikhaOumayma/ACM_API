/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link IScoreResponse} class. ObjectMapper om = new ObjectMapper(); Root root =
 * om.readValue(myJsonString), Root.class);
 * 
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class IScoreResponse {

	/** The error response. */
	@JsonProperty("ErrorResponse")
	public List<ErrorResponse> errorResponse;

	/** The CustomerResponse. */
	@JsonProperty("Response")
	public CustomerResponse response;

	/** The is no hit. */
	@JsonProperty("IsNoHit")
	public Boolean isNoHit;

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "IScoreResponse ["
				+ (errorResponse != null ? "errorResponse=" + errorResponse + ", " : "")
				+ (response != null ? "response=" + response + ", " : "")
				+ (isNoHit != null ? "isNoHit=" + isNoHit : "") + "]";
	}

}
