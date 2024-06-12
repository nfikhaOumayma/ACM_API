/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link CustomerLiveRequest} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class CustomerLiveRequest {

	/** The request header. */
	@JsonProperty("HEADER")
	public RequestHeader requestHeader;

	/** The report parameters. */
	@JsonProperty("REPORT_PARAMETERS")
	public RequestReportParameters reportParameters;

	/** The request person search parameters. */
	@JsonProperty("PERSON_SEARCH_PARAMETERS")
	public RequestPersonSearchParameters requestPersonSearchParameters;

	/** The request application details. */
	@JsonProperty("APPLICATION_DETAILS")
	public RequestApplicationDetails requestApplicationDetails;

	/**
	 * Instantiates a new customer live request.
	 */
	public CustomerLiveRequest() {

		// EMPTY
	}

	/**
	 * Instantiates a new customer live request.
	 *
	 * @param requestHeader the request header
	 * @param reportParameters the report parameters
	 * @param requestPersonSearchParameters the request person search parameters
	 * @param requestApplicationDetails the request application details
	 */
	public CustomerLiveRequest(RequestHeader requestHeader,
			RequestReportParameters reportParameters,
			RequestPersonSearchParameters requestPersonSearchParameters,
			RequestApplicationDetails requestApplicationDetails) {

		this.requestHeader = requestHeader;
		this.reportParameters = reportParameters;
		this.requestPersonSearchParameters = requestPersonSearchParameters;
		this.requestApplicationDetails = requestApplicationDetails;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "CustomerLiveRequest ["
				+ (requestHeader != null ? "requestHeader=" + requestHeader + ", " : "")
				+ (reportParameters != null ? "reportParameters=" + reportParameters + ", " : "")
				+ (requestPersonSearchParameters != null
						? "requestPersonSearchParameters=" + requestPersonSearchParameters + ", "
						: "")
				+ (requestApplicationDetails != null
						? "requestApplicationDetails=" + requestApplicationDetails
						: "")
				+ "]";
	}

}
