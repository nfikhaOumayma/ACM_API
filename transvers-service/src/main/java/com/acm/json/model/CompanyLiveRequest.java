/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link CompanyLiveRequest} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class CompanyLiveRequest {

	/** The request header. */
	@JsonProperty("HEADER")
	public RequestHeader requestHeader;

	/** The report parameters. */
	@JsonProperty("REPORT_PARAMETERS")
	public RequestReportParameters reportParameters;

	/** The request company search parameters. */
	@JsonProperty("COMPANY_SEARCH_PARAMETERS")
	public RequestCompanySearchParameters requestCompanySearchParameters;

	/** The request application details. */
	@JsonProperty("APPLICATION_DETAILS")
	public RequestApplicationDetails requestApplicationDetails;

	/**
	 * Instantiates a new company live request.
	 */
	public CompanyLiveRequest() {

		// EMPTY
	}

	/**
	 * Instantiates a new company live request.
	 *
	 * @param requestHeader the request header
	 * @param reportParameters the report parameters
	 * @param requestCompanySearchParameters the request company search parameters
	 * @param requestApplicationDetails the request application details
	 */
	public CompanyLiveRequest(RequestHeader requestHeader, RequestReportParameters reportParameters,
			RequestCompanySearchParameters requestCompanySearchParameters,
			RequestApplicationDetails requestApplicationDetails) {

		this.requestHeader = requestHeader;
		this.reportParameters = reportParameters;
		this.requestCompanySearchParameters = requestCompanySearchParameters;
		this.requestApplicationDetails = requestApplicationDetails;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "CompanyLiveRequest ["
				+ (requestHeader != null ? "requestHeader=" + requestHeader + ", " : "")
				+ (reportParameters != null ? "reportParameters=" + reportParameters + ", " : "")
				+ (requestCompanySearchParameters != null
						? "requestCompanySearchParameters=" + requestCompanySearchParameters + ", "
						: "")
				+ (requestApplicationDetails != null
						? "requestApplicationDetails=" + requestApplicationDetails
						: "")
				+ "]";
	}

}
