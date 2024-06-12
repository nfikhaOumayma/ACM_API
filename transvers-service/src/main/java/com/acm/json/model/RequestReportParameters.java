/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link RequestReportParameters} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class RequestReportParameters {

	/** The search type. */
	@JsonProperty("SEARCH_TYPE")
	public String searchType;

	/** The language. */
	@JsonProperty("LANGUAGE")
	public String language;

	/** The product ID. */
	@JsonProperty("PRODUCT_ID")
	public String productID;

	/** The response type. */
	@JsonProperty("RESPONSE_TYPE")
	public String responseType;

	/**
	 * Instantiates a new request report parameters.
	 */
	public RequestReportParameters() {

		// EMPTY
	}

	/**
	 * Instantiates a new request report parameters.
	 *
	 * @param searchType the search type
	 * @param language the language
	 * @param productID the product ID
	 * @param responseType the response type
	 */
	public RequestReportParameters(String searchType, String language, String productID,
			String responseType) {

		this.searchType = searchType;
		this.language = language;
		this.productID = productID;
		this.responseType = responseType;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "RequestReportParameters ["
				+ (searchType != null ? "searchType=" + searchType + ", " : "")
				+ (language != null ? "language=" + language + ", " : "")
				+ (productID != null ? "productID=" + productID + ", " : "")
				+ (responseType != null ? "responseType=" + responseType : "") + "]";
	}

}
