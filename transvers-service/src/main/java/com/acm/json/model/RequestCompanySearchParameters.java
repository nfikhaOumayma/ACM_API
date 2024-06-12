/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link RequestCompanySearchParameters} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class RequestCompanySearchParameters {

	/** The date of registration. */
	@JsonProperty("DATE_OF_REGISTRATION")
	public String dateOfRegistration;

	/** The identifier type. */
	@JsonProperty("IDENTIFIER_TYPE")
	public String identifierType;

	/** The identifier value. */
	@JsonProperty("IDENTIFIER_VALUE")
	public String identifierValue;

	/** The name. */
	@JsonProperty("NAME")
	public String name;

	/**
	 * Instantiates a new request company search parameters.
	 */
	public RequestCompanySearchParameters() {

		// Empty
	}

	/**
	 * Instantiates a new request company search parameters.
	 *
	 * @param dateOfRegistration the date of registration
	 * @param identifierType the identifier type
	 * @param identifierValue the identifier value
	 * @param name the name
	 */
	public RequestCompanySearchParameters(String dateOfRegistration, String identifierType,
			String identifierValue, String name) {

		this.dateOfRegistration = dateOfRegistration;
		this.identifierType = identifierType;
		this.identifierValue = identifierValue;
		this.name = name;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "RequestCompanySearchParameters ["
				+ (dateOfRegistration != null ? "dateOfRegistration=" + dateOfRegistration + ", "
						: "")
				+ (identifierType != null ? "identifierType=" + identifierType + ", " : "")
				+ (identifierValue != null ? "identifierValue=" + identifierValue + ", " : "")
				+ (name != null ? "name=" + name : "") + "]";
	}

}
