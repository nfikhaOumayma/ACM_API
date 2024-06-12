/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link RequestPersonSearchParameters} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class RequestPersonSearchParameters {

	/** The name. */
	@JsonProperty("NAME")
	private String name;

	/** The identifier type. */
	@JsonProperty("IDENTIFIER_TYPE")
	public String identifierType;

	/** The identifier value. */
	@JsonProperty("IDENTIFIER_VALUE")
	public String identifierValue;

	/** The date of birth. */
	@JsonProperty("DATE_OF_BIRTH")
	public String dateOfBirth;

	/** The gender. */
	@JsonProperty("GENDER")
	public String gender;

	/** The nationality. */
	@JsonProperty("NATIONALITY")
	public String nationality;

	/**
	 * Instantiates a new request person search parameters.
	 */
	public RequestPersonSearchParameters() {

		// EMPTY
	}

	/**
	 * Instantiates a new request person search parameters.
	 *
	 * @param name the name
	 * @param identifierType the identifier type
	 * @param identifierValue the identifier value
	 * @param dateOfBirth the date of birth
	 * @param gender the gender
	 * @param nationality the nationality
	 */
	public RequestPersonSearchParameters(String name, String identifierType, String identifierValue,
			String dateOfBirth, String gender, String nationality) {

		this.name = name;
		this.identifierType = identifierType;
		this.identifierValue = identifierValue;
		this.dateOfBirth = dateOfBirth;
		this.gender = gender;
		this.nationality = nationality;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "RequestPersonSearchParameters [" + (name != null ? "name=" + name + ", " : "")
				+ (identifierType != null ? "identifierType=" + identifierType + ", " : "")
				+ (identifierValue != null ? "identifierValue=" + identifierValue + ", " : "")
				+ (dateOfBirth != null ? "dateOfBirth=" + dateOfBirth + ", " : "")
				+ (gender != null ? "gender=" + gender + ", " : "")
				+ (nationality != null ? "nationality=" + nationality : "") + "]";
	}

}
