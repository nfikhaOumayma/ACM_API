/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.vneuron.utils.dtos;

/**
 * {@link Nationality } class.
 *
 * @author kouali
 * @since 0.1.0
 */
public class Nationality {

	/** The country. */
	public String country;

	/** The type. */
	public String type;

	/** The nationality id. */
	public int nationality_id;

	/**
	 * Gets the country.
	 *
	 * @return the country
	 */
	public String getCountry() {

		return country;
	}

	/**
	 * Sets the country.
	 *
	 * @param country the country to set
	 */
	public void setCountry(String country) {

		this.country = country;
	}

	/**
	 * Gets the type.
	 *
	 * @return the type
	 */
	public String getType() {

		return type;
	}

	/**
	 * Sets the type.
	 *
	 * @param type the type to set
	 */
	public void setType(String type) {

		this.type = type;
	}

	/**
	 * Gets the nationality id.
	 *
	 * @return the nationality_id
	 */
	public int getNationality_id() {

		return nationality_id;
	}

	/**
	 * Sets the nationality id.
	 *
	 * @param nationality_id the nationality_id to set
	 */
	public void setNationality_id(int nationality_id) {

		this.nationality_id = nationality_id;
	}

}
