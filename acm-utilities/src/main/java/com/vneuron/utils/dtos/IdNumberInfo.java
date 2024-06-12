/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.vneuron.utils.dtos;

/**
 * {@link IdNumberInfo } class.
 *
 * @author kouali
 * @since 0.1.0
 */
public class IdNumberInfo {

	/** The number info id. */
	public int number_info_id;

	/** The identification type. */
	public String identification_type;

	/** The identification value. */
	public String identification_value;

	/**
	 * Gets the number info id.
	 *
	 * @return the number_info_id
	 */
	public int getNumber_info_id() {

		return number_info_id;
	}

	/**
	 * Sets the number info id.
	 *
	 * @param number_info_id the number_info_id to set
	 */
	public void setNumber_info_id(int number_info_id) {

		this.number_info_id = number_info_id;
	}

	/**
	 * Gets the identification type.
	 *
	 * @return the identification_type
	 */
	public String getIdentification_type() {

		return identification_type;
	}

	/**
	 * Sets the identification type.
	 *
	 * @param identification_type the identification_type to set
	 */
	public void setIdentification_type(String identification_type) {

		this.identification_type = identification_type;
	}

	/**
	 * Gets the identification value.
	 *
	 * @return the identification_value
	 */
	public String getIdentification_value() {

		return identification_value;
	}

	/**
	 * Sets the identification value.
	 *
	 * @param identification_value the identification_value to set
	 */
	public void setIdentification_value(String identification_value) {

		this.identification_value = identification_value;
	}

}
