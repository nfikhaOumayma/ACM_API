/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.vneuron.utils.dtos;

/**
 * {@link BirthDate } class.
 *
 * @author kouali
 * @since 0.1.0
 */
public class BirthDate {

	/** The birth id. */
	public int birth_id;

	/** The birth date. */
	public String birth_date;

	/** The exact date. */
	public boolean exact_date;

	/** The interval min date. */
	public Object interval_min_date;

	/** The interval max date. */
	public Object interval_max_date;

	/**
	 * Gets the birth id.
	 *
	 * @return the birth_id
	 */
	public int getBirth_id() {

		return birth_id;
	}

	/**
	 * Sets the birth id.
	 *
	 * @param birth_id the birth_id to set
	 */
	public void setBirth_id(int birth_id) {

		this.birth_id = birth_id;
	}

	/**
	 * Gets the birth date.
	 *
	 * @return the birth_date
	 */
	public String getBirth_date() {

		return birth_date;
	}

	/**
	 * Sets the birth date.
	 *
	 * @param birth_date the birth_date to set
	 */
	public void setBirth_date(String birth_date) {

		this.birth_date = birth_date;
	}

	/**
	 * Checks if is exact date.
	 *
	 * @return the exact_date
	 */
	public boolean isExact_date() {

		return exact_date;
	}

	/**
	 * Sets the exact date.
	 *
	 * @param exact_date the exact_date to set
	 */
	public void setExact_date(boolean exact_date) {

		this.exact_date = exact_date;
	}

	/**
	 * Gets the interval min date.
	 *
	 * @return the interval_min_date
	 */
	public Object getInterval_min_date() {

		return interval_min_date;
	}

	/**
	 * Sets the interval min date.
	 *
	 * @param interval_min_date the interval_min_date to set
	 */
	public void setInterval_min_date(Object interval_min_date) {

		this.interval_min_date = interval_min_date;
	}

	/**
	 * Gets the interval max date.
	 *
	 * @return the interval_max_date
	 */
	public Object getInterval_max_date() {

		return interval_max_date;
	}

	/**
	 * Sets the interval max date.
	 *
	 * @param interval_max_date the interval_max_date to set
	 */
	public void setInterval_max_date(Object interval_max_date) {

		this.interval_max_date = interval_max_date;
	}

}
