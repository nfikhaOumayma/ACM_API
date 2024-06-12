/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * {@link IncentiveHistoryDTO} class.
 *
 * @author YesserSomai
 * @since 1.0.8
 */
public class IncentiveHistoryDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1434102135907421135L;

	/** The year. */
	private Integer year;

	/** The month. */
	private Integer month;

	/**
	 * Instantiates a new incentive history DTO.
	 */
	public IncentiveHistoryDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Gets the year.
	 *
	 * @return the year
	 */
	public Integer getYear() {

		return year;
	}

	/**
	 * Sets the year.
	 *
	 * @param year the year to set
	 */
	public void setYear(Integer year) {

		this.year = year;
	}

	/**
	 * Gets the month.
	 *
	 * @return the month
	 */
	public Integer getMonth() {

		return month;
	}

	/**
	 * Sets the month.
	 *
	 * @param month the month to set
	 */
	public void setMonth(Integer month) {

		this.month = month;
	}

}
