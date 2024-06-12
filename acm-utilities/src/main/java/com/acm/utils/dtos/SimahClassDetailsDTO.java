/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * The Class SimahClassDetailsDTO.
 */
public class SimahClassDetailsDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 4799585786806479219L;

	/** The id class details. */
	private Integer idClassDetails;

	/** The salary from. */
	private BigDecimal salaryFrom;

	/** The salary to. */
	private BigDecimal salaryTo;

	/** The rate. */
	private Integer rate;

	/** The class type. */
	private SimahClassTypeDTO simahClassType;

	/**
	 * Instantiates a new class details DTO.
	 */
	public SimahClassDetailsDTO() {

	}

	/**
	 * Gets the id class details.
	 *
	 * @return the id class details
	 */
	public Integer getIdClassDetails() {

		return idClassDetails;
	}

	/**
	 * Sets the id class details.
	 *
	 * @param idClassDetails the new id class details
	 */
	public void setIdClassDetails(Integer idClassDetails) {

		this.idClassDetails = idClassDetails;
	}

	/**
	 * Gets the salary from.
	 *
	 * @return the salary from
	 */
	public BigDecimal getSalaryFrom() {

		return salaryFrom;
	}

	/**
	 * Sets the salary from.
	 *
	 * @param salaryFrom the new salary from
	 */
	public void setSalaryFrom(BigDecimal salaryFrom) {

		this.salaryFrom = salaryFrom;
	}

	/**
	 * Gets the salary to.
	 *
	 * @return the salary to
	 */
	public BigDecimal getSalaryTo() {

		return salaryTo;
	}

	/**
	 * Sets the salary to.
	 *
	 * @param salaryTo the new salary to
	 */
	public void setSalaryTo(BigDecimal salaryTo) {

		this.salaryTo = salaryTo;
	}

	/**
	 * Gets the rate.
	 *
	 * @return the rate
	 */
	public Integer getRate() {

		return rate;
	}

	/**
	 * Sets the rate.
	 *
	 * @param rate the new rate
	 */
	public void setRate(Integer rate) {

		this.rate = rate;
	}

	/**
	 * Gets the simah class type.
	 *
	 * @return the simah class type
	 */
	public SimahClassTypeDTO getSimahClassType() {

		return simahClassType;
	}

	/**
	 * Sets the simah class type.
	 *
	 * @param simahClassType the new simah class type
	 */
	public void setSimahClassType(SimahClassTypeDTO simahClassType) {

		this.simahClassType = simahClassType;
	}

}
