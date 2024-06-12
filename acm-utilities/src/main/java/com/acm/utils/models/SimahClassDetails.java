/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

/**
 * The Class SimahClassDetails.
 */
@Entity
@Table(name = "SIMAH_CLASS_DETAILS")
public class SimahClassDetails implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -3392983909759145302L;

	/** The id class details. */
	@Id
	@Column(name = "ID_SIMAH_CLASS_DETAILS", unique = true, nullable = false)
	private Integer id;

	/** The salary from. */
	@Column(name = "SALARY_FROM", nullable = false)
	private BigDecimal salaryFrom;

	/** The salary to. */
	@Column(name = "SALARY_TO", nullable = false)
	private BigDecimal salaryTo;

	/** The rate. */
	@Column(name = "RATE", nullable = false)
	private Integer rate;

	/** The simah class type. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_CLASS_TYPE")
	private SimahClassType simahClassType;

	/**
	 * Instantiates a new class details.
	 */
	public SimahClassDetails() {

	}

	/**
	 * Gets the id.
	 *
	 * @return the id
	 */
	public Integer getId() {

		return id;
	}

	/**
	 * Sets the id.
	 *
	 * @param id the new id
	 */
	public void setId(Integer id) {

		this.id = id;
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
	public SimahClassType getSimahClassType() {

		return simahClassType;
	}

	/**
	 * Sets the simah class type.
	 *
	 * @param simahClassType the new simah class type
	 */
	public void setSimahClassType(SimahClassType simahClassType) {

		this.simahClassType = simahClassType;
	}

}
