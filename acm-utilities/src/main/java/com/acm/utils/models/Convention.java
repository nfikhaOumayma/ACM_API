/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

/**
 * The Class Convention.
 */
@Entity
@Table(name = "CONVENTION")
public class Convention implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 8252918111601978059L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID", unique = true, nullable = false)
	private Long id;

	/** The name Supplier. */
	@Column(name = "DISCOUNT_RATE")
	private Long discountRate;
	/** The start Date Convention . */
	@Column(name = "START_DATE_CONVENTION")
	private Date startDateConvention;
	/** The end Date Convention. */
	@Column(name = "END_DATE_CONVENTION")
	private Date endDateConvention;
	/** The rebate. */
	@Column(name = "REBATE")
	private Long rebate;

	/** The objective turnover volume. */
	@Column(name = "OBJECTIVE_TURNOVER_VOLUME")
	private Long objectiveTurnoverVolume;

	/** The objective file number. */
	@Column(name = "OBJECTIVE_FILE_NUMBER")
	private Long objectiveFileNumber;

	/** The ca. */
	@Column(name = "CA")
	private Long ca;

	/** The apply rate. */
	@Column(name = "APPLYRATE")
	private Boolean applyRate;

	/** The id supplier. */
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "ID_SUPPLIER")
	private Supplier supplier;

	/**
	 * Gets the id.
	 *
	 * @return the id
	 */
	public Long getId() {

		return id;
	}

	/**
	 * Sets the id.
	 *
	 * @param id the new id
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the discount rate.
	 *
	 * @return the discount rate
	 */
	public Long getDiscountRate() {

		return discountRate;
	}

	/**
	 * Sets the discount rate.
	 *
	 * @param discountRate the new discount rate
	 */
	public void setDiscountRate(Long discountRate) {

		this.discountRate = discountRate;
	}

	/**
	 * Gets the start date convention.
	 *
	 * @return the start date convention
	 */
	public Date getStartDateConvention() {

		return startDateConvention;
	}

	/**
	 * Sets the start date convention.
	 *
	 * @param startDateConvention the new start date convention
	 */
	public void setStartDateConvention(Date startDateConvention) {

		this.startDateConvention = startDateConvention;
	}

	/**
	 * Gets the end date convention.
	 *
	 * @return the end date convention
	 */
	public Date getEndDateConvention() {

		return endDateConvention;
	}

	/**
	 * Sets the end date convention.
	 *
	 * @param endDateConvention the new end date convention
	 */
	public void setEndDateConvention(Date endDateConvention) {

		this.endDateConvention = endDateConvention;
	}

	/**
	 * Gets the rebate.
	 *
	 * @return the rebate
	 */
	public Long getRebate() {

		return rebate;
	}

	/**
	 * Sets the rebate.
	 *
	 * @param rebate the new rebate
	 */
	public void setRebate(Long rebate) {

		this.rebate = rebate;
	}

	/**
	 * Gets the supplier.
	 *
	 * @return the supplier
	 */
	public Supplier getSupplier() {

		return supplier;
	}

	/**
	 * Sets the supplier.
	 *
	 * @param supplier the new supplier
	 */
	public void setSupplier(Supplier supplier) {

		this.supplier = supplier;
	}

	/**
	 * Gets the ca.
	 *
	 * @return the ca
	 */
	public Long getCa() {

		return ca;
	}

	/**
	 * Sets the ca.
	 *
	 * @param ca the new ca
	 */
	public void setCa(Long ca) {

		this.ca = ca;
	}

	/**
	 * Gets the apply rate.
	 *
	 * @return the apply rate
	 */
	public Boolean getApplyRate() {

		return applyRate;
	}

	/**
	 * Sets the apply rate.
	 *
	 * @param applyRate the new apply rate
	 */
	public void setApplyRate(Boolean applyRate) {

		this.applyRate = applyRate;
	}

	/**
	 * Gets the objective turnover volume.
	 *
	 * @return the objective turnover volume
	 */
	public Long getObjectiveTurnoverVolume() {

		return objectiveTurnoverVolume;
	}

	/**
	 * Sets the objective turnover volume.
	 *
	 * @param objectiveTurnoverVolume the new objective turnover volume
	 */
	public void setObjectiveTurnoverVolume(Long objectiveTurnoverVolume) {

		this.objectiveTurnoverVolume = objectiveTurnoverVolume;
	}

	/**
	 * Gets the objective file number.
	 *
	 * @return the objective file number
	 */
	public Long getObjectiveFileNumber() {

		return objectiveFileNumber;
	}

	/**
	 * Sets the objective file number.
	 *
	 * @param objectiveFileNumber the new objective file number
	 */
	public void setObjectiveFileNumber(Long objectiveFileNumber) {

		this.objectiveFileNumber = objectiveFileNumber;
	}

}
