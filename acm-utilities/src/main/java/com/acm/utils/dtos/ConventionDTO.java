/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import com.acm.utils.models.Supplier;

/**
 * The {@link Supplier} class.
 * 
 * @author KhaledOuali
 * @since 1.12
 */
public class ConventionDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;

	/** The id. */
	private Long id;

	/** The name Supplier. */
	private Long discountRate;
	/** The start Date Convention . */
	private Date startDateConvention;
	/** The end Date Convention. */
	private Date endDateConvention;
	/** The rebate. */
	private Long rebate;
	/** The turnover. */
	private Long ca;

	/** The objective turnover volume. */
	private Long objectiveTurnoverVolume;

	/** The objective file number. */
	private Long objectiveFileNumber;

	/** The apply rate. */
	private Boolean applyRate;
	/** The supplier. */
	private Supplier supplier;

	/** The list docs type. */
	private List<SettingDocumentTypeDTO> listDocsType;

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

	/**
	 * Gets the list docs type.
	 *
	 * @return the list docs type
	 */
	public List<SettingDocumentTypeDTO> getListDocsType() {

		return listDocsType;
	}

	/**
	 * Sets the list docs type.
	 *
	 * @param listDocsType the new list docs type
	 */
	public void setListDocsType(List<SettingDocumentTypeDTO> listDocsType) {

		this.listDocsType = listDocsType;
	}

}
