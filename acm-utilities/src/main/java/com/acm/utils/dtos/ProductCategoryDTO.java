/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * {@link ProductCategoryDTO} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class ProductCategoryDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 8644540456678486307L;

	/** The id. */
	private Long id;

	/** The code. */
	private String code;

	/** The description. */
	private String description;

	/** The product ids list. */
	private String productIdsList;

	/** The incentive repayment. */
	private Boolean incentiveRepayment;

	/** The incentive registration. */
	private Boolean incentiveRegistration;

	/** The incentive operation. */
	private Boolean incentiveOperation;

	/** The incentive legal. */
	private Boolean incentiveLegal;

	/**
	 * Instantiates a new product category DTO.
	 */
	public ProductCategoryDTO() {

		/*
		 * EMPTY
		 */
	}

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
	 * @param id the id to set
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the code.
	 *
	 * @return the code
	 */
	public String getCode() {

		return code;
	}

	/**
	 * Sets the code.
	 *
	 * @param code the code to set
	 */
	public void setCode(String code) {

		this.code = code;
	}

	/**
	 * Gets the description.
	 *
	 * @return the description
	 */
	public String getDescription() {

		return description;
	}

	/**
	 * Sets the description.
	 *
	 * @param description the description to set
	 */
	public void setDescription(String description) {

		this.description = description;
	}

	/**
	 * Gets the product ids list.
	 *
	 * @return the productIdsList
	 */
	public String getProductIdsList() {

		return productIdsList;
	}

	/**
	 * Sets the product ids list.
	 *
	 * @param productIdsList the productIdsList to set
	 */
	public void setProductIdsList(String productIdsList) {

		this.productIdsList = productIdsList;
	}

	/**
	 * Gets the incentive repayment.
	 *
	 * @return the incentive repayment
	 */
	public Boolean getIncentiveRepayment() {

		return incentiveRepayment;
	}

	/**
	 * Sets the incentive repayment.
	 *
	 * @param incentiveRepayment the new incentive repayment
	 */
	public void setIncentiveRepayment(Boolean incentiveRepayment) {

		this.incentiveRepayment = incentiveRepayment;
	}

	/**
	 * Gets the incentive registration.
	 *
	 * @return the incentive registration
	 */
	public Boolean getIncentiveRegistration() {

		return incentiveRegistration;
	}

	/**
	 * Sets the incentive registration.
	 *
	 * @param incentiveRegistration the new incentive registration
	 */
	public void setIncentiveRegistration(Boolean incentiveRegistration) {

		this.incentiveRegistration = incentiveRegistration;
	}

	/**
	 * Gets the incentive operation.
	 *
	 * @return the incentive operation
	 */
	public Boolean getIncentiveOperation() {

		return incentiveOperation;
	}

	/**
	 * Sets the incentive operation.
	 *
	 * @param incentiveOperation the new incentive operation
	 */
	public void setIncentiveOperation(Boolean incentiveOperation) {

		this.incentiveOperation = incentiveOperation;
	}

	/**
	 * Gets the incentive legal.
	 *
	 * @return the incentive legal
	 */
	public Boolean getIncentiveLegal() {

		return incentiveLegal;
	}

	/**
	 * Sets the incentive legal.
	 *
	 * @param incentiveLegal the new incentive legal
	 */
	public void setIncentiveLegal(Boolean incentiveLegal) {

		this.incentiveLegal = incentiveLegal;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "ProductCategory [" + (id != null ? "id=" + id + ", " : "")
				+ (code != null ? "code=" + code + ", " : "")
				+ (description != null ? "description=" + description + ", " : "")
				+ (productIdsList != null ? "productIdsList=" + productIdsList : "") + "]";
	}

}
