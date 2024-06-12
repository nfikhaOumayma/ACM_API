/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * {@link SettingRequiredStepDTO} class.
 *
 * @author YesserSomai
 * @since 1.0.3
 */
public class SettingRequiredStepDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1054295509164987125L;

	/** The id. */
	private Long id;

	/** The product id. */
	private Integer productId;

	/** The code. -- FIELD VISIT || -- AUDIT REVIEW || -- RISK REVIEW. */
	private String code;

	/** The description. */
	private String description;

	/** The mandatory. */
	private Boolean mandatory;

	/**
	 * Instantiates a new setting required step DTO.
	 */
	public SettingRequiredStepDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new setting required step DTO.
	 *
	 * @param productId the product id
	 * @param mandatory the mandatory
	 */
	public SettingRequiredStepDTO(Integer productId, Boolean mandatory) {

		this.productId = productId;
		this.mandatory = mandatory;
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
	 * Gets the product id.
	 *
	 * @return the productId
	 */
	public Integer getProductId() {

		return productId;
	}

	/**
	 * Sets the product id.
	 *
	 * @param productId the productId to set
	 */
	public void setProductId(Integer productId) {

		this.productId = productId;
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
	 * Gets the mandatory.
	 *
	 * @return the mandatory
	 */
	public Boolean getMandatory() {

		return mandatory;
	}

	/**
	 * Sets the mandatory.
	 *
	 * @param mandatory the mandatory to set
	 */
	public void setMandatory(Boolean mandatory) {

		this.mandatory = mandatory;
	}

}
