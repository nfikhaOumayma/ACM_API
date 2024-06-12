/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

import com.acm.utils.dtos.SettingRequiredStepDTO;

/**
 * {@link SettingRequiredStepDTO} class.
 *
 * @author YesserSomai
 * @since 1.0.3
 */
@Entity
@Table(name = "ACM_SETTING_REQUIRED_STEP")
public class SettingRequiredStep extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 2715912519056636909L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_SETTING_REQUIRED_STEP", unique = true, nullable = false)
	private Long id;

	/** The product id. */
	@Column(name = "ID_PRODUCT", nullable = false)
	private Integer productId;

	/** The code. -- FIELD VISIT || -- AUDIT REVIEW || -- RISK REVIEW. */
	@Column(name = "CODE", nullable = false)
	private String code;

	/** The description. */
	@Column(name = "DESCRIPTION")
	private String description;

	/** The mandatory. */
	@Column(name = "MANDATORY")
	private Boolean mandatory;

	/**
	 * Instantiates a new setting gurantor collateral.
	 */
	public SettingRequiredStep() {

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
