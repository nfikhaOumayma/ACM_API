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

/**
 * {@link IncentiveSettingConstant} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
@Entity
@Table(name = "ACM_INCENTIVE_SETTING_CONSTANT")
public class IncentiveSettingConstant extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 2015179393226043151L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_INCENTIVE_SETTING_CONSTANT", unique = true, nullable = false)
	private Long id;

	/** The code. */
	@Column(name = "CODE", nullable = false)
	private String code;

	/** The description. */
	@Column(name = "DESCRIPTION", nullable = false)
	private String description;

	/**
	 * The category ACM_INCENTIVE_REPAYMENT / ACM_INCENTIVE_REGESTRATION / ACM_INCENTIVE_OPERATION /
	 * ACM_INCENTIVE_LEGAL / FREQUENCY / INCENTIVE_SETTING_TYPE /
	 * INCENTIVE_REGESTRATION_CUSTOMER_TYPE.
	 */
	@Column(name = "CATEGORY", nullable = false)
	private String category;

	/**
	 * Instantiates a new incentive setting based on.
	 */
	public IncentiveSettingConstant() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new incentive setting constant.
	 *
	 * @param id the id
	 */
	public IncentiveSettingConstant(Long id) {

		this.id = id;
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
	 * Gets the category.
	 *
	 * @return the category
	 */
	public String getCategory() {

		return category;
	}

	/**
	 * Sets the category.
	 *
	 * @param category the category to set
	 */
	public void setCategory(String category) {

		this.category = category;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "IncentiveSettingConstant [" + (id != null ? "id=" + id + ", " : "")
				+ (code != null ? "code=" + code + ", " : "")
				+ (description != null ? "description=" + description + ", " : "")
				+ (category != null ? "category=" + category : "") + "]";
	}

}
