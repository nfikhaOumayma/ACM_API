/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * {@link IncentiveSettingConstantDTO} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class IncentiveSettingConstantDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -8329198541439292077L;

	/** The id. */
	private Long id;

	/** The code. */
	private String code;

	/** The description. */
	private String description;

	/**
	 * The category ACM_INCENTIVE_REPAYMENT / ACM_INCENTIVE_REGESTRATION / ACM_INCENTIVE_OPERATION /
	 * ACM_INCENTIVE_LEGAL / FREQUENCY / INCENTIVE_SETTING_TYPE /
	 * INCENTIVE_REGESTRATION_CUSTOMER_TYPE.
	 */
	private String category;

	/**
	 * Instantiates a new incentive setting based on DTO.
	 */
	public IncentiveSettingConstantDTO() {

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
