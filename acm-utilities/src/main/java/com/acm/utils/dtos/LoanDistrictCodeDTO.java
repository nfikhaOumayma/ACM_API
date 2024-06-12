/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * {@link LoanDistrictCodeDTO} class.
 *
 * @author MoezMhiri
 * @since 1.0.7
 */
public class LoanDistrictCodeDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 3075825174191891183L;

	/** The loan district code ID. */
	private Long loanDistrictCodeID;

	/** The code. */
	private String code;

	/** The description. */
	private String description;

	/** The default item. */
	private Integer defaultItem;

	/** The enabled. */
	private Boolean enabled;

	/**
	 * Instantiates a new loan district code DTO.
	 */
	public LoanDistrictCodeDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new loan district code DTO.
	 *
	 * @param loanDistrictCodeID the loan district code ID
	 * @param code the code
	 * @param description the description
	 * @param defaultItem the default item
	 * @param enabled the enabled
	 */
	public LoanDistrictCodeDTO(Long loanDistrictCodeID, String code, String description,
			Integer defaultItem, Boolean enabled) {

		this.loanDistrictCodeID = loanDistrictCodeID;
		this.code = code;
		this.description = description;
		this.defaultItem = defaultItem;
		this.enabled = enabled;
	}

	/**
	 * Gets the loan district code ID.
	 *
	 * @return the loanDistrictCodeID
	 */
	public Long getLoanDistrictCodeID() {

		return loanDistrictCodeID;
	}

	/**
	 * Sets the loan district code ID.
	 *
	 * @param loanDistrictCodeID the loanDistrictCodeID to set
	 */
	public void setLoanDistrictCodeID(Long loanDistrictCodeID) {

		this.loanDistrictCodeID = loanDistrictCodeID;
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
	 * Gets the default item.
	 *
	 * @return the defaultItem
	 */
	public Integer getDefaultItem() {

		return defaultItem;
	}

	/**
	 * Sets the default item.
	 *
	 * @param defaultItem the defaultItem to set
	 */
	public void setDefaultItem(Integer defaultItem) {

		this.defaultItem = defaultItem;
	}

	/**
	 * Gets the enabled.
	 *
	 * @return the enabled
	 */
	public Boolean getEnabled() {

		return enabled;
	}

	/**
	 * Sets the enabled.
	 *
	 * @param enabled the enabled to set
	 */
	public void setEnabled(Boolean enabled) {

		this.enabled = enabled;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "LoanDisctrictCodeDTO [loanDistrictCodeID=" + loanDistrictCodeID + ", code=" + code
				+ ", description=" + description + ", defaultItem=" + defaultItem + ", enabled="
				+ enabled + "]";
	}

}
