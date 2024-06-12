/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * {@link LoanGuarantorSourceDTO} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class LoanGuarantorSourceDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 2583631944518618025L;

	/** The loan guarantor source ID. */
	private Long loanGuarantorSourceID;

	/** The code. */
	private String code;

	/** The description. */
	private String description;

	/** The default item. */
	private Integer defaultItem;

	/** The enabled. */
	private Boolean enabled;

	/**
	 * Instantiates a new loan guarantor source DTO.
	 */
	public LoanGuarantorSourceDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new loan guarantor source DTO.
	 *
	 * @param loanGuarantorSourceID the loan guarantor source ID
	 * @param code the code
	 * @param description the description
	 * @param defaultItem the default item
	 * @param enabled the enabled
	 */
	public LoanGuarantorSourceDTO(Long loanGuarantorSourceID, String code, String description,
			Integer defaultItem, Boolean enabled) {

		this.loanGuarantorSourceID = loanGuarantorSourceID;
		this.code = code;
		this.description = description;
		this.defaultItem = defaultItem;
		this.enabled = enabled;
	}

	/**
	 * Gets the loan guarantor source ID.
	 *
	 * @return the loanGuarantorSourceID
	 */
	public Long getLoanGuarantorSourceID() {

		return loanGuarantorSourceID;
	}

	/**
	 * Sets the loan guarantor source ID.
	 *
	 * @param loanGuarantorSourceID the loanGuarantorSourceID to set
	 */
	public void setLoanGuarantorSourceID(Long loanGuarantorSourceID) {

		this.loanGuarantorSourceID = loanGuarantorSourceID;
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

		return "LoanGuarantorSourceDTO [loanGuarantorSourceID=" + loanGuarantorSourceID + ", code="
				+ code + ", description=" + description + ", defaultItem=" + defaultItem
				+ ", enabled=" + enabled + "]";
	}

}
