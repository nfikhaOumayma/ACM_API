/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * {@link LoanRefinanceReasonDTO} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class LoanRefinanceReasonDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 6363966141326778129L;

	/** The loan refinance reason ID. */
	private Long loanRefinanceReasonID;

	/** The code. */
	private String code;

	/** The description. */
	private String description;

	/** The all products. */
	private Boolean allProducts;

	/** The loan product I ds. */
	private String loanProductIDs;

	/** The default item. */
	private Integer defaultItem;

	/** The enabled. */
	private Boolean enabled;

	/**
	 * Instantiates a new loan refinance reason DTO.
	 */
	public LoanRefinanceReasonDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new loan refinance reason DTO.
	 *
	 * @param loanRefinanceReasonID the loan refinance reason ID
	 * @param code the code
	 * @param description the description
	 * @param allProducts the all products
	 * @param loanProductIDs the loan product I ds
	 * @param defaultItem the default item
	 * @param enabled the enabled
	 */
	public LoanRefinanceReasonDTO(Long loanRefinanceReasonID, String code, String description,
			Boolean allProducts, String loanProductIDs, Integer defaultItem, Boolean enabled) {

		this.loanRefinanceReasonID = loanRefinanceReasonID;
		this.code = code;
		this.description = description;
		this.allProducts = allProducts;
		this.loanProductIDs = loanProductIDs;
		this.defaultItem = defaultItem;
		this.enabled = enabled;
	}

	/**
	 * Gets the loan refinance reason ID.
	 *
	 * @return the loanRefinanceReasonID
	 */
	public Long getLoanRefinanceReasonID() {

		return loanRefinanceReasonID;
	}

	/**
	 * Sets the loan refinance reason ID.
	 *
	 * @param loanRefinanceReasonID the loanRefinanceReasonID to set
	 */
	public void setLoanRefinanceReasonID(Long loanRefinanceReasonID) {

		this.loanRefinanceReasonID = loanRefinanceReasonID;
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
	 * Gets the all products.
	 *
	 * @return the allProducts
	 */
	public Boolean getAllProducts() {

		return allProducts;
	}

	/**
	 * Sets the all products.
	 *
	 * @param allProducts the allProducts to set
	 */
	public void setAllProducts(Boolean allProducts) {

		this.allProducts = allProducts;
	}

	/**
	 * Gets the loan product I ds.
	 *
	 * @return the loanProductIDs
	 */
	public String getLoanProductIDs() {

		return loanProductIDs;
	}

	/**
	 * Sets the loan product I ds.
	 *
	 * @param loanProductIDs the loanProductIDs to set
	 */
	public void setLoanProductIDs(String loanProductIDs) {

		this.loanProductIDs = loanProductIDs;
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

		return "LoanRefinanceReasonDTO [loanRefinanceReasonID=" + loanRefinanceReasonID + ", code="
				+ code + ", description=" + description + ", allProducts=" + allProducts
				+ ", loanProductIDs=" + loanProductIDs + ", defaultItem=" + defaultItem
				+ ", enabled=" + enabled + "]";
	}

}
