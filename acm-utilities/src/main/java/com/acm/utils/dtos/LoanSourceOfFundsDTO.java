/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * {@link LoanSourceOfFundsDTO} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class LoanSourceOfFundsDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1415318594096158918L;

	/** The loan source of funds ID. */
	private Long loanSourceOfFundsID;

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

	/** The high level source fund. */
	private Long highLevelSourceFund;

	/** The sourceof fundtype. */
	private Long sourceofFundtype;

	/** The currency ID. */
	private Long currencyID;

	/**
	 * Instantiates a new loan source of funds DTO.
	 */
	public LoanSourceOfFundsDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new loan source of funds DTO.
	 *
	 * @param loanSourceOfFundsID the loan source of funds ID
	 * @param code the code
	 * @param description the description
	 * @param allProducts the all products
	 * @param loanProductIDs the loan product I ds
	 * @param defaultItem the default item
	 * @param enabled the enabled
	 * @param highLevelSourceFund the high level source fund
	 * @param sourceofFundtype the sourceof fundtype
	 * @param currencyID the currency ID
	 */
	public LoanSourceOfFundsDTO(Long loanSourceOfFundsID, String code, String description,
			Boolean allProducts, String loanProductIDs, Integer defaultItem, Boolean enabled,
			Long highLevelSourceFund, Long sourceofFundtype, Long currencyID) {

		this.loanSourceOfFundsID = loanSourceOfFundsID;
		this.code = code;
		this.description = description;
		this.allProducts = allProducts;
		this.loanProductIDs = loanProductIDs;
		this.defaultItem = defaultItem;
		this.enabled = enabled;
		this.highLevelSourceFund = highLevelSourceFund;
		this.sourceofFundtype = sourceofFundtype;
		this.currencyID = currencyID;
	}

	/**
	 * Gets the loan source of funds ID.
	 *
	 * @return the loanSourceOfFundsID
	 */
	public Long getLoanSourceOfFundsID() {

		return loanSourceOfFundsID;
	}

	/**
	 * Sets the loan source of funds ID.
	 *
	 * @param loanSourceOfFundsID the loanSourceOfFundsID to set
	 */
	public void setLoanSourceOfFundsID(Long loanSourceOfFundsID) {

		this.loanSourceOfFundsID = loanSourceOfFundsID;
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

	/**
	 * Gets the high level source fund.
	 *
	 * @return the highLevelSourceFund
	 */
	public Long getHighLevelSourceFund() {

		return highLevelSourceFund;
	}

	/**
	 * Sets the high level source fund.
	 *
	 * @param highLevelSourceFund the highLevelSourceFund to set
	 */
	public void setHighLevelSourceFund(Long highLevelSourceFund) {

		this.highLevelSourceFund = highLevelSourceFund;
	}

	/**
	 * Gets the sourceof fundtype.
	 *
	 * @return the sourceofFundtype
	 */
	public Long getSourceofFundtype() {

		return sourceofFundtype;
	}

	/**
	 * Sets the sourceof fundtype.
	 *
	 * @param sourceofFundtype the sourceofFundtype to set
	 */
	public void setSourceofFundtype(Long sourceofFundtype) {

		this.sourceofFundtype = sourceofFundtype;
	}

	/**
	 * Gets the currency ID.
	 *
	 * @return the currencyID
	 */
	public Long getCurrencyID() {

		return currencyID;
	}

	/**
	 * Sets the currency ID.
	 *
	 * @param currencyID the currencyID to set
	 */
	public void setCurrencyID(Long currencyID) {

		this.currencyID = currencyID;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "LoanSourceOfFundsDTO [loanSourceOfFundsID=" + loanSourceOfFundsID + ", code=" + code
				+ ", description=" + description + ", allProducts=" + allProducts
				+ ", loanProductIDs=" + loanProductIDs + ", defaultItem=" + defaultItem
				+ ", enabled=" + enabled + ", highLevelSourceFund=" + highLevelSourceFund
				+ ", sourceofFundtype=" + sourceofFundtype + ", currencyID=" + currencyID + "]";
	}

}
