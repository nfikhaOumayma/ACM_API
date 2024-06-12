/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The Class ApplicationDetailsDTO.
 */
public class ApplicationDetailsDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;

	/** The amount. */
	@JsonProperty("amount")
	private int amount;

	/** The product type. */
	@JsonProperty("productType")
	private int productType;

	/**
	 * Instantiates a new application details DTO.
	 */
	public ApplicationDetailsDTO() {

		super();
	}

	/**
	 * Gets the amount.
	 *
	 * @return the amount
	 */
	public int getAmount() {

		return amount;
	}

	/**
	 * Sets the amount.
	 *
	 * @param amount the new amount
	 */
	public void setAmount(int amount) {

		this.amount = amount;
	}

	/**
	 * Gets the product type.
	 *
	 * @return the product type
	 */
	public int getProductType() {

		return productType;
	}

	/**
	 * Sets the product type.
	 *
	 * @param productType the new product type
	 */
	public void setProductType(int productType) {

		this.productType = productType;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "ApplicationDetailsDTO [amount=" + amount + ", productType=" + productType + "]";
	}

}
