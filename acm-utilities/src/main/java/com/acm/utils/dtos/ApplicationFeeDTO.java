/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.math.BigDecimal;
import java.util.Date;

/**
 * {@link ApplicationFeeDTO} class.
 *
 * @author MoezMhiri
 * @since 1.1.5
 */
public class ApplicationFeeDTO extends GenericDTO implements java.io.Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -5121562226101196776L;

	/** The transaction id. */
	private Long transactionId;

	/** The value date. */
	private Date valueDate;

	/** The amount. */
	private BigDecimal amount;

	/** The CU fee ID. */
	private long cufeeID;

	/** The code. */
	private String code;

	/** The description. */
	private String description;

	/** The product id. */
	private Integer productId;

	/**
	 * Instantiates a new application fee DTO.
	 */
	public ApplicationFeeDTO() {

		/*
		 * Empty
		 */
	}

	/**
	 * Instantiates a new application fee DTO.
	 *
	 * @param transactionId the transaction id
	 * @param valueDate the value date
	 * @param amount the amount
	 */
	public ApplicationFeeDTO(Long transactionId, Date valueDate, BigDecimal amount) {

		this.transactionId = transactionId;
		this.valueDate = valueDate;
		this.amount = amount;
	}

	/**
	 * Gets the transaction id.
	 *
	 * @return the transaction id
	 */
	public Long getTransactionId() {

		return transactionId;
	}

	/**
	 * Sets the transaction id.
	 *
	 * @param transactionId the new transaction id
	 */
	public void setTransactionId(Long transactionId) {

		this.transactionId = transactionId;
	}

	/**
	 * Gets the value date.
	 *
	 * @return the value date
	 */
	public Date getValueDate() {

		return valueDate;
	}

	/**
	 * Sets the value date.
	 *
	 * @param valueDate the new value date
	 */
	public void setValueDate(Date valueDate) {

		this.valueDate = valueDate;
	}

	/**
	 * Gets the amount.
	 *
	 * @return the amount
	 */
	public BigDecimal getAmount() {

		return amount;
	}

	/**
	 * Sets the amount.
	 *
	 * @param amount the new amount
	 */
	public void setAmount(BigDecimal amount) {

		this.amount = amount;
	}

	/**
	 * Gets the cufee ID.
	 *
	 * @return the cufeeID
	 */
	public long getCufeeID() {

		return cufeeID;
	}

	/**
	 * Sets the cufee ID.
	 *
	 * @param cufeeID the cufeeID to set
	 */
	public void setCufeeID(long cufeeID) {

		this.cufeeID = cufeeID;
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
	 * @param description the new description
	 */
	public void setDescription(String description) {

		this.description = description;
	}

	/**
	 * Gets the product id.
	 *
	 * @return the product id
	 */
	public Integer getProductId() {

		return productId;
	}

	/**
	 * Sets the product id.
	 *
	 * @param productId the new product id
	 */
	public void setProductId(Integer productId) {

		this.productId = productId;
	}

}
