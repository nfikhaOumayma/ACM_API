/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.math.BigDecimal;
import java.util.Date;

/**
 * {@link FeeRepaymentDTO} class.
 *
 * @author SalmenFATNASSI
 * @since 1.0.8
 */
public class FeeRepaymentDTO extends GenericDTO implements java.io.Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -5121562226101196776L;

	/** The transaction id. */
	private Long transactionId;

	/** The value date. */
	private Date valueDate;

	/** The amount. */
	private BigDecimal amount;

	/**
	 * Instantiates a new fee repayment DTO.
	 */
	public FeeRepaymentDTO() {

		/*
		 * Empty
		 */
	}

	/**
	 * Instantiates a new fee repayment DTO.
	 *
	 * @param transactionId the transaction id
	 * @param valueDate the value date
	 * @param amount the amount
	 */
	public FeeRepaymentDTO(Long transactionId, Date valueDate, BigDecimal amount) {

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

}
