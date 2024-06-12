/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

/**
 * {@link CustomerActiveAccountDTO} class.
 *
 * @author MoezMhiri
 * @since 1.8.0
 */
public class CustomerActiveAccountDTO extends GenericDTO implements java.io.Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -7637384454449523010L;

	/** The customer id. */
	private Long customerId;

	/** The loan id. */
	private Long loanId;

	/**
	 * Instantiates a new customer DTO.
	 */
	public CustomerActiveAccountDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Gets the customer id.
	 *
	 * @return the customerId
	 */
	public Long getCustomerId() {

		return customerId;
	}

	/**
	 * Sets the customer id.
	 *
	 * @param customerId the customerId to set
	 */
	public void setCustomerId(Long customerId) {

		this.customerId = customerId;
	}

	/**
	 * Gets the loan id.
	 *
	 * @return the loanId
	 */
	public Long getLoanId() {

		return loanId;
	}

	/**
	 * Sets the loan id.
	 *
	 * @param loanId the loanId to set
	 */
	public void setLoanId(Long loanId) {

		this.loanId = loanId;
	}

}
