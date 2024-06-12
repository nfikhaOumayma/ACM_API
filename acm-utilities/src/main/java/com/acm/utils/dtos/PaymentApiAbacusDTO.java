/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * The Class PaymentApiAbacusDTO.
 */
public class PaymentApiAbacusDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 3776507944566723555L;

	/** The pay fee. */
	private boolean payFee;

	/** The account number. */
	private String accountNumber;

	/** The amount. */
	private double amount;

	/** The notes. */
	private String notes;

	/**
	 * Checks if is pay fee.
	 *
	 * @return true, if is pay fee
	 */
	public boolean isPayFee() {

		return payFee;
	}

	/**
	 * Sets the pay fee.
	 *
	 * @param payFee the new pay fee
	 */
	public void setPayFee(boolean payFee) {

		this.payFee = payFee;
	}

	/**
	 * Gets the amount.
	 *
	 * @return the amount
	 */
	public double getAmount() {

		return amount;
	}

	/**
	 * Sets the amount.
	 *
	 * @param amount the new amount
	 */
	public void setAmount(double amount) {

		this.amount = amount;
	}

	/**
	 * Gets the notes.
	 *
	 * @return the notes
	 */
	public String getNotes() {

		return notes;
	}

	/**
	 * Sets the notes.
	 *
	 * @param notes the new notes
	 */
	public void setNotes(String notes) {

		this.notes = notes;
	}

	/**
	 * Instantiates a new payment api abacus DTO.
	 */
	public PaymentApiAbacusDTO() {

		super();
	}

	/**
	 * Gets the account number.
	 *
	 * @return the account number
	 */
	public String getAccountNumber() {

		return accountNumber;
	}

	/**
	 * Sets the account number.
	 *
	 * @param accountNumber the new account number
	 */
	public void setAccountNumber(String accountNumber) {

		this.accountNumber = accountNumber;
	}

	/**
	 * Instantiates a new payment api abacus DTO.
	 *
	 * @param payFee the pay fee
	 * @param accountNumber the account number
	 * @param amount the amount
	 * @param notes the notes
	 */
	public PaymentApiAbacusDTO(boolean payFee, String accountNumber, double amount, String notes) {

		super();
		this.payFee = payFee;
		this.accountNumber = accountNumber;
		this.amount = amount;
		this.notes = notes;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "PaymentApiAbacusDTO [payFee=" + payFee + ", accountNumber=" + accountNumber
				+ ", amount=" + amount + ", notes=" + notes + "]";
	}

}
