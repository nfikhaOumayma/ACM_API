/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * {@link ExpensesJournalPageDTO} class.
 *
 * @author Ines Dridi
 * @since 1.0.8
 */
public class ExpensesJournalPageDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -814631055473139457L;

	/** The id. */
	private Long id;

	/** The expenses description. */
	private String expensesDescription;

	/** The expenses description. */
	private String expensesReference;

	/** The amount. */
	private Long amount;

	/** The credit account. */
	private String creditAccount;

	/** The debit account. */
	private String debitAccount;

	/**
	 * Instantiates a new expenses journal page DTO.
	 */
	public ExpensesJournalPageDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new expenses journal page DTO.
	 *
	 * @param expensesDescription the expenses description
	 * @param amount the amount
	 * @param creditAccount the credit account
	 * @param debitAccount the debit account
	 * @param expensesReference the expensesReference
	 */
	public ExpensesJournalPageDTO(String expensesDescription, Long amount, String creditAccount,
			String debitAccount, String expensesReference) {

		super();
		this.expensesDescription = expensesDescription;
		this.amount = amount;
		this.creditAccount = creditAccount;
		this.debitAccount = debitAccount;
		this.expensesReference = expensesReference;
	}

	/**
	 * Instantiates a new expenses journal page DTO.
	 *
	 * @param expensesDescription the expenses description
	 * @param expensesReference the expenses reference
	 * @param amount the amount
	 * @param creditAccount the credit account
	 * @param debitAccount the debit account
	 */
	public ExpensesJournalPageDTO(String expensesDescription, String expensesReference, Long amount,
			String creditAccount, String debitAccount) {

		super();
		this.expensesDescription = expensesDescription;
		this.expensesReference = expensesReference;
		this.amount = amount;
		this.creditAccount = creditAccount;
		this.debitAccount = debitAccount;
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
	 * @param id the new id
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the expenses description.
	 *
	 * @return the expenses description
	 */
	public String getExpensesDescription() {

		return expensesDescription;
	}

	/**
	 * Sets the expenses description.
	 *
	 * @param expensesDescription the new expenses description
	 */
	public void setExpensesDescription(String expensesDescription) {

		this.expensesDescription = expensesDescription;
	}

	/**
	 * Gets the amount.
	 *
	 * @return the amount
	 */
	public Long getAmount() {

		return amount;
	}

	/**
	 * Sets the amount.
	 *
	 * @param amount the new amount
	 */
	public void setAmount(Long amount) {

		this.amount = amount;
	}

	/**
	 * Gets the credit account.
	 *
	 * @return the credit account
	 */
	public String getCreditAccount() {

		return creditAccount;
	}

	/**
	 * Sets the credit account.
	 *
	 * @param creditAccount the new credit account
	 */
	public void setCreditAccount(String creditAccount) {

		this.creditAccount = creditAccount;
	}

	/**
	 * Gets the debit account.
	 *
	 * @return the debit account
	 */
	public String getDebitAccount() {

		return debitAccount;
	}

	/**
	 * Sets the debit account.
	 *
	 * @param debitAccount the new debit account
	 */
	public void setDebitAccount(String debitAccount) {

		this.debitAccount = debitAccount;
	}

	/**
	 * Gets the expenses reference.
	 *
	 * @return the expenses reference
	 */
	public String getExpensesReference() {

		return expensesReference;
	}

	/**
	 * Sets the expenses reference.
	 *
	 * @param expensesReference the new expenses reference
	 */
	public void setExpensesReference(String expensesReference) {

		this.expensesReference = expensesReference;
	}

}
