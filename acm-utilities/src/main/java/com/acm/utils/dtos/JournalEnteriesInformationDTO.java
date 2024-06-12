/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.math.BigDecimal;

/**
 * {@link JournalEnteriesInformationDTO } class.
 *
 * @author kouali
 * @since 0.1.0
 */
public class JournalEnteriesInformationDTO {

	/** The description. */
	private String description;

	/** The reference. */
	private String reference;

	/** The value date. */
	private String valueDate;

	/** The account ID. */
	private Long accountID;

	/** The credit. */
	private boolean credit;

	/** The amount. */
	private BigDecimal amount;

	/** The currency amount. */
	private BigDecimal currencyAmount;

	/** The currency ID. */
	private long currencyID;

	/** The exchange rate. */
	private int exchangeRate;

	/** The branch ID. */
	private long branchID;

	/** The journal ID. */
	private int journalID;

	/** The is IBT entry required. */
	private boolean isIBTEntryRequired;

	/** The is IBT entry. */
	private boolean isIBTEntry;

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
	 * Gets the reference.
	 *
	 * @return the reference
	 */
	public String getReference() {

		return reference;
	}

	/**
	 * Sets the reference.
	 *
	 * @param reference the reference to set
	 */
	public void setReference(String reference) {

		this.reference = reference;
	}

	/**
	 * Gets the value date.
	 *
	 * @return the valueDate
	 */
	public String getValueDate() {

		return valueDate;
	}

	/**
	 * Sets the value date.
	 *
	 * @param valueDate the valueDate to set
	 */
	public void setValueDate(String valueDate) {

		this.valueDate = valueDate;
	}

	/**
	 * Gets the account ID.
	 *
	 * @return the accountID
	 */
	public Long getAccountID() {

		return accountID;
	}

	/**
	 * Sets the account ID.
	 *
	 * @param accountID the accountID to set
	 */
	public void setAccountID(Long accountID) {

		this.accountID = accountID;
	}

	/**
	 * Checks if is credit.
	 *
	 * @return the credit
	 */
	public boolean isCredit() {

		return credit;
	}

	/**
	 * Sets the credit.
	 *
	 * @param credit the credit to set
	 */
	public void setCredit(boolean credit) {

		this.credit = credit;
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
	 * @param amount the amount to set
	 */
	public void setAmount(BigDecimal amount) {

		this.amount = amount;
	}

	/**
	 * Gets the currency amount.
	 *
	 * @return the currencyAmount
	 */
	public BigDecimal getCurrencyAmount() {

		return currencyAmount;
	}

	/**
	 * Sets the currency amount.
	 *
	 * @param currencyAmount the currencyAmount to set
	 */
	public void setCurrencyAmount(BigDecimal currencyAmount) {

		this.currencyAmount = currencyAmount;
	}

	/**
	 * Gets the currency ID.
	 *
	 * @return the currencyID
	 */
	public long getCurrencyID() {

		return currencyID;
	}

	/**
	 * Sets the currency ID.
	 *
	 * @param currencyID the currencyID to set
	 */
	public void setCurrencyID(long currencyID) {

		this.currencyID = currencyID;
	}

	/**
	 * Gets the exchange rate.
	 *
	 * @return the exchangeRate
	 */
	public int getExchangeRate() {

		return exchangeRate;
	}

	/**
	 * Sets the exchange rate.
	 *
	 * @param exchangeRate the exchangeRate to set
	 */
	public void setExchangeRate(int exchangeRate) {

		this.exchangeRate = exchangeRate;
	}

	/**
	 * Gets the branch ID.
	 *
	 * @return the branchID
	 */
	public long getBranchID() {

		return branchID;
	}

	/**
	 * Sets the branch ID.
	 *
	 * @param branchID the branchID to set
	 */
	public void setBranchID(long branchID) {

		this.branchID = branchID;
	}

	/**
	 * Gets the journal ID.
	 *
	 * @return the journalID
	 */
	public int getJournalID() {

		return journalID;
	}

	/**
	 * Sets the journal ID.
	 *
	 * @param journalID the journalID to set
	 */
	public void setJournalID(int journalID) {

		this.journalID = journalID;
	}

	/**
	 * Gets the checks if is IBT entry required.
	 *
	 * @return the isIBTEntryRequired
	 */
	public boolean getIsIBTEntryRequired() {

		return isIBTEntryRequired;
	}

	/**
	 * Sets the checks if is IBT entry required.
	 *
	 * @param isIBTEntryRequired the isIBTEntryRequired to set
	 */
	public void setIsIBTEntryRequired(boolean isIBTEntryRequired) {

		this.isIBTEntryRequired = isIBTEntryRequired;
	}

	/**
	 * Gets the checks if is IBT entry.
	 *
	 * @return the isIBTEntry
	 */
	public boolean getIsIBTEntry() {

		return isIBTEntry;
	}

	/**
	 * Sets the checks if is IBT entry.
	 *
	 * @param isIBTEntry the isIBTEntry to set
	 */
	public void setIsIBTEntry(boolean isIBTEntry) {

		this.isIBTEntry = isIBTEntry;
	}

}
