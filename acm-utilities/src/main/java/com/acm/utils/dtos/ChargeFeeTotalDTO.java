package com.acm.utils.dtos;

import com.fasterxml.jackson.annotation.JsonProperty;

public class ChargeFeeTotalDTO extends GenericDTO {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -4785879925142900517L;

	/** The name. */
	@JsonProperty("name")
	public Object name;

	/** The open balance. */
	@JsonProperty("openBalance")
	public Object openBalance;

	/** The debit amount. */
	@JsonProperty("debitAmount")
	public Object debitAmount;

	/** The credit amount. */
	@JsonProperty("creditAmount")
	public Object creditAmount;

	/** The amount paid. */
	@JsonProperty("amountPaid")
	public Object amountPaid;

	/** The closing balance. */
	@JsonProperty("closingBalance")
	public Object closingBalance;

	/** The can edit amount. */
	@JsonProperty("canEditAmount")
	public Object canEditAmount;

	/** The id. */
	@JsonProperty("id")
	public Object id;

	/**
	 * Gets the name.
	 *
	 * @return the name
	 */
	public Object getName() {

		return name;
	}

	/**
	 * Sets the name.
	 *
	 * @param name the new name
	 */
	public void setName(Object name) {

		this.name = name;
	}

	/**
	 * Gets the open balance.
	 *
	 * @return the open balance
	 */
	public Object getOpenBalance() {

		return openBalance;
	}

	/**
	 * Sets the open balance.
	 *
	 * @param openBalance the new open balance
	 */
	public void setOpenBalance(Object openBalance) {

		this.openBalance = openBalance;
	}

	/**
	 * Gets the debit amount.
	 *
	 * @return the debit amount
	 */
	public Object getDebitAmount() {

		return debitAmount;
	}

	/**
	 * Sets the debit amount.
	 *
	 * @param debitAmount the new debit amount
	 */
	public void setDebitAmount(Object debitAmount) {

		this.debitAmount = debitAmount;
	}

	/**
	 * Gets the credit amount.
	 *
	 * @return the credit amount
	 */
	public Object getCreditAmount() {

		return creditAmount;
	}

	/**
	 * Sets the credit amount.
	 *
	 * @param creditAmount the new credit amount
	 */
	public void setCreditAmount(Object creditAmount) {

		this.creditAmount = creditAmount;
	}

	/**
	 * Gets the amount paid.
	 *
	 * @return the amount paid
	 */
	public Object getAmountPaid() {

		return amountPaid;
	}

	/**
	 * Sets the amount paid.
	 *
	 * @param amountPaid the new amount paid
	 */
	public void setAmountPaid(Object amountPaid) {

		this.amountPaid = amountPaid;
	}

	/**
	 * Gets the closing balance.
	 *
	 * @return the closing balance
	 */
	public Object getClosingBalance() {

		return closingBalance;
	}

	/**
	 * Sets the closing balance.
	 *
	 * @param closingBalance the new closing balance
	 */
	public void setClosingBalance(Object closingBalance) {

		this.closingBalance = closingBalance;
	}

	/**
	 * Gets the can edit amount.
	 *
	 * @return the can edit amount
	 */
	public Object getCanEditAmount() {

		return canEditAmount;
	}

	/**
	 * Sets the can edit amount.
	 *
	 * @param canEditAmount the new can edit amount
	 */
	public void setCanEditAmount(Object canEditAmount) {

		this.canEditAmount = canEditAmount;
	}

	/**
	 * Gets the id.
	 *
	 * @return the id
	 */
	public Object getId() {

		return id;
	}

	/**
	 * Sets the id.
	 *
	 * @param id the new id
	 */
	public void setId(Object id) {

		this.id = id;
	}

}
