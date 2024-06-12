/*
 * 
 */
package com.acm.utils.dtos;

import java.math.BigDecimal;

/**
 * The Class ChargeFeeDTO.
 */
public class ChargeFeeDTO extends GenericDTO {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 8698302357169603329L;

	/** The cu account fee ID. */
	public Object cuAccountFeeID;

	/** The cu fee ID. */
	public int cuFeeID;

	/** The manually added. */
	public boolean manuallyAdded;

	/** The can edit amount. */
	public boolean canEditAmount;

	/** The adjust fee. */
	public Object adjustFee;

	/** The account ID. */
	public int accountID;

	/** The paid. */
	public boolean paid;

	/** The charged. */
	public boolean charged;

	/** The allow amount change. */
	public boolean allowAmountChange;

	/** The amount. */
	public BigDecimal amount;

	/** The amount paid. */
	public double amountPaid;

	/** The is vat. */
	public boolean isVat;

	/** The open balance. */
	public BigDecimal openBalance;

	/** The debit amount. */
	public Object debitAmount;

	/** The credit amount. */
	public Object creditAmount;

	/** The closing balance. */
	public Object closingBalance;

	/** The id. */
	public Object id;

	/**
	 * Gets the cu account fee ID.
	 *
	 * @return the cu account fee ID
	 */
	public Object getCuAccountFeeID() {

		return cuAccountFeeID;
	}

	/**
	 * Sets the cu account fee ID.
	 *
	 * @param cuAccountFeeID the new cu account fee ID
	 */
	public void setCuAccountFeeID(Object cuAccountFeeID) {

		this.cuAccountFeeID = cuAccountFeeID;
	}

	/**
	 * Gets the cu fee ID.
	 *
	 * @return the cu fee ID
	 */
	public int getCuFeeID() {

		return cuFeeID;
	}

	/**
	 * Sets the cu fee ID.
	 *
	 * @param cuFeeID the new cu fee ID
	 */
	public void setCuFeeID(int cuFeeID) {

		this.cuFeeID = cuFeeID;
	}

	/**
	 * Checks if is manually added.
	 *
	 * @return true, if is manually added
	 */
	public boolean isManuallyAdded() {

		return manuallyAdded;
	}

	/**
	 * Sets the manually added.
	 *
	 * @param manuallyAdded the new manually added
	 */
	public void setManuallyAdded(boolean manuallyAdded) {

		this.manuallyAdded = manuallyAdded;
	}

	/**
	 * Checks if is can edit amount.
	 *
	 * @return true, if is can edit amount
	 */
	public boolean isCanEditAmount() {

		return canEditAmount;
	}

	/**
	 * Sets the can edit amount.
	 *
	 * @param canEditAmount the new can edit amount
	 */
	public void setCanEditAmount(boolean canEditAmount) {

		this.canEditAmount = canEditAmount;
	}

	/**
	 * Gets the adjust fee.
	 *
	 * @return the adjust fee
	 */
	public Object getAdjustFee() {

		return adjustFee;
	}

	/**
	 * Sets the adjust fee.
	 *
	 * @param adjustFee the new adjust fee
	 */
	public void setAdjustFee(Object adjustFee) {

		this.adjustFee = adjustFee;
	}

	/**
	 * Gets the account ID.
	 *
	 * @return the account ID
	 */
	public int getAccountID() {

		return accountID;
	}

	/**
	 * Sets the account ID.
	 *
	 * @param accountID the new account ID
	 */
	public void setAccountID(int accountID) {

		this.accountID = accountID;
	}

	/**
	 * Checks if is paid.
	 *
	 * @return true, if is paid
	 */
	public boolean isPaid() {

		return paid;
	}

	/**
	 * Sets the paid.
	 *
	 * @param paid the new paid
	 */
	public void setPaid(boolean paid) {

		this.paid = paid;
	}

	/**
	 * Checks if is charged.
	 *
	 * @return true, if is charged
	 */
	public boolean isCharged() {

		return charged;
	}

	/**
	 * Sets the charged.
	 *
	 * @param charged the new charged
	 */
	public void setCharged(boolean charged) {

		this.charged = charged;
	}

	/**
	 * Checks if is allow amount change.
	 *
	 * @return true, if is allow amount change
	 */
	public boolean isAllowAmountChange() {

		return allowAmountChange;
	}

	/**
	 * Sets the allow amount change.
	 *
	 * @param allowAmountChange the new allow amount change
	 */
	public void setAllowAmountChange(boolean allowAmountChange) {

		this.allowAmountChange = allowAmountChange;
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
	 * Gets the amount paid.
	 *
	 * @return the amount paid
	 */
	public double getAmountPaid() {

		return amountPaid;
	}

	/**
	 * Sets the amount paid.
	 *
	 * @param amountPaid the new amount paid
	 */
	public void setAmountPaid(double amountPaid) {

		this.amountPaid = amountPaid;
	}

	/**
	 * Checks if is vat.
	 *
	 * @return true, if is vat
	 */
	public boolean isVat() {

		return isVat;
	}

	/**
	 * Sets the vat.
	 *
	 * @param isVat the new vat
	 */
	public void setVat(boolean isVat) {

		this.isVat = isVat;
	}

	/**
	 * Gets the open balance.
	 *
	 * @return the open balance
	 */
	public BigDecimal getOpenBalance() {

		return openBalance;
	}

	/**
	 * Sets the open balance.
	 *
	 * @param openBalance the new open balance
	 */
	public void setOpenBalance(BigDecimal openBalance) {

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
