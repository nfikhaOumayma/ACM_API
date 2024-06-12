/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

/**
 * {@link ReceiptPrintParameters } class.
 *
 * @author kouali
 * @since 0.1.0
 */
public class ReceiptPrintParameters {

	/** The receipt no. */
	public int receiptNo;

	/** The branch code. */
	public String branchCode;

	/** The hide balance. */
	public boolean hideBalance;

	/** The reprint. */
	public boolean reprint;

	/** The print cheque. */
	public boolean printCheque;

	/** The with cash out receipt. */
	public boolean withCashOutReceipt;

	/** The use default format. */
	public boolean useDefaultFormat;

	/** The format. */
	public String format;

	/** The can print. */
	public boolean canPrint;

	/** The is supervise batch. */
	public boolean isSuperviseBatch;

	/**
	 * Gets the receipt no.
	 *
	 * @return the receiptNo
	 */
	public int getReceiptNo() {

		return receiptNo;
	}

	/**
	 * Sets the receipt no.
	 *
	 * @param receiptNo the receiptNo to set
	 */
	public void setReceiptNo(int receiptNo) {

		this.receiptNo = receiptNo;
	}

	/**
	 * Gets the branch code.
	 *
	 * @return the branchCode
	 */
	public String getBranchCode() {

		return branchCode;
	}

	/**
	 * Sets the branch code.
	 *
	 * @param branchCode the branchCode to set
	 */
	public void setBranchCode(String branchCode) {

		this.branchCode = branchCode;
	}

	/**
	 * Checks if is hide balance.
	 *
	 * @return the hideBalance
	 */
	public boolean isHideBalance() {

		return hideBalance;
	}

	/**
	 * Sets the hide balance.
	 *
	 * @param hideBalance the hideBalance to set
	 */
	public void setHideBalance(boolean hideBalance) {

		this.hideBalance = hideBalance;
	}

	/**
	 * Checks if is reprint.
	 *
	 * @return the reprint
	 */
	public boolean isReprint() {

		return reprint;
	}

	/**
	 * Sets the reprint.
	 *
	 * @param reprint the reprint to set
	 */
	public void setReprint(boolean reprint) {

		this.reprint = reprint;
	}

	/**
	 * Checks if is prints the cheque.
	 *
	 * @return the printCheque
	 */
	public boolean isPrintCheque() {

		return printCheque;
	}

	/**
	 * Sets the prints the cheque.
	 *
	 * @param printCheque the printCheque to set
	 */
	public void setPrintCheque(boolean printCheque) {

		this.printCheque = printCheque;
	}

	/**
	 * Checks if is with cash out receipt.
	 *
	 * @return the withCashOutReceipt
	 */
	public boolean isWithCashOutReceipt() {

		return withCashOutReceipt;
	}

	/**
	 * Sets the with cash out receipt.
	 *
	 * @param withCashOutReceipt the withCashOutReceipt to set
	 */
	public void setWithCashOutReceipt(boolean withCashOutReceipt) {

		this.withCashOutReceipt = withCashOutReceipt;
	}

	/**
	 * Checks if is use default format.
	 *
	 * @return the useDefaultFormat
	 */
	public boolean isUseDefaultFormat() {

		return useDefaultFormat;
	}

	/**
	 * Sets the use default format.
	 *
	 * @param useDefaultFormat the useDefaultFormat to set
	 */
	public void setUseDefaultFormat(boolean useDefaultFormat) {

		this.useDefaultFormat = useDefaultFormat;
	}

	/**
	 * Gets the format.
	 *
	 * @return the format
	 */
	public String getFormat() {

		return format;
	}

	/**
	 * Sets the format.
	 *
	 * @param format the format to set
	 */
	public void setFormat(String format) {

		this.format = format;
	}

	/**
	 * Checks if is can print.
	 *
	 * @return the canPrint
	 */
	public boolean isCanPrint() {

		return canPrint;
	}

	/**
	 * Sets the can print.
	 *
	 * @param canPrint the canPrint to set
	 */
	public void setCanPrint(boolean canPrint) {

		this.canPrint = canPrint;
	}

	/**
	 * Checks if is supervise batch.
	 *
	 * @return the isSuperviseBatch
	 */
	public boolean isSuperviseBatch() {

		return isSuperviseBatch;
	}

	/**
	 * Sets the supervise batch.
	 *
	 * @param isSuperviseBatch the isSuperviseBatch to set
	 */
	public void setSuperviseBatch(boolean isSuperviseBatch) {

		this.isSuperviseBatch = isSuperviseBatch;
	}

}
