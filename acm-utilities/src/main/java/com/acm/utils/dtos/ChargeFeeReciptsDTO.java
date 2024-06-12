/*
 * 
 */
package com.acm.utils.dtos;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The Class ChargeFeeReciptsDTO.
 */
public class ChargeFeeReciptsDTO extends GenericDTO {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -7995421975464477118L;

	/** The receipt no. */
	@JsonProperty("receiptNo")
	public int receiptNo;

	/** The branch code. */
	@JsonProperty("branchCode")
	public String branchCode;

	/** The hide balance. */
	@JsonProperty("hideBalance")
	public boolean hideBalance;

	/** The reprint. */
	@JsonProperty("reprint")
	public boolean reprint;

	/** The print cheque. */
	@JsonProperty("printCheque")
	public boolean printCheque;

	/** The with cash out receipt. */
	@JsonProperty("withCashOutReceipt")
	public boolean withCashOutReceipt;

	/** The use default format. */
	@JsonProperty("useDefaultFormat")
	public boolean useDefaultFormat;

	/** The format. */
	@JsonProperty("format")
	public String format;

	/** The can print. */
	@JsonProperty("canPrint")
	public boolean canPrint;

	/** The is supervise batch. */
	@JsonProperty("isSuperviseBatch")
	public boolean isSuperviseBatch;

	/**
	 * Gets the receipt no.
	 *
	 * @return the receipt no
	 */
	public int getReceiptNo() {

		return receiptNo;
	}

	/**
	 * Sets the receipt no.
	 *
	 * @param receiptNo the new receipt no
	 */
	public void setReceiptNo(int receiptNo) {

		this.receiptNo = receiptNo;
	}

	/**
	 * Gets the branch code.
	 *
	 * @return the branch code
	 */
	public String getBranchCode() {

		return branchCode;
	}

	/**
	 * Sets the branch code.
	 *
	 * @param branchCode the new branch code
	 */
	public void setBranchCode(String branchCode) {

		this.branchCode = branchCode;
	}

	/**
	 * Checks if is hide balance.
	 *
	 * @return true, if is hide balance
	 */
	public boolean isHideBalance() {

		return hideBalance;
	}

	/**
	 * Sets the hide balance.
	 *
	 * @param hideBalance the new hide balance
	 */
	public void setHideBalance(boolean hideBalance) {

		this.hideBalance = hideBalance;
	}

	/**
	 * Checks if is reprint.
	 *
	 * @return true, if is reprint
	 */
	public boolean isReprint() {

		return reprint;
	}

	/**
	 * Sets the reprint.
	 *
	 * @param reprint the new reprint
	 */
	public void setReprint(boolean reprint) {

		this.reprint = reprint;
	}

	/**
	 * Checks if is prints the cheque.
	 *
	 * @return true, if is prints the cheque
	 */
	public boolean isPrintCheque() {

		return printCheque;
	}

	/**
	 * Sets the prints the cheque.
	 *
	 * @param printCheque the new prints the cheque
	 */
	public void setPrintCheque(boolean printCheque) {

		this.printCheque = printCheque;
	}

	/**
	 * Checks if is with cash out receipt.
	 *
	 * @return true, if is with cash out receipt
	 */
	public boolean isWithCashOutReceipt() {

		return withCashOutReceipt;
	}

	/**
	 * Sets the with cash out receipt.
	 *
	 * @param withCashOutReceipt the new with cash out receipt
	 */
	public void setWithCashOutReceipt(boolean withCashOutReceipt) {

		this.withCashOutReceipt = withCashOutReceipt;
	}

	/**
	 * Checks if is use default format.
	 *
	 * @return true, if is use default format
	 */
	public boolean isUseDefaultFormat() {

		return useDefaultFormat;
	}

	/**
	 * Sets the use default format.
	 *
	 * @param useDefaultFormat the new use default format
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
	 * @param format the new format
	 */
	public void setFormat(String format) {

		this.format = format;
	}

	/**
	 * Checks if is can print.
	 *
	 * @return true, if is can print
	 */
	public boolean isCanPrint() {

		return canPrint;
	}

	/**
	 * Sets the can print.
	 *
	 * @param canPrint the new can print
	 */
	public void setCanPrint(boolean canPrint) {

		this.canPrint = canPrint;
	}

	/**
	 * Checks if is supervise batch.
	 *
	 * @return true, if is supervise batch
	 */
	public boolean isSuperviseBatch() {

		return isSuperviseBatch;
	}

	/**
	 * Sets the supervise batch.
	 *
	 * @param isSuperviseBatch the new supervise batch
	 */
	public void setSuperviseBatch(boolean isSuperviseBatch) {

		this.isSuperviseBatch = isSuperviseBatch;
	}

}
