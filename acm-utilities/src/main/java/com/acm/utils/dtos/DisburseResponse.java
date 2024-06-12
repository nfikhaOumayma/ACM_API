/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.util.ArrayList;

/**
 * {@link DisburseResponse} class.
 *
 * @author kouali
 * @since 0.1.0
 */
public class DisburseResponse {

	/** The receipt print parameters. */
	public ReceiptPrintParameters receiptPrintParameters;

	/** The customer ID. */
	public int customerID;

	/** The account loan trn. */
	public ArrayList<AccountLoanTrn> accountLoanTrn;

	/** The analysis links. */
	public Object analysisLinks;

	/** The surveys. */
	public Object surveys;

	/** The cu transfer account id. */
	public Object cuTransferAccountId;

	/** The temp token. */
	public Object tempToken;

	/** The currency ID loan issue. */
	public Object currencyIDLoanIssue;

	/** The issue date. */
	public Object issueDate;

	/** The issue date value. */
	public Object issueDateValue;

	/** The cheque details. */
	public Object chequeDetails;

	/** The is from loan issue. */
	public boolean isFromLoanIssue;

	/** The is issue fee payment changed. */
	public boolean isIssueFeePaymentChanged;

	/** The is issue fee payment changed CU account ID. */
	public int isIssueFeePaymentChangedCUAccountID;

	/** The issue fee payment method. */
	public int issueFeePaymentMethod;

	/** The cu account ID. */
	public Long cuAccountID;

	/** The from API. */
	public Object fromAPI;

	/** The account number. */
	public Object accountNumber;

	/** The amount. */
	public double amount;

	/** The notes. */
	public String notes;

	/** The transaction overrides. */
	public Object transactionOverrides;

	/** The receipt no. */
	public int receiptNo;

	/** The is supervise batch receipt. */
	public boolean isSuperviseBatchReceipt;

	/** The is rule of 78. */
	public boolean isRuleOf78;

	/** The has override. */
	public boolean hasOverride;

	/** The external ref no. */
	public String externalRefNo;

	/** The override user name. */
	public Object overrideUserName;

	/** The override password. */
	public Object overridePassword;

	/** The is external. */
	public boolean isExternal;

	/**
	 * Gets the receipt print parameters.
	 *
	 * @return the receiptPrintParameters
	 */
	public ReceiptPrintParameters getReceiptPrintParameters() {

		return receiptPrintParameters;
	}

	/**
	 * Sets the receipt print parameters.
	 *
	 * @param receiptPrintParameters the receiptPrintParameters to set
	 */
	public void setReceiptPrintParameters(ReceiptPrintParameters receiptPrintParameters) {

		this.receiptPrintParameters = receiptPrintParameters;
	}

	/**
	 * Gets the customer ID.
	 *
	 * @return the customerID
	 */
	public int getCustomerID() {

		return customerID;
	}

	/**
	 * Sets the customer ID.
	 *
	 * @param customerID the customerID to set
	 */
	public void setCustomerID(int customerID) {

		this.customerID = customerID;
	}

	/**
	 * Gets the account loan trn.
	 *
	 * @return the accountLoanTrn
	 */
	public ArrayList<AccountLoanTrn> getAccountLoanTrn() {

		return accountLoanTrn;
	}

	/**
	 * Sets the account loan trn.
	 *
	 * @param accountLoanTrn the accountLoanTrn to set
	 */
	public void setAccountLoanTrn(ArrayList<AccountLoanTrn> accountLoanTrn) {

		this.accountLoanTrn = accountLoanTrn;
	}

	/**
	 * Gets the analysis links.
	 *
	 * @return the analysisLinks
	 */
	public Object getAnalysisLinks() {

		return analysisLinks;
	}

	/**
	 * Sets the analysis links.
	 *
	 * @param analysisLinks the analysisLinks to set
	 */
	public void setAnalysisLinks(Object analysisLinks) {

		this.analysisLinks = analysisLinks;
	}

	/**
	 * Gets the surveys.
	 *
	 * @return the surveys
	 */
	public Object getSurveys() {

		return surveys;
	}

	/**
	 * Sets the surveys.
	 *
	 * @param surveys the surveys to set
	 */
	public void setSurveys(Object surveys) {

		this.surveys = surveys;
	}

	/**
	 * Gets the cu transfer account id.
	 *
	 * @return the cuTransferAccountId
	 */
	public Object getCuTransferAccountId() {

		return cuTransferAccountId;
	}

	/**
	 * Sets the cu transfer account id.
	 *
	 * @param cuTransferAccountId the cuTransferAccountId to set
	 */
	public void setCuTransferAccountId(Object cuTransferAccountId) {

		this.cuTransferAccountId = cuTransferAccountId;
	}

	/**
	 * Gets the temp token.
	 *
	 * @return the tempToken
	 */
	public Object getTempToken() {

		return tempToken;
	}

	/**
	 * Sets the temp token.
	 *
	 * @param tempToken the tempToken to set
	 */
	public void setTempToken(Object tempToken) {

		this.tempToken = tempToken;
	}

	/**
	 * Gets the currency ID loan issue.
	 *
	 * @return the currencyIDLoanIssue
	 */
	public Object getCurrencyIDLoanIssue() {

		return currencyIDLoanIssue;
	}

	/**
	 * Sets the currency ID loan issue.
	 *
	 * @param currencyIDLoanIssue the currencyIDLoanIssue to set
	 */
	public void setCurrencyIDLoanIssue(Object currencyIDLoanIssue) {

		this.currencyIDLoanIssue = currencyIDLoanIssue;
	}

	/**
	 * Gets the issue date.
	 *
	 * @return the issueDate
	 */
	public Object getIssueDate() {

		return issueDate;
	}

	/**
	 * Sets the issue date.
	 *
	 * @param issueDate the issueDate to set
	 */
	public void setIssueDate(Object issueDate) {

		this.issueDate = issueDate;
	}

	/**
	 * Gets the issue date value.
	 *
	 * @return the issueDateValue
	 */
	public Object getIssueDateValue() {

		return issueDateValue;
	}

	/**
	 * Sets the issue date value.
	 *
	 * @param issueDateValue the issueDateValue to set
	 */
	public void setIssueDateValue(Object issueDateValue) {

		this.issueDateValue = issueDateValue;
	}

	/**
	 * Gets the cheque details.
	 *
	 * @return the chequeDetails
	 */
	public Object getChequeDetails() {

		return chequeDetails;
	}

	/**
	 * Sets the cheque details.
	 *
	 * @param chequeDetails the chequeDetails to set
	 */
	public void setChequeDetails(Object chequeDetails) {

		this.chequeDetails = chequeDetails;
	}

	/**
	 * Checks if is from loan issue.
	 *
	 * @return the isFromLoanIssue
	 */
	public boolean isFromLoanIssue() {

		return isFromLoanIssue;
	}

	/**
	 * Sets the from loan issue.
	 *
	 * @param isFromLoanIssue the isFromLoanIssue to set
	 */
	public void setFromLoanIssue(boolean isFromLoanIssue) {

		this.isFromLoanIssue = isFromLoanIssue;
	}

	/**
	 * Checks if is issue fee payment changed.
	 *
	 * @return the isIssueFeePaymentChanged
	 */
	public boolean isIssueFeePaymentChanged() {

		return isIssueFeePaymentChanged;
	}

	/**
	 * Sets the issue fee payment changed.
	 *
	 * @param isIssueFeePaymentChanged the isIssueFeePaymentChanged to set
	 */
	public void setIssueFeePaymentChanged(boolean isIssueFeePaymentChanged) {

		this.isIssueFeePaymentChanged = isIssueFeePaymentChanged;
	}

	/**
	 * Gets the checks if is issue fee payment changed CU account ID.
	 *
	 * @return the isIssueFeePaymentChangedCUAccountID
	 */
	public int getIsIssueFeePaymentChangedCUAccountID() {

		return isIssueFeePaymentChangedCUAccountID;
	}

	/**
	 * Sets the checks if is issue fee payment changed CU account ID.
	 *
	 * @param isIssueFeePaymentChangedCUAccountID the isIssueFeePaymentChangedCUAccountID to set
	 */
	public void setIsIssueFeePaymentChangedCUAccountID(int isIssueFeePaymentChangedCUAccountID) {

		this.isIssueFeePaymentChangedCUAccountID = isIssueFeePaymentChangedCUAccountID;
	}

	/**
	 * Gets the issue fee payment method.
	 *
	 * @return the issueFeePaymentMethod
	 */
	public int getIssueFeePaymentMethod() {

		return issueFeePaymentMethod;
	}

	/**
	 * Sets the issue fee payment method.
	 *
	 * @param issueFeePaymentMethod the issueFeePaymentMethod to set
	 */
	public void setIssueFeePaymentMethod(int issueFeePaymentMethod) {

		this.issueFeePaymentMethod = issueFeePaymentMethod;
	}

	/**
	 * Gets the cu account ID.
	 *
	 * @return the cuAccountID
	 */
	public Long getCuAccountID() {

		return cuAccountID;
	}

	/**
	 * Sets the cu account ID.
	 *
	 * @param cuAccountID the cuAccountID to set
	 */
	public void setCuAccountID(Long cuAccountID) {

		this.cuAccountID = cuAccountID;
	}

	/**
	 * Gets the from API.
	 *
	 * @return the fromAPI
	 */
	public Object getFromAPI() {

		return fromAPI;
	}

	/**
	 * Sets the from API.
	 *
	 * @param fromAPI the fromAPI to set
	 */
	public void setFromAPI(Object fromAPI) {

		this.fromAPI = fromAPI;
	}

	/**
	 * Gets the account number.
	 *
	 * @return the accountNumber
	 */
	public Object getAccountNumber() {

		return accountNumber;
	}

	/**
	 * Sets the account number.
	 *
	 * @param accountNumber the accountNumber to set
	 */
	public void setAccountNumber(Object accountNumber) {

		this.accountNumber = accountNumber;
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
	 * @param amount the amount to set
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
	 * @param notes the notes to set
	 */
	public void setNotes(String notes) {

		this.notes = notes;
	}

	/**
	 * Gets the transaction overrides.
	 *
	 * @return the transactionOverrides
	 */
	public Object getTransactionOverrides() {

		return transactionOverrides;
	}

	/**
	 * Sets the transaction overrides.
	 *
	 * @param transactionOverrides the transactionOverrides to set
	 */
	public void setTransactionOverrides(Object transactionOverrides) {

		this.transactionOverrides = transactionOverrides;
	}

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
	 * Checks if is supervise batch receipt.
	 *
	 * @return the isSuperviseBatchReceipt
	 */
	public boolean isSuperviseBatchReceipt() {

		return isSuperviseBatchReceipt;
	}

	/**
	 * Sets the supervise batch receipt.
	 *
	 * @param isSuperviseBatchReceipt the isSuperviseBatchReceipt to set
	 */
	public void setSuperviseBatchReceipt(boolean isSuperviseBatchReceipt) {

		this.isSuperviseBatchReceipt = isSuperviseBatchReceipt;
	}

	/**
	 * Checks if is rule of 78.
	 *
	 * @return the isRuleOf78
	 */
	public boolean isRuleOf78() {

		return isRuleOf78;
	}

	/**
	 * Sets the rule of 78.
	 *
	 * @param isRuleOf78 the isRuleOf78 to set
	 */
	public void setRuleOf78(boolean isRuleOf78) {

		this.isRuleOf78 = isRuleOf78;
	}

	/**
	 * Checks if is checks for override.
	 *
	 * @return the hasOverride
	 */
	public boolean isHasOverride() {

		return hasOverride;
	}

	/**
	 * Sets the checks for override.
	 *
	 * @param hasOverride the hasOverride to set
	 */
	public void setHasOverride(boolean hasOverride) {

		this.hasOverride = hasOverride;
	}

	/**
	 * Gets the external ref no.
	 *
	 * @return the externalRefNo
	 */
	public String getExternalRefNo() {

		return externalRefNo;
	}

	/**
	 * Sets the external ref no.
	 *
	 * @param externalRefNo the externalRefNo to set
	 */
	public void setExternalRefNo(String externalRefNo) {

		this.externalRefNo = externalRefNo;
	}

	/**
	 * Gets the override user name.
	 *
	 * @return the overrideUserName
	 */
	public Object getOverrideUserName() {

		return overrideUserName;
	}

	/**
	 * Sets the override user name.
	 *
	 * @param overrideUserName the overrideUserName to set
	 */
	public void setOverrideUserName(Object overrideUserName) {

		this.overrideUserName = overrideUserName;
	}

	/**
	 * Gets the override password.
	 *
	 * @return the overridePassword
	 */
	public Object getOverridePassword() {

		return overridePassword;
	}

	/**
	 * Sets the override password.
	 *
	 * @param overridePassword the overridePassword to set
	 */
	public void setOverridePassword(Object overridePassword) {

		this.overridePassword = overridePassword;
	}

	/**
	 * Checks if is external.
	 *
	 * @return the isExternal
	 */
	public boolean isExternal() {

		return isExternal;
	}

	/**
	 * Sets the external.
	 *
	 * @param isExternal the isExternal to set
	 */
	public void setExternal(boolean isExternal) {

		this.isExternal = isExternal;
	}

}
