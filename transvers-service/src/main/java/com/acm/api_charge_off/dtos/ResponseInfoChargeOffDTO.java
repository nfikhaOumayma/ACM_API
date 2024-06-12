/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_charge_off.dtos;

import java.util.List;
import java.util.Map;

import com.acm.utils.dtos.GenericDTO;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The Class ResponseInfoChargeOffDTO.
 */
public class ResponseInfoChargeOffDTO extends GenericDTO {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -4300880140521098347L;

	/** The principal. */
	@JsonProperty("principal")
	public PrincipalDTO principal;

	/** The interest. */
	@JsonProperty("interest")
	public InterestDTO interest;

	/** The analysis links. */
	@JsonProperty("analysisLinks")
	private Object analysisLinks;

	/** The surveys. */
	@JsonProperty("surveys")
	private Object surveys;

	/** The error message. */
	@JsonProperty("errorMessage")
	private Object errorMessage;

	/** The charge off product ID. */
	@JsonProperty("chargeOffProductID")
	private int chargeOffProductID;

	/** The expiry date. */
	@JsonProperty("expiryDate")
	private String expiryDate;

	/** The loan status. */
	@JsonProperty("loanStatus")
	private int loanStatus;

	/** The original product ID. */
	@JsonProperty("originalProductID")
	private int originalProductID;

	/** The accounts. */
	@JsonProperty("accounts")
	private List<AccountDTO> accounts;

	/** The fees. */
	@JsonProperty("fees")
	private List<Object> fees;

	/** The is arrears. */
	@JsonProperty("isArrears")
	private Object isArrears;

	/** The is credit line. */
	@JsonProperty("isCreditLine")
	private boolean isCreditLine;

	/** The is tender edit restricted. */
	@JsonProperty("isTenderEditRestricted")
	private Object isTenderEditRestricted;

	/** The arrears. */
	@JsonProperty("arrears")
	private Object arrears;

	/** The days late. */
	@JsonProperty("daysLate")
	private int daysLate;

	/** The installment. */
	@JsonProperty("installment")
	private String installment;

	/** The due date. */
	@JsonProperty("dueDate")
	private String dueDate;

	/** The payment. */
	@JsonProperty("payment")
	private String payment;

	/** The account rating. */
	@JsonProperty("accountRating")
	private String accountRating;

	/** The due. */
	@JsonProperty("due")
	private String due;

	/** The balance date. */
	@JsonProperty("balanceDate")
	private Object balanceDate;

	/** The cleared funds. */
	@JsonProperty("clearedFunds")
	private Object clearedFunds;

	/** The tendered. */
	@JsonProperty("tendered")
	private Object tendered;

	/** The change. */
	@JsonProperty("change")
	private Object change;

	/** The field changed. */
	@JsonProperty("fieldChanged")
	private Object fieldChanged;

	/** The receipt type. */
	@JsonProperty("receiptType")
	private int receiptType;

	/** The totals. */
	@JsonProperty("totals")
	private TotalsDTO totals;

	/** The receipt print parameters. */
	@JsonProperty("receiptPrintParameters")
	private ReceiptPrintParametersDTO receiptPrintParameters;

	/** The currency ID. */
	@JsonProperty("currencyID")
	private int currencyID;

	/** The arrears days. */
	@JsonProperty("arrearsDays")
	private Map<String, Integer> arrearsDays;

	/** The written off information. */
	@JsonProperty("writtenOffInformation")
	private Object writtenOffInformation;

	/** The finger print URL. */
	@JsonProperty("fingerPrintURL")
	private Object fingerPrintURL;

	/** The is transfer account. */
	@JsonProperty("isTransferAccount")
	private boolean isTransferAccount;

	/** The is loan account. */
	@JsonProperty("isLoanAccount")
	private boolean isLoanAccount;

	/** The insurance. */
	@JsonProperty("insurance")
	private Object insurance;

	/** The dont settle linked savings. */
	@JsonProperty("dontSettleLinkedSavings")
	private boolean dontSettleLinkedSavings;

	/** The temp token. */
	@JsonProperty("tempToken")
	private Object tempToken;

	/** The can edit fee percentage. */
	@JsonProperty("canEditFeePercentage")
	private boolean canEditFeePercentage;

	/** The allow apply band fee. */
	@JsonProperty("allowApplyBandFee")
	private boolean allowApplyBandFee;

	/** The credit line accout. */
	@JsonProperty("creditLineAccout")
	private Object creditLineAccout;

	/** The is external. */
	@JsonProperty("isExternal")
	private boolean isExternal;

	/** The is savings account close. */
	@JsonProperty("isSavingsAccountClose")
	private boolean isSavingsAccountClose;

	/** The savings account close transaction amount. */
	@JsonProperty("savingsAccountCloseTransactionAmount")
	private Object savingsAccountCloseTransactionAmount;

	/** The product currency ID. */
	@JsonProperty("productCurrencyID")
	private int productCurrencyID;

	/** The parent account time stamp. */
	@JsonProperty("parentAccountTimeStamp")
	private Object parentAccountTimeStamp;

	/** The cu account ID. */
	@JsonProperty("cuAccountID")
	private int cuAccountID;

	/** The from API. */
	@JsonProperty("fromAPI")
	private Object fromAPI;

	/** The account number. */
	@JsonProperty("accountNumber")
	private Object accountNumber;

	/** The amount. */
	@JsonProperty("amount")
	private double amount;

	/** The notes. */
	@JsonProperty("notes")
	private String notes;

	/** The transaction overrides. */
	@JsonProperty("transactionOverrides")
	private Object transactionOverrides;

	/** The receipt no. */
	@JsonProperty("receiptNo")
	private int receiptNo;

	/** The is supervise batch receipt. */
	@JsonProperty("isSuperviseBatchReceipt")
	private boolean isSuperviseBatchReceipt;

	/** The is rule of 78. */
	@JsonProperty("isRuleOf78")
	private boolean isRuleOf78;

	/** The has override. */
	@JsonProperty("hasOverride")
	private boolean hasOverride;

	/** The external ref no. */
	@JsonProperty("externalRefNo")
	private String externalRefNo;

	/** The override user name. */
	@JsonProperty("overrideUserName")
	private Object overrideUserName;

	/** The override password. */
	@JsonProperty("overridePassword")
	private Object overridePassword;

	/**
	 * Gets the principal.
	 *
	 * @return the principal
	 */
	public PrincipalDTO getPrincipal() {

		return principal;
	}

	/**
	 * Sets the principal.
	 *
	 * @param principal the new principal
	 */
	public void setPrincipal(PrincipalDTO principal) {

		this.principal = principal;
	}

	/**
	 * Gets the interest.
	 *
	 * @return the interest
	 */
	public InterestDTO getInterest() {

		return interest;
	}

	/**
	 * Sets the interest.
	 *
	 * @param interest the new interest
	 */
	public void setInterest(InterestDTO interest) {

		this.interest = interest;
	}

	/**
	 * Gets the analysis links.
	 *
	 * @return the analysis links
	 */
	public Object getAnalysisLinks() {

		return analysisLinks;
	}

	/**
	 * Sets the analysis links.
	 *
	 * @param analysisLinks the new analysis links
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
	 * @param surveys the new surveys
	 */
	public void setSurveys(Object surveys) {

		this.surveys = surveys;
	}

	/**
	 * Gets the error message.
	 *
	 * @return the error message
	 */
	public Object getErrorMessage() {

		return errorMessage;
	}

	/**
	 * Sets the error message.
	 *
	 * @param errorMessage the new error message
	 */
	public void setErrorMessage(Object errorMessage) {

		this.errorMessage = errorMessage;
	}

	/**
	 * Gets the charge off product ID.
	 *
	 * @return the charge off product ID
	 */
	public int getChargeOffProductID() {

		return chargeOffProductID;
	}

	/**
	 * Sets the charge off product ID.
	 *
	 * @param chargeOffProductID the new charge off product ID
	 */
	public void setChargeOffProductID(int chargeOffProductID) {

		this.chargeOffProductID = chargeOffProductID;
	}

	/**
	 * Gets the expiry date.
	 *
	 * @return the expiry date
	 */
	public String getExpiryDate() {

		return expiryDate;
	}

	/**
	 * Sets the expiry date.
	 *
	 * @param expiryDate the new expiry date
	 */
	public void setExpiryDate(String expiryDate) {

		this.expiryDate = expiryDate;
	}

	/**
	 * Gets the loan status.
	 *
	 * @return the loan status
	 */
	public int getLoanStatus() {

		return loanStatus;
	}

	/**
	 * Sets the loan status.
	 *
	 * @param loanStatus the new loan status
	 */
	public void setLoanStatus(int loanStatus) {

		this.loanStatus = loanStatus;
	}

	/**
	 * Gets the original product ID.
	 *
	 * @return the original product ID
	 */
	public int getOriginalProductID() {

		return originalProductID;
	}

	/**
	 * Sets the original product ID.
	 *
	 * @param originalProductID the new original product ID
	 */
	public void setOriginalProductID(int originalProductID) {

		this.originalProductID = originalProductID;
	}

	/**
	 * Gets the accounts.
	 *
	 * @return the accounts
	 */
	public List<AccountDTO> getAccounts() {

		return accounts;
	}

	/**
	 * Sets the accounts.
	 *
	 * @param accounts the new accounts
	 */
	public void setAccounts(List<AccountDTO> accounts) {

		this.accounts = accounts;
	}

	/**
	 * Gets the fees.
	 *
	 * @return the fees
	 */
	public List<Object> getFees() {

		return fees;
	}

	/**
	 * Sets the fees.
	 *
	 * @param fees the new fees
	 */
	public void setFees(List<Object> fees) {

		this.fees = fees;
	}

	/**
	 * Gets the checks if is arrears.
	 *
	 * @return the checks if is arrears
	 */
	public Object getIsArrears() {

		return isArrears;
	}

	/**
	 * Sets the arrears.
	 *
	 * @param isArrears the new arrears
	 */
	public void setIsArrears(Object isArrears) {

		this.isArrears = isArrears;
	}

	/**
	 * Checks if is credit line.
	 *
	 * @return true, if is credit line
	 */
	public boolean isCreditLine() {

		return isCreditLine;
	}

	/**
	 * Sets the credit line.
	 *
	 * @param isCreditLine the new credit line
	 */
	public void setCreditLine(boolean isCreditLine) {

		this.isCreditLine = isCreditLine;
	}

	/**
	 * Gets the checks if is tender edit restricted.
	 *
	 * @return the checks if is tender edit restricted
	 */
	public Object getIsTenderEditRestricted() {

		return isTenderEditRestricted;
	}

	/**
	 * Sets the checks if is tender edit restricted.
	 *
	 * @param isTenderEditRestricted the new checks if is tender edit restricted
	 */
	public void setIsTenderEditRestricted(Object isTenderEditRestricted) {

		this.isTenderEditRestricted = isTenderEditRestricted;
	}

	/**
	 * Gets the arrears.
	 *
	 * @return the arrears
	 */
	public Object getArrears() {

		return arrears;
	}

	/**
	 * Sets the arrears.
	 *
	 * @param arrears the new arrears
	 */
	public void setArrears(Object arrears) {

		this.arrears = arrears;
	}

	/**
	 * Gets the days late.
	 *
	 * @return the days late
	 */
	public int getDaysLate() {

		return daysLate;
	}

	/**
	 * Sets the days late.
	 *
	 * @param daysLate the new days late
	 */
	public void setDaysLate(int daysLate) {

		this.daysLate = daysLate;
	}

	/**
	 * Gets the installment.
	 *
	 * @return the installment
	 */
	public String getInstallment() {

		return installment;
	}

	/**
	 * Sets the installment.
	 *
	 * @param installment the new installment
	 */
	public void setInstallment(String installment) {

		this.installment = installment;
	}

	/**
	 * Gets the due date.
	 *
	 * @return the due date
	 */
	public String getDueDate() {

		return dueDate;
	}

	/**
	 * Sets the due date.
	 *
	 * @param dueDate the new due date
	 */
	public void setDueDate(String dueDate) {

		this.dueDate = dueDate;
	}

	/**
	 * Gets the payment.
	 *
	 * @return the payment
	 */
	public String getPayment() {

		return payment;
	}

	/**
	 * Sets the payment.
	 *
	 * @param payment the new payment
	 */
	public void setPayment(String payment) {

		this.payment = payment;
	}

	/**
	 * Gets the account rating.
	 *
	 * @return the account rating
	 */
	public String getAccountRating() {

		return accountRating;
	}

	/**
	 * Sets the account rating.
	 *
	 * @param accountRating the new account rating
	 */
	public void setAccountRating(String accountRating) {

		this.accountRating = accountRating;
	}

	/**
	 * Gets the due.
	 *
	 * @return the due
	 */
	public String getDue() {

		return due;
	}

	/**
	 * Sets the due.
	 *
	 * @param due the new due
	 */
	public void setDue(String due) {

		this.due = due;
	}

	/**
	 * Gets the balance date.
	 *
	 * @return the balance date
	 */
	public Object getBalanceDate() {

		return balanceDate;
	}

	/**
	 * Sets the balance date.
	 *
	 * @param balanceDate the new balance date
	 */
	public void setBalanceDate(Object balanceDate) {

		this.balanceDate = balanceDate;
	}

	/**
	 * Gets the cleared funds.
	 *
	 * @return the cleared funds
	 */
	public Object getClearedFunds() {

		return clearedFunds;
	}

	/**
	 * Sets the cleared funds.
	 *
	 * @param clearedFunds the new cleared funds
	 */
	public void setClearedFunds(Object clearedFunds) {

		this.clearedFunds = clearedFunds;
	}

	/**
	 * Gets the tendered.
	 *
	 * @return the tendered
	 */
	public Object getTendered() {

		return tendered;
	}

	/**
	 * Sets the tendered.
	 *
	 * @param tendered the new tendered
	 */
	public void setTendered(Object tendered) {

		this.tendered = tendered;
	}

	/**
	 * Gets the change.
	 *
	 * @return the change
	 */
	public Object getChange() {

		return change;
	}

	/**
	 * Sets the change.
	 *
	 * @param change the new change
	 */
	public void setChange(Object change) {

		this.change = change;
	}

	/**
	 * Gets the field changed.
	 *
	 * @return the field changed
	 */
	public Object getFieldChanged() {

		return fieldChanged;
	}

	/**
	 * Sets the field changed.
	 *
	 * @param fieldChanged the new field changed
	 */
	public void setFieldChanged(Object fieldChanged) {

		this.fieldChanged = fieldChanged;
	}

	/**
	 * Gets the receipt type.
	 *
	 * @return the receipt type
	 */
	public int getReceiptType() {

		return receiptType;
	}

	/**
	 * Sets the receipt type.
	 *
	 * @param receiptType the new receipt type
	 */
	public void setReceiptType(int receiptType) {

		this.receiptType = receiptType;
	}

	/**
	 * Gets the totals.
	 *
	 * @return the totals
	 */
	public TotalsDTO getTotals() {

		return totals;
	}

	/**
	 * Sets the totals.
	 *
	 * @param totals the new totals
	 */
	public void setTotals(TotalsDTO totals) {

		this.totals = totals;
	}

	/**
	 * Gets the receipt print parameters.
	 *
	 * @return the receipt print parameters
	 */
	public ReceiptPrintParametersDTO getReceiptPrintParameters() {

		return receiptPrintParameters;
	}

	/**
	 * Sets the receipt print parameters.
	 *
	 * @param receiptPrintParameters the new receipt print parameters
	 */
	public void setReceiptPrintParameters(ReceiptPrintParametersDTO receiptPrintParameters) {

		this.receiptPrintParameters = receiptPrintParameters;
	}

	/**
	 * Gets the currency ID.
	 *
	 * @return the currency ID
	 */
	public int getCurrencyID() {

		return currencyID;
	}

	/**
	 * Sets the currency ID.
	 *
	 * @param currencyID the new currency ID
	 */
	public void setCurrencyID(int currencyID) {

		this.currencyID = currencyID;
	}

	/**
	 * Gets the arrears days.
	 *
	 * @return the arrears days
	 */
	public Map<String, Integer> getArrearsDays() {

		return arrearsDays;
	}

	/**
	 * Sets the arrears days.
	 *
	 * @param arrearsDays the arrears days
	 */
	public void setArrearsDays(Map<String, Integer> arrearsDays) {

		this.arrearsDays = arrearsDays;
	}

	/**
	 * Gets the written off information.
	 *
	 * @return the written off information
	 */
	public Object getWrittenOffInformation() {

		return writtenOffInformation;
	}

	/**
	 * Sets the written off information.
	 *
	 * @param writtenOffInformation the new written off information
	 */
	public void setWrittenOffInformation(Object writtenOffInformation) {

		this.writtenOffInformation = writtenOffInformation;
	}

	/**
	 * Gets the finger print URL.
	 *
	 * @return the finger print URL
	 */
	public Object getFingerPrintURL() {

		return fingerPrintURL;
	}

	/**
	 * Sets the finger print URL.
	 *
	 * @param fingerPrintURL the new finger print URL
	 */
	public void setFingerPrintURL(Object fingerPrintURL) {

		this.fingerPrintURL = fingerPrintURL;
	}

	/**
	 * Checks if is transfer account.
	 *
	 * @return true, if is transfer account
	 */
	public boolean isTransferAccount() {

		return isTransferAccount;
	}

	/**
	 * Sets the transfer account.
	 *
	 * @param isTransferAccount the new transfer account
	 */
	public void setTransferAccount(boolean isTransferAccount) {

		this.isTransferAccount = isTransferAccount;
	}

	/**
	 * Checks if is loan account.
	 *
	 * @return true, if is loan account
	 */
	public boolean isLoanAccount() {

		return isLoanAccount;
	}

	/**
	 * Sets the loan account.
	 *
	 * @param isLoanAccount the new loan account
	 */
	public void setLoanAccount(boolean isLoanAccount) {

		this.isLoanAccount = isLoanAccount;
	}

	/**
	 * Gets the insurance.
	 *
	 * @return the insurance
	 */
	public Object getInsurance() {

		return insurance;
	}

	/**
	 * Sets the insurance.
	 *
	 * @param insurance the new insurance
	 */
	public void setInsurance(Object insurance) {

		this.insurance = insurance;
	}

	/**
	 * Checks if is dont settle linked savings.
	 *
	 * @return true, if is dont settle linked savings
	 */
	public boolean isDontSettleLinkedSavings() {

		return dontSettleLinkedSavings;
	}

	/**
	 * Sets the dont settle linked savings.
	 *
	 * @param dontSettleLinkedSavings the new dont settle linked savings
	 */
	public void setDontSettleLinkedSavings(boolean dontSettleLinkedSavings) {

		this.dontSettleLinkedSavings = dontSettleLinkedSavings;
	}

	/**
	 * Gets the temp token.
	 *
	 * @return the temp token
	 */
	public Object getTempToken() {

		return tempToken;
	}

	/**
	 * Sets the temp token.
	 *
	 * @param tempToken the new temp token
	 */
	public void setTempToken(Object tempToken) {

		this.tempToken = tempToken;
	}

	/**
	 * Checks if is can edit fee percentage.
	 *
	 * @return true, if is can edit fee percentage
	 */
	public boolean isCanEditFeePercentage() {

		return canEditFeePercentage;
	}

	/**
	 * Sets the can edit fee percentage.
	 *
	 * @param canEditFeePercentage the new can edit fee percentage
	 */
	public void setCanEditFeePercentage(boolean canEditFeePercentage) {

		this.canEditFeePercentage = canEditFeePercentage;
	}

	/**
	 * Checks if is allow apply band fee.
	 *
	 * @return true, if is allow apply band fee
	 */
	public boolean isAllowApplyBandFee() {

		return allowApplyBandFee;
	}

	/**
	 * Sets the allow apply band fee.
	 *
	 * @param allowApplyBandFee the new allow apply band fee
	 */
	public void setAllowApplyBandFee(boolean allowApplyBandFee) {

		this.allowApplyBandFee = allowApplyBandFee;
	}

	/**
	 * Gets the credit line accout.
	 *
	 * @return the credit line accout
	 */
	public Object getCreditLineAccout() {

		return creditLineAccout;
	}

	/**
	 * Sets the credit line accout.
	 *
	 * @param creditLineAccout the new credit line accout
	 */
	public void setCreditLineAccout(Object creditLineAccout) {

		this.creditLineAccout = creditLineAccout;
	}

	/**
	 * Checks if is external.
	 *
	 * @return true, if is external
	 */
	public boolean isExternal() {

		return isExternal;
	}

	/**
	 * Sets the external.
	 *
	 * @param isExternal the new external
	 */
	public void setExternal(boolean isExternal) {

		this.isExternal = isExternal;
	}

	/**
	 * Checks if is savings account close.
	 *
	 * @return true, if is savings account close
	 */
	public boolean isSavingsAccountClose() {

		return isSavingsAccountClose;
	}

	/**
	 * Sets the savings account close.
	 *
	 * @param isSavingsAccountClose the new savings account close
	 */
	public void setSavingsAccountClose(boolean isSavingsAccountClose) {

		this.isSavingsAccountClose = isSavingsAccountClose;
	}

	/**
	 * Gets the savings account close transaction amount.
	 *
	 * @return the savings account close transaction amount
	 */
	public Object getSavingsAccountCloseTransactionAmount() {

		return savingsAccountCloseTransactionAmount;
	}

	/**
	 * Sets the savings account close transaction amount.
	 *
	 * @param savingsAccountCloseTransactionAmount the new savings account close transaction amount
	 */
	public void setSavingsAccountCloseTransactionAmount(
			Object savingsAccountCloseTransactionAmount) {

		this.savingsAccountCloseTransactionAmount = savingsAccountCloseTransactionAmount;
	}

	/**
	 * Gets the product currency ID.
	 *
	 * @return the product currency ID
	 */
	public int getProductCurrencyID() {

		return productCurrencyID;
	}

	/**
	 * Sets the product currency ID.
	 *
	 * @param productCurrencyID the new product currency ID
	 */
	public void setProductCurrencyID(int productCurrencyID) {

		this.productCurrencyID = productCurrencyID;
	}

	/**
	 * Gets the parent account time stamp.
	 *
	 * @return the parent account time stamp
	 */
	public Object getParentAccountTimeStamp() {

		return parentAccountTimeStamp;
	}

	/**
	 * Sets the parent account time stamp.
	 *
	 * @param parentAccountTimeStamp the new parent account time stamp
	 */
	public void setParentAccountTimeStamp(Object parentAccountTimeStamp) {

		this.parentAccountTimeStamp = parentAccountTimeStamp;
	}

	/**
	 * Gets the cu account ID.
	 *
	 * @return the cu account ID
	 */
	public int getCuAccountID() {

		return cuAccountID;
	}

	/**
	 * Sets the cu account ID.
	 *
	 * @param cuAccountID the new cu account ID
	 */
	public void setCuAccountID(int cuAccountID) {

		this.cuAccountID = cuAccountID;
	}

	/**
	 * Gets the from API.
	 *
	 * @return the from API
	 */
	public Object getFromAPI() {

		return fromAPI;
	}

	/**
	 * Sets the from API.
	 *
	 * @param fromAPI the new from API
	 */
	public void setFromAPI(Object fromAPI) {

		this.fromAPI = fromAPI;
	}

	/**
	 * Gets the account number.
	 *
	 * @return the account number
	 */
	public Object getAccountNumber() {

		return accountNumber;
	}

	/**
	 * Sets the account number.
	 *
	 * @param accountNumber the new account number
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
	 * Gets the transaction overrides.
	 *
	 * @return the transaction overrides
	 */
	public Object getTransactionOverrides() {

		return transactionOverrides;
	}

	/**
	 * Sets the transaction overrides.
	 *
	 * @param transactionOverrides the new transaction overrides
	 */
	public void setTransactionOverrides(Object transactionOverrides) {

		this.transactionOverrides = transactionOverrides;
	}

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
	 * Checks if is supervise batch receipt.
	 *
	 * @return true, if is supervise batch receipt
	 */
	public boolean isSuperviseBatchReceipt() {

		return isSuperviseBatchReceipt;
	}

	/**
	 * Sets the supervise batch receipt.
	 *
	 * @param isSuperviseBatchReceipt the new supervise batch receipt
	 */
	public void setSuperviseBatchReceipt(boolean isSuperviseBatchReceipt) {

		this.isSuperviseBatchReceipt = isSuperviseBatchReceipt;
	}

	/**
	 * Checks if is rule of 78.
	 *
	 * @return true, if is rule of 78
	 */
	public boolean isRuleOf78() {

		return isRuleOf78;
	}

	/**
	 * Sets the rule of 78.
	 *
	 * @param isRuleOf78 the new rule of 78
	 */
	public void setRuleOf78(boolean isRuleOf78) {

		this.isRuleOf78 = isRuleOf78;
	}

	/**
	 * Checks if is checks for override.
	 *
	 * @return true, if is checks for override
	 */
	public boolean isHasOverride() {

		return hasOverride;
	}

	/**
	 * Sets the checks for override.
	 *
	 * @param hasOverride the new checks for override
	 */
	public void setHasOverride(boolean hasOverride) {

		this.hasOverride = hasOverride;
	}

	/**
	 * Gets the external ref no.
	 *
	 * @return the external ref no
	 */
	public String getExternalRefNo() {

		return externalRefNo;
	}

	/**
	 * Sets the external ref no.
	 *
	 * @param externalRefNo the new external ref no
	 */
	public void setExternalRefNo(String externalRefNo) {

		this.externalRefNo = externalRefNo;
	}

	/**
	 * Gets the override user name.
	 *
	 * @return the override user name
	 */
	public Object getOverrideUserName() {

		return overrideUserName;
	}

	/**
	 * Sets the override user name.
	 *
	 * @param overrideUserName the new override user name
	 */
	public void setOverrideUserName(Object overrideUserName) {

		this.overrideUserName = overrideUserName;
	}

	/**
	 * Gets the override password.
	 *
	 * @return the override password
	 */
	public Object getOverridePassword() {

		return overridePassword;
	}

	/**
	 * Sets the override password.
	 *
	 * @param overridePassword the new override password
	 */
	public void setOverridePassword(Object overridePassword) {

		this.overridePassword = overridePassword;
	}

}
