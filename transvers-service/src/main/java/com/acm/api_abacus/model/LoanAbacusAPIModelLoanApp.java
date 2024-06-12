/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.model;

import java.io.Serializable;
import java.util.Date;

/**
 * {@link LoanAbacusAPIModelLoanApp} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class LoanAbacusAPIModelLoanApp implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 826623854798375537L;

	/** The cu loan ID. */
	private int cuLoanID;

	/** The loan part. */
	private LoanAbacusAPIModelLoanPart loanPart;

	/** The loan reason ID. */
	private int loanReasonID;

	/** The loan source of funds ID. */
	private int loanSourceOfFundsID;

	/** The cu loan guarantor source ID. */
	private int cuLoanGuarantorSourceID;

	/** The cu loan district code ID. */
	private int cuLoanDistrictCodeID;

	/** The cu loan refinance reason ID. */
	private int cuLoanRefinanceReasonID;

	/** The status. */
	private int status;

	/** The multiple parts. */
	private boolean multipleParts;

	/** The redraw. */
	private boolean redraw;

	/** The apply amount total. */
	private double applyAmountTotal;

	/** The apply date. */
	private Date applyDate;

	/** The approve amount total. */
	private double approveAmountTotal;

	/** The approve date. */
	private Date approveDate;

	/** The cancel reason ID. */
	private int cancelReasonID;

	/** The cancel notes. */
	private String cancelNotes;

	/** The cais flag. */
	private int caisFlag;

	/** The cais flags equifax. */
	private int caisFlagsEquifax;

	/** The settlement customer ID. */
	private int settlementCustomerID;

	/** The settlement CU account ID. */
	private int settlementCUAccountID;

	/** The settlement CU account is third party. */
	private boolean settlementCUAccountIsThirdParty;

	/** The payment customer ID. */
	private int paymentCustomerID;

	/** The payment CU account ID. */
	private int paymentCUAccountID;

	/** The payment CU account is third party. */
	private boolean paymentCUAccountIsThirdParty;

	/** The transfer accrued interest only. */
	private boolean transferAccruedInterestOnly;

	/** The restructured. */
	private boolean restructured;

	/** The final part date. */
	private Date finalPartDate;

	/** The last restructured date. */
	private double lastRestructuredDate;

	/** The cu loan processes. */
	private Object cuLoanProcesses;

	/** The last panalty interest date. */
	private Date lastPanaltyInterestDate;

	/** The apply user ID. */
	private int applyUserID;

	/** The loan collaterals. */
	private Object loanCollaterals;

	/** The renewal old CU account ID. */
	private int renewalOldCUAccountID;

	/** The excluded assets. */
	private double excludedAssets;

	/** The collateral amount. */
	private double collateralAmount;

	/** The transfer savings account ID. */
	private int transferSavingsAccountID;

	/** The issue date. */
	private Date issueDate;

	/** The loan status. */
	private int loanStatus;

	/** The unfinished. */
	private int unfinished;

	/** The account number. */
	private String accountNumber;

	/** The process ID. */
	private int processID;

	/** The is process completed. */
	private boolean isProcessCompleted;

	/** The process description. */
	private String processDescription;

	/** The menu key. */
	private String menuKey;

	/** The is sign off. */
	private boolean isSignOff;

	/** The is user access. */
	private boolean isUserAccess;

	/** The cu account ID. */
	private int cuAccountID;

	/**
	 * Instantiates a new loan abacus API model loan app.
	 */
	public LoanAbacusAPIModelLoanApp() {

		// Empty
	}

	/**
	 * Gets the cu loan ID.
	 *
	 * @return the cuLoanID
	 */
	public int getCuLoanID() {

		return cuLoanID;
	}

	/**
	 * Sets the cu loan ID.
	 *
	 * @param cuLoanID the cuLoanID to set
	 */
	public void setCuLoanID(int cuLoanID) {

		this.cuLoanID = cuLoanID;
	}

	/**
	 * Gets the loan part.
	 *
	 * @return the loanPart
	 */
	public LoanAbacusAPIModelLoanPart getLoanPart() {

		return loanPart;
	}

	/**
	 * Sets the loan part.
	 *
	 * @param loanPart the loanPart to set
	 */
	public void setLoanPart(LoanAbacusAPIModelLoanPart loanPart) {

		this.loanPart = loanPart;
	}

	/**
	 * Gets the loan reason ID.
	 *
	 * @return the loanReasonID
	 */
	public int getLoanReasonID() {

		return loanReasonID;
	}

	/**
	 * Sets the loan reason ID.
	 *
	 * @param loanReasonID the loanReasonID to set
	 */
	public void setLoanReasonID(int loanReasonID) {

		this.loanReasonID = loanReasonID;
	}

	/**
	 * Gets the loan source of funds ID.
	 *
	 * @return the loanSourceOfFundsID
	 */
	public int getLoanSourceOfFundsID() {

		return loanSourceOfFundsID;
	}

	/**
	 * Sets the loan source of funds ID.
	 *
	 * @param loanSourceOfFundsID the loanSourceOfFundsID to set
	 */
	public void setLoanSourceOfFundsID(int loanSourceOfFundsID) {

		this.loanSourceOfFundsID = loanSourceOfFundsID;
	}

	/**
	 * Gets the cu loan guarantor source ID.
	 *
	 * @return the cuLoanGuarantorSourceID
	 */
	public int getCuLoanGuarantorSourceID() {

		return cuLoanGuarantorSourceID;
	}

	/**
	 * Sets the cu loan guarantor source ID.
	 *
	 * @param cuLoanGuarantorSourceID the cuLoanGuarantorSourceID to set
	 */
	public void setCuLoanGuarantorSourceID(int cuLoanGuarantorSourceID) {

		this.cuLoanGuarantorSourceID = cuLoanGuarantorSourceID;
	}

	/**
	 * Gets the cu loan district code ID.
	 *
	 * @return the cuLoanDistrictCodeID
	 */
	public int getCuLoanDistrictCodeID() {

		return cuLoanDistrictCodeID;
	}

	/**
	 * Sets the cu loan district code ID.
	 *
	 * @param cuLoanDistrictCodeID the cuLoanDistrictCodeID to set
	 */
	public void setCuLoanDistrictCodeID(int cuLoanDistrictCodeID) {

		this.cuLoanDistrictCodeID = cuLoanDistrictCodeID;
	}

	/**
	 * Gets the cu loan refinance reason ID.
	 *
	 * @return the cuLoanRefinanceReasonID
	 */
	public int getCuLoanRefinanceReasonID() {

		return cuLoanRefinanceReasonID;
	}

	/**
	 * Sets the cu loan refinance reason ID.
	 *
	 * @param cuLoanRefinanceReasonID the cuLoanRefinanceReasonID to set
	 */
	public void setCuLoanRefinanceReasonID(int cuLoanRefinanceReasonID) {

		this.cuLoanRefinanceReasonID = cuLoanRefinanceReasonID;
	}

	/**
	 * Gets the status.
	 *
	 * @return the status
	 */
	public int getStatus() {

		return status;
	}

	/**
	 * Sets the status.
	 *
	 * @param status the status to set
	 */
	public void setStatus(int status) {

		this.status = status;
	}

	/**
	 * Checks if is multiple parts.
	 *
	 * @return the multipleParts
	 */
	public boolean isMultipleParts() {

		return multipleParts;
	}

	/**
	 * Sets the multiple parts.
	 *
	 * @param multipleParts the multipleParts to set
	 */
	public void setMultipleParts(boolean multipleParts) {

		this.multipleParts = multipleParts;
	}

	/**
	 * Checks if is redraw.
	 *
	 * @return the redraw
	 */
	public boolean isRedraw() {

		return redraw;
	}

	/**
	 * Sets the redraw.
	 *
	 * @param redraw the redraw to set
	 */
	public void setRedraw(boolean redraw) {

		this.redraw = redraw;
	}

	/**
	 * Gets the apply amount total.
	 *
	 * @return the applyAmountTotal
	 */
	public double getApplyAmountTotal() {

		return applyAmountTotal;
	}

	/**
	 * Sets the apply amount total.
	 *
	 * @param applyAmountTotal the applyAmountTotal to set
	 */
	public void setApplyAmountTotal(double applyAmountTotal) {

		this.applyAmountTotal = applyAmountTotal;
	}

	/**
	 * Gets the apply date.
	 *
	 * @return the applyDate
	 */
	public Date getApplyDate() {

		return applyDate;
	}

	/**
	 * Sets the apply date.
	 *
	 * @param applyDate the applyDate to set
	 */
	public void setApplyDate(Date applyDate) {

		this.applyDate = applyDate;
	}

	/**
	 * Gets the approve amount total.
	 *
	 * @return the approveAmountTotal
	 */
	public double getApproveAmountTotal() {

		return approveAmountTotal;
	}

	/**
	 * Sets the approve amount total.
	 *
	 * @param approveAmountTotal the approveAmountTotal to set
	 */
	public void setApproveAmountTotal(double approveAmountTotal) {

		this.approveAmountTotal = approveAmountTotal;
	}

	/**
	 * Gets the approve date.
	 *
	 * @return the approveDate
	 */
	public Date getApproveDate() {

		return approveDate;
	}

	/**
	 * Sets the approve date.
	 *
	 * @param approveDate the approveDate to set
	 */
	public void setApproveDate(Date approveDate) {

		this.approveDate = approveDate;
	}

	/**
	 * Gets the cancel reason ID.
	 *
	 * @return the cancelReasonID
	 */
	public int getCancelReasonID() {

		return cancelReasonID;
	}

	/**
	 * Sets the cancel reason ID.
	 *
	 * @param cancelReasonID the cancelReasonID to set
	 */
	public void setCancelReasonID(int cancelReasonID) {

		this.cancelReasonID = cancelReasonID;
	}

	/**
	 * Gets the cancel notes.
	 *
	 * @return the cancelNotes
	 */
	public String getCancelNotes() {

		return cancelNotes;
	}

	/**
	 * Sets the cancel notes.
	 *
	 * @param cancelNotes the cancelNotes to set
	 */
	public void setCancelNotes(String cancelNotes) {

		this.cancelNotes = cancelNotes;
	}

	/**
	 * Gets the cais flag.
	 *
	 * @return the caisFlag
	 */
	public int getCaisFlag() {

		return caisFlag;
	}

	/**
	 * Sets the cais flag.
	 *
	 * @param caisFlag the caisFlag to set
	 */
	public void setCaisFlag(int caisFlag) {

		this.caisFlag = caisFlag;
	}

	/**
	 * Gets the cais flags equifax.
	 *
	 * @return the caisFlagsEquifax
	 */
	public int getCaisFlagsEquifax() {

		return caisFlagsEquifax;
	}

	/**
	 * Sets the cais flags equifax.
	 *
	 * @param caisFlagsEquifax the caisFlagsEquifax to set
	 */
	public void setCaisFlagsEquifax(int caisFlagsEquifax) {

		this.caisFlagsEquifax = caisFlagsEquifax;
	}

	/**
	 * Gets the settlement customer ID.
	 *
	 * @return the settlementCustomerID
	 */
	public int getSettlementCustomerID() {

		return settlementCustomerID;
	}

	/**
	 * Sets the settlement customer ID.
	 *
	 * @param settlementCustomerID the settlementCustomerID to set
	 */
	public void setSettlementCustomerID(int settlementCustomerID) {

		this.settlementCustomerID = settlementCustomerID;
	}

	/**
	 * Gets the settlement CU account ID.
	 *
	 * @return the settlementCUAccountID
	 */
	public int getSettlementCUAccountID() {

		return settlementCUAccountID;
	}

	/**
	 * Sets the settlement CU account ID.
	 *
	 * @param settlementCUAccountID the settlementCUAccountID to set
	 */
	public void setSettlementCUAccountID(int settlementCUAccountID) {

		this.settlementCUAccountID = settlementCUAccountID;
	}

	/**
	 * Checks if is settlement CU account is third party.
	 *
	 * @return the settlementCUAccountIsThirdParty
	 */
	public boolean isSettlementCUAccountIsThirdParty() {

		return settlementCUAccountIsThirdParty;
	}

	/**
	 * Sets the settlement CU account is third party.
	 *
	 * @param settlementCUAccountIsThirdParty the settlementCUAccountIsThirdParty to set
	 */
	public void setSettlementCUAccountIsThirdParty(boolean settlementCUAccountIsThirdParty) {

		this.settlementCUAccountIsThirdParty = settlementCUAccountIsThirdParty;
	}

	/**
	 * Gets the payment customer ID.
	 *
	 * @return the paymentCustomerID
	 */
	public int getPaymentCustomerID() {

		return paymentCustomerID;
	}

	/**
	 * Sets the payment customer ID.
	 *
	 * @param paymentCustomerID the paymentCustomerID to set
	 */
	public void setPaymentCustomerID(int paymentCustomerID) {

		this.paymentCustomerID = paymentCustomerID;
	}

	/**
	 * Gets the payment CU account ID.
	 *
	 * @return the paymentCUAccountID
	 */
	public int getPaymentCUAccountID() {

		return paymentCUAccountID;
	}

	/**
	 * Sets the payment CU account ID.
	 *
	 * @param paymentCUAccountID the paymentCUAccountID to set
	 */
	public void setPaymentCUAccountID(int paymentCUAccountID) {

		this.paymentCUAccountID = paymentCUAccountID;
	}

	/**
	 * Checks if is payment CU account is third party.
	 *
	 * @return the paymentCUAccountIsThirdParty
	 */
	public boolean isPaymentCUAccountIsThirdParty() {

		return paymentCUAccountIsThirdParty;
	}

	/**
	 * Sets the payment CU account is third party.
	 *
	 * @param paymentCUAccountIsThirdParty the paymentCUAccountIsThirdParty to set
	 */
	public void setPaymentCUAccountIsThirdParty(boolean paymentCUAccountIsThirdParty) {

		this.paymentCUAccountIsThirdParty = paymentCUAccountIsThirdParty;
	}

	/**
	 * Checks if is transfer accrued interest only.
	 *
	 * @return the transferAccruedInterestOnly
	 */
	public boolean isTransferAccruedInterestOnly() {

		return transferAccruedInterestOnly;
	}

	/**
	 * Sets the transfer accrued interest only.
	 *
	 * @param transferAccruedInterestOnly the transferAccruedInterestOnly to set
	 */
	public void setTransferAccruedInterestOnly(boolean transferAccruedInterestOnly) {

		this.transferAccruedInterestOnly = transferAccruedInterestOnly;
	}

	/**
	 * Checks if is restructured.
	 *
	 * @return the restructured
	 */
	public boolean isRestructured() {

		return restructured;
	}

	/**
	 * Sets the restructured.
	 *
	 * @param restructured the restructured to set
	 */
	public void setRestructured(boolean restructured) {

		this.restructured = restructured;
	}

	/**
	 * Gets the final part date.
	 *
	 * @return the finalPartDate
	 */
	public Date getFinalPartDate() {

		return finalPartDate;
	}

	/**
	 * Sets the final part date.
	 *
	 * @param finalPartDate the finalPartDate to set
	 */
	public void setFinalPartDate(Date finalPartDate) {

		this.finalPartDate = finalPartDate;
	}

	/**
	 * Gets the last restructured date.
	 *
	 * @return the lastRestructuredDate
	 */
	public double getLastRestructuredDate() {

		return lastRestructuredDate;
	}

	/**
	 * Sets the last restructured date.
	 *
	 * @param lastRestructuredDate the lastRestructuredDate to set
	 */
	public void setLastRestructuredDate(double lastRestructuredDate) {

		this.lastRestructuredDate = lastRestructuredDate;
	}

	/**
	 * Gets the cu loan processes.
	 *
	 * @return the cuLoanProcesses
	 */
	public Object getCuLoanProcesses() {

		return cuLoanProcesses;
	}

	/**
	 * Sets the cu loan processes.
	 *
	 * @param cuLoanProcesses the cuLoanProcesses to set
	 */
	public void setCuLoanProcesses(Object cuLoanProcesses) {

		this.cuLoanProcesses = cuLoanProcesses;
	}

	/**
	 * Gets the last panalty interest date.
	 *
	 * @return the lastPanaltyInterestDate
	 */
	public Date getLastPanaltyInterestDate() {

		return lastPanaltyInterestDate;
	}

	/**
	 * Sets the last panalty interest date.
	 *
	 * @param lastPanaltyInterestDate the lastPanaltyInterestDate to set
	 */
	public void setLastPanaltyInterestDate(Date lastPanaltyInterestDate) {

		this.lastPanaltyInterestDate = lastPanaltyInterestDate;
	}

	/**
	 * Gets the apply user ID.
	 *
	 * @return the applyUserID
	 */
	public int getApplyUserID() {

		return applyUserID;
	}

	/**
	 * Sets the apply user ID.
	 *
	 * @param applyUserID the applyUserID to set
	 */
	public void setApplyUserID(int applyUserID) {

		this.applyUserID = applyUserID;
	}

	/**
	 * Gets the loan collaterals.
	 *
	 * @return the loanCollaterals
	 */
	public Object getLoanCollaterals() {

		return loanCollaterals;
	}

	/**
	 * Sets the loan collaterals.
	 *
	 * @param loanCollaterals the loanCollaterals to set
	 */
	public void setLoanCollaterals(Object loanCollaterals) {

		this.loanCollaterals = loanCollaterals;
	}

	/**
	 * Gets the renewal old CU account ID.
	 *
	 * @return the renewalOldCUAccountID
	 */
	public int getRenewalOldCUAccountID() {

		return renewalOldCUAccountID;
	}

	/**
	 * Sets the renewal old CU account ID.
	 *
	 * @param renewalOldCUAccountID the renewalOldCUAccountID to set
	 */
	public void setRenewalOldCUAccountID(int renewalOldCUAccountID) {

		this.renewalOldCUAccountID = renewalOldCUAccountID;
	}

	/**
	 * Gets the excluded assets.
	 *
	 * @return the excludedAssets
	 */
	public double getExcludedAssets() {

		return excludedAssets;
	}

	/**
	 * Sets the excluded assets.
	 *
	 * @param excludedAssets the excludedAssets to set
	 */
	public void setExcludedAssets(double excludedAssets) {

		this.excludedAssets = excludedAssets;
	}

	/**
	 * Gets the collateral amount.
	 *
	 * @return the collateralAmount
	 */
	public double getCollateralAmount() {

		return collateralAmount;
	}

	/**
	 * Sets the collateral amount.
	 *
	 * @param collateralAmount the collateralAmount to set
	 */
	public void setCollateralAmount(double collateralAmount) {

		this.collateralAmount = collateralAmount;
	}

	/**
	 * Gets the transfer savings account ID.
	 *
	 * @return the transferSavingsAccountID
	 */
	public int getTransferSavingsAccountID() {

		return transferSavingsAccountID;
	}

	/**
	 * Sets the transfer savings account ID.
	 *
	 * @param transferSavingsAccountID the transferSavingsAccountID to set
	 */
	public void setTransferSavingsAccountID(int transferSavingsAccountID) {

		this.transferSavingsAccountID = transferSavingsAccountID;
	}

	/**
	 * Gets the issue date.
	 *
	 * @return the issueDate
	 */
	public Date getIssueDate() {

		return issueDate;
	}

	/**
	 * Sets the issue date.
	 *
	 * @param issueDate the issueDate to set
	 */
	public void setIssueDate(Date issueDate) {

		this.issueDate = issueDate;
	}

	/**
	 * Gets the loan status.
	 *
	 * @return the loanStatus
	 */
	public int getLoanStatus() {

		return loanStatus;
	}

	/**
	 * Sets the loan status.
	 *
	 * @param loanStatus the loanStatus to set
	 */
	public void setLoanStatus(int loanStatus) {

		this.loanStatus = loanStatus;
	}

	/**
	 * Gets the unfinished.
	 *
	 * @return the unfinished
	 */
	public int getUnfinished() {

		return unfinished;
	}

	/**
	 * Sets the unfinished.
	 *
	 * @param unfinished the unfinished to set
	 */
	public void setUnfinished(int unfinished) {

		this.unfinished = unfinished;
	}

	/**
	 * Gets the account number.
	 *
	 * @return the accountNumber
	 */
	public String getAccountNumber() {

		return accountNumber;
	}

	/**
	 * Sets the account number.
	 *
	 * @param accountNumber the accountNumber to set
	 */
	public void setAccountNumber(String accountNumber) {

		this.accountNumber = accountNumber;
	}

	/**
	 * Gets the process ID.
	 *
	 * @return the processID
	 */
	public int getProcessID() {

		return processID;
	}

	/**
	 * Sets the process ID.
	 *
	 * @param processID the processID to set
	 */
	public void setProcessID(int processID) {

		this.processID = processID;
	}

	/**
	 * Checks if is process completed.
	 *
	 * @return the isProcessCompleted
	 */
	public boolean isProcessCompleted() {

		return isProcessCompleted;
	}

	/**
	 * Sets the process completed.
	 *
	 * @param isProcessCompleted the isProcessCompleted to set
	 */
	public void setProcessCompleted(boolean isProcessCompleted) {

		this.isProcessCompleted = isProcessCompleted;
	}

	/**
	 * Gets the process description.
	 *
	 * @return the processDescription
	 */
	public String getProcessDescription() {

		return processDescription;
	}

	/**
	 * Sets the process description.
	 *
	 * @param processDescription the processDescription to set
	 */
	public void setProcessDescription(String processDescription) {

		this.processDescription = processDescription;
	}

	/**
	 * Gets the menu key.
	 *
	 * @return the menuKey
	 */
	public String getMenuKey() {

		return menuKey;
	}

	/**
	 * Sets the menu key.
	 *
	 * @param menuKey the menuKey to set
	 */
	public void setMenuKey(String menuKey) {

		this.menuKey = menuKey;
	}

	/**
	 * Checks if is sign off.
	 *
	 * @return the isSignOff
	 */
	public boolean isSignOff() {

		return isSignOff;
	}

	/**
	 * Sets the sign off.
	 *
	 * @param isSignOff the isSignOff to set
	 */
	public void setSignOff(boolean isSignOff) {

		this.isSignOff = isSignOff;
	}

	/**
	 * Checks if is user access.
	 *
	 * @return the isUserAccess
	 */
	public boolean isUserAccess() {

		return isUserAccess;
	}

	/**
	 * Sets the user access.
	 *
	 * @param isUserAccess the isUserAccess to set
	 */
	public void setUserAccess(boolean isUserAccess) {

		this.isUserAccess = isUserAccess;
	}

	/**
	 * Gets the cu account ID.
	 *
	 * @return the cuAccountID
	 */
	public int getCuAccountID() {

		return cuAccountID;
	}

	/**
	 * Sets the cu account ID.
	 *
	 * @param cuAccountID the cuAccountID to set
	 */
	public void setCuAccountID(int cuAccountID) {

		this.cuAccountID = cuAccountID;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "LoanAbacusAPIModelLoanApp [cuLoanID=" + cuLoanID + ", loanPart=" + loanPart
				+ ", loanReasonID=" + loanReasonID + ", loanSourceOfFundsID=" + loanSourceOfFundsID
				+ ", cuLoanGuarantorSourceID=" + cuLoanGuarantorSourceID + ", cuLoanDistrictCodeID="
				+ cuLoanDistrictCodeID + ", cuLoanRefinanceReasonID=" + cuLoanRefinanceReasonID
				+ ", status=" + status + ", multipleParts=" + multipleParts + ", redraw=" + redraw
				+ ", applyAmountTotal=" + applyAmountTotal + ", applyDate=" + applyDate
				+ ", approveAmountTotal=" + approveAmountTotal + ", approveDate=" + approveDate
				+ ", cancelReasonID=" + cancelReasonID + ", cancelNotes=" + cancelNotes
				+ ", caisFlag=" + caisFlag + ", caisFlagsEquifax=" + caisFlagsEquifax
				+ ", settlementCustomerID=" + settlementCustomerID + ", settlementCUAccountID="
				+ settlementCUAccountID + ", settlementCUAccountIsThirdParty="
				+ settlementCUAccountIsThirdParty + ", paymentCustomerID=" + paymentCustomerID
				+ ", paymentCUAccountID=" + paymentCUAccountID + ", paymentCUAccountIsThirdParty="
				+ paymentCUAccountIsThirdParty + ", transferAccruedInterestOnly="
				+ transferAccruedInterestOnly + ", restructured=" + restructured
				+ ", finalPartDate=" + finalPartDate + ", lastRestructuredDate="
				+ lastRestructuredDate + ", cuLoanProcesses=" + cuLoanProcesses
				+ ", lastPanaltyInterestDate=" + lastPanaltyInterestDate + ", applyUserID="
				+ applyUserID + ", loanCollaterals=" + loanCollaterals + ", renewalOldCUAccountID="
				+ renewalOldCUAccountID + ", excludedAssets=" + excludedAssets
				+ ", collateralAmount=" + collateralAmount + ", transferSavingsAccountID="
				+ transferSavingsAccountID + ", issueDate=" + issueDate + ", loanStatus="
				+ loanStatus + ", unfinished=" + unfinished + ", accountNumber=" + accountNumber
				+ ", processID=" + processID + ", isProcessCompleted=" + isProcessCompleted
				+ ", processDescription=" + processDescription + ", menuKey=" + menuKey
				+ ", isSignOff=" + isSignOff + ", isUserAccess=" + isUserAccess + ", cuAccountID="
				+ cuAccountID + "]";
	}

}
