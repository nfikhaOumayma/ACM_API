package com.acm.utils.dtos;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The Class ResponseGetInfoPaymentAbacusDTO.
 */
public class ResponseGetInfoPaymentAbacusDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 184523099321017000L;
	
	/** The principal due. */
	@JsonProperty("principalDue")
	private double principalDue;

	/** The interest due. */
	@JsonProperty("interestDue")
	private double interestDue;

	/** The opening balance. */
	@JsonProperty("openingBalance")
	private double openingBalance;

	/** The closing balance. */
	@JsonProperty("closingBalance")
	private double closingBalance;

	/** The account rating. */
	@JsonProperty("accountRating")
	private String accountRating;

	/** The balance date. */
	@JsonProperty("balanceDate")
	private String balanceDate;

	/** The issue amount. */
	@JsonProperty("issueAmount")
	private double issueAmount;

	/** The issue date. */
	@JsonProperty("issueDate")
	private String issueDate;

	/** The arrears information. */
	@JsonProperty("arrearsInformation")
	private String arrearsInformation;

	/** The arrears amount. */
	@JsonProperty("arrearsAmount")
	private double arrearsAmount;

	/** The arrears days. */
	@JsonProperty("arrearsDays")
	private int arrearsDays;

	/** The current installment number. */
	@JsonProperty("currentInstallmentNumber")
	private int currentInstallmentNumber;

	/** The next due date. */
	@JsonProperty("nextDueDate")
	private String nextDueDate;

	/** The penalty due. */
	@JsonProperty("penaltyDue")
	private double penaltyDue;

	/** The fees due. */
	@JsonProperty("feesDue")
	private double feesDue;

	/** The customer name. */
	@JsonProperty("customerName")
	private String customerName;

	/** The account ID. */
	@JsonProperty("accountID")
	private int accountID;

	/** The session date. */
	@JsonProperty("sessionDate")
	private String sessionDate;

	/**
	 * Instantiates a new response get info payment abacus DTO.
	 */
	public ResponseGetInfoPaymentAbacusDTO() {

		super();
	}

	/**
	 * Gets the principal due.
	 *
	 * @return the principal due
	 */
	public double getPrincipalDue() {

		return principalDue;
	}

	/**
	 * Sets the principal due.
	 *
	 * @param principalDue the new principal due
	 */
	public void setPrincipalDue(double principalDue) {

		this.principalDue = principalDue;
	}

	/**
	 * Gets the interest due.
	 *
	 * @return the interest due
	 */
	public double getInterestDue() {

		return interestDue;
	}

	/**
	 * Sets the interest due.
	 *
	 * @param interestDue the new interest due
	 */
	public void setInterestDue(double interestDue) {

		this.interestDue = interestDue;
	}

	/**
	 * Gets the opening balance.
	 *
	 * @return the opening balance
	 */
	public double getOpeningBalance() {

		return openingBalance;
	}

	/**
	 * Sets the opening balance.
	 *
	 * @param openingBalance the new opening balance
	 */
	public void setOpeningBalance(double openingBalance) {

		this.openingBalance = openingBalance;
	}

	/**
	 * Gets the closing balance.
	 *
	 * @return the closing balance
	 */
	public double getClosingBalance() {

		return closingBalance;
	}

	/**
	 * Sets the closing balance.
	 *
	 * @param closingBalance the new closing balance
	 */
	public void setClosingBalance(double closingBalance) {

		this.closingBalance = closingBalance;
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
	 * Gets the balance date.
	 *
	 * @return the balance date
	 */
	public String getBalanceDate() {

		return balanceDate;
	}

	/**
	 * Sets the balance date.
	 *
	 * @param balanceDate the new balance date
	 */
	public void setBalanceDate(String balanceDate) {

		this.balanceDate = balanceDate;
	}

	/**
	 * Gets the issue amount.
	 *
	 * @return the issue amount
	 */
	public double getIssueAmount() {

		return issueAmount;
	}

	/**
	 * Sets the issue amount.
	 *
	 * @param issueAmount the new issue amount
	 */
	public void setIssueAmount(double issueAmount) {

		this.issueAmount = issueAmount;
	}

	/**
	 * Gets the issue date.
	 *
	 * @return the issue date
	 */
	public String getIssueDate() {

		return issueDate;
	}

	/**
	 * Sets the issue date.
	 *
	 * @param issueDate the new issue date
	 */
	public void setIssueDate(String issueDate) {

		this.issueDate = issueDate;
	}

	/**
	 * Gets the arrears information.
	 *
	 * @return the arrears information
	 */
	public String getArrearsInformation() {

		return arrearsInformation;
	}

	/**
	 * Sets the arrears information.
	 *
	 * @param arrearsInformation the new arrears information
	 */
	public void setArrearsInformation(String arrearsInformation) {

		this.arrearsInformation = arrearsInformation;
	}

	/**
	 * Gets the arrears amount.
	 *
	 * @return the arrears amount
	 */
	public double getArrearsAmount() {

		return arrearsAmount;
	}

	/**
	 * Sets the arrears amount.
	 *
	 * @param arrearsAmount the new arrears amount
	 */
	public void setArrearsAmount(double arrearsAmount) {

		this.arrearsAmount = arrearsAmount;
	}

	/**
	 * Gets the arrears days.
	 *
	 * @return the arrears days
	 */
	public int getArrearsDays() {

		return arrearsDays;
	}

	/**
	 * Sets the arrears days.
	 *
	 * @param arrearsDays the new arrears days
	 */
	public void setArrearsDays(int arrearsDays) {

		this.arrearsDays = arrearsDays;
	}

	/**
	 * Gets the current installment number.
	 *
	 * @return the current installment number
	 */
	public int getCurrentInstallmentNumber() {

		return currentInstallmentNumber;
	}

	/**
	 * Sets the current installment number.
	 *
	 * @param currentInstallmentNumber the new current installment number
	 */
	public void setCurrentInstallmentNumber(int currentInstallmentNumber) {

		this.currentInstallmentNumber = currentInstallmentNumber;
	}

	/**
	 * Gets the next due date.
	 *
	 * @return the next due date
	 */
	public String getNextDueDate() {

		return nextDueDate;
	}

	/**
	 * Sets the next due date.
	 *
	 * @param nextDueDate the new next due date
	 */
	public void setNextDueDate(String nextDueDate) {

		this.nextDueDate = nextDueDate;
	}

	/**
	 * Gets the penalty due.
	 *
	 * @return the penalty due
	 */
	public double getPenaltyDue() {

		return penaltyDue;
	}

	/**
	 * Sets the penalty due.
	 *
	 * @param penaltyDue the new penalty due
	 */
	public void setPenaltyDue(double penaltyDue) {

		this.penaltyDue = penaltyDue;
	}

	/**
	 * Gets the fees due.
	 *
	 * @return the fees due
	 */
	public double getFeesDue() {

		return feesDue;
	}

	/**
	 * Sets the fees due.
	 *
	 * @param feesDue the new fees due
	 */
	public void setFeesDue(double feesDue) {

		this.feesDue = feesDue;
	}

	/**
	 * Gets the customer name.
	 *
	 * @return the customer name
	 */
	public String getCustomerName() {

		return customerName;
	}

	/**
	 * Sets the customer name.
	 *
	 * @param customerName the new customer name
	 */
	public void setCustomerName(String customerName) {

		this.customerName = customerName;
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
	 * Gets the session date.
	 *
	 * @return the session date
	 */
	public String getSessionDate() {

		return sessionDate;
	}

	/**
	 * Sets the session date.
	 *
	 * @param sessionDate the new session date
	 */
	public void setSessionDate(String sessionDate) {

		this.sessionDate = sessionDate;
	}

	/**
	 * Gets the serialversionuid.
	 *
	 * @return the serialversionuid
	 */
	public static long getSerialversionuid() {

		return serialVersionUID;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "ResponseGetInfoPaymentAbacusDTO [principalDue=" + principalDue + ", interestDue="
				+ interestDue + ", openingBalance=" + openingBalance + ", closingBalance="
				+ closingBalance + ", accountRating=" + accountRating + ", balanceDate="
				+ balanceDate + ", issueAmount=" + issueAmount + ", issueDate=" + issueDate
				+ ", arrearsInformation=" + arrearsInformation + ", arrearsAmount=" + arrearsAmount
				+ ", arrearsDays=" + arrearsDays + ", currentInstallmentNumber="
				+ currentInstallmentNumber + ", nextDueDate=" + nextDueDate + ", penaltyDue="
				+ penaltyDue + ", feesDue=" + feesDue + ", customerName=" + customerName
				+ ", accountID=" + accountID + ", sessionDate=" + sessionDate + "]";
	}

}
