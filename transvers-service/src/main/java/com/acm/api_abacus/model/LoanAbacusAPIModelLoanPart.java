/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.model;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * {@link LoanAbacusAPIModelLoanPart} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class LoanAbacusAPIModelLoanPart implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -4889600939976648732L;

	/** The cu loan part ID. */
	private int cuLoanPartID;

	/** The use schedule interest. */
	private boolean useScheduleInterest;

	/** The interest rate. */
	private double interestRate;

	/** The devaluation rate. */
	private double devaluationRate;

	/** The complimentary interest rate. */
	private double complimentaryInterestRate;

	/** The loan amount. */
	private double loanAmount;

	/** The issue amount. */
	private double issueAmount;

	/** The refinanced. */
	private boolean refinanced;

	/** The issue date. */
	private Date issueDate;

	/** The level payments. */
	private boolean levelPayments;

	/** The initial payment date. */
	private Date initialPaymentDate;

	/** The specified initial payment date. */
	private Date specifiedInitialPaymentDate;

	/** The re payment period num. */
	private int rePaymentPeriodNum;

	/** The re payment period. */
	private int rePaymentPeriod;

	/** The int pay period num. */
	private int intPayPeriodNum;

	/** The ignore odd days. */
	private boolean ignoreOddDays;

	/** The periods deferred ID. */
	private int periodsDeferredID;

	/** The periods deferred. */
	private int periodsDeferred;

	/** The first repayment offset. */
	private int firstRepaymentOffset;

	/** The grace period. */
	private boolean gracePeriod;

	/** The term period num. */
	private int termPeriodNum;

	/** The term period. */
	private int termPeriod;

	/** The calculate initial payment date. */
	private boolean calculateInitialPaymentDate;

	/** The loan calculation mode. */
	private int loanCalculationMode;

	/** The separateon schedule. */
	private boolean separateonSchedule;

	/** The savings acc ID. */
	private int savingsAccID;

	/** The mandatory acc ID. */
	private int mandatoryAccID;

	/** The normal payment. */
	private double normalPayment;

	/** The balloon amount. */
	private double balloonAmount;

	/** The day count. */
	private int dayCount;

	/** The fee amt 1. */
	private double feeAmt1;

	/** The capitalise interest when refinancing. */
	private boolean capitaliseInterestWhenRefinancing;

	/** The use specifed day. */
	private boolean useSpecifedDay;

	/** The is reviewed. */
	private boolean isReviewed;

	/** The review period num. */
	private int reviewPeriodNum;

	/** The reference number. */
	private String referenceNumber;

	/** The review period. */
	private int reviewPeriod;

	/** The week num. */
	private int weekNum;

	/** The specified day. */
	private int specifiedDay;

	/** The loan calculator amount type. */
	private int loanCalculatorAmountType;

	/** The balloon payment method. */
	private int balloonPaymentMethod;

	/** The issue fee percentage 1. */
	private double issueFeePercentage1;

	/** The issue fee percentage 2. */
	private double issueFeePercentage2;

	/** The fee amt 2. */
	private double feeAmt2;

	/** The issue fee. */
	private double issueFee;

	/** The term period ID. */
	private int termPeriodID;

	/** The repayment period ID. */
	private int repaymentPeriodID;

	/** The cu loan schedule. */
	private List<LoanAbacusAPIModelCuLoanSchedule> cuLoanSchedule;

	/** The currency ID. */
	private int currencyID;

	/** The final payment date. */
	private Date finalPaymentDate;

	/** The effective int rate. */
	private double effectiveIntRate;

	/** The edited normal payment. */
	private double editedNormalPayment;

	/** The specific provisioning rate. */
	private double specificProvisioningRate;

	/** The payments sum up. */
	private boolean paymentsSumUp;

	/** The cu insurance ID. */
	private int cuInsuranceID;

	/** The insured amount. */
	private double insuredAmount;

	/** The insured term. */
	private int insuredTerm;

	/** The insurance premium rate. */
	private double insurancePremiumRate;

	/** The insurance premium. */
	private double insurancePremium;

	/** The interest loss. */
	private double interestLoss;

	/**
	 * Instantiates a new loan abacus API model loan part.
	 */
	public LoanAbacusAPIModelLoanPart() {

		/*
		 * Empty
		 */
	}

	/**
	 * Gets the cu loan part ID.
	 *
	 * @return the cuLoanPartID
	 */
	public int getCuLoanPartID() {

		return cuLoanPartID;
	}

	/**
	 * Sets the cu loan part ID.
	 *
	 * @param cuLoanPartID the cuLoanPartID to set
	 */
	public void setCuLoanPartID(int cuLoanPartID) {

		this.cuLoanPartID = cuLoanPartID;
	}

	/**
	 * Checks if is use schedule interest.
	 *
	 * @return the useScheduleInterest
	 */
	public boolean isUseScheduleInterest() {

		return useScheduleInterest;
	}

	/**
	 * Sets the use schedule interest.
	 *
	 * @param useScheduleInterest the useScheduleInterest to set
	 */
	public void setUseScheduleInterest(boolean useScheduleInterest) {

		this.useScheduleInterest = useScheduleInterest;
	}

	/**
	 * Gets the interest rate.
	 *
	 * @return the interestRate
	 */
	public double getInterestRate() {

		return interestRate;
	}

	/**
	 * Sets the interest rate.
	 *
	 * @param interestRate the interestRate to set
	 */
	public void setInterestRate(double interestRate) {

		this.interestRate = interestRate;
	}

	/**
	 * Gets the devaluation rate.
	 *
	 * @return the devaluationRate
	 */
	public double getDevaluationRate() {

		return devaluationRate;
	}

	/**
	 * Sets the devaluation rate.
	 *
	 * @param devaluationRate the devaluationRate to set
	 */
	public void setDevaluationRate(double devaluationRate) {

		this.devaluationRate = devaluationRate;
	}

	/**
	 * Gets the complimentary interest rate.
	 *
	 * @return the complimentaryInterestRate
	 */
	public double getComplimentaryInterestRate() {

		return complimentaryInterestRate;
	}

	/**
	 * Sets the complimentary interest rate.
	 *
	 * @param complimentaryInterestRate the complimentaryInterestRate to set
	 */
	public void setComplimentaryInterestRate(double complimentaryInterestRate) {

		this.complimentaryInterestRate = complimentaryInterestRate;
	}

	/**
	 * Gets the loan amount.
	 *
	 * @return the loanAmount
	 */
	public double getLoanAmount() {

		return loanAmount;
	}

	/**
	 * Sets the loan amount.
	 *
	 * @param loanAmount the loanAmount to set
	 */
	public void setLoanAmount(double loanAmount) {

		this.loanAmount = loanAmount;
	}

	/**
	 * Gets the issue amount.
	 *
	 * @return the issueAmount
	 */
	public double getIssueAmount() {

		return issueAmount;
	}

	/**
	 * Sets the issue amount.
	 *
	 * @param issueAmount the issueAmount to set
	 */
	public void setIssueAmount(double issueAmount) {

		this.issueAmount = issueAmount;
	}

	/**
	 * Checks if is refinanced.
	 *
	 * @return the refinanced
	 */
	public boolean isRefinanced() {

		return refinanced;
	}

	/**
	 * Sets the refinanced.
	 *
	 * @param refinanced the refinanced to set
	 */
	public void setRefinanced(boolean refinanced) {

		this.refinanced = refinanced;
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
	 * Checks if is level payments.
	 *
	 * @return the levelPayments
	 */
	public boolean isLevelPayments() {

		return levelPayments;
	}

	/**
	 * Sets the level payments.
	 *
	 * @param levelPayments the levelPayments to set
	 */
	public void setLevelPayments(boolean levelPayments) {

		this.levelPayments = levelPayments;
	}

	/**
	 * Gets the initial payment date.
	 *
	 * @return the initialPaymentDate
	 */
	public Date getInitialPaymentDate() {

		return initialPaymentDate;
	}

	/**
	 * Sets the initial payment date.
	 *
	 * @param initialPaymentDate the initialPaymentDate to set
	 */
	public void setInitialPaymentDate(Date initialPaymentDate) {

		this.initialPaymentDate = initialPaymentDate;
	}

	/**
	 * Gets the specified initial payment date.
	 *
	 * @return the specifiedInitialPaymentDate
	 */
	public Date getSpecifiedInitialPaymentDate() {

		return specifiedInitialPaymentDate;
	}

	/**
	 * Sets the specified initial payment date.
	 *
	 * @param specifiedInitialPaymentDate the specifiedInitialPaymentDate to set
	 */
	public void setSpecifiedInitialPaymentDate(Date specifiedInitialPaymentDate) {

		this.specifiedInitialPaymentDate = specifiedInitialPaymentDate;
	}

	/**
	 * Gets the re payment period num.
	 *
	 * @return the rePaymentPeriodNum
	 */
	public int getRePaymentPeriodNum() {

		return rePaymentPeriodNum;
	}

	/**
	 * Sets the re payment period num.
	 *
	 * @param rePaymentPeriodNum the rePaymentPeriodNum to set
	 */
	public void setRePaymentPeriodNum(int rePaymentPeriodNum) {

		this.rePaymentPeriodNum = rePaymentPeriodNum;
	}

	/**
	 * Gets the re payment period.
	 *
	 * @return the rePaymentPeriod
	 */
	public int getRePaymentPeriod() {

		return rePaymentPeriod;
	}

	/**
	 * Sets the re payment period.
	 *
	 * @param rePaymentPeriod the rePaymentPeriod to set
	 */
	public void setRePaymentPeriod(int rePaymentPeriod) {

		this.rePaymentPeriod = rePaymentPeriod;
	}

	/**
	 * Gets the int pay period num.
	 *
	 * @return the intPayPeriodNum
	 */
	public int getIntPayPeriodNum() {

		return intPayPeriodNum;
	}

	/**
	 * Sets the int pay period num.
	 *
	 * @param intPayPeriodNum the intPayPeriodNum to set
	 */
	public void setIntPayPeriodNum(int intPayPeriodNum) {

		this.intPayPeriodNum = intPayPeriodNum;
	}

	/**
	 * Checks if is ignore odd days.
	 *
	 * @return the ignoreOddDays
	 */
	public boolean isIgnoreOddDays() {

		return ignoreOddDays;
	}

	/**
	 * Sets the ignore odd days.
	 *
	 * @param ignoreOddDays the ignoreOddDays to set
	 */
	public void setIgnoreOddDays(boolean ignoreOddDays) {

		this.ignoreOddDays = ignoreOddDays;
	}

	/**
	 * Gets the periods deferred ID.
	 *
	 * @return the periodsDeferredID
	 */
	public int getPeriodsDeferredID() {

		return periodsDeferredID;
	}

	/**
	 * Sets the periods deferred ID.
	 *
	 * @param periodsDeferredID the periodsDeferredID to set
	 */
	public void setPeriodsDeferredID(int periodsDeferredID) {

		this.periodsDeferredID = periodsDeferredID;
	}

	/**
	 * Gets the periods deferred.
	 *
	 * @return the periodsDeferred
	 */
	public int getPeriodsDeferred() {

		return periodsDeferred;
	}

	/**
	 * Sets the periods deferred.
	 *
	 * @param periodsDeferred the periodsDeferred to set
	 */
	public void setPeriodsDeferred(int periodsDeferred) {

		this.periodsDeferred = periodsDeferred;
	}

	/**
	 * Gets the first repayment offset.
	 *
	 * @return the firstRepaymentOffset
	 */
	public int getFirstRepaymentOffset() {

		return firstRepaymentOffset;
	}

	/**
	 * Sets the first repayment offset.
	 *
	 * @param firstRepaymentOffset the firstRepaymentOffset to set
	 */
	public void setFirstRepaymentOffset(int firstRepaymentOffset) {

		this.firstRepaymentOffset = firstRepaymentOffset;
	}

	/**
	 * Checks if is grace period.
	 *
	 * @return the gracePeriod
	 */
	public boolean isGracePeriod() {

		return gracePeriod;
	}

	/**
	 * Sets the grace period.
	 *
	 * @param gracePeriod the gracePeriod to set
	 */
	public void setGracePeriod(boolean gracePeriod) {

		this.gracePeriod = gracePeriod;
	}

	/**
	 * Gets the term period num.
	 *
	 * @return the termPeriodNum
	 */
	public int getTermPeriodNum() {

		return termPeriodNum;
	}

	/**
	 * Sets the term period num.
	 *
	 * @param termPeriodNum the termPeriodNum to set
	 */
	public void setTermPeriodNum(int termPeriodNum) {

		this.termPeriodNum = termPeriodNum;
	}

	/**
	 * Gets the term period.
	 *
	 * @return the termPeriod
	 */
	public int getTermPeriod() {

		return termPeriod;
	}

	/**
	 * Sets the term period.
	 *
	 * @param termPeriod the termPeriod to set
	 */
	public void setTermPeriod(int termPeriod) {

		this.termPeriod = termPeriod;
	}

	/**
	 * Checks if is calculate initial payment date.
	 *
	 * @return the calculateInitialPaymentDate
	 */
	public boolean isCalculateInitialPaymentDate() {

		return calculateInitialPaymentDate;
	}

	/**
	 * Sets the calculate initial payment date.
	 *
	 * @param calculateInitialPaymentDate the calculateInitialPaymentDate to set
	 */
	public void setCalculateInitialPaymentDate(boolean calculateInitialPaymentDate) {

		this.calculateInitialPaymentDate = calculateInitialPaymentDate;
	}

	/**
	 * Gets the loan calculation mode.
	 *
	 * @return the loanCalculationMode
	 */
	public int getLoanCalculationMode() {

		return loanCalculationMode;
	}

	/**
	 * Sets the loan calculation mode.
	 *
	 * @param loanCalculationMode the loanCalculationMode to set
	 */
	public void setLoanCalculationMode(int loanCalculationMode) {

		this.loanCalculationMode = loanCalculationMode;
	}

	/**
	 * Checks if is separateon schedule.
	 *
	 * @return the separateonSchedule
	 */
	public boolean isSeparateonSchedule() {

		return separateonSchedule;
	}

	/**
	 * Sets the separateon schedule.
	 *
	 * @param separateonSchedule the separateonSchedule to set
	 */
	public void setSeparateonSchedule(boolean separateonSchedule) {

		this.separateonSchedule = separateonSchedule;
	}

	/**
	 * Gets the savings acc ID.
	 *
	 * @return the savingsAccID
	 */
	public int getSavingsAccID() {

		return savingsAccID;
	}

	/**
	 * Sets the savings acc ID.
	 *
	 * @param savingsAccID the savingsAccID to set
	 */
	public void setSavingsAccID(int savingsAccID) {

		this.savingsAccID = savingsAccID;
	}

	/**
	 * Gets the mandatory acc ID.
	 *
	 * @return the mandatoryAccID
	 */
	public int getMandatoryAccID() {

		return mandatoryAccID;
	}

	/**
	 * Sets the mandatory acc ID.
	 *
	 * @param mandatoryAccID the mandatoryAccID to set
	 */
	public void setMandatoryAccID(int mandatoryAccID) {

		this.mandatoryAccID = mandatoryAccID;
	}

	/**
	 * Gets the normal payment.
	 *
	 * @return the normalPayment
	 */
	public double getNormalPayment() {

		return normalPayment;
	}

	/**
	 * Sets the normal payment.
	 *
	 * @param normalPayment the normalPayment to set
	 */
	public void setNormalPayment(double normalPayment) {

		this.normalPayment = normalPayment;
	}

	/**
	 * Gets the balloon amount.
	 *
	 * @return the balloonAmount
	 */
	public double getBalloonAmount() {

		return balloonAmount;
	}

	/**
	 * Sets the balloon amount.
	 *
	 * @param balloonAmount the balloonAmount to set
	 */
	public void setBalloonAmount(double balloonAmount) {

		this.balloonAmount = balloonAmount;
	}

	/**
	 * Gets the day count.
	 *
	 * @return the dayCount
	 */
	public int getDayCount() {

		return dayCount;
	}

	/**
	 * Sets the day count.
	 *
	 * @param dayCount the dayCount to set
	 */
	public void setDayCount(int dayCount) {

		this.dayCount = dayCount;
	}

	/**
	 * Gets the fee amt 1.
	 *
	 * @return the feeAmt1
	 */
	public double getFeeAmt1() {

		return feeAmt1;
	}

	/**
	 * Sets the fee amt 1.
	 *
	 * @param feeAmt1 the feeAmt1 to set
	 */
	public void setFeeAmt1(double feeAmt1) {

		this.feeAmt1 = feeAmt1;
	}

	/**
	 * Checks if is capitalise interest when refinancing.
	 *
	 * @return the capitaliseInterestWhenRefinancing
	 */
	public boolean isCapitaliseInterestWhenRefinancing() {

		return capitaliseInterestWhenRefinancing;
	}

	/**
	 * Sets the capitalise interest when refinancing.
	 *
	 * @param capitaliseInterestWhenRefinancing the capitaliseInterestWhenRefinancing to set
	 */
	public void setCapitaliseInterestWhenRefinancing(boolean capitaliseInterestWhenRefinancing) {

		this.capitaliseInterestWhenRefinancing = capitaliseInterestWhenRefinancing;
	}

	/**
	 * Checks if is use specifed day.
	 *
	 * @return the useSpecifedDay
	 */
	public boolean isUseSpecifedDay() {

		return useSpecifedDay;
	}

	/**
	 * Sets the use specifed day.
	 *
	 * @param useSpecifedDay the useSpecifedDay to set
	 */
	public void setUseSpecifedDay(boolean useSpecifedDay) {

		this.useSpecifedDay = useSpecifedDay;
	}

	/**
	 * Checks if is reviewed.
	 *
	 * @return the isReviewed
	 */
	public boolean isReviewed() {

		return isReviewed;
	}

	/**
	 * Sets the reviewed.
	 *
	 * @param isReviewed the isReviewed to set
	 */
	public void setReviewed(boolean isReviewed) {

		this.isReviewed = isReviewed;
	}

	/**
	 * Gets the review period num.
	 *
	 * @return the reviewPeriodNum
	 */
	public int getReviewPeriodNum() {

		return reviewPeriodNum;
	}

	/**
	 * Sets the review period num.
	 *
	 * @param reviewPeriodNum the reviewPeriodNum to set
	 */
	public void setReviewPeriodNum(int reviewPeriodNum) {

		this.reviewPeriodNum = reviewPeriodNum;
	}

	/**
	 * Gets the reference number.
	 *
	 * @return the referenceNumber
	 */
	public String getReferenceNumber() {

		return referenceNumber;
	}

	/**
	 * Sets the reference number.
	 *
	 * @param referenceNumber the referenceNumber to set
	 */
	public void setReferenceNumber(String referenceNumber) {

		this.referenceNumber = referenceNumber;
	}

	/**
	 * Gets the review period.
	 *
	 * @return the reviewPeriod
	 */
	public int getReviewPeriod() {

		return reviewPeriod;
	}

	/**
	 * Sets the review period.
	 *
	 * @param reviewPeriod the reviewPeriod to set
	 */
	public void setReviewPeriod(int reviewPeriod) {

		this.reviewPeriod = reviewPeriod;
	}

	/**
	 * Gets the week num.
	 *
	 * @return the weekNum
	 */
	public int getWeekNum() {

		return weekNum;
	}

	/**
	 * Sets the week num.
	 *
	 * @param weekNum the weekNum to set
	 */
	public void setWeekNum(int weekNum) {

		this.weekNum = weekNum;
	}

	/**
	 * Gets the specified day.
	 *
	 * @return the specifiedDay
	 */
	public int getSpecifiedDay() {

		return specifiedDay;
	}

	/**
	 * Sets the specified day.
	 *
	 * @param specifiedDay the specifiedDay to set
	 */
	public void setSpecifiedDay(int specifiedDay) {

		this.specifiedDay = specifiedDay;
	}

	/**
	 * Gets the loan calculator amount type.
	 *
	 * @return the loanCalculatorAmountType
	 */
	public int getLoanCalculatorAmountType() {

		return loanCalculatorAmountType;
	}

	/**
	 * Sets the loan calculator amount type.
	 *
	 * @param loanCalculatorAmountType the loanCalculatorAmountType to set
	 */
	public void setLoanCalculatorAmountType(int loanCalculatorAmountType) {

		this.loanCalculatorAmountType = loanCalculatorAmountType;
	}

	/**
	 * Gets the balloon payment method.
	 *
	 * @return the balloonPaymentMethod
	 */
	public int getBalloonPaymentMethod() {

		return balloonPaymentMethod;
	}

	/**
	 * Sets the balloon payment method.
	 *
	 * @param balloonPaymentMethod the balloonPaymentMethod to set
	 */
	public void setBalloonPaymentMethod(int balloonPaymentMethod) {

		this.balloonPaymentMethod = balloonPaymentMethod;
	}

	/**
	 * Gets the issue fee percentage 1.
	 *
	 * @return the issueFeePercentage1
	 */
	public double getIssueFeePercentage1() {

		return issueFeePercentage1;
	}

	/**
	 * Sets the issue fee percentage 1.
	 *
	 * @param issueFeePercentage1 the issueFeePercentage1 to set
	 */
	public void setIssueFeePercentage1(double issueFeePercentage1) {

		this.issueFeePercentage1 = issueFeePercentage1;
	}

	/**
	 * Gets the issue fee percentage 2.
	 *
	 * @return the issueFeePercentage2
	 */
	public double getIssueFeePercentage2() {

		return issueFeePercentage2;
	}

	/**
	 * Sets the issue fee percentage 2.
	 *
	 * @param issueFeePercentage2 the issueFeePercentage2 to set
	 */
	public void setIssueFeePercentage2(double issueFeePercentage2) {

		this.issueFeePercentage2 = issueFeePercentage2;
	}

	/**
	 * Gets the fee amt 2.
	 *
	 * @return the feeAmt2
	 */
	public double getFeeAmt2() {

		return feeAmt2;
	}

	/**
	 * Sets the fee amt 2.
	 *
	 * @param feeAmt2 the feeAmt2 to set
	 */
	public void setFeeAmt2(double feeAmt2) {

		this.feeAmt2 = feeAmt2;
	}

	/**
	 * Gets the issue fee.
	 *
	 * @return the issueFee
	 */
	public double getIssueFee() {

		return issueFee;
	}

	/**
	 * Sets the issue fee.
	 *
	 * @param issueFee the issueFee to set
	 */
	public void setIssueFee(double issueFee) {

		this.issueFee = issueFee;
	}

	/**
	 * Gets the term period ID.
	 *
	 * @return the termPeriodID
	 */
	public int getTermPeriodID() {

		return termPeriodID;
	}

	/**
	 * Sets the term period ID.
	 *
	 * @param termPeriodID the termPeriodID to set
	 */
	public void setTermPeriodID(int termPeriodID) {

		this.termPeriodID = termPeriodID;
	}

	/**
	 * Gets the repayment period ID.
	 *
	 * @return the repaymentPeriodID
	 */
	public int getRepaymentPeriodID() {

		return repaymentPeriodID;
	}

	/**
	 * Sets the repayment period ID.
	 *
	 * @param repaymentPeriodID the repaymentPeriodID to set
	 */
	public void setRepaymentPeriodID(int repaymentPeriodID) {

		this.repaymentPeriodID = repaymentPeriodID;
	}

	/**
	 * Gets the cu loan schedule.
	 *
	 * @return the cuLoanSchedule
	 */
	public List<LoanAbacusAPIModelCuLoanSchedule> getCuLoanSchedule() {

		return cuLoanSchedule;
	}

	/**
	 * Sets the cu loan schedule.
	 *
	 * @param cuLoanSchedule the cuLoanSchedule to set
	 */
	public void setCuLoanSchedule(List<LoanAbacusAPIModelCuLoanSchedule> cuLoanSchedule) {

		this.cuLoanSchedule = cuLoanSchedule;
	}

	/**
	 * Gets the currency ID.
	 *
	 * @return the currencyID
	 */
	public int getCurrencyID() {

		return currencyID;
	}

	/**
	 * Sets the currency ID.
	 *
	 * @param currencyID the currencyID to set
	 */
	public void setCurrencyID(int currencyID) {

		this.currencyID = currencyID;
	}

	/**
	 * Gets the final payment date.
	 *
	 * @return the finalPaymentDate
	 */
	public Date getFinalPaymentDate() {

		return finalPaymentDate;
	}

	/**
	 * Sets the final payment date.
	 *
	 * @param finalPaymentDate the finalPaymentDate to set
	 */
	public void setFinalPaymentDate(Date finalPaymentDate) {

		this.finalPaymentDate = finalPaymentDate;
	}

	/**
	 * Gets the effective int rate.
	 *
	 * @return the effectiveIntRate
	 */
	public double getEffectiveIntRate() {

		return effectiveIntRate;
	}

	/**
	 * Sets the effective int rate.
	 *
	 * @param effectiveIntRate the effectiveIntRate to set
	 */
	public void setEffectiveIntRate(double effectiveIntRate) {

		this.effectiveIntRate = effectiveIntRate;
	}

	/**
	 * Gets the edited normal payment.
	 *
	 * @return the editedNormalPayment
	 */
	public double getEditedNormalPayment() {

		return editedNormalPayment;
	}

	/**
	 * Sets the edited normal payment.
	 *
	 * @param editedNormalPayment the editedNormalPayment to set
	 */
	public void setEditedNormalPayment(double editedNormalPayment) {

		this.editedNormalPayment = editedNormalPayment;
	}

	/**
	 * Gets the specific provisioning rate.
	 *
	 * @return the specificProvisioningRate
	 */
	public double getSpecificProvisioningRate() {

		return specificProvisioningRate;
	}

	/**
	 * Sets the specific provisioning rate.
	 *
	 * @param specificProvisioningRate the specificProvisioningRate to set
	 */
	public void setSpecificProvisioningRate(double specificProvisioningRate) {

		this.specificProvisioningRate = specificProvisioningRate;
	}

	/**
	 * Checks if is payments sum up.
	 *
	 * @return the paymentsSumUp
	 */
	public boolean isPaymentsSumUp() {

		return paymentsSumUp;
	}

	/**
	 * Sets the payments sum up.
	 *
	 * @param paymentsSumUp the paymentsSumUp to set
	 */
	public void setPaymentsSumUp(boolean paymentsSumUp) {

		this.paymentsSumUp = paymentsSumUp;
	}

	/**
	 * Gets the cu insurance ID.
	 *
	 * @return the cuInsuranceID
	 */
	public int getCuInsuranceID() {

		return cuInsuranceID;
	}

	/**
	 * Sets the cu insurance ID.
	 *
	 * @param cuInsuranceID the cuInsuranceID to set
	 */
	public void setCuInsuranceID(int cuInsuranceID) {

		this.cuInsuranceID = cuInsuranceID;
	}

	/**
	 * Gets the insured amount.
	 *
	 * @return the insuredAmount
	 */
	public double getInsuredAmount() {

		return insuredAmount;
	}

	/**
	 * Sets the insured amount.
	 *
	 * @param insuredAmount the insuredAmount to set
	 */
	public void setInsuredAmount(double insuredAmount) {

		this.insuredAmount = insuredAmount;
	}

	/**
	 * Gets the insured term.
	 *
	 * @return the insuredTerm
	 */
	public int getInsuredTerm() {

		return insuredTerm;
	}

	/**
	 * Sets the insured term.
	 *
	 * @param insuredTerm the insuredTerm to set
	 */
	public void setInsuredTerm(int insuredTerm) {

		this.insuredTerm = insuredTerm;
	}

	/**
	 * Gets the insurance premium rate.
	 *
	 * @return the insurancePremiumRate
	 */
	public double getInsurancePremiumRate() {

		return insurancePremiumRate;
	}

	/**
	 * Sets the insurance premium rate.
	 *
	 * @param insurancePremiumRate the insurancePremiumRate to set
	 */
	public void setInsurancePremiumRate(double insurancePremiumRate) {

		this.insurancePremiumRate = insurancePremiumRate;
	}

	/**
	 * Gets the insurance premium.
	 *
	 * @return the insurancePremium
	 */
	public double getInsurancePremium() {

		return insurancePremium;
	}

	/**
	 * Sets the insurance premium.
	 *
	 * @param insurancePremium the insurancePremium to set
	 */
	public void setInsurancePremium(double insurancePremium) {

		this.insurancePremium = insurancePremium;
	}

	/**
	 * Gets the interest loss.
	 *
	 * @return the interestLoss
	 */
	public double getInterestLoss() {

		return interestLoss;
	}

	/**
	 * Sets the interest loss.
	 *
	 * @param interestLoss the interestLoss to set
	 */
	public void setInterestLoss(double interestLoss) {

		this.interestLoss = interestLoss;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "LoanAbacusAPIModelLoanPart [cuLoanPartID=" + cuLoanPartID + ", useScheduleInterest="
				+ useScheduleInterest + ", interestRate=" + interestRate + ", devaluationRate="
				+ devaluationRate + ", complimentaryInterestRate=" + complimentaryInterestRate
				+ ", loanAmount=" + loanAmount + ", issueAmount=" + issueAmount + ", refinanced="
				+ refinanced + ", issueDate=" + issueDate + ", levelPayments=" + levelPayments
				+ ", initialPaymentDate=" + initialPaymentDate + ", specifiedInitialPaymentDate="
				+ specifiedInitialPaymentDate + ", rePaymentPeriodNum=" + rePaymentPeriodNum
				+ ", rePaymentPeriod=" + rePaymentPeriod + ", intPayPeriodNum=" + intPayPeriodNum
				+ ", ignoreOddDays=" + ignoreOddDays + ", periodsDeferredID=" + periodsDeferredID
				+ ", periodsDeferred=" + periodsDeferred + ", firstRepaymentOffset="
				+ firstRepaymentOffset + ", gracePeriod=" + gracePeriod + ", termPeriodNum="
				+ termPeriodNum + ", termPeriod=" + termPeriod + ", calculateInitialPaymentDate="
				+ calculateInitialPaymentDate + ", loanCalculationMode=" + loanCalculationMode
				+ ", separateonSchedule=" + separateonSchedule + ", savingsAccID=" + savingsAccID
				+ ", mandatoryAccID=" + mandatoryAccID + ", normalPayment=" + normalPayment
				+ ", balloonAmount=" + balloonAmount + ", dayCount=" + dayCount + ", feeAmt1="
				+ feeAmt1 + ", capitaliseInterestWhenRefinancing="
				+ capitaliseInterestWhenRefinancing + ", useSpecifedDay=" + useSpecifedDay
				+ ", isReviewed=" + isReviewed + ", reviewPeriodNum=" + reviewPeriodNum
				+ ", referenceNumber=" + referenceNumber + ", reviewPeriod=" + reviewPeriod
				+ ", weekNum=" + weekNum + ", specifiedDay=" + specifiedDay
				+ ", loanCalculatorAmountType=" + loanCalculatorAmountType
				+ ", balloonPaymentMethod=" + balloonPaymentMethod + ", issueFeePercentage1="
				+ issueFeePercentage1 + ", issueFeePercentage2=" + issueFeePercentage2
				+ ", feeAmt2=" + feeAmt2 + ", issueFee=" + issueFee + ", termPeriodID="
				+ termPeriodID + ", repaymentPeriodID=" + repaymentPeriodID + ", cuLoanSchedule="
				+ cuLoanSchedule + ", currencyID=" + currencyID + ", finalPaymentDate="
				+ finalPaymentDate + ", effectiveIntRate=" + effectiveIntRate
				+ ", editedNormalPayment=" + editedNormalPayment + ", specificProvisioningRate="
				+ specificProvisioningRate + ", paymentsSumUp=" + paymentsSumUp + ", cuInsuranceID="
				+ cuInsuranceID + ", insuredAmount=" + insuredAmount + ", insuredTerm="
				+ insuredTerm + ", insurancePremiumRate=" + insurancePremiumRate
				+ ", insurancePremium=" + insurancePremium + ", interestLoss=" + interestLoss + "]";
	}

}
