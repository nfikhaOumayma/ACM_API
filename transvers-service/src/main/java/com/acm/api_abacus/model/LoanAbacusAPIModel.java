/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.model;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * {@link LoanAbacusAPIModel} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class LoanAbacusAPIModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -9114288605011853766L;

	/** The community loans. */
	private List<LoanAbacusAPIModelCommunityLoan> communityLoans;

	/** The loan guarantor. */
	private Object loanGuarantor;

	/** The cancel reason ID. */
	private int cancelReasonID;

	/** The notes. */
	private LoanAbacusAPIModelNotes notes;

	/** The loan edit schedule dto. */
	private Object loanEditScheduleDto;

	/** The edit. */
	private Object edit;

	/** The actual total repayment. */
	private double actualTotalRepayment;

	/** The loan agreement type. */
	private Object loanAgreementType;

	/** The is collateral process included. */
	private boolean isCollateralProcessIncluded;

	/** The is guarantor process included. */
	private boolean isGuarantorProcessIncluded;

	/** The customer financial analysis links. */
	private List<Object> customerFinancialAnalysisLinks;

	/** The loan status. */
	private int loanStatus;

	/** The cu insurance ID. */
	private int cuInsuranceID;

	/** The customer external loans. */
	private List<Object> customerExternalLoans;

	/** The person employee. */
	private Object personEmployee;

	/** The cu credit line account ID. */
	private Object cuCreditLineAccountID;

	/** The credit line framework. */
	private Object creditLineFramework;

	/** The insurance premium. */
	private double insurancePremium;

	/** The apr. */
	private double apr;

	/** The interest loss. */
	private double interestLoss;

	/** The out standing interest. */
	private double outStandingInterest;

	/** The is reject UD fand analysis. */
	private boolean isRejectUDFandAnalysis;

	/** The cu loan ID. */
	private int cuLoanID;

	/** The community CU loan ID. */
	private int communityCULoanID;

	/** The loan app. */
	private LoanAbacusAPIModelLoanApp loanApp;

	/** The excluded. */
	private boolean excluded;

	/** The issued. */
	private boolean issued;

	/** The product loan. */
	private LoanAbacusAPIModelProductLoan productLoan;

	/** The cu account ID. */
	private int cuAccountID;

	/** The customer ID. */
	private int customerID;

	/** The product id. */
	private int productId;

	/** The cu account portfolio ID. */
	private int cuAccountPortfolioID;

	/** The dr amount. */
	private double drAmount;

	/** The cr amount. */
	private double crAmount;

	/** The surveys. */
	private List<LoanAbacusAPIModelSurvey> surveys;

	/** The analysis links. */
	private List<Object> analysisLinks;

	/** The description. */
	private String description;

	/** The name. */
	private String name;

	/** The cu account rating ID. */
	private int cuAccountRatingID;

	/** The cu account industry code ID. */
	private int cuAccountIndustryCodeID;

	/** The regular statements. */
	private boolean regularStatements;

	/** The statement frequency num. */
	private int statementFrequencyNum;

	/** The statement frequency ID. */
	private int statementFrequencyID;

	/** The statement last run. */
	private String statementLastRun;

	/** The currency id. */
	private int currencyId;

	/** The active. */
	private boolean active;

	/** The acc status. */
	private int accStatus;

	/** The account number. */
	private String accountNumber;

	/** The product type. */
	private int productType;

	/** The product type ID. */
	private int productTypeID;

	/** The parent account ID. */
	private int parentAccountID;

	/** The balance date. */
	private Date balanceDate;

	/** The balance. */
	private double balance;

	/** The minimum share balance. */
	private double minimumShareBalance;

	/** The locked percentage. */
	private double lockedPercentage;

	/** The locked loan product id. */
	private int lockedLoanProductId;

	/** The interest. */
	private double interest;

	/** The available balance. */
	private double availableBalance;

	/** The share products. */
	private Object shareProducts;

	/**
	 * Instantiates a new loan abacus API model.
	 */
	public LoanAbacusAPIModel() {

		// Empty
	}

	/**
	 * Gets the community loans.
	 *
	 * @return the communityLoans
	 */
	public List<LoanAbacusAPIModelCommunityLoan> getCommunityLoans() {

		return communityLoans;
	}

	/**
	 * Sets the community loans.
	 *
	 * @param communityLoans the communityLoans to set
	 */
	public void setCommunityLoans(List<LoanAbacusAPIModelCommunityLoan> communityLoans) {

		this.communityLoans = communityLoans;
	}

	/**
	 * Gets the loan guarantor.
	 *
	 * @return the loanGuarantor
	 */
	public Object getLoanGuarantor() {

		return loanGuarantor;
	}

	/**
	 * Sets the loan guarantor.
	 *
	 * @param loanGuarantor the loanGuarantor to set
	 */
	public void setLoanGuarantor(Object loanGuarantor) {

		this.loanGuarantor = loanGuarantor;
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
	 * Gets the notes.
	 *
	 * @return the notes
	 */
	public LoanAbacusAPIModelNotes getNotes() {

		return notes;
	}

	/**
	 * Sets the notes.
	 *
	 * @param notes the notes to set
	 */
	public void setNotes(LoanAbacusAPIModelNotes notes) {

		this.notes = notes;
	}

	/**
	 * Gets the loan edit schedule dto.
	 *
	 * @return the loanEditScheduleDto
	 */
	public Object getLoanEditScheduleDto() {

		return loanEditScheduleDto;
	}

	/**
	 * Sets the loan edit schedule dto.
	 *
	 * @param loanEditScheduleDto the loanEditScheduleDto to set
	 */
	public void setLoanEditScheduleDto(Object loanEditScheduleDto) {

		this.loanEditScheduleDto = loanEditScheduleDto;
	}

	/**
	 * Gets the edits the.
	 *
	 * @return the edit
	 */
	public Object getEdit() {

		return edit;
	}

	/**
	 * Sets the edits the.
	 *
	 * @param edit the edit to set
	 */
	public void setEdit(Object edit) {

		this.edit = edit;
	}

	/**
	 * Gets the actual total repayment.
	 *
	 * @return the actualTotalRepayment
	 */
	public double getActualTotalRepayment() {

		return actualTotalRepayment;
	}

	/**
	 * Sets the actual total repayment.
	 *
	 * @param actualTotalRepayment the actualTotalRepayment to set
	 */
	public void setActualTotalRepayment(double actualTotalRepayment) {

		this.actualTotalRepayment = actualTotalRepayment;
	}

	/**
	 * Gets the loan agreement type.
	 *
	 * @return the loanAgreementType
	 */
	public Object getLoanAgreementType() {

		return loanAgreementType;
	}

	/**
	 * Sets the loan agreement type.
	 *
	 * @param loanAgreementType the loanAgreementType to set
	 */
	public void setLoanAgreementType(Object loanAgreementType) {

		this.loanAgreementType = loanAgreementType;
	}

	/**
	 * Checks if is collateral process included.
	 *
	 * @return the isCollateralProcessIncluded
	 */
	public boolean isCollateralProcessIncluded() {

		return isCollateralProcessIncluded;
	}

	/**
	 * Sets the collateral process included.
	 *
	 * @param isCollateralProcessIncluded the isCollateralProcessIncluded to set
	 */
	public void setCollateralProcessIncluded(boolean isCollateralProcessIncluded) {

		this.isCollateralProcessIncluded = isCollateralProcessIncluded;
	}

	/**
	 * Checks if is guarantor process included.
	 *
	 * @return the isGuarantorProcessIncluded
	 */
	public boolean isGuarantorProcessIncluded() {

		return isGuarantorProcessIncluded;
	}

	/**
	 * Sets the guarantor process included.
	 *
	 * @param isGuarantorProcessIncluded the isGuarantorProcessIncluded to set
	 */
	public void setGuarantorProcessIncluded(boolean isGuarantorProcessIncluded) {

		this.isGuarantorProcessIncluded = isGuarantorProcessIncluded;
	}

	/**
	 * Gets the customer financial analysis links.
	 *
	 * @return the customerFinancialAnalysisLinks
	 */
	public List<Object> getCustomerFinancialAnalysisLinks() {

		return customerFinancialAnalysisLinks;
	}

	/**
	 * Sets the customer financial analysis links.
	 *
	 * @param customerFinancialAnalysisLinks the customerFinancialAnalysisLinks to set
	 */
	public void setCustomerFinancialAnalysisLinks(List<Object> customerFinancialAnalysisLinks) {

		this.customerFinancialAnalysisLinks = customerFinancialAnalysisLinks;
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
	 * Gets the customer external loans.
	 *
	 * @return the customerExternalLoans
	 */
	public List<Object> getCustomerExternalLoans() {

		return customerExternalLoans;
	}

	/**
	 * Sets the customer external loans.
	 *
	 * @param customerExternalLoans the customerExternalLoans to set
	 */
	public void setCustomerExternalLoans(List<Object> customerExternalLoans) {

		this.customerExternalLoans = customerExternalLoans;
	}

	/**
	 * Gets the person employee.
	 *
	 * @return the personEmployee
	 */
	public Object getPersonEmployee() {

		return personEmployee;
	}

	/**
	 * Sets the person employee.
	 *
	 * @param personEmployee the personEmployee to set
	 */
	public void setPersonEmployee(Object personEmployee) {

		this.personEmployee = personEmployee;
	}

	/**
	 * Gets the cu credit line account ID.
	 *
	 * @return the cuCreditLineAccountID
	 */
	public Object getCuCreditLineAccountID() {

		return cuCreditLineAccountID;
	}

	/**
	 * Sets the cu credit line account ID.
	 *
	 * @param cuCreditLineAccountID the cuCreditLineAccountID to set
	 */
	public void setCuCreditLineAccountID(Object cuCreditLineAccountID) {

		this.cuCreditLineAccountID = cuCreditLineAccountID;
	}

	/**
	 * Gets the credit line framework.
	 *
	 * @return the creditLineFramework
	 */
	public Object getCreditLineFramework() {

		return creditLineFramework;
	}

	/**
	 * Sets the credit line framework.
	 *
	 * @param creditLineFramework the creditLineFramework to set
	 */
	public void setCreditLineFramework(Object creditLineFramework) {

		this.creditLineFramework = creditLineFramework;
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
	 * Gets the apr.
	 *
	 * @return the apr
	 */
	public double getApr() {

		return apr;
	}

	/**
	 * Sets the apr.
	 *
	 * @param apr the apr to set
	 */
	public void setApr(double apr) {

		this.apr = apr;
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

	/**
	 * Gets the out standing interest.
	 *
	 * @return the outStandingInterest
	 */
	public double getOutStandingInterest() {

		return outStandingInterest;
	}

	/**
	 * Sets the out standing interest.
	 *
	 * @param outStandingInterest the outStandingInterest to set
	 */
	public void setOutStandingInterest(double outStandingInterest) {

		this.outStandingInterest = outStandingInterest;
	}

	/**
	 * Checks if is reject UD fand analysis.
	 *
	 * @return the isRejectUDFandAnalysis
	 */
	public boolean isRejectUDFandAnalysis() {

		return isRejectUDFandAnalysis;
	}

	/**
	 * Sets the reject UD fand analysis.
	 *
	 * @param isRejectUDFandAnalysis the isRejectUDFandAnalysis to set
	 */
	public void setRejectUDFandAnalysis(boolean isRejectUDFandAnalysis) {

		this.isRejectUDFandAnalysis = isRejectUDFandAnalysis;
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
	 * Gets the community CU loan ID.
	 *
	 * @return the communityCULoanID
	 */
	public int getCommunityCULoanID() {

		return communityCULoanID;
	}

	/**
	 * Sets the community CU loan ID.
	 *
	 * @param communityCULoanID the communityCULoanID to set
	 */
	public void setCommunityCULoanID(int communityCULoanID) {

		this.communityCULoanID = communityCULoanID;
	}

	/**
	 * Gets the loan app.
	 *
	 * @return the loanApp
	 */
	public LoanAbacusAPIModelLoanApp getLoanApp() {

		return loanApp;
	}

	/**
	 * Sets the loan app.
	 *
	 * @param loanApp the loanApp to set
	 */
	public void setLoanApp(LoanAbacusAPIModelLoanApp loanApp) {

		this.loanApp = loanApp;
	}

	/**
	 * Checks if is excluded.
	 *
	 * @return the excluded
	 */
	public boolean isExcluded() {

		return excluded;
	}

	/**
	 * Sets the excluded.
	 *
	 * @param excluded the excluded to set
	 */
	public void setExcluded(boolean excluded) {

		this.excluded = excluded;
	}

	/**
	 * Checks if is issued.
	 *
	 * @return the issued
	 */
	public boolean isIssued() {

		return issued;
	}

	/**
	 * Sets the issued.
	 *
	 * @param issued the issued to set
	 */
	public void setIssued(boolean issued) {

		this.issued = issued;
	}

	/**
	 * Gets the product loan.
	 *
	 * @return the productLoan
	 */
	public LoanAbacusAPIModelProductLoan getProductLoan() {

		return productLoan;
	}

	/**
	 * Sets the product loan.
	 *
	 * @param productLoan the productLoan to set
	 */
	public void setProductLoan(LoanAbacusAPIModelProductLoan productLoan) {

		this.productLoan = productLoan;
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
	 * Gets the product id.
	 *
	 * @return the productId
	 */
	public int getProductId() {

		return productId;
	}

	/**
	 * Sets the product id.
	 *
	 * @param productId the productId to set
	 */
	public void setProductId(int productId) {

		this.productId = productId;
	}

	/**
	 * Gets the cu account portfolio ID.
	 *
	 * @return the cuAccountPortfolioID
	 */
	public int getCuAccountPortfolioID() {

		return cuAccountPortfolioID;
	}

	/**
	 * Sets the cu account portfolio ID.
	 *
	 * @param cuAccountPortfolioID the cuAccountPortfolioID to set
	 */
	public void setCuAccountPortfolioID(int cuAccountPortfolioID) {

		this.cuAccountPortfolioID = cuAccountPortfolioID;
	}

	/**
	 * Gets the dr amount.
	 *
	 * @return the drAmount
	 */
	public double getDrAmount() {

		return drAmount;
	}

	/**
	 * Sets the dr amount.
	 *
	 * @param drAmount the drAmount to set
	 */
	public void setDrAmount(double drAmount) {

		this.drAmount = drAmount;
	}

	/**
	 * Gets the cr amount.
	 *
	 * @return the crAmount
	 */
	public double getCrAmount() {

		return crAmount;
	}

	/**
	 * Sets the cr amount.
	 *
	 * @param crAmount the crAmount to set
	 */
	public void setCrAmount(double crAmount) {

		this.crAmount = crAmount;
	}

	/**
	 * Gets the surveys.
	 *
	 * @return the surveys
	 */
	public List<LoanAbacusAPIModelSurvey> getSurveys() {

		return surveys;
	}

	/**
	 * Sets the surveys.
	 *
	 * @param surveys the surveys to set
	 */
	public void setSurveys(List<LoanAbacusAPIModelSurvey> surveys) {

		this.surveys = surveys;
	}

	/**
	 * Gets the analysis links.
	 *
	 * @return the analysisLinks
	 */
	public List<Object> getAnalysisLinks() {

		return analysisLinks;
	}

	/**
	 * Sets the analysis links.
	 *
	 * @param analysisLinks the analysisLinks to set
	 */
	public void setAnalysisLinks(List<Object> analysisLinks) {

		this.analysisLinks = analysisLinks;
	}

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
	 * Gets the name.
	 *
	 * @return the name
	 */
	public String getName() {

		return name;
	}

	/**
	 * Sets the name.
	 *
	 * @param name the name to set
	 */
	public void setName(String name) {

		this.name = name;
	}

	/**
	 * Gets the cu account rating ID.
	 *
	 * @return the cuAccountRatingID
	 */
	public int getCuAccountRatingID() {

		return cuAccountRatingID;
	}

	/**
	 * Sets the cu account rating ID.
	 *
	 * @param cuAccountRatingID the cuAccountRatingID to set
	 */
	public void setCuAccountRatingID(int cuAccountRatingID) {

		this.cuAccountRatingID = cuAccountRatingID;
	}

	/**
	 * Gets the cu account industry code ID.
	 *
	 * @return the cuAccountIndustryCodeID
	 */
	public int getCuAccountIndustryCodeID() {

		return cuAccountIndustryCodeID;
	}

	/**
	 * Sets the cu account industry code ID.
	 *
	 * @param cuAccountIndustryCodeID the cuAccountIndustryCodeID to set
	 */
	public void setCuAccountIndustryCodeID(int cuAccountIndustryCodeID) {

		this.cuAccountIndustryCodeID = cuAccountIndustryCodeID;
	}

	/**
	 * Checks if is regular statements.
	 *
	 * @return the regularStatements
	 */
	public boolean isRegularStatements() {

		return regularStatements;
	}

	/**
	 * Sets the regular statements.
	 *
	 * @param regularStatements the regularStatements to set
	 */
	public void setRegularStatements(boolean regularStatements) {

		this.regularStatements = regularStatements;
	}

	/**
	 * Gets the statement frequency num.
	 *
	 * @return the statementFrequencyNum
	 */
	public int getStatementFrequencyNum() {

		return statementFrequencyNum;
	}

	/**
	 * Sets the statement frequency num.
	 *
	 * @param statementFrequencyNum the statementFrequencyNum to set
	 */
	public void setStatementFrequencyNum(int statementFrequencyNum) {

		this.statementFrequencyNum = statementFrequencyNum;
	}

	/**
	 * Gets the statement frequency ID.
	 *
	 * @return the statementFrequencyID
	 */
	public int getStatementFrequencyID() {

		return statementFrequencyID;
	}

	/**
	 * Sets the statement frequency ID.
	 *
	 * @param statementFrequencyID the statementFrequencyID to set
	 */
	public void setStatementFrequencyID(int statementFrequencyID) {

		this.statementFrequencyID = statementFrequencyID;
	}

	/**
	 * Gets the statement last run.
	 *
	 * @return the statementLastRun
	 */
	public String getStatementLastRun() {

		return statementLastRun;
	}

	/**
	 * Sets the statement last run.
	 *
	 * @param statementLastRun the statementLastRun to set
	 */
	public void setStatementLastRun(String statementLastRun) {

		this.statementLastRun = statementLastRun;
	}

	/**
	 * Gets the currency id.
	 *
	 * @return the currencyId
	 */
	public int getCurrencyId() {

		return currencyId;
	}

	/**
	 * Sets the currency id.
	 *
	 * @param currencyId the currencyId to set
	 */
	public void setCurrencyId(int currencyId) {

		this.currencyId = currencyId;
	}

	/**
	 * Checks if is active.
	 *
	 * @return the active
	 */
	public boolean isActive() {

		return active;
	}

	/**
	 * Sets the active.
	 *
	 * @param active the active to set
	 */
	public void setActive(boolean active) {

		this.active = active;
	}

	/**
	 * Gets the acc status.
	 *
	 * @return the accStatus
	 */
	public int getAccStatus() {

		return accStatus;
	}

	/**
	 * Sets the acc status.
	 *
	 * @param accStatus the accStatus to set
	 */
	public void setAccStatus(int accStatus) {

		this.accStatus = accStatus;
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
	 * Gets the product type.
	 *
	 * @return the productType
	 */
	public int getProductType() {

		return productType;
	}

	/**
	 * Sets the product type.
	 *
	 * @param productType the productType to set
	 */
	public void setProductType(int productType) {

		this.productType = productType;
	}

	/**
	 * Gets the product type ID.
	 *
	 * @return the productTypeID
	 */
	public int getProductTypeID() {

		return productTypeID;
	}

	/**
	 * Sets the product type ID.
	 *
	 * @param productTypeID the productTypeID to set
	 */
	public void setProductTypeID(int productTypeID) {

		this.productTypeID = productTypeID;
	}

	/**
	 * Gets the parent account ID.
	 *
	 * @return the parentAccountID
	 */
	public int getParentAccountID() {

		return parentAccountID;
	}

	/**
	 * Sets the parent account ID.
	 *
	 * @param parentAccountID the parentAccountID to set
	 */
	public void setParentAccountID(int parentAccountID) {

		this.parentAccountID = parentAccountID;
	}

	/**
	 * Gets the balance date.
	 *
	 * @return the balanceDate
	 */
	public Date getBalanceDate() {

		return balanceDate;
	}

	/**
	 * Sets the balance date.
	 *
	 * @param balanceDate the balanceDate to set
	 */
	public void setBalanceDate(Date balanceDate) {

		this.balanceDate = balanceDate;
	}

	/**
	 * Gets the balance.
	 *
	 * @return the balance
	 */
	public double getBalance() {

		return balance;
	}

	/**
	 * Sets the balance.
	 *
	 * @param balance the balance to set
	 */
	public void setBalance(double balance) {

		this.balance = balance;
	}

	/**
	 * Gets the minimum share balance.
	 *
	 * @return the minimumShareBalance
	 */
	public double getMinimumShareBalance() {

		return minimumShareBalance;
	}

	/**
	 * Sets the minimum share balance.
	 *
	 * @param minimumShareBalance the minimumShareBalance to set
	 */
	public void setMinimumShareBalance(double minimumShareBalance) {

		this.minimumShareBalance = minimumShareBalance;
	}

	/**
	 * Gets the locked percentage.
	 *
	 * @return the lockedPercentage
	 */
	public double getLockedPercentage() {

		return lockedPercentage;
	}

	/**
	 * Sets the locked percentage.
	 *
	 * @param lockedPercentage the lockedPercentage to set
	 */
	public void setLockedPercentage(double lockedPercentage) {

		this.lockedPercentage = lockedPercentage;
	}

	/**
	 * Gets the locked loan product id.
	 *
	 * @return the lockedLoanProductId
	 */
	public int getLockedLoanProductId() {

		return lockedLoanProductId;
	}

	/**
	 * Sets the locked loan product id.
	 *
	 * @param lockedLoanProductId the lockedLoanProductId to set
	 */
	public void setLockedLoanProductId(int lockedLoanProductId) {

		this.lockedLoanProductId = lockedLoanProductId;
	}

	/**
	 * Gets the interest.
	 *
	 * @return the interest
	 */
	public double getInterest() {

		return interest;
	}

	/**
	 * Sets the interest.
	 *
	 * @param interest the interest to set
	 */
	public void setInterest(double interest) {

		this.interest = interest;
	}

	/**
	 * Gets the available balance.
	 *
	 * @return the availableBalance
	 */
	public double getAvailableBalance() {

		return availableBalance;
	}

	/**
	 * Sets the available balance.
	 *
	 * @param availableBalance the availableBalance to set
	 */
	public void setAvailableBalance(double availableBalance) {

		this.availableBalance = availableBalance;
	}

	/**
	 * Gets the share products.
	 *
	 * @return the shareProducts
	 */
	public Object getShareProducts() {

		return shareProducts;
	}

	/**
	 * Sets the share products.
	 *
	 * @param shareProducts the shareProducts to set
	 */
	public void setShareProducts(Object shareProducts) {

		this.shareProducts = shareProducts;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "LoanAbacusAPIModel [communityLoans=" + communityLoans + ", loanGuarantor="
				+ loanGuarantor + ", cancelReasonID=" + cancelReasonID + ", notes=" + notes
				+ ", loanEditScheduleDto=" + loanEditScheduleDto + ", edit=" + edit
				+ ", actualTotalRepayment=" + actualTotalRepayment + ", loanAgreementType="
				+ loanAgreementType + ", isCollateralProcessIncluded=" + isCollateralProcessIncluded
				+ ", isGuarantorProcessIncluded=" + isGuarantorProcessIncluded
				+ ", customerFinancialAnalysisLinks=" + customerFinancialAnalysisLinks
				+ ", loanStatus=" + loanStatus + ", cuInsuranceID=" + cuInsuranceID
				+ ", customerExternalLoans=" + customerExternalLoans + ", personEmployee="
				+ personEmployee + ", cuCreditLineAccountID=" + cuCreditLineAccountID
				+ ", creditLineFramework=" + creditLineFramework + ", insurancePremium="
				+ insurancePremium + ", apr=" + apr + ", interestLoss=" + interestLoss
				+ ", outStandingInterest=" + outStandingInterest + ", isRejectUDFandAnalysis="
				+ isRejectUDFandAnalysis + ", cuLoanID=" + cuLoanID + ", communityCULoanID="
				+ communityCULoanID + ", loanApp=" + loanApp + ", excluded=" + excluded
				+ ", issued=" + issued + ", productLoan=" + productLoan + ", cuAccountID="
				+ cuAccountID + ", customerID=" + customerID + ", productId=" + productId
				+ ", cuAccountPortfolioID=" + cuAccountPortfolioID + ", drAmount=" + drAmount
				+ ", crAmount=" + crAmount + ", surveys=" + surveys + ", analysisLinks="
				+ analysisLinks + ", description=" + description + ", name=" + name
				+ ", cuAccountRatingID=" + cuAccountRatingID + ", cuAccountIndustryCodeID="
				+ cuAccountIndustryCodeID + ", regularStatements=" + regularStatements
				+ ", statementFrequencyNum=" + statementFrequencyNum + ", statementFrequencyID="
				+ statementFrequencyID + ", statementLastRun=" + statementLastRun + ", currencyId="
				+ currencyId + ", active=" + active + ", accStatus=" + accStatus
				+ ", accountNumber=" + accountNumber + ", productType=" + productType
				+ ", productTypeID=" + productTypeID + ", parentAccountID=" + parentAccountID
				+ ", balanceDate=" + balanceDate + ", balance=" + balance + ", minimumShareBalance="
				+ minimumShareBalance + ", lockedPercentage=" + lockedPercentage
				+ ", lockedLoanProductId=" + lockedLoanProductId + ", interest=" + interest
				+ ", availableBalance=" + availableBalance + ", shareProducts=" + shareProducts
				+ "]";
	}

}
