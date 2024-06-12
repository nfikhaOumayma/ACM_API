/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.model;

import java.io.Serializable;
import java.util.Date;

/**
 * {@link LoanAbacusAPIModelProductLoan} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class LoanAbacusAPIModelProductLoan implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 4079064467566453122L;

	/** The product id. */
	private int productId;

	/** The code. */
	private String code;

	/** The description. */
	private String description;

	/** The cu account portfolio required. */
	private boolean cuAccountPortfolioRequired;

	/** The max accounts. */
	private int maxAccounts;

	/** The active. */
	private boolean active;

	/** The currency id. */
	private int currencyId;

	/** The regular statements. */
	private boolean regularStatements;

	/** The statement frequency ID. */
	private int statementFrequencyID;

	/** The statement frequency num. */
	private int statementFrequencyNum;

	/** The min denomination. */
	private double minDenomination;

	/** The passbook printer ID. */
	private int passbookPrinterID;

	/** The allow lock account rating. */
	private boolean allowLockAccountRating;

	/** The lock account rating period ID. */
	private int lockAccountRatingPeriodID;

	/** The lock account rating period num. */
	private int lockAccountRatingPeriodNum;

	/** The minimum balance. */
	private double minimumBalance;

	/** The level payments. */
	private boolean levelPayments;

	/** The maximum balance. */
	private double maximumBalance;

	/** The minimum cheque. */
	private double minimumCheque;

	/** The maximum cheque. */
	private double maximumCheque;

	/** The default cheque. */
	private double defaultCheque;

	/** The minimum term. */
	private int minimumTerm;

	/** The default term. */
	private int defaultTerm;

	/** The default term period ID. */
	private int defaultTermPeriodID;

	/** The maximum term. */
	private int maximumTerm;

	/** The maximum member age. */
	private int maximumMemberAge;

	/** The minimum member age. */
	private int minimumMemberAge;

	/** The interest charge date. */
	private Date interestChargeDate;

	/** The charge off. */
	private boolean chargeOff;

	/** The recover interest. */
	private boolean recoverInterest;

	/** The keep collecting interest. */
	private boolean keepCollectingInterest;

	/** The use default rate. */
	private boolean useDefaultRate;

	/** The penalty interest amount type. */
	private int penaltyInterestAmountType;

	/** The penalty interest rate. */
	private double penaltyInterestRate;

	/** The penalty interest flat amount. */
	private double penaltyInterestFlatAmount;

	/** The penalty interest fee ID. */
	private int penaltyInterestFeeID;

	/** The penalty int grace period. */
	private int penaltyIntGracePeriod;

	/** The multiply penalty interest by number of group members. */
	private boolean multiplyPenaltyInterestByNumberOfGroupMembers;

	/** The flat interest method. */
	private int flatInterestMethod;

	/** The pre pay interest. */
	private boolean prePayInterest;

	/** The rebate rate. */
	private double rebateRate;

	/** The rebate trn message. */
	private String rebateTrnMessage;

	/** The rebate next payment date. */
	private Date rebateNextPaymentDate;

	/** The rebate pay method. */
	private int rebatePayMethod;

	/** The rebate trn start date. */
	private Date rebateTrnStartDate;

	/** The rebate trn end date. */
	private Date rebateTrnEndDate;

	/** The rebate limits. */
	private int rebateLimits;

	/** The annuities ORD. */
	private boolean annuitiesORD;

	/** The compounding. */
	private boolean compounding;

	/** The compound period num. */
	private int compoundPeriodNum;

	/** The compound period ID. */
	private int compoundPeriodID;

	/** The allow top up. */
	private boolean allowTopUp;

	/** The allow parts. */
	private boolean allowParts;

	/** The allow redraw. */
	private boolean allowRedraw;

	/** The allow balloon payments. */
	private boolean allowBalloonPayments;

	/** The allow single balloon payment only. */
	private boolean allowSingleBalloonPaymentOnly;

	/** The allow deferred payments. */
	private boolean allowDeferredPayments;

	/** The allow transfer from savings. */
	private boolean allowTransferFromSavings;

	/** The default to single scheduled payment. */
	private boolean defaultToSingleScheduledPayment;

	/** The issue fee ID. */
	private int issueFeeID;

	/** The charge issue fee. */
	private boolean chargeIssueFee;

	/** The issue fee ID 1. */
	private int issueFeeID1;

	/** The charge issue fee 1. */
	private double chargeIssueFee1;

	/** The issue fee ID 2. */
	private double issueFeeID2;

	/** The charge issue fee 2. */
	private double chargeIssueFee2;

	/** The issue fee ID 3. */
	private double issueFeeID3;

	/** The charge issue fee 3. */
	private double chargeIssueFee3;

	/** The is issue fee calculated first time only. */
	private boolean isIssueFeeCalculatedFirstTimeOnly;

	/** The is issue fee calculated on approval amount. */
	private boolean isIssueFeeCalculatedOnApprovalAmount;

	/** The issue fee age limits apply to ID. */
	private int issueFeeAgeLimitsApplyToID;

	/** The issue fee age minimum. */
	private int issueFeeAgeMinimum;

	/** The issue fee age maximum. */
	private int issueFeeAgeMaximum;

	/** The discharge fee ID. */
	private int dischargeFeeID;

	/** The additional amount charged in discharge fee. */
	private double additionalAmountChargedInDischargeFee;

	/** The early discharge fee threshold in months. */
	private int earlyDischargeFeeThresholdInMonths;

	/** The early discharge fee number of days to charge. */
	private int earlyDischargeFeeNumberOfDaysToCharge;

	/** The devaluate loans. */
	private boolean devaluateLoans;

	/** The devaluation global. */
	private boolean devaluationGlobal;

	/** The devaluation method. */
	private int devaluationMethod;

	/** The devalue fee ID. */
	private int devalueFeeID;

	/** The devalue rate. */
	private double devalueRate;

	/** The maximum deferred period. */
	private int maximumDeferredPeriod;

	/** The minimum deferred period. */
	private int minimumDeferredPeriod;

	/** The repayment frequency periods. */
	private int repaymentFrequencyPeriods;

	/** The exclude due from arrears. */
	private boolean excludeDueFromArrears;

	/** The uds fee ID. */
	private int udsFeeID;

	/** The service fee ID. */
	private int serviceFeeID;

	/** The service fee period num. */
	private int serviceFeePeriodNum;

	/** The service fee period ID. */
	private int serviceFeePeriodID;

	/** The use peak debt. */
	private boolean usePeakDebt;

	/** The peak debt method. */
	private int peakDebtMethod;

	/** The service fees date. */
	private Date serviceFeesDate;

	/** The requires review. */
	private boolean requiresReview;

	/** The review period num. */
	private int reviewPeriodNum;

	/** The review period ID. */
	private int reviewPeriodID;

	/** The min savings. */
	private int minSavings;

	/** The min collateral. */
	private int minCollateral;

	/** The max collateral amount. */
	private double maxCollateralAmount;

	/** The calculate initial payment date. */
	private boolean calculateInitialPaymentDate;

	/** The use schedule interest. */
	private boolean useScheduleInterest;

	/** The group loans only. */
	private boolean groupLoansOnly;

	/** The min group size. */
	private int minGroupSize;

	/** The capitalise interest when refinancing. */
	private boolean capitaliseInterestWhenRefinancing;

	/** The round down. */
	private int roundDown;

	/** The increment days. */
	private boolean incrementDays;

	/** The closed days. */
	private int closedDays;

	/** The accrue interest daily. */
	private boolean accrueInterestDaily;

	/** The repayment method. */
	private int repaymentMethod;

	/** The due today format. */
	private String dueTodayFormat;

	/** The arrears format. */
	private String arrearsFormat;

	/** The pre paid format. */
	private String prePaidFormat;

	/** The complimentary interest. */
	private boolean complimentaryInterest;

	/** The complimentary interest rate. */
	private double complimentaryInterestRate;

	/** The day count default. */
	private int dayCountDefault;

	/** The general provision percentage. */
	private double generalProvisionPercentage;

	/** The general provision percentage restructured. */
	private double generalProvisionPercentageRestructured;

	/** The general provision cutoff. */
	private int generalProvisionCutoff;

	/** The general provision cutoff period ID. */
	private int generalProvisionCutoffPeriodID;

	/** The include savings in provision. */
	private boolean includeSavingsInProvision;

	/** The include collateral in provision. */
	private boolean includeCollateralInProvision;

	/** The include guarantees in provision. */
	private boolean includeGuaranteesInProvision;

	/** The include interest due in provision. */
	private boolean includeInterestDueInProvision;

	/** The ignore arrears below. */
	private double ignoreArrearsBelow;

	/** The bank of indonesia brackets. */
	private boolean bankOfIndonesiaBrackets;

	/** The specific provision period. */
	private int specificProvisionPeriod;

	/** The specific provision mature period. */
	private int specificProvisionMaturePeriod;

	/** The use default provision settings. */
	private boolean useDefaultProvisionSettings;

	/** The use default doubtful debts settings. */
	private boolean useDefaultDoubtfulDebtsSettings;

	/** The default charge off product ID. */
	private int defaultChargeOffProductID;

	/** The doubtful debts period. */
	private int doubtfulDebtsPeriod;

	/** The general provision date. */
	private Date generalProvisionDate;

	/** The specific provision date. */
	private Date specificProvisionDate;

	/** The doubtful debts date. */
	private Date doubtfulDebtsDate;

	/** The line of credit. */
	private boolean lineOfCredit;

	/** The loc min repayment. */
	private double locMinRepayment;

	/** The loc default repay percent. */
	private double locDefaultRepayPercent;

	/** The loc reschedule on issue. */
	private boolean locRescheduleOnIssue;

	/** The emergency loan. */
	private boolean emergencyLoan;

	/** The allow settlement accounts. */
	private boolean allowSettlementAccounts;

	/** The settlement number of attempts. */
	private int settlementNumberOfAttempts;

	/** The settlement attempt fee ID. */
	private int settlementAttemptFeeID;

	/** The settlement attempt fee amount. */
	private double settlementAttemptFeeAmount;

	/** The settlement attempt fee pay. */
	private boolean settlementAttemptFeePay;

	/** The settlement fail fee ID. */
	private int settlementFailFeeID;

	/** The settlement fail fee amount. */
	private double settlementFailFeeAmount;

	/** The settlement fail fee pay. */
	private boolean settlementFailFeePay;

	/** The settlement fee per fail. */
	private boolean settlementFeePerFail;

	/** The first payment limit in days. */
	private int firstPaymentLimitInDays;

	/** The allow standing instructions. */
	private boolean allowStandingInstructions;

	/** The number of attempts. */
	private int numberOfAttempts;

	/** The attempt fee ID. */
	private int attemptFeeID;

	/** The attempt fee amount. */
	private double attemptFeeAmount;

	/** The attempt fee pay. */
	private boolean attemptFeePay;

	/** The fail fee ID. */
	private int failFeeID;

	/** The fail fee amount. */
	private double failFeeAmount;

	/** The fail fee pay. */
	private boolean failFeePay;

	/** The fee per fail. */
	private boolean feePerFail;

	/** The dormancy fee ID. */
	private int dormancyFeeID;

	/** The dormancy fee period num. */
	private int dormancyFeePeriodNum;

	/** The dormancy fee period ID. */
	private int dormancyFeePeriodID;

	/** The dormancy fee last run. */
	private Date dormancyFeeLastRun;

	/** The dormancy period num. */
	private int dormancyPeriodNum;

	/** The dormancy period ID. */
	private int dormancyPeriodID;

	/** The dormancy transaction sources. */
	private int dormancyTransactionSources;

	/** The dormancy transaction types. */
	private int dormancyTransactionTypes;

	/** The allow dormancy. */
	private boolean allowDormancy;

	/** The use overdrawn date. */
	private boolean useOverdrawnDate;

	/** The restructured loan expiry period num. */
	private int restructuredLoanExpiryPeriodNum;

	/** The restructured loan expiry period ID. */
	private int restructuredLoanExpiryPeriodID;

	/** The interest method. */
	private int interestMethod;

	/** The default interest method. */
	private int defaultInterestMethod;

	/** The linked savings payments required. */
	private boolean linkedSavingsPaymentsRequired;

	/** The linked savings payment amount. */
	private double linkedSavingsPaymentAmount;

	/** The linked savings payment percentage. */
	private double linkedSavingsPaymentPercentage;

	/** The linked savings payment allow underpaid. */
	private boolean linkedSavingsPaymentAllowUnderpaid;

	/** The linked savings payment require override for underpaid. */
	private boolean linkedSavingsPaymentRequireOverrideForUnderpaid;

	/** The linked savings payment withholding. */
	private boolean linkedSavingsPaymentWithholding;

	/** The linked savings payment type. */
	private int linkedSavingsPaymentType;

	/** The use loan approval groups. */
	private boolean useLoanApprovalGroups;

	/** The cheque book enabled. */
	private boolean chequeBookEnabled;

	/** The cheque stop fee ID. */
	private int chequeStopFeeID;

	/** The cheque stop fee amount. */
	private double chequeStopFeeAmount;

	/** The cheque stop fee pay. */
	private boolean chequeStopFeePay;

	/** The cheque un stop fee ID. */
	private int chequeUnStopFeeID;

	/** The cheque unstop fee amount. */
	private double chequeUnstopFeeAmount;

	/** The cheque un stop fee pay. */
	private boolean chequeUnStopFeePay;

	/** The cheque payer void fee ID. */
	private int chequePayerVoidFeeID;

	/** The cheque payer void fee amount. */
	private double chequePayerVoidFeeAmount;

	/** The cheque payer void fee pay. */
	private boolean chequePayerVoidFeePay;

	/** The cheque payee void fee ID. */
	private int chequePayeeVoidFeeID;

	/** The cheque payee void fee amount. */
	private double chequePayeeVoidFeeAmount;

	/** The cheque payee void fee pay. */
	private boolean chequePayeeVoidFeePay;

	/** The cheque payer dishonour fee ID. */
	private int chequePayerDishonourFeeID;

	/** The cheque payer dishonour fee amount. */
	private double chequePayerDishonourFeeAmount;

	/** The cheque payer dishonour fee pay. */
	private boolean chequePayerDishonourFeePay;

	/** The cheque payee dishonour fee ID. */
	private int chequePayeeDishonourFeeID;

	/** The cheque payee dishonour fee amount. */
	private double chequePayeeDishonourFeeAmount;

	/** The cheque payee dishonour fee pay. */
	private boolean chequePayeeDishonourFeePay;

	/** The cheque print fee ID. */
	private int chequePrintFeeID;

	/** The cheque print fee amount. */
	private double chequePrintFeeAmount;

	/** The cheque print fee pay. */
	private boolean chequePrintFeePay;

	/** The early payout fee ID. */
	private int earlyPayoutFeeID;

	/** The early payout fee amount. */
	private double earlyPayoutFeeAmount;

	/** The atm enabled. */
	private boolean atmEnabled;

	/** The atm card limit. */
	private double atmCardLimit;

	/** The atm daily limit. */
	private double atmDailyLimit;

	/** The atm withdrawal fee ID. */
	private int atmWithdrawalFeeID;

	/** The atm service fee ID. */
	private int atmServiceFeeID;

	/** The atm service fee period num. */
	private int atmServiceFeePeriodNum;

	/** The atm service fee period ID. */
	private int atmServiceFeePeriodID;

	/** The atm service fee last run. */
	private Date atmServiceFeeLastRun;

	/** The atm min balance. */
	private double atmMinBalance;

	/** The service fee ID 2. */
	private int serviceFeeID2;

	/** The service fee period num 2. */
	private int serviceFeePeriodNum2;

	/** The service fee period ID 2. */
	private int serviceFeePeriodID2;

	/** The close LOC automatically. */
	private boolean closeLOCAutomatically;

	/** The lock provisioning band. */
	private boolean lockProvisioningBand;

	/** The lock provisioning band period num. */
	private int lockProvisioningBandPeriodNum;

	/** The lock provisioning band period ID. */
	private int lockProvisioningBandPeriodID;

	/** The interest cap method. */
	private int interestCapMethod;

	/** The service oldest debt first. */
	private boolean serviceOldestDebtFirst;

	/** The include interest due in arrears. */
	private boolean includeInterestDueInArrears;

	/** The include devaluation fee in arrears. */
	private boolean includeDevaluationFeeInArrears;

	/** The offset prepaid balance against interest due in arrears. */
	private boolean offsetPrepaidBalanceAgainstInterestDueInArrears;

	/** The include overdrawn date in arrears. */
	private boolean includeOverdrawnDateInArrears;

	/** The ignore schedule in arrears. */
	private boolean ignoreScheduleInArrears;

	/** The collect savings during write off. */
	private boolean collectSavingsDuringWriteOff;

	/** The is RPI insurance required. */
	private boolean isRPIInsuranceRequired;

	/** The cu account industry code required. */
	private boolean cuAccountIndustryCodeRequired;

	/** The allow loan issue override. */
	private boolean allowLoanIssueOverride;

	/** The used. */
	private boolean used;

	/** The include due in schedule recalc. */
	private boolean includeDueInScheduleRecalc;

	/** The attempt fee external. */
	private boolean attemptFeeExternal;

	/** The interest refund minimum period num. */
	private int interestRefundMinimumPeriodNum;

	/** The interest rate period. */
	private int interestRatePeriod;

	/** The interest rate period num. */
	private int interestRatePeriodNum;

	/** The flat rate interest period. */
	private int flatRateInterestPeriod;

	/** The flat rate interest period num. */
	private int flatRateInterestPeriodNum;

	/** The flat rate interest rate. */
	private double flatRateInterestRate;

	/** The use single loan agreement. */
	private boolean useSingleLoanAgreement;

	/** The loan agreement id. */
	private int loanAgreementId;

	/** The use loan repayment date limits. */
	private Date useLoanRepaymentDateLimits;

	/** The allow payout with interest due. */
	private boolean allowPayoutWithInterestDue;

	/** The allow payout with fees due. */
	private boolean allowPayoutWithFeesDue;

	/** The is seasonal loan. */
	private boolean isSeasonalLoan;

	/** The use holidays and closed days in penalty interest calculations. */
	private boolean useHolidaysAndClosedDaysInPenaltyInterestCalculations;

	/** The deferred period value. */
	private int deferredPeriodValue;

	/** The deferred period option ID. */
	private int deferredPeriodOptionID;

	/** The renew principal and interest. */
	private boolean renewPrincipalAndInterest;

	/** The cal interest accrual daily. */
	private boolean calInterestAccrualDaily;

	/** The separateon schedule 1. */
	private boolean separateonSchedule1;

	/** The separateon schedule 2. */
	private boolean separateonSchedule2;

	/** The fee type 1. */
	private int feeType1;

	/** The fee type 2. */
	private int feeType2;

	/** The is use default interest rate by loan count. */
	private boolean isUseDefaultInterestRateByLoanCount;

	/** The is allow to override int rate at application level. */
	private boolean isAllowToOverrideIntRateAtApplicationLevel;

	/** The schedule rounding. */
	private boolean scheduleRounding;

	/** The schedule rounding ID. */
	private boolean scheduleRoundingID;

	/** The penalty interest per payment. */
	private boolean penaltyInterestPerPayment;

	/** The allow bank cheque printing. */
	private boolean allowBankChequePrinting;

	/** The allow change bank account. */
	private boolean allowChangeBankAccount;

	/** The is use default interest rate by tiered settings. */
	private boolean isUseDefaultInterestRateByTieredSettings;

	/** The withhold due amount. */
	private boolean withholdDueAmount;

	/** The prepay next due only. */
	private boolean prepayNextDueOnly;

	/** The rescheduleon overpayment. */
	private boolean rescheduleonOverpayment;

	/** The penalty per periodic arrears. */
	private boolean penaltyPerPeriodicArrears;

	/** The transfer accrue interest only. */
	private boolean transferAccrueInterestOnly;

	/** The transfer accrue interest only on schedule dates. */
	private boolean transferAccrueInterestOnlyOnScheduleDates;

	/** The insurance product ID. */
	private int insuranceProductID;

	/** The is insurance mandatory. */
	private boolean isInsuranceMandatory;

	/** The use tiered linked savings payment. */
	private boolean useTieredLinkedSavingsPayment;

	/** The number of reviews. */
	private int numberOfReviews;

	/** The loan district code required. */
	private boolean loanDistrictCodeRequired;

	/** The use fixed penalty. */
	private boolean useFixedPenalty;

	/** The use credit scoring limits. */
	private boolean useCreditScoringLimits;

	/** The interest rounding adjustment type. */
	private int interestRoundingAdjustmentType;

	/** The declined interest. */
	private boolean declinedInterest;

	/** The half balloon payments. */
	private boolean halfBalloonPayments;

	/** The prevent change account portfolio on accounts. */
	private boolean preventChangeAccountPortfolioOnAccounts;

	/** The credit control ID. */
	private int creditControlID;

	/** The credit control woff ID. */
	private int creditControlWoffID;

	/** The ignore term and issue during restructure. */
	private boolean ignoreTermAndIssueDuringRestructure;

	/** The collect linked savings payment for. */
	private int collectLinkedSavingsPaymentFor;

	/** The check percentage of loan amount in linked savings. */
	private boolean checkPercentageOfLoanAmountInLinkedSavings;

	/** The linked savings percentage of loan. */
	private double linkedSavingsPercentageOfLoan;

	/** The mandatory account activity. */
	private boolean mandatoryAccountActivity;

	/** The mandatory account activity period ID. */
	private int mandatoryAccountActivityPeriodID;

	/** The mandatory account activity period num. */
	private int mandatoryAccountActivityPeriodNum;

	/** The number of transactions. */
	private int numberOfTransactions;

	/** The mandatory savings required. */
	private boolean mandatorySavingsRequired;

	/** The mandatory savings amount. */
	private double mandatorySavingsAmount;

	/** The is separate installments in schedule. */
	private boolean isSeparateInstallmentsInSchedule;

	/** The transfer frm mandatory to linked savings. */
	private boolean transferFrmMandatoryToLinkedSavings;

	/** The calculate payment percentage based on original loan amount. */
	private boolean calculatePaymentPercentageBasedOnOriginalLoanAmount;

	/** The allow mandatory override. */
	private boolean allowMandatoryOverride;

	/** The nth installment period. */
	private int nthInstallmentPeriod;

	/** The allow top up in arrears. */
	private boolean allowTopUpInArrears;

	/** The not set same account for linked and mandatory savings. */
	private boolean notSetSameAccountForLinkedAndMandatorySavings;

	/** The fixed penalty grace periods. */
	private int fixedPenaltyGracePeriods;

	/** The balloon payment method. */
	private int balloonPaymentMethod;

	/** The effective interest rate method ID. */
	private int effectiveInterestRateMethodID;

	/** The apr calculation method ID. */
	private int aprCalculationMethodID;

	/** The allow issue more than approved amount. */
	private int allowIssueMoreThanApprovedAmount;

	/** The allow issue less than approved amount. */
	private int allowIssueLessThanApprovedAmount;

	/** The not tocollect future interest in payout and transfer. */
	private boolean notTocollectFutureInterestInPayoutAndTransfer;

	/** The product limits by customers branch. */
	private boolean productLimitsByCustomersBranch;

	/** The collect penalty by outstanding period. */
	private boolean collectPenaltyByOutstandingPeriod;

	/** The use restructered product control account. */
	private boolean useRestructeredProductControlAccount;

	/** The linked savings product ID. */
	private int linkedSavingsProductID;

	/** The loan reason requried. */
	private boolean loanReasonRequried;

	/** The is internet banking. */
	private boolean isInternetBanking;

	/** The split outstanding interest. */
	private boolean splitOutstandingInterest;

	/** The use default day count fraction by loan term. */
	private boolean useDefaultDayCountFractionByLoanTerm;

	/** The early payout fee type. */
	private int earlyPayoutFeeType;

	/** The early payout fee grace period ID. */
	private int earlyPayoutFeeGracePeriodID;

	/** The early payout fee grace period num. */
	private int earlyPayoutFeeGracePeriodNum;

	/** The provision method. */
	private int provisionMethod;

	/** The is source of fund mandatory. */
	private boolean isSourceOfFundMandatory;

	/** The allow cooling period. */
	private boolean allowCoolingPeriod;

	/** The nth cooling period. */
	private int nthCoolingPeriod;

	/** The cooling period. */
	private int coolingPeriod;

	/** The ignore closed holidays for flat loans. */
	private boolean ignoreClosedHolidaysForFlatLoans;

	/** The use guarantor type restriction. */
	private boolean useGuarantorTypeRestriction;

	/** The deduct unposted deferred fees in provision. */
	private boolean deductUnpostedDeferredFeesInProvision;

	/** The provision against. */
	private int provisionAgainst;

	/** The include interest due till the run date for the scheduled interest loans in provision. */
	private boolean includeInterestDueTillTheRunDateForTheScheduledInterestLoansInProvision;

	/** The exclude payment account balance in provision. */
	private boolean excludePaymentAccountBalanceInProvision;

	/** The allow override payout fee. */
	private int allowOverridePayoutFee;

	/** The ib product details. */
	private Object ibProductDetails;

	/** The ib deferred period option ID. */
	private int ibDeferredPeriodOptionID;

	/** The int rate. */
	private double intRate;

	/** The max rate. */
	private double maxRate;

	/** The min rate. */
	private double minRate;

	/** The rate. */
	private double rate;

	/** The default interest rate. */
	private double defaultInterestRate;

	/**
	 * Instantiates a new loan abacus API model product loan.
	 */
	public LoanAbacusAPIModelProductLoan() {

		/*
		 * Empty
		 */
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
	 * Gets the code.
	 *
	 * @return the code
	 */
	public String getCode() {

		return code;
	}

	/**
	 * Sets the code.
	 *
	 * @param code the code to set
	 */
	public void setCode(String code) {

		this.code = code;
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
	 * Checks if is cu account portfolio required.
	 *
	 * @return the cuAccountPortfolioRequired
	 */
	public boolean isCuAccountPortfolioRequired() {

		return cuAccountPortfolioRequired;
	}

	/**
	 * Sets the cu account portfolio required.
	 *
	 * @param cuAccountPortfolioRequired the cuAccountPortfolioRequired to set
	 */
	public void setCuAccountPortfolioRequired(boolean cuAccountPortfolioRequired) {

		this.cuAccountPortfolioRequired = cuAccountPortfolioRequired;
	}

	/**
	 * Gets the max accounts.
	 *
	 * @return the maxAccounts
	 */
	public int getMaxAccounts() {

		return maxAccounts;
	}

	/**
	 * Sets the max accounts.
	 *
	 * @param maxAccounts the maxAccounts to set
	 */
	public void setMaxAccounts(int maxAccounts) {

		this.maxAccounts = maxAccounts;
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
	 * Gets the min denomination.
	 *
	 * @return the minDenomination
	 */
	public double getMinDenomination() {

		return minDenomination;
	}

	/**
	 * Sets the min denomination.
	 *
	 * @param minDenomination the minDenomination to set
	 */
	public void setMinDenomination(double minDenomination) {

		this.minDenomination = minDenomination;
	}

	/**
	 * Gets the passbook printer ID.
	 *
	 * @return the passbookPrinterID
	 */
	public int getPassbookPrinterID() {

		return passbookPrinterID;
	}

	/**
	 * Sets the passbook printer ID.
	 *
	 * @param passbookPrinterID the passbookPrinterID to set
	 */
	public void setPassbookPrinterID(int passbookPrinterID) {

		this.passbookPrinterID = passbookPrinterID;
	}

	/**
	 * Checks if is allow lock account rating.
	 *
	 * @return the allowLockAccountRating
	 */
	public boolean isAllowLockAccountRating() {

		return allowLockAccountRating;
	}

	/**
	 * Sets the allow lock account rating.
	 *
	 * @param allowLockAccountRating the allowLockAccountRating to set
	 */
	public void setAllowLockAccountRating(boolean allowLockAccountRating) {

		this.allowLockAccountRating = allowLockAccountRating;
	}

	/**
	 * Gets the lock account rating period ID.
	 *
	 * @return the lockAccountRatingPeriodID
	 */
	public int getLockAccountRatingPeriodID() {

		return lockAccountRatingPeriodID;
	}

	/**
	 * Sets the lock account rating period ID.
	 *
	 * @param lockAccountRatingPeriodID the lockAccountRatingPeriodID to set
	 */
	public void setLockAccountRatingPeriodID(int lockAccountRatingPeriodID) {

		this.lockAccountRatingPeriodID = lockAccountRatingPeriodID;
	}

	/**
	 * Gets the lock account rating period num.
	 *
	 * @return the lockAccountRatingPeriodNum
	 */
	public int getLockAccountRatingPeriodNum() {

		return lockAccountRatingPeriodNum;
	}

	/**
	 * Sets the lock account rating period num.
	 *
	 * @param lockAccountRatingPeriodNum the lockAccountRatingPeriodNum to set
	 */
	public void setLockAccountRatingPeriodNum(int lockAccountRatingPeriodNum) {

		this.lockAccountRatingPeriodNum = lockAccountRatingPeriodNum;
	}

	/**
	 * Gets the minimum balance.
	 *
	 * @return the minimumBalance
	 */
	public double getMinimumBalance() {

		return minimumBalance;
	}

	/**
	 * Sets the minimum balance.
	 *
	 * @param minimumBalance the minimumBalance to set
	 */
	public void setMinimumBalance(double minimumBalance) {

		this.minimumBalance = minimumBalance;
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
	 * Gets the maximum balance.
	 *
	 * @return the maximumBalance
	 */
	public double getMaximumBalance() {

		return maximumBalance;
	}

	/**
	 * Sets the maximum balance.
	 *
	 * @param maximumBalance the maximumBalance to set
	 */
	public void setMaximumBalance(double maximumBalance) {

		this.maximumBalance = maximumBalance;
	}

	/**
	 * Gets the minimum cheque.
	 *
	 * @return the minimumCheque
	 */
	public double getMinimumCheque() {

		return minimumCheque;
	}

	/**
	 * Sets the minimum cheque.
	 *
	 * @param minimumCheque the minimumCheque to set
	 */
	public void setMinimumCheque(double minimumCheque) {

		this.minimumCheque = minimumCheque;
	}

	/**
	 * Gets the maximum cheque.
	 *
	 * @return the maximumCheque
	 */
	public double getMaximumCheque() {

		return maximumCheque;
	}

	/**
	 * Sets the maximum cheque.
	 *
	 * @param maximumCheque the maximumCheque to set
	 */
	public void setMaximumCheque(double maximumCheque) {

		this.maximumCheque = maximumCheque;
	}

	/**
	 * Gets the default cheque.
	 *
	 * @return the defaultCheque
	 */
	public double getDefaultCheque() {

		return defaultCheque;
	}

	/**
	 * Sets the default cheque.
	 *
	 * @param defaultCheque the defaultCheque to set
	 */
	public void setDefaultCheque(double defaultCheque) {

		this.defaultCheque = defaultCheque;
	}

	/**
	 * Gets the minimum term.
	 *
	 * @return the minimumTerm
	 */
	public int getMinimumTerm() {

		return minimumTerm;
	}

	/**
	 * Sets the minimum term.
	 *
	 * @param minimumTerm the minimumTerm to set
	 */
	public void setMinimumTerm(int minimumTerm) {

		this.minimumTerm = minimumTerm;
	}

	/**
	 * Gets the default term.
	 *
	 * @return the defaultTerm
	 */
	public int getDefaultTerm() {

		return defaultTerm;
	}

	/**
	 * Sets the default term.
	 *
	 * @param defaultTerm the defaultTerm to set
	 */
	public void setDefaultTerm(int defaultTerm) {

		this.defaultTerm = defaultTerm;
	}

	/**
	 * Gets the default term period ID.
	 *
	 * @return the defaultTermPeriodID
	 */
	public int getDefaultTermPeriodID() {

		return defaultTermPeriodID;
	}

	/**
	 * Sets the default term period ID.
	 *
	 * @param defaultTermPeriodID the defaultTermPeriodID to set
	 */
	public void setDefaultTermPeriodID(int defaultTermPeriodID) {

		this.defaultTermPeriodID = defaultTermPeriodID;
	}

	/**
	 * Gets the maximum term.
	 *
	 * @return the maximumTerm
	 */
	public int getMaximumTerm() {

		return maximumTerm;
	}

	/**
	 * Sets the maximum term.
	 *
	 * @param maximumTerm the maximumTerm to set
	 */
	public void setMaximumTerm(int maximumTerm) {

		this.maximumTerm = maximumTerm;
	}

	/**
	 * Gets the maximum member age.
	 *
	 * @return the maximumMemberAge
	 */
	public int getMaximumMemberAge() {

		return maximumMemberAge;
	}

	/**
	 * Sets the maximum member age.
	 *
	 * @param maximumMemberAge the maximumMemberAge to set
	 */
	public void setMaximumMemberAge(int maximumMemberAge) {

		this.maximumMemberAge = maximumMemberAge;
	}

	/**
	 * Gets the minimum member age.
	 *
	 * @return the minimumMemberAge
	 */
	public int getMinimumMemberAge() {

		return minimumMemberAge;
	}

	/**
	 * Sets the minimum member age.
	 *
	 * @param minimumMemberAge the minimumMemberAge to set
	 */
	public void setMinimumMemberAge(int minimumMemberAge) {

		this.minimumMemberAge = minimumMemberAge;
	}

	/**
	 * Gets the interest charge date.
	 *
	 * @return the interestChargeDate
	 */
	public Date getInterestChargeDate() {

		return interestChargeDate;
	}

	/**
	 * Sets the interest charge date.
	 *
	 * @param interestChargeDate the interestChargeDate to set
	 */
	public void setInterestChargeDate(Date interestChargeDate) {

		this.interestChargeDate = interestChargeDate;
	}

	/**
	 * Checks if is charge off.
	 *
	 * @return the chargeOff
	 */
	public boolean isChargeOff() {

		return chargeOff;
	}

	/**
	 * Sets the charge off.
	 *
	 * @param chargeOff the chargeOff to set
	 */
	public void setChargeOff(boolean chargeOff) {

		this.chargeOff = chargeOff;
	}

	/**
	 * Checks if is recover interest.
	 *
	 * @return the recoverInterest
	 */
	public boolean isRecoverInterest() {

		return recoverInterest;
	}

	/**
	 * Sets the recover interest.
	 *
	 * @param recoverInterest the recoverInterest to set
	 */
	public void setRecoverInterest(boolean recoverInterest) {

		this.recoverInterest = recoverInterest;
	}

	/**
	 * Checks if is keep collecting interest.
	 *
	 * @return the keepCollectingInterest
	 */
	public boolean isKeepCollectingInterest() {

		return keepCollectingInterest;
	}

	/**
	 * Sets the keep collecting interest.
	 *
	 * @param keepCollectingInterest the keepCollectingInterest to set
	 */
	public void setKeepCollectingInterest(boolean keepCollectingInterest) {

		this.keepCollectingInterest = keepCollectingInterest;
	}

	/**
	 * Checks if is use default rate.
	 *
	 * @return the useDefaultRate
	 */
	public boolean isUseDefaultRate() {

		return useDefaultRate;
	}

	/**
	 * Sets the use default rate.
	 *
	 * @param useDefaultRate the useDefaultRate to set
	 */
	public void setUseDefaultRate(boolean useDefaultRate) {

		this.useDefaultRate = useDefaultRate;
	}

	/**
	 * Gets the penalty interest amount type.
	 *
	 * @return the penaltyInterestAmountType
	 */
	public int getPenaltyInterestAmountType() {

		return penaltyInterestAmountType;
	}

	/**
	 * Sets the penalty interest amount type.
	 *
	 * @param penaltyInterestAmountType the penaltyInterestAmountType to set
	 */
	public void setPenaltyInterestAmountType(int penaltyInterestAmountType) {

		this.penaltyInterestAmountType = penaltyInterestAmountType;
	}

	/**
	 * Gets the penalty interest rate.
	 *
	 * @return the penaltyInterestRate
	 */
	public double getPenaltyInterestRate() {

		return penaltyInterestRate;
	}

	/**
	 * Sets the penalty interest rate.
	 *
	 * @param penaltyInterestRate the penaltyInterestRate to set
	 */
	public void setPenaltyInterestRate(double penaltyInterestRate) {

		this.penaltyInterestRate = penaltyInterestRate;
	}

	/**
	 * Gets the penalty interest flat amount.
	 *
	 * @return the penaltyInterestFlatAmount
	 */
	public double getPenaltyInterestFlatAmount() {

		return penaltyInterestFlatAmount;
	}

	/**
	 * Sets the penalty interest flat amount.
	 *
	 * @param penaltyInterestFlatAmount the penaltyInterestFlatAmount to set
	 */
	public void setPenaltyInterestFlatAmount(double penaltyInterestFlatAmount) {

		this.penaltyInterestFlatAmount = penaltyInterestFlatAmount;
	}

	/**
	 * Gets the penalty interest fee ID.
	 *
	 * @return the penaltyInterestFeeID
	 */
	public int getPenaltyInterestFeeID() {

		return penaltyInterestFeeID;
	}

	/**
	 * Sets the penalty interest fee ID.
	 *
	 * @param penaltyInterestFeeID the penaltyInterestFeeID to set
	 */
	public void setPenaltyInterestFeeID(int penaltyInterestFeeID) {

		this.penaltyInterestFeeID = penaltyInterestFeeID;
	}

	/**
	 * Gets the penalty int grace period.
	 *
	 * @return the penaltyIntGracePeriod
	 */
	public int getPenaltyIntGracePeriod() {

		return penaltyIntGracePeriod;
	}

	/**
	 * Sets the penalty int grace period.
	 *
	 * @param penaltyIntGracePeriod the penaltyIntGracePeriod to set
	 */
	public void setPenaltyIntGracePeriod(int penaltyIntGracePeriod) {

		this.penaltyIntGracePeriod = penaltyIntGracePeriod;
	}

	/**
	 * Checks if is multiply penalty interest by number of group members.
	 *
	 * @return the multiplyPenaltyInterestByNumberOfGroupMembers
	 */
	public boolean isMultiplyPenaltyInterestByNumberOfGroupMembers() {

		return multiplyPenaltyInterestByNumberOfGroupMembers;
	}

	/**
	 * Sets the multiply penalty interest by number of group members.
	 *
	 * @param multiplyPenaltyInterestByNumberOfGroupMembers the
	 *        multiplyPenaltyInterestByNumberOfGroupMembers to set
	 */
	public void setMultiplyPenaltyInterestByNumberOfGroupMembers(
			boolean multiplyPenaltyInterestByNumberOfGroupMembers) {

		this.multiplyPenaltyInterestByNumberOfGroupMembers =
				multiplyPenaltyInterestByNumberOfGroupMembers;
	}

	/**
	 * Gets the flat interest method.
	 *
	 * @return the flatInterestMethod
	 */
	public int getFlatInterestMethod() {

		return flatInterestMethod;
	}

	/**
	 * Sets the flat interest method.
	 *
	 * @param flatInterestMethod the flatInterestMethod to set
	 */
	public void setFlatInterestMethod(int flatInterestMethod) {

		this.flatInterestMethod = flatInterestMethod;
	}

	/**
	 * Checks if is pre pay interest.
	 *
	 * @return the prePayInterest
	 */
	public boolean isPrePayInterest() {

		return prePayInterest;
	}

	/**
	 * Sets the pre pay interest.
	 *
	 * @param prePayInterest the prePayInterest to set
	 */
	public void setPrePayInterest(boolean prePayInterest) {

		this.prePayInterest = prePayInterest;
	}

	/**
	 * Gets the rebate rate.
	 *
	 * @return the rebateRate
	 */
	public double getRebateRate() {

		return rebateRate;
	}

	/**
	 * Sets the rebate rate.
	 *
	 * @param rebateRate the rebateRate to set
	 */
	public void setRebateRate(double rebateRate) {

		this.rebateRate = rebateRate;
	}

	/**
	 * Gets the rebate trn message.
	 *
	 * @return the rebateTrnMessage
	 */
	public String getRebateTrnMessage() {

		return rebateTrnMessage;
	}

	/**
	 * Sets the rebate trn message.
	 *
	 * @param rebateTrnMessage the rebateTrnMessage to set
	 */
	public void setRebateTrnMessage(String rebateTrnMessage) {

		this.rebateTrnMessage = rebateTrnMessage;
	}

	/**
	 * Gets the rebate next payment date.
	 *
	 * @return the rebateNextPaymentDate
	 */
	public Date getRebateNextPaymentDate() {

		return rebateNextPaymentDate;
	}

	/**
	 * Sets the rebate next payment date.
	 *
	 * @param rebateNextPaymentDate the rebateNextPaymentDate to set
	 */
	public void setRebateNextPaymentDate(Date rebateNextPaymentDate) {

		this.rebateNextPaymentDate = rebateNextPaymentDate;
	}

	/**
	 * Gets the rebate pay method.
	 *
	 * @return the rebatePayMethod
	 */
	public int getRebatePayMethod() {

		return rebatePayMethod;
	}

	/**
	 * Sets the rebate pay method.
	 *
	 * @param rebatePayMethod the rebatePayMethod to set
	 */
	public void setRebatePayMethod(int rebatePayMethod) {

		this.rebatePayMethod = rebatePayMethod;
	}

	/**
	 * Gets the rebate trn start date.
	 *
	 * @return the rebateTrnStartDate
	 */
	public Date getRebateTrnStartDate() {

		return rebateTrnStartDate;
	}

	/**
	 * Sets the rebate trn start date.
	 *
	 * @param rebateTrnStartDate the rebateTrnStartDate to set
	 */
	public void setRebateTrnStartDate(Date rebateTrnStartDate) {

		this.rebateTrnStartDate = rebateTrnStartDate;
	}

	/**
	 * Gets the rebate trn end date.
	 *
	 * @return the rebateTrnEndDate
	 */
	public Date getRebateTrnEndDate() {

		return rebateTrnEndDate;
	}

	/**
	 * Sets the rebate trn end date.
	 *
	 * @param rebateTrnEndDate the rebateTrnEndDate to set
	 */
	public void setRebateTrnEndDate(Date rebateTrnEndDate) {

		this.rebateTrnEndDate = rebateTrnEndDate;
	}

	/**
	 * Checks if is annuities ORD.
	 *
	 * @return the annuitiesORD
	 */
	public boolean isAnnuitiesORD() {

		return annuitiesORD;
	}

	/**
	 * Sets the annuities ORD.
	 *
	 * @param annuitiesORD the annuitiesORD to set
	 */
	public void setAnnuitiesORD(boolean annuitiesORD) {

		this.annuitiesORD = annuitiesORD;
	}

	/**
	 * Checks if is compounding.
	 *
	 * @return the compounding
	 */
	public boolean isCompounding() {

		return compounding;
	}

	/**
	 * Sets the compounding.
	 *
	 * @param compounding the compounding to set
	 */
	public void setCompounding(boolean compounding) {

		this.compounding = compounding;
	}

	/**
	 * Gets the compound period num.
	 *
	 * @return the compoundPeriodNum
	 */
	public int getCompoundPeriodNum() {

		return compoundPeriodNum;
	}

	/**
	 * Sets the compound period num.
	 *
	 * @param compoundPeriodNum the compoundPeriodNum to set
	 */
	public void setCompoundPeriodNum(int compoundPeriodNum) {

		this.compoundPeriodNum = compoundPeriodNum;
	}

	/**
	 * Gets the compound period ID.
	 *
	 * @return the compoundPeriodID
	 */
	public int getCompoundPeriodID() {

		return compoundPeriodID;
	}

	/**
	 * Sets the compound period ID.
	 *
	 * @param compoundPeriodID the compoundPeriodID to set
	 */
	public void setCompoundPeriodID(int compoundPeriodID) {

		this.compoundPeriodID = compoundPeriodID;
	}

	/**
	 * Checks if is allow top up.
	 *
	 * @return the allowTopUp
	 */
	public boolean isAllowTopUp() {

		return allowTopUp;
	}

	/**
	 * Sets the allow top up.
	 *
	 * @param allowTopUp the allowTopUp to set
	 */
	public void setAllowTopUp(boolean allowTopUp) {

		this.allowTopUp = allowTopUp;
	}

	/**
	 * Checks if is allow parts.
	 *
	 * @return the allowParts
	 */
	public boolean isAllowParts() {

		return allowParts;
	}

	/**
	 * Sets the allow parts.
	 *
	 * @param allowParts the allowParts to set
	 */
	public void setAllowParts(boolean allowParts) {

		this.allowParts = allowParts;
	}

	/**
	 * Checks if is allow redraw.
	 *
	 * @return the allowRedraw
	 */
	public boolean isAllowRedraw() {

		return allowRedraw;
	}

	/**
	 * Sets the allow redraw.
	 *
	 * @param allowRedraw the allowRedraw to set
	 */
	public void setAllowRedraw(boolean allowRedraw) {

		this.allowRedraw = allowRedraw;
	}

	/**
	 * Checks if is allow balloon payments.
	 *
	 * @return the allowBalloonPayments
	 */
	public boolean isAllowBalloonPayments() {

		return allowBalloonPayments;
	}

	/**
	 * Sets the allow balloon payments.
	 *
	 * @param allowBalloonPayments the allowBalloonPayments to set
	 */
	public void setAllowBalloonPayments(boolean allowBalloonPayments) {

		this.allowBalloonPayments = allowBalloonPayments;
	}

	/**
	 * Checks if is allow single balloon payment only.
	 *
	 * @return the allowSingleBalloonPaymentOnly
	 */
	public boolean isAllowSingleBalloonPaymentOnly() {

		return allowSingleBalloonPaymentOnly;
	}

	/**
	 * Sets the allow single balloon payment only.
	 *
	 * @param allowSingleBalloonPaymentOnly the allowSingleBalloonPaymentOnly to set
	 */
	public void setAllowSingleBalloonPaymentOnly(boolean allowSingleBalloonPaymentOnly) {

		this.allowSingleBalloonPaymentOnly = allowSingleBalloonPaymentOnly;
	}

	/**
	 * Checks if is allow deferred payments.
	 *
	 * @return the allowDeferredPayments
	 */
	public boolean isAllowDeferredPayments() {

		return allowDeferredPayments;
	}

	/**
	 * Sets the allow deferred payments.
	 *
	 * @param allowDeferredPayments the allowDeferredPayments to set
	 */
	public void setAllowDeferredPayments(boolean allowDeferredPayments) {

		this.allowDeferredPayments = allowDeferredPayments;
	}

	/**
	 * Checks if is allow transfer from savings.
	 *
	 * @return the allowTransferFromSavings
	 */
	public boolean isAllowTransferFromSavings() {

		return allowTransferFromSavings;
	}

	/**
	 * Sets the allow transfer from savings.
	 *
	 * @param allowTransferFromSavings the allowTransferFromSavings to set
	 */
	public void setAllowTransferFromSavings(boolean allowTransferFromSavings) {

		this.allowTransferFromSavings = allowTransferFromSavings;
	}

	/**
	 * Checks if is default to single scheduled payment.
	 *
	 * @return the defaultToSingleScheduledPayment
	 */
	public boolean isDefaultToSingleScheduledPayment() {

		return defaultToSingleScheduledPayment;
	}

	/**
	 * Sets the default to single scheduled payment.
	 *
	 * @param defaultToSingleScheduledPayment the defaultToSingleScheduledPayment to set
	 */
	public void setDefaultToSingleScheduledPayment(boolean defaultToSingleScheduledPayment) {

		this.defaultToSingleScheduledPayment = defaultToSingleScheduledPayment;
	}

	/**
	 * Gets the issue fee ID.
	 *
	 * @return the issueFeeID
	 */
	public int getIssueFeeID() {

		return issueFeeID;
	}

	/**
	 * Sets the issue fee ID.
	 *
	 * @param issueFeeID the issueFeeID to set
	 */
	public void setIssueFeeID(int issueFeeID) {

		this.issueFeeID = issueFeeID;
	}

	/**
	 * Checks if is charge issue fee.
	 *
	 * @return the chargeIssueFee
	 */
	public boolean isChargeIssueFee() {

		return chargeIssueFee;
	}

	/**
	 * Sets the charge issue fee.
	 *
	 * @param chargeIssueFee the chargeIssueFee to set
	 */
	public void setChargeIssueFee(boolean chargeIssueFee) {

		this.chargeIssueFee = chargeIssueFee;
	}

	/**
	 * Gets the issue fee ID 1.
	 *
	 * @return the issueFeeID1
	 */
	public int getIssueFeeID1() {

		return issueFeeID1;
	}

	/**
	 * Sets the issue fee ID 1.
	 *
	 * @param issueFeeID1 the issueFeeID1 to set
	 */
	public void setIssueFeeID1(int issueFeeID1) {

		this.issueFeeID1 = issueFeeID1;
	}

	/**
	 * Checks if is issue fee calculated first time only.
	 *
	 * @return the isIssueFeeCalculatedFirstTimeOnly
	 */
	public boolean isIssueFeeCalculatedFirstTimeOnly() {

		return isIssueFeeCalculatedFirstTimeOnly;
	}

	/**
	 * Sets the issue fee calculated first time only.
	 *
	 * @param isIssueFeeCalculatedFirstTimeOnly the isIssueFeeCalculatedFirstTimeOnly to set
	 */
	public void setIssueFeeCalculatedFirstTimeOnly(boolean isIssueFeeCalculatedFirstTimeOnly) {

		this.isIssueFeeCalculatedFirstTimeOnly = isIssueFeeCalculatedFirstTimeOnly;
	}

	/**
	 * Checks if is issue fee calculated on approval amount.
	 *
	 * @return the isIssueFeeCalculatedOnApprovalAmount
	 */
	public boolean isIssueFeeCalculatedOnApprovalAmount() {

		return isIssueFeeCalculatedOnApprovalAmount;
	}

	/**
	 * Sets the issue fee calculated on approval amount.
	 *
	 * @param isIssueFeeCalculatedOnApprovalAmount the isIssueFeeCalculatedOnApprovalAmount to set
	 */
	public void setIssueFeeCalculatedOnApprovalAmount(
			boolean isIssueFeeCalculatedOnApprovalAmount) {

		this.isIssueFeeCalculatedOnApprovalAmount = isIssueFeeCalculatedOnApprovalAmount;
	}

	/**
	 * Gets the issue fee age limits apply to ID.
	 *
	 * @return the issueFeeAgeLimitsApplyToID
	 */
	public int getIssueFeeAgeLimitsApplyToID() {

		return issueFeeAgeLimitsApplyToID;
	}

	/**
	 * Sets the issue fee age limits apply to ID.
	 *
	 * @param issueFeeAgeLimitsApplyToID the issueFeeAgeLimitsApplyToID to set
	 */
	public void setIssueFeeAgeLimitsApplyToID(int issueFeeAgeLimitsApplyToID) {

		this.issueFeeAgeLimitsApplyToID = issueFeeAgeLimitsApplyToID;
	}

	/**
	 * Gets the issue fee age minimum.
	 *
	 * @return the issueFeeAgeMinimum
	 */
	public int getIssueFeeAgeMinimum() {

		return issueFeeAgeMinimum;
	}

	/**
	 * Sets the issue fee age minimum.
	 *
	 * @param issueFeeAgeMinimum the issueFeeAgeMinimum to set
	 */
	public void setIssueFeeAgeMinimum(int issueFeeAgeMinimum) {

		this.issueFeeAgeMinimum = issueFeeAgeMinimum;
	}

	/**
	 * Gets the issue fee age maximum.
	 *
	 * @return the issueFeeAgeMaximum
	 */
	public int getIssueFeeAgeMaximum() {

		return issueFeeAgeMaximum;
	}

	/**
	 * Sets the issue fee age maximum.
	 *
	 * @param issueFeeAgeMaximum the issueFeeAgeMaximum to set
	 */
	public void setIssueFeeAgeMaximum(int issueFeeAgeMaximum) {

		this.issueFeeAgeMaximum = issueFeeAgeMaximum;
	}

	/**
	 * Gets the discharge fee ID.
	 *
	 * @return the dischargeFeeID
	 */
	public int getDischargeFeeID() {

		return dischargeFeeID;
	}

	/**
	 * Sets the discharge fee ID.
	 *
	 * @param dischargeFeeID the dischargeFeeID to set
	 */
	public void setDischargeFeeID(int dischargeFeeID) {

		this.dischargeFeeID = dischargeFeeID;
	}

	/**
	 * Gets the additional amount charged in discharge fee.
	 *
	 * @return the additionalAmountChargedInDischargeFee
	 */
	public double getAdditionalAmountChargedInDischargeFee() {

		return additionalAmountChargedInDischargeFee;
	}

	/**
	 * Sets the additional amount charged in discharge fee.
	 *
	 * @param additionalAmountChargedInDischargeFee the additionalAmountChargedInDischargeFee to set
	 */
	public void setAdditionalAmountChargedInDischargeFee(
			double additionalAmountChargedInDischargeFee) {

		this.additionalAmountChargedInDischargeFee = additionalAmountChargedInDischargeFee;
	}

	/**
	 * Gets the early discharge fee threshold in months.
	 *
	 * @return the earlyDischargeFeeThresholdInMonths
	 */
	public int getEarlyDischargeFeeThresholdInMonths() {

		return earlyDischargeFeeThresholdInMonths;
	}

	/**
	 * Sets the early discharge fee threshold in months.
	 *
	 * @param earlyDischargeFeeThresholdInMonths the earlyDischargeFeeThresholdInMonths to set
	 */
	public void setEarlyDischargeFeeThresholdInMonths(int earlyDischargeFeeThresholdInMonths) {

		this.earlyDischargeFeeThresholdInMonths = earlyDischargeFeeThresholdInMonths;
	}

	/**
	 * Gets the early discharge fee number of days to charge.
	 *
	 * @return the earlyDischargeFeeNumberOfDaysToCharge
	 */
	public int getEarlyDischargeFeeNumberOfDaysToCharge() {

		return earlyDischargeFeeNumberOfDaysToCharge;
	}

	/**
	 * Sets the early discharge fee number of days to charge.
	 *
	 * @param earlyDischargeFeeNumberOfDaysToCharge the earlyDischargeFeeNumberOfDaysToCharge to set
	 */
	public void setEarlyDischargeFeeNumberOfDaysToCharge(
			int earlyDischargeFeeNumberOfDaysToCharge) {

		this.earlyDischargeFeeNumberOfDaysToCharge = earlyDischargeFeeNumberOfDaysToCharge;
	}

	/**
	 * Checks if is devaluate loans.
	 *
	 * @return the devaluateLoans
	 */
	public boolean isDevaluateLoans() {

		return devaluateLoans;
	}

	/**
	 * Sets the devaluate loans.
	 *
	 * @param devaluateLoans the devaluateLoans to set
	 */
	public void setDevaluateLoans(boolean devaluateLoans) {

		this.devaluateLoans = devaluateLoans;
	}

	/**
	 * Checks if is devaluation global.
	 *
	 * @return the devaluationGlobal
	 */
	public boolean isDevaluationGlobal() {

		return devaluationGlobal;
	}

	/**
	 * Sets the devaluation global.
	 *
	 * @param devaluationGlobal the devaluationGlobal to set
	 */
	public void setDevaluationGlobal(boolean devaluationGlobal) {

		this.devaluationGlobal = devaluationGlobal;
	}

	/**
	 * Gets the devaluation method.
	 *
	 * @return the devaluationMethod
	 */
	public int getDevaluationMethod() {

		return devaluationMethod;
	}

	/**
	 * Sets the devaluation method.
	 *
	 * @param devaluationMethod the devaluationMethod to set
	 */
	public void setDevaluationMethod(int devaluationMethod) {

		this.devaluationMethod = devaluationMethod;
	}

	/**
	 * Gets the devalue fee ID.
	 *
	 * @return the devalueFeeID
	 */
	public int getDevalueFeeID() {

		return devalueFeeID;
	}

	/**
	 * Sets the devalue fee ID.
	 *
	 * @param devalueFeeID the devalueFeeID to set
	 */
	public void setDevalueFeeID(int devalueFeeID) {

		this.devalueFeeID = devalueFeeID;
	}

	/**
	 * Gets the devalue rate.
	 *
	 * @return the devalueRate
	 */
	public double getDevalueRate() {

		return devalueRate;
	}

	/**
	 * Sets the devalue rate.
	 *
	 * @param devalueRate the devalueRate to set
	 */
	public void setDevalueRate(double devalueRate) {

		this.devalueRate = devalueRate;
	}

	/**
	 * Gets the maximum deferred period.
	 *
	 * @return the maximumDeferredPeriod
	 */
	public int getMaximumDeferredPeriod() {

		return maximumDeferredPeriod;
	}

	/**
	 * Sets the maximum deferred period.
	 *
	 * @param maximumDeferredPeriod the maximumDeferredPeriod to set
	 */
	public void setMaximumDeferredPeriod(int maximumDeferredPeriod) {

		this.maximumDeferredPeriod = maximumDeferredPeriod;
	}

	/**
	 * Gets the minimum deferred period.
	 *
	 * @return the minimumDeferredPeriod
	 */
	public int getMinimumDeferredPeriod() {

		return minimumDeferredPeriod;
	}

	/**
	 * Sets the minimum deferred period.
	 *
	 * @param minimumDeferredPeriod the minimumDeferredPeriod to set
	 */
	public void setMinimumDeferredPeriod(int minimumDeferredPeriod) {

		this.minimumDeferredPeriod = minimumDeferredPeriod;
	}

	/**
	 * Gets the repayment frequency periods.
	 *
	 * @return the repaymentFrequencyPeriods
	 */
	public int getRepaymentFrequencyPeriods() {

		return repaymentFrequencyPeriods;
	}

	/**
	 * Sets the repayment frequency periods.
	 *
	 * @param repaymentFrequencyPeriods the repaymentFrequencyPeriods to set
	 */
	public void setRepaymentFrequencyPeriods(int repaymentFrequencyPeriods) {

		this.repaymentFrequencyPeriods = repaymentFrequencyPeriods;
	}

	/**
	 * Checks if is exclude due from arrears.
	 *
	 * @return the excludeDueFromArrears
	 */
	public boolean isExcludeDueFromArrears() {

		return excludeDueFromArrears;
	}

	/**
	 * Sets the exclude due from arrears.
	 *
	 * @param excludeDueFromArrears the excludeDueFromArrears to set
	 */
	public void setExcludeDueFromArrears(boolean excludeDueFromArrears) {

		this.excludeDueFromArrears = excludeDueFromArrears;
	}

	/**
	 * Gets the uds fee ID.
	 *
	 * @return the udsFeeID
	 */
	public int getUdsFeeID() {

		return udsFeeID;
	}

	/**
	 * Sets the uds fee ID.
	 *
	 * @param udsFeeID the udsFeeID to set
	 */
	public void setUdsFeeID(int udsFeeID) {

		this.udsFeeID = udsFeeID;
	}

	/**
	 * Gets the service fee ID.
	 *
	 * @return the serviceFeeID
	 */
	public int getServiceFeeID() {

		return serviceFeeID;
	}

	/**
	 * Sets the service fee ID.
	 *
	 * @param serviceFeeID the serviceFeeID to set
	 */
	public void setServiceFeeID(int serviceFeeID) {

		this.serviceFeeID = serviceFeeID;
	}

	/**
	 * Gets the service fee period num.
	 *
	 * @return the serviceFeePeriodNum
	 */
	public int getServiceFeePeriodNum() {

		return serviceFeePeriodNum;
	}

	/**
	 * Sets the service fee period num.
	 *
	 * @param serviceFeePeriodNum the serviceFeePeriodNum to set
	 */
	public void setServiceFeePeriodNum(int serviceFeePeriodNum) {

		this.serviceFeePeriodNum = serviceFeePeriodNum;
	}

	/**
	 * Gets the service fee period ID.
	 *
	 * @return the serviceFeePeriodID
	 */
	public int getServiceFeePeriodID() {

		return serviceFeePeriodID;
	}

	/**
	 * Sets the service fee period ID.
	 *
	 * @param serviceFeePeriodID the serviceFeePeriodID to set
	 */
	public void setServiceFeePeriodID(int serviceFeePeriodID) {

		this.serviceFeePeriodID = serviceFeePeriodID;
	}

	/**
	 * Checks if is use peak debt.
	 *
	 * @return the usePeakDebt
	 */
	public boolean isUsePeakDebt() {

		return usePeakDebt;
	}

	/**
	 * Sets the use peak debt.
	 *
	 * @param usePeakDebt the usePeakDebt to set
	 */
	public void setUsePeakDebt(boolean usePeakDebt) {

		this.usePeakDebt = usePeakDebt;
	}

	/**
	 * Gets the peak debt method.
	 *
	 * @return the peakDebtMethod
	 */
	public int getPeakDebtMethod() {

		return peakDebtMethod;
	}

	/**
	 * Sets the peak debt method.
	 *
	 * @param peakDebtMethod the peakDebtMethod to set
	 */
	public void setPeakDebtMethod(int peakDebtMethod) {

		this.peakDebtMethod = peakDebtMethod;
	}

	/**
	 * Gets the service fees date.
	 *
	 * @return the serviceFeesDate
	 */
	public Date getServiceFeesDate() {

		return serviceFeesDate;
	}

	/**
	 * Sets the service fees date.
	 *
	 * @param serviceFeesDate the serviceFeesDate to set
	 */
	public void setServiceFeesDate(Date serviceFeesDate) {

		this.serviceFeesDate = serviceFeesDate;
	}

	/**
	 * Checks if is requires review.
	 *
	 * @return the requiresReview
	 */
	public boolean isRequiresReview() {

		return requiresReview;
	}

	/**
	 * Sets the requires review.
	 *
	 * @param requiresReview the requiresReview to set
	 */
	public void setRequiresReview(boolean requiresReview) {

		this.requiresReview = requiresReview;
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
	 * Gets the review period ID.
	 *
	 * @return the reviewPeriodID
	 */
	public int getReviewPeriodID() {

		return reviewPeriodID;
	}

	/**
	 * Sets the review period ID.
	 *
	 * @param reviewPeriodID the reviewPeriodID to set
	 */
	public void setReviewPeriodID(int reviewPeriodID) {

		this.reviewPeriodID = reviewPeriodID;
	}

	/**
	 * Gets the min savings.
	 *
	 * @return the minSavings
	 */
	public int getMinSavings() {

		return minSavings;
	}

	/**
	 * Sets the min savings.
	 *
	 * @param minSavings the minSavings to set
	 */
	public void setMinSavings(int minSavings) {

		this.minSavings = minSavings;
	}

	/**
	 * Gets the min collateral.
	 *
	 * @return the minCollateral
	 */
	public int getMinCollateral() {

		return minCollateral;
	}

	/**
	 * Sets the min collateral.
	 *
	 * @param minCollateral the minCollateral to set
	 */
	public void setMinCollateral(int minCollateral) {

		this.minCollateral = minCollateral;
	}

	/**
	 * Gets the max collateral amount.
	 *
	 * @return the maxCollateralAmount
	 */
	public double getMaxCollateralAmount() {

		return maxCollateralAmount;
	}

	/**
	 * Sets the max collateral amount.
	 *
	 * @param maxCollateralAmount the maxCollateralAmount to set
	 */
	public void setMaxCollateralAmount(double maxCollateralAmount) {

		this.maxCollateralAmount = maxCollateralAmount;
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
	 * Checks if is group loans only.
	 *
	 * @return the groupLoansOnly
	 */
	public boolean isGroupLoansOnly() {

		return groupLoansOnly;
	}

	/**
	 * Sets the group loans only.
	 *
	 * @param groupLoansOnly the groupLoansOnly to set
	 */
	public void setGroupLoansOnly(boolean groupLoansOnly) {

		this.groupLoansOnly = groupLoansOnly;
	}

	/**
	 * Gets the min group size.
	 *
	 * @return the minGroupSize
	 */
	public int getMinGroupSize() {

		return minGroupSize;
	}

	/**
	 * Sets the min group size.
	 *
	 * @param minGroupSize the minGroupSize to set
	 */
	public void setMinGroupSize(int minGroupSize) {

		this.minGroupSize = minGroupSize;
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
	 * Gets the round down.
	 *
	 * @return the roundDown
	 */
	public int getRoundDown() {

		return roundDown;
	}

	/**
	 * Sets the round down.
	 *
	 * @param roundDown the roundDown to set
	 */
	public void setRoundDown(int roundDown) {

		this.roundDown = roundDown;
	}

	/**
	 * Checks if is increment days.
	 *
	 * @return the incrementDays
	 */
	public boolean isIncrementDays() {

		return incrementDays;
	}

	/**
	 * Sets the increment days.
	 *
	 * @param incrementDays the incrementDays to set
	 */
	public void setIncrementDays(boolean incrementDays) {

		this.incrementDays = incrementDays;
	}

	/**
	 * Checks if is accrue interest daily.
	 *
	 * @return the accrueInterestDaily
	 */
	public boolean isAccrueInterestDaily() {

		return accrueInterestDaily;
	}

	/**
	 * Sets the accrue interest daily.
	 *
	 * @param accrueInterestDaily the accrueInterestDaily to set
	 */
	public void setAccrueInterestDaily(boolean accrueInterestDaily) {

		this.accrueInterestDaily = accrueInterestDaily;
	}

	/**
	 * Gets the repayment method.
	 *
	 * @return the repaymentMethod
	 */
	public int getRepaymentMethod() {

		return repaymentMethod;
	}

	/**
	 * Sets the repayment method.
	 *
	 * @param repaymentMethod the repaymentMethod to set
	 */
	public void setRepaymentMethod(int repaymentMethod) {

		this.repaymentMethod = repaymentMethod;
	}

	/**
	 * Gets the due today format.
	 *
	 * @return the dueTodayFormat
	 */
	public String getDueTodayFormat() {

		return dueTodayFormat;
	}

	/**
	 * Sets the due today format.
	 *
	 * @param dueTodayFormat the dueTodayFormat to set
	 */
	public void setDueTodayFormat(String dueTodayFormat) {

		this.dueTodayFormat = dueTodayFormat;
	}

	/**
	 * Gets the arrears format.
	 *
	 * @return the arrearsFormat
	 */
	public String getArrearsFormat() {

		return arrearsFormat;
	}

	/**
	 * Sets the arrears format.
	 *
	 * @param arrearsFormat the arrearsFormat to set
	 */
	public void setArrearsFormat(String arrearsFormat) {

		this.arrearsFormat = arrearsFormat;
	}

	/**
	 * Gets the pre paid format.
	 *
	 * @return the prePaidFormat
	 */
	public String getPrePaidFormat() {

		return prePaidFormat;
	}

	/**
	 * Sets the pre paid format.
	 *
	 * @param prePaidFormat the prePaidFormat to set
	 */
	public void setPrePaidFormat(String prePaidFormat) {

		this.prePaidFormat = prePaidFormat;
	}

	/**
	 * Checks if is complimentary interest.
	 *
	 * @return the complimentaryInterest
	 */
	public boolean isComplimentaryInterest() {

		return complimentaryInterest;
	}

	/**
	 * Sets the complimentary interest.
	 *
	 * @param complimentaryInterest the complimentaryInterest to set
	 */
	public void setComplimentaryInterest(boolean complimentaryInterest) {

		this.complimentaryInterest = complimentaryInterest;
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
	 * Gets the day count default.
	 *
	 * @return the dayCountDefault
	 */
	public int getDayCountDefault() {

		return dayCountDefault;
	}

	/**
	 * Sets the day count default.
	 *
	 * @param dayCountDefault the dayCountDefault to set
	 */
	public void setDayCountDefault(int dayCountDefault) {

		this.dayCountDefault = dayCountDefault;
	}

	/**
	 * Gets the general provision percentage.
	 *
	 * @return the generalProvisionPercentage
	 */
	public double getGeneralProvisionPercentage() {

		return generalProvisionPercentage;
	}

	/**
	 * Sets the general provision percentage.
	 *
	 * @param generalProvisionPercentage the generalProvisionPercentage to set
	 */
	public void setGeneralProvisionPercentage(double generalProvisionPercentage) {

		this.generalProvisionPercentage = generalProvisionPercentage;
	}

	/**
	 * Gets the general provision percentage restructured.
	 *
	 * @return the generalProvisionPercentageRestructured
	 */
	public double getGeneralProvisionPercentageRestructured() {

		return generalProvisionPercentageRestructured;
	}

	/**
	 * Sets the general provision percentage restructured.
	 *
	 * @param generalProvisionPercentageRestructured the generalProvisionPercentageRestructured to
	 *        set
	 */
	public void setGeneralProvisionPercentageRestructured(
			double generalProvisionPercentageRestructured) {

		this.generalProvisionPercentageRestructured = generalProvisionPercentageRestructured;
	}

	/**
	 * Gets the general provision cutoff.
	 *
	 * @return the generalProvisionCutoff
	 */
	public int getGeneralProvisionCutoff() {

		return generalProvisionCutoff;
	}

	/**
	 * Sets the general provision cutoff.
	 *
	 * @param generalProvisionCutoff the generalProvisionCutoff to set
	 */
	public void setGeneralProvisionCutoff(int generalProvisionCutoff) {

		this.generalProvisionCutoff = generalProvisionCutoff;
	}

	/**
	 * Gets the general provision cutoff period ID.
	 *
	 * @return the generalProvisionCutoffPeriodID
	 */
	public int getGeneralProvisionCutoffPeriodID() {

		return generalProvisionCutoffPeriodID;
	}

	/**
	 * Sets the general provision cutoff period ID.
	 *
	 * @param generalProvisionCutoffPeriodID the generalProvisionCutoffPeriodID to set
	 */
	public void setGeneralProvisionCutoffPeriodID(int generalProvisionCutoffPeriodID) {

		this.generalProvisionCutoffPeriodID = generalProvisionCutoffPeriodID;
	}

	/**
	 * Checks if is include savings in provision.
	 *
	 * @return the includeSavingsInProvision
	 */
	public boolean isIncludeSavingsInProvision() {

		return includeSavingsInProvision;
	}

	/**
	 * Sets the include savings in provision.
	 *
	 * @param includeSavingsInProvision the includeSavingsInProvision to set
	 */
	public void setIncludeSavingsInProvision(boolean includeSavingsInProvision) {

		this.includeSavingsInProvision = includeSavingsInProvision;
	}

	/**
	 * Checks if is include collateral in provision.
	 *
	 * @return the includeCollateralInProvision
	 */
	public boolean isIncludeCollateralInProvision() {

		return includeCollateralInProvision;
	}

	/**
	 * Sets the include collateral in provision.
	 *
	 * @param includeCollateralInProvision the includeCollateralInProvision to set
	 */
	public void setIncludeCollateralInProvision(boolean includeCollateralInProvision) {

		this.includeCollateralInProvision = includeCollateralInProvision;
	}

	/**
	 * Checks if is include guarantees in provision.
	 *
	 * @return the includeGuaranteesInProvision
	 */
	public boolean isIncludeGuaranteesInProvision() {

		return includeGuaranteesInProvision;
	}

	/**
	 * Sets the include guarantees in provision.
	 *
	 * @param includeGuaranteesInProvision the includeGuaranteesInProvision to set
	 */
	public void setIncludeGuaranteesInProvision(boolean includeGuaranteesInProvision) {

		this.includeGuaranteesInProvision = includeGuaranteesInProvision;
	}

	/**
	 * Checks if is include interest due in provision.
	 *
	 * @return the includeInterestDueInProvision
	 */
	public boolean isIncludeInterestDueInProvision() {

		return includeInterestDueInProvision;
	}

	/**
	 * Sets the include interest due in provision.
	 *
	 * @param includeInterestDueInProvision the includeInterestDueInProvision to set
	 */
	public void setIncludeInterestDueInProvision(boolean includeInterestDueInProvision) {

		this.includeInterestDueInProvision = includeInterestDueInProvision;
	}

	/**
	 * Gets the ignore arrears below.
	 *
	 * @return the ignoreArrearsBelow
	 */
	public double getIgnoreArrearsBelow() {

		return ignoreArrearsBelow;
	}

	/**
	 * Sets the ignore arrears below.
	 *
	 * @param ignoreArrearsBelow the ignoreArrearsBelow to set
	 */
	public void setIgnoreArrearsBelow(double ignoreArrearsBelow) {

		this.ignoreArrearsBelow = ignoreArrearsBelow;
	}

	/**
	 * Checks if is bank of indonesia brackets.
	 *
	 * @return the bankOfIndonesiaBrackets
	 */
	public boolean isBankOfIndonesiaBrackets() {

		return bankOfIndonesiaBrackets;
	}

	/**
	 * Sets the bank of indonesia brackets.
	 *
	 * @param bankOfIndonesiaBrackets the bankOfIndonesiaBrackets to set
	 */
	public void setBankOfIndonesiaBrackets(boolean bankOfIndonesiaBrackets) {

		this.bankOfIndonesiaBrackets = bankOfIndonesiaBrackets;
	}

	/**
	 * Gets the specific provision period.
	 *
	 * @return the specificProvisionPeriod
	 */
	public int getSpecificProvisionPeriod() {

		return specificProvisionPeriod;
	}

	/**
	 * Sets the specific provision period.
	 *
	 * @param specificProvisionPeriod the specificProvisionPeriod to set
	 */
	public void setSpecificProvisionPeriod(int specificProvisionPeriod) {

		this.specificProvisionPeriod = specificProvisionPeriod;
	}

	/**
	 * Gets the specific provision mature period.
	 *
	 * @return the specificProvisionMaturePeriod
	 */
	public int getSpecificProvisionMaturePeriod() {

		return specificProvisionMaturePeriod;
	}

	/**
	 * Sets the specific provision mature period.
	 *
	 * @param specificProvisionMaturePeriod the specificProvisionMaturePeriod to set
	 */
	public void setSpecificProvisionMaturePeriod(int specificProvisionMaturePeriod) {

		this.specificProvisionMaturePeriod = specificProvisionMaturePeriod;
	}

	/**
	 * Checks if is use default provision settings.
	 *
	 * @return the useDefaultProvisionSettings
	 */
	public boolean isUseDefaultProvisionSettings() {

		return useDefaultProvisionSettings;
	}

	/**
	 * Sets the use default provision settings.
	 *
	 * @param useDefaultProvisionSettings the useDefaultProvisionSettings to set
	 */
	public void setUseDefaultProvisionSettings(boolean useDefaultProvisionSettings) {

		this.useDefaultProvisionSettings = useDefaultProvisionSettings;
	}

	/**
	 * Checks if is use default doubtful debts settings.
	 *
	 * @return the useDefaultDoubtfulDebtsSettings
	 */
	public boolean isUseDefaultDoubtfulDebtsSettings() {

		return useDefaultDoubtfulDebtsSettings;
	}

	/**
	 * Sets the use default doubtful debts settings.
	 *
	 * @param useDefaultDoubtfulDebtsSettings the useDefaultDoubtfulDebtsSettings to set
	 */
	public void setUseDefaultDoubtfulDebtsSettings(boolean useDefaultDoubtfulDebtsSettings) {

		this.useDefaultDoubtfulDebtsSettings = useDefaultDoubtfulDebtsSettings;
	}

	/**
	 * Gets the default charge off product ID.
	 *
	 * @return the defaultChargeOffProductID
	 */
	public int getDefaultChargeOffProductID() {

		return defaultChargeOffProductID;
	}

	/**
	 * Sets the default charge off product ID.
	 *
	 * @param defaultChargeOffProductID the defaultChargeOffProductID to set
	 */
	public void setDefaultChargeOffProductID(int defaultChargeOffProductID) {

		this.defaultChargeOffProductID = defaultChargeOffProductID;
	}

	/**
	 * Gets the doubtful debts period.
	 *
	 * @return the doubtfulDebtsPeriod
	 */
	public int getDoubtfulDebtsPeriod() {

		return doubtfulDebtsPeriod;
	}

	/**
	 * Sets the doubtful debts period.
	 *
	 * @param doubtfulDebtsPeriod the doubtfulDebtsPeriod to set
	 */
	public void setDoubtfulDebtsPeriod(int doubtfulDebtsPeriod) {

		this.doubtfulDebtsPeriod = doubtfulDebtsPeriod;
	}

	/**
	 * Gets the general provision date.
	 *
	 * @return the generalProvisionDate
	 */
	public Date getGeneralProvisionDate() {

		return generalProvisionDate;
	}

	/**
	 * Sets the general provision date.
	 *
	 * @param generalProvisionDate the generalProvisionDate to set
	 */
	public void setGeneralProvisionDate(Date generalProvisionDate) {

		this.generalProvisionDate = generalProvisionDate;
	}

	/**
	 * Gets the specific provision date.
	 *
	 * @return the specificProvisionDate
	 */
	public Date getSpecificProvisionDate() {

		return specificProvisionDate;
	}

	/**
	 * Sets the specific provision date.
	 *
	 * @param specificProvisionDate the specificProvisionDate to set
	 */
	public void setSpecificProvisionDate(Date specificProvisionDate) {

		this.specificProvisionDate = specificProvisionDate;
	}

	/**
	 * Gets the doubtful debts date.
	 *
	 * @return the doubtfulDebtsDate
	 */
	public Date getDoubtfulDebtsDate() {

		return doubtfulDebtsDate;
	}

	/**
	 * Sets the doubtful debts date.
	 *
	 * @param doubtfulDebtsDate the doubtfulDebtsDate to set
	 */
	public void setDoubtfulDebtsDate(Date doubtfulDebtsDate) {

		this.doubtfulDebtsDate = doubtfulDebtsDate;
	}

	/**
	 * Checks if is line of credit.
	 *
	 * @return the lineOfCredit
	 */
	public boolean isLineOfCredit() {

		return lineOfCredit;
	}

	/**
	 * Sets the line of credit.
	 *
	 * @param lineOfCredit the lineOfCredit to set
	 */
	public void setLineOfCredit(boolean lineOfCredit) {

		this.lineOfCredit = lineOfCredit;
	}

	/**
	 * Gets the loc min repayment.
	 *
	 * @return the locMinRepayment
	 */
	public double getLocMinRepayment() {

		return locMinRepayment;
	}

	/**
	 * Sets the loc min repayment.
	 *
	 * @param locMinRepayment the locMinRepayment to set
	 */
	public void setLocMinRepayment(double locMinRepayment) {

		this.locMinRepayment = locMinRepayment;
	}

	/**
	 * Gets the loc default repay percent.
	 *
	 * @return the locDefaultRepayPercent
	 */
	public double getLocDefaultRepayPercent() {

		return locDefaultRepayPercent;
	}

	/**
	 * Sets the loc default repay percent.
	 *
	 * @param locDefaultRepayPercent the locDefaultRepayPercent to set
	 */
	public void setLocDefaultRepayPercent(double locDefaultRepayPercent) {

		this.locDefaultRepayPercent = locDefaultRepayPercent;
	}

	/**
	 * Checks if is loc reschedule on issue.
	 *
	 * @return the locRescheduleOnIssue
	 */
	public boolean isLocRescheduleOnIssue() {

		return locRescheduleOnIssue;
	}

	/**
	 * Sets the loc reschedule on issue.
	 *
	 * @param locRescheduleOnIssue the locRescheduleOnIssue to set
	 */
	public void setLocRescheduleOnIssue(boolean locRescheduleOnIssue) {

		this.locRescheduleOnIssue = locRescheduleOnIssue;
	}

	/**
	 * Checks if is emergency loan.
	 *
	 * @return the emergencyLoan
	 */
	public boolean isEmergencyLoan() {

		return emergencyLoan;
	}

	/**
	 * Sets the emergency loan.
	 *
	 * @param emergencyLoan the emergencyLoan to set
	 */
	public void setEmergencyLoan(boolean emergencyLoan) {

		this.emergencyLoan = emergencyLoan;
	}

	/**
	 * Checks if is allow settlement accounts.
	 *
	 * @return the allowSettlementAccounts
	 */
	public boolean isAllowSettlementAccounts() {

		return allowSettlementAccounts;
	}

	/**
	 * Sets the allow settlement accounts.
	 *
	 * @param allowSettlementAccounts the allowSettlementAccounts to set
	 */
	public void setAllowSettlementAccounts(boolean allowSettlementAccounts) {

		this.allowSettlementAccounts = allowSettlementAccounts;
	}

	/**
	 * Gets the settlement number of attempts.
	 *
	 * @return the settlementNumberOfAttempts
	 */
	public int getSettlementNumberOfAttempts() {

		return settlementNumberOfAttempts;
	}

	/**
	 * Sets the settlement number of attempts.
	 *
	 * @param settlementNumberOfAttempts the settlementNumberOfAttempts to set
	 */
	public void setSettlementNumberOfAttempts(int settlementNumberOfAttempts) {

		this.settlementNumberOfAttempts = settlementNumberOfAttempts;
	}

	/**
	 * Gets the settlement attempt fee ID.
	 *
	 * @return the settlementAttemptFeeID
	 */
	public int getSettlementAttemptFeeID() {

		return settlementAttemptFeeID;
	}

	/**
	 * Sets the settlement attempt fee ID.
	 *
	 * @param settlementAttemptFeeID the settlementAttemptFeeID to set
	 */
	public void setSettlementAttemptFeeID(int settlementAttemptFeeID) {

		this.settlementAttemptFeeID = settlementAttemptFeeID;
	}

	/**
	 * Gets the settlement attempt fee amount.
	 *
	 * @return the settlementAttemptFeeAmount
	 */
	public double getSettlementAttemptFeeAmount() {

		return settlementAttemptFeeAmount;
	}

	/**
	 * Sets the settlement attempt fee amount.
	 *
	 * @param settlementAttemptFeeAmount the settlementAttemptFeeAmount to set
	 */
	public void setSettlementAttemptFeeAmount(double settlementAttemptFeeAmount) {

		this.settlementAttemptFeeAmount = settlementAttemptFeeAmount;
	}

	/**
	 * Checks if is settlement attempt fee pay.
	 *
	 * @return the settlementAttemptFeePay
	 */
	public boolean isSettlementAttemptFeePay() {

		return settlementAttemptFeePay;
	}

	/**
	 * Sets the settlement attempt fee pay.
	 *
	 * @param settlementAttemptFeePay the settlementAttemptFeePay to set
	 */
	public void setSettlementAttemptFeePay(boolean settlementAttemptFeePay) {

		this.settlementAttemptFeePay = settlementAttemptFeePay;
	}

	/**
	 * Gets the settlement fail fee ID.
	 *
	 * @return the settlementFailFeeID
	 */
	public int getSettlementFailFeeID() {

		return settlementFailFeeID;
	}

	/**
	 * Sets the settlement fail fee ID.
	 *
	 * @param settlementFailFeeID the settlementFailFeeID to set
	 */
	public void setSettlementFailFeeID(int settlementFailFeeID) {

		this.settlementFailFeeID = settlementFailFeeID;
	}

	/**
	 * Gets the settlement fail fee amount.
	 *
	 * @return the settlementFailFeeAmount
	 */
	public double getSettlementFailFeeAmount() {

		return settlementFailFeeAmount;
	}

	/**
	 * Sets the settlement fail fee amount.
	 *
	 * @param settlementFailFeeAmount the settlementFailFeeAmount to set
	 */
	public void setSettlementFailFeeAmount(double settlementFailFeeAmount) {

		this.settlementFailFeeAmount = settlementFailFeeAmount;
	}

	/**
	 * Checks if is settlement fail fee pay.
	 *
	 * @return the settlementFailFeePay
	 */
	public boolean isSettlementFailFeePay() {

		return settlementFailFeePay;
	}

	/**
	 * Sets the settlement fail fee pay.
	 *
	 * @param settlementFailFeePay the settlementFailFeePay to set
	 */
	public void setSettlementFailFeePay(boolean settlementFailFeePay) {

		this.settlementFailFeePay = settlementFailFeePay;
	}

	/**
	 * Checks if is settlement fee per fail.
	 *
	 * @return the settlementFeePerFail
	 */
	public boolean isSettlementFeePerFail() {

		return settlementFeePerFail;
	}

	/**
	 * Sets the settlement fee per fail.
	 *
	 * @param settlementFeePerFail the settlementFeePerFail to set
	 */
	public void setSettlementFeePerFail(boolean settlementFeePerFail) {

		this.settlementFeePerFail = settlementFeePerFail;
	}

	/**
	 * Gets the first payment limit in days.
	 *
	 * @return the firstPaymentLimitInDays
	 */
	public int getFirstPaymentLimitInDays() {

		return firstPaymentLimitInDays;
	}

	/**
	 * Sets the first payment limit in days.
	 *
	 * @param firstPaymentLimitInDays the firstPaymentLimitInDays to set
	 */
	public void setFirstPaymentLimitInDays(int firstPaymentLimitInDays) {

		this.firstPaymentLimitInDays = firstPaymentLimitInDays;
	}

	/**
	 * Checks if is allow standing instructions.
	 *
	 * @return the allowStandingInstructions
	 */
	public boolean isAllowStandingInstructions() {

		return allowStandingInstructions;
	}

	/**
	 * Sets the allow standing instructions.
	 *
	 * @param allowStandingInstructions the allowStandingInstructions to set
	 */
	public void setAllowStandingInstructions(boolean allowStandingInstructions) {

		this.allowStandingInstructions = allowStandingInstructions;
	}

	/**
	 * Gets the number of attempts.
	 *
	 * @return the numberOfAttempts
	 */
	public int getNumberOfAttempts() {

		return numberOfAttempts;
	}

	/**
	 * Sets the number of attempts.
	 *
	 * @param numberOfAttempts the numberOfAttempts to set
	 */
	public void setNumberOfAttempts(int numberOfAttempts) {

		this.numberOfAttempts = numberOfAttempts;
	}

	/**
	 * Gets the attempt fee ID.
	 *
	 * @return the attemptFeeID
	 */
	public int getAttemptFeeID() {

		return attemptFeeID;
	}

	/**
	 * Sets the attempt fee ID.
	 *
	 * @param attemptFeeID the attemptFeeID to set
	 */
	public void setAttemptFeeID(int attemptFeeID) {

		this.attemptFeeID = attemptFeeID;
	}

	/**
	 * Gets the attempt fee amount.
	 *
	 * @return the attemptFeeAmount
	 */
	public double getAttemptFeeAmount() {

		return attemptFeeAmount;
	}

	/**
	 * Sets the attempt fee amount.
	 *
	 * @param attemptFeeAmount the attemptFeeAmount to set
	 */
	public void setAttemptFeeAmount(double attemptFeeAmount) {

		this.attemptFeeAmount = attemptFeeAmount;
	}

	/**
	 * Checks if is attempt fee pay.
	 *
	 * @return the attemptFeePay
	 */
	public boolean isAttemptFeePay() {

		return attemptFeePay;
	}

	/**
	 * Sets the attempt fee pay.
	 *
	 * @param attemptFeePay the attemptFeePay to set
	 */
	public void setAttemptFeePay(boolean attemptFeePay) {

		this.attemptFeePay = attemptFeePay;
	}

	/**
	 * Gets the fail fee ID.
	 *
	 * @return the failFeeID
	 */
	public int getFailFeeID() {

		return failFeeID;
	}

	/**
	 * Sets the fail fee ID.
	 *
	 * @param failFeeID the failFeeID to set
	 */
	public void setFailFeeID(int failFeeID) {

		this.failFeeID = failFeeID;
	}

	/**
	 * Gets the fail fee amount.
	 *
	 * @return the failFeeAmount
	 */
	public double getFailFeeAmount() {

		return failFeeAmount;
	}

	/**
	 * Sets the fail fee amount.
	 *
	 * @param failFeeAmount the failFeeAmount to set
	 */
	public void setFailFeeAmount(double failFeeAmount) {

		this.failFeeAmount = failFeeAmount;
	}

	/**
	 * Checks if is fail fee pay.
	 *
	 * @return the failFeePay
	 */
	public boolean isFailFeePay() {

		return failFeePay;
	}

	/**
	 * Sets the fail fee pay.
	 *
	 * @param failFeePay the failFeePay to set
	 */
	public void setFailFeePay(boolean failFeePay) {

		this.failFeePay = failFeePay;
	}

	/**
	 * Checks if is fee per fail.
	 *
	 * @return the feePerFail
	 */
	public boolean isFeePerFail() {

		return feePerFail;
	}

	/**
	 * Sets the fee per fail.
	 *
	 * @param feePerFail the feePerFail to set
	 */
	public void setFeePerFail(boolean feePerFail) {

		this.feePerFail = feePerFail;
	}

	/**
	 * Gets the dormancy fee ID.
	 *
	 * @return the dormancyFeeID
	 */
	public int getDormancyFeeID() {

		return dormancyFeeID;
	}

	/**
	 * Sets the dormancy fee ID.
	 *
	 * @param dormancyFeeID the dormancyFeeID to set
	 */
	public void setDormancyFeeID(int dormancyFeeID) {

		this.dormancyFeeID = dormancyFeeID;
	}

	/**
	 * Gets the dormancy fee period num.
	 *
	 * @return the dormancyFeePeriodNum
	 */
	public int getDormancyFeePeriodNum() {

		return dormancyFeePeriodNum;
	}

	/**
	 * Sets the dormancy fee period num.
	 *
	 * @param dormancyFeePeriodNum the dormancyFeePeriodNum to set
	 */
	public void setDormancyFeePeriodNum(int dormancyFeePeriodNum) {

		this.dormancyFeePeriodNum = dormancyFeePeriodNum;
	}

	/**
	 * Gets the dormancy fee period ID.
	 *
	 * @return the dormancyFeePeriodID
	 */
	public int getDormancyFeePeriodID() {

		return dormancyFeePeriodID;
	}

	/**
	 * Sets the dormancy fee period ID.
	 *
	 * @param dormancyFeePeriodID the dormancyFeePeriodID to set
	 */
	public void setDormancyFeePeriodID(int dormancyFeePeriodID) {

		this.dormancyFeePeriodID = dormancyFeePeriodID;
	}

	/**
	 * Gets the dormancy fee last run.
	 *
	 * @return the dormancyFeeLastRun
	 */
	public Date getDormancyFeeLastRun() {

		return dormancyFeeLastRun;
	}

	/**
	 * Sets the dormancy fee last run.
	 *
	 * @param dormancyFeeLastRun the dormancyFeeLastRun to set
	 */
	public void setDormancyFeeLastRun(Date dormancyFeeLastRun) {

		this.dormancyFeeLastRun = dormancyFeeLastRun;
	}

	/**
	 * Gets the dormancy period num.
	 *
	 * @return the dormancyPeriodNum
	 */
	public int getDormancyPeriodNum() {

		return dormancyPeriodNum;
	}

	/**
	 * Sets the dormancy period num.
	 *
	 * @param dormancyPeriodNum the dormancyPeriodNum to set
	 */
	public void setDormancyPeriodNum(int dormancyPeriodNum) {

		this.dormancyPeriodNum = dormancyPeriodNum;
	}

	/**
	 * Gets the dormancy period ID.
	 *
	 * @return the dormancyPeriodID
	 */
	public int getDormancyPeriodID() {

		return dormancyPeriodID;
	}

	/**
	 * Sets the dormancy period ID.
	 *
	 * @param dormancyPeriodID the dormancyPeriodID to set
	 */
	public void setDormancyPeriodID(int dormancyPeriodID) {

		this.dormancyPeriodID = dormancyPeriodID;
	}

	/**
	 * Gets the dormancy transaction sources.
	 *
	 * @return the dormancyTransactionSources
	 */
	public int getDormancyTransactionSources() {

		return dormancyTransactionSources;
	}

	/**
	 * Sets the dormancy transaction sources.
	 *
	 * @param dormancyTransactionSources the dormancyTransactionSources to set
	 */
	public void setDormancyTransactionSources(int dormancyTransactionSources) {

		this.dormancyTransactionSources = dormancyTransactionSources;
	}

	/**
	 * Gets the dormancy transaction types.
	 *
	 * @return the dormancyTransactionTypes
	 */
	public int getDormancyTransactionTypes() {

		return dormancyTransactionTypes;
	}

	/**
	 * Sets the dormancy transaction types.
	 *
	 * @param dormancyTransactionTypes the dormancyTransactionTypes to set
	 */
	public void setDormancyTransactionTypes(int dormancyTransactionTypes) {

		this.dormancyTransactionTypes = dormancyTransactionTypes;
	}

	/**
	 * Checks if is allow dormancy.
	 *
	 * @return the allowDormancy
	 */
	public boolean isAllowDormancy() {

		return allowDormancy;
	}

	/**
	 * Sets the allow dormancy.
	 *
	 * @param allowDormancy the allowDormancy to set
	 */
	public void setAllowDormancy(boolean allowDormancy) {

		this.allowDormancy = allowDormancy;
	}

	/**
	 * Checks if is use overdrawn date.
	 *
	 * @return the useOverdrawnDate
	 */
	public boolean isUseOverdrawnDate() {

		return useOverdrawnDate;
	}

	/**
	 * Sets the use overdrawn date.
	 *
	 * @param useOverdrawnDate the useOverdrawnDate to set
	 */
	public void setUseOverdrawnDate(boolean useOverdrawnDate) {

		this.useOverdrawnDate = useOverdrawnDate;
	}

	/**
	 * Gets the restructured loan expiry period num.
	 *
	 * @return the restructuredLoanExpiryPeriodNum
	 */
	public int getRestructuredLoanExpiryPeriodNum() {

		return restructuredLoanExpiryPeriodNum;
	}

	/**
	 * Sets the restructured loan expiry period num.
	 *
	 * @param restructuredLoanExpiryPeriodNum the restructuredLoanExpiryPeriodNum to set
	 */
	public void setRestructuredLoanExpiryPeriodNum(int restructuredLoanExpiryPeriodNum) {

		this.restructuredLoanExpiryPeriodNum = restructuredLoanExpiryPeriodNum;
	}

	/**
	 * Gets the restructured loan expiry period ID.
	 *
	 * @return the restructuredLoanExpiryPeriodID
	 */
	public int getRestructuredLoanExpiryPeriodID() {

		return restructuredLoanExpiryPeriodID;
	}

	/**
	 * Sets the restructured loan expiry period ID.
	 *
	 * @param restructuredLoanExpiryPeriodID the restructuredLoanExpiryPeriodID to set
	 */
	public void setRestructuredLoanExpiryPeriodID(int restructuredLoanExpiryPeriodID) {

		this.restructuredLoanExpiryPeriodID = restructuredLoanExpiryPeriodID;
	}

	/**
	 * Gets the interest method.
	 *
	 * @return the interestMethod
	 */
	public int getInterestMethod() {

		return interestMethod;
	}

	/**
	 * Sets the interest method.
	 *
	 * @param interestMethod the interestMethod to set
	 */
	public void setInterestMethod(int interestMethod) {

		this.interestMethod = interestMethod;
	}

	/**
	 * Gets the default interest method.
	 *
	 * @return the defaultInterestMethod
	 */
	public int getDefaultInterestMethod() {

		return defaultInterestMethod;
	}

	/**
	 * Sets the default interest method.
	 *
	 * @param defaultInterestMethod the defaultInterestMethod to set
	 */
	public void setDefaultInterestMethod(int defaultInterestMethod) {

		this.defaultInterestMethod = defaultInterestMethod;
	}

	/**
	 * Checks if is linked savings payments required.
	 *
	 * @return the linkedSavingsPaymentsRequired
	 */
	public boolean isLinkedSavingsPaymentsRequired() {

		return linkedSavingsPaymentsRequired;
	}

	/**
	 * Sets the linked savings payments required.
	 *
	 * @param linkedSavingsPaymentsRequired the linkedSavingsPaymentsRequired to set
	 */
	public void setLinkedSavingsPaymentsRequired(boolean linkedSavingsPaymentsRequired) {

		this.linkedSavingsPaymentsRequired = linkedSavingsPaymentsRequired;
	}

	/**
	 * Gets the linked savings payment amount.
	 *
	 * @return the linkedSavingsPaymentAmount
	 */
	public double getLinkedSavingsPaymentAmount() {

		return linkedSavingsPaymentAmount;
	}

	/**
	 * Sets the linked savings payment amount.
	 *
	 * @param linkedSavingsPaymentAmount the linkedSavingsPaymentAmount to set
	 */
	public void setLinkedSavingsPaymentAmount(double linkedSavingsPaymentAmount) {

		this.linkedSavingsPaymentAmount = linkedSavingsPaymentAmount;
	}

	/**
	 * Gets the linked savings payment percentage.
	 *
	 * @return the linkedSavingsPaymentPercentage
	 */
	public double getLinkedSavingsPaymentPercentage() {

		return linkedSavingsPaymentPercentage;
	}

	/**
	 * Sets the linked savings payment percentage.
	 *
	 * @param linkedSavingsPaymentPercentage the linkedSavingsPaymentPercentage to set
	 */
	public void setLinkedSavingsPaymentPercentage(double linkedSavingsPaymentPercentage) {

		this.linkedSavingsPaymentPercentage = linkedSavingsPaymentPercentage;
	}

	/**
	 * Checks if is linked savings payment allow underpaid.
	 *
	 * @return the linkedSavingsPaymentAllowUnderpaid
	 */
	public boolean isLinkedSavingsPaymentAllowUnderpaid() {

		return linkedSavingsPaymentAllowUnderpaid;
	}

	/**
	 * Sets the linked savings payment allow underpaid.
	 *
	 * @param linkedSavingsPaymentAllowUnderpaid the linkedSavingsPaymentAllowUnderpaid to set
	 */
	public void setLinkedSavingsPaymentAllowUnderpaid(boolean linkedSavingsPaymentAllowUnderpaid) {

		this.linkedSavingsPaymentAllowUnderpaid = linkedSavingsPaymentAllowUnderpaid;
	}

	/**
	 * Checks if is linked savings payment require override for underpaid.
	 *
	 * @return the linkedSavingsPaymentRequireOverrideForUnderpaid
	 */
	public boolean isLinkedSavingsPaymentRequireOverrideForUnderpaid() {

		return linkedSavingsPaymentRequireOverrideForUnderpaid;
	}

	/**
	 * Sets the linked savings payment require override for underpaid.
	 *
	 * @param linkedSavingsPaymentRequireOverrideForUnderpaid the
	 *        linkedSavingsPaymentRequireOverrideForUnderpaid to set
	 */
	public void setLinkedSavingsPaymentRequireOverrideForUnderpaid(
			boolean linkedSavingsPaymentRequireOverrideForUnderpaid) {

		this.linkedSavingsPaymentRequireOverrideForUnderpaid =
				linkedSavingsPaymentRequireOverrideForUnderpaid;
	}

	/**
	 * Checks if is linked savings payment withholding.
	 *
	 * @return the linkedSavingsPaymentWithholding
	 */
	public boolean isLinkedSavingsPaymentWithholding() {

		return linkedSavingsPaymentWithholding;
	}

	/**
	 * Sets the linked savings payment withholding.
	 *
	 * @param linkedSavingsPaymentWithholding the linkedSavingsPaymentWithholding to set
	 */
	public void setLinkedSavingsPaymentWithholding(boolean linkedSavingsPaymentWithholding) {

		this.linkedSavingsPaymentWithholding = linkedSavingsPaymentWithholding;
	}

	/**
	 * Gets the linked savings payment type.
	 *
	 * @return the linkedSavingsPaymentType
	 */
	public int getLinkedSavingsPaymentType() {

		return linkedSavingsPaymentType;
	}

	/**
	 * Sets the linked savings payment type.
	 *
	 * @param linkedSavingsPaymentType the linkedSavingsPaymentType to set
	 */
	public void setLinkedSavingsPaymentType(int linkedSavingsPaymentType) {

		this.linkedSavingsPaymentType = linkedSavingsPaymentType;
	}

	/**
	 * Checks if is use loan approval groups.
	 *
	 * @return the useLoanApprovalGroups
	 */
	public boolean isUseLoanApprovalGroups() {

		return useLoanApprovalGroups;
	}

	/**
	 * Sets the use loan approval groups.
	 *
	 * @param useLoanApprovalGroups the useLoanApprovalGroups to set
	 */
	public void setUseLoanApprovalGroups(boolean useLoanApprovalGroups) {

		this.useLoanApprovalGroups = useLoanApprovalGroups;
	}

	/**
	 * Checks if is cheque book enabled.
	 *
	 * @return the chequeBookEnabled
	 */
	public boolean isChequeBookEnabled() {

		return chequeBookEnabled;
	}

	/**
	 * Sets the cheque book enabled.
	 *
	 * @param chequeBookEnabled the chequeBookEnabled to set
	 */
	public void setChequeBookEnabled(boolean chequeBookEnabled) {

		this.chequeBookEnabled = chequeBookEnabled;
	}

	/**
	 * Gets the cheque stop fee ID.
	 *
	 * @return the chequeStopFeeID
	 */
	public int getChequeStopFeeID() {

		return chequeStopFeeID;
	}

	/**
	 * Sets the cheque stop fee ID.
	 *
	 * @param chequeStopFeeID the chequeStopFeeID to set
	 */
	public void setChequeStopFeeID(int chequeStopFeeID) {

		this.chequeStopFeeID = chequeStopFeeID;
	}

	/**
	 * Gets the cheque stop fee amount.
	 *
	 * @return the chequeStopFeeAmount
	 */
	public double getChequeStopFeeAmount() {

		return chequeStopFeeAmount;
	}

	/**
	 * Sets the cheque stop fee amount.
	 *
	 * @param chequeStopFeeAmount the chequeStopFeeAmount to set
	 */
	public void setChequeStopFeeAmount(double chequeStopFeeAmount) {

		this.chequeStopFeeAmount = chequeStopFeeAmount;
	}

	/**
	 * Checks if is cheque stop fee pay.
	 *
	 * @return the chequeStopFeePay
	 */
	public boolean isChequeStopFeePay() {

		return chequeStopFeePay;
	}

	/**
	 * Sets the cheque stop fee pay.
	 *
	 * @param chequeStopFeePay the chequeStopFeePay to set
	 */
	public void setChequeStopFeePay(boolean chequeStopFeePay) {

		this.chequeStopFeePay = chequeStopFeePay;
	}

	/**
	 * Gets the cheque un stop fee ID.
	 *
	 * @return the chequeUnStopFeeID
	 */
	public int getChequeUnStopFeeID() {

		return chequeUnStopFeeID;
	}

	/**
	 * Sets the cheque un stop fee ID.
	 *
	 * @param chequeUnStopFeeID the chequeUnStopFeeID to set
	 */
	public void setChequeUnStopFeeID(int chequeUnStopFeeID) {

		this.chequeUnStopFeeID = chequeUnStopFeeID;
	}

	/**
	 * Gets the cheque unstop fee amount.
	 *
	 * @return the chequeUnstopFeeAmount
	 */
	public double getChequeUnstopFeeAmount() {

		return chequeUnstopFeeAmount;
	}

	/**
	 * Sets the cheque unstop fee amount.
	 *
	 * @param chequeUnstopFeeAmount the chequeUnstopFeeAmount to set
	 */
	public void setChequeUnstopFeeAmount(double chequeUnstopFeeAmount) {

		this.chequeUnstopFeeAmount = chequeUnstopFeeAmount;
	}

	/**
	 * Checks if is cheque un stop fee pay.
	 *
	 * @return the chequeUnStopFeePay
	 */
	public boolean isChequeUnStopFeePay() {

		return chequeUnStopFeePay;
	}

	/**
	 * Sets the cheque un stop fee pay.
	 *
	 * @param chequeUnStopFeePay the chequeUnStopFeePay to set
	 */
	public void setChequeUnStopFeePay(boolean chequeUnStopFeePay) {

		this.chequeUnStopFeePay = chequeUnStopFeePay;
	}

	/**
	 * Gets the cheque payer void fee ID.
	 *
	 * @return the chequePayerVoidFeeID
	 */
	public int getChequePayerVoidFeeID() {

		return chequePayerVoidFeeID;
	}

	/**
	 * Sets the cheque payer void fee ID.
	 *
	 * @param chequePayerVoidFeeID the chequePayerVoidFeeID to set
	 */
	public void setChequePayerVoidFeeID(int chequePayerVoidFeeID) {

		this.chequePayerVoidFeeID = chequePayerVoidFeeID;
	}

	/**
	 * Gets the cheque payer void fee amount.
	 *
	 * @return the chequePayerVoidFeeAmount
	 */
	public double getChequePayerVoidFeeAmount() {

		return chequePayerVoidFeeAmount;
	}

	/**
	 * Sets the cheque payer void fee amount.
	 *
	 * @param chequePayerVoidFeeAmount the chequePayerVoidFeeAmount to set
	 */
	public void setChequePayerVoidFeeAmount(double chequePayerVoidFeeAmount) {

		this.chequePayerVoidFeeAmount = chequePayerVoidFeeAmount;
	}

	/**
	 * Checks if is cheque payer void fee pay.
	 *
	 * @return the chequePayerVoidFeePay
	 */
	public boolean isChequePayerVoidFeePay() {

		return chequePayerVoidFeePay;
	}

	/**
	 * Sets the cheque payer void fee pay.
	 *
	 * @param chequePayerVoidFeePay the chequePayerVoidFeePay to set
	 */
	public void setChequePayerVoidFeePay(boolean chequePayerVoidFeePay) {

		this.chequePayerVoidFeePay = chequePayerVoidFeePay;
	}

	/**
	 * Gets the cheque payee void fee ID.
	 *
	 * @return the chequePayeeVoidFeeID
	 */
	public int getChequePayeeVoidFeeID() {

		return chequePayeeVoidFeeID;
	}

	/**
	 * Sets the cheque payee void fee ID.
	 *
	 * @param chequePayeeVoidFeeID the chequePayeeVoidFeeID to set
	 */
	public void setChequePayeeVoidFeeID(int chequePayeeVoidFeeID) {

		this.chequePayeeVoidFeeID = chequePayeeVoidFeeID;
	}

	/**
	 * Gets the cheque payee void fee amount.
	 *
	 * @return the chequePayeeVoidFeeAmount
	 */
	public double getChequePayeeVoidFeeAmount() {

		return chequePayeeVoidFeeAmount;
	}

	/**
	 * Sets the cheque payee void fee amount.
	 *
	 * @param chequePayeeVoidFeeAmount the chequePayeeVoidFeeAmount to set
	 */
	public void setChequePayeeVoidFeeAmount(double chequePayeeVoidFeeAmount) {

		this.chequePayeeVoidFeeAmount = chequePayeeVoidFeeAmount;
	}

	/**
	 * Checks if is cheque payee void fee pay.
	 *
	 * @return the chequePayeeVoidFeePay
	 */
	public boolean isChequePayeeVoidFeePay() {

		return chequePayeeVoidFeePay;
	}

	/**
	 * Sets the cheque payee void fee pay.
	 *
	 * @param chequePayeeVoidFeePay the chequePayeeVoidFeePay to set
	 */
	public void setChequePayeeVoidFeePay(boolean chequePayeeVoidFeePay) {

		this.chequePayeeVoidFeePay = chequePayeeVoidFeePay;
	}

	/**
	 * Gets the cheque payer dishonour fee ID.
	 *
	 * @return the chequePayerDishonourFeeID
	 */
	public int getChequePayerDishonourFeeID() {

		return chequePayerDishonourFeeID;
	}

	/**
	 * Sets the cheque payer dishonour fee ID.
	 *
	 * @param chequePayerDishonourFeeID the chequePayerDishonourFeeID to set
	 */
	public void setChequePayerDishonourFeeID(int chequePayerDishonourFeeID) {

		this.chequePayerDishonourFeeID = chequePayerDishonourFeeID;
	}

	/**
	 * Gets the cheque payer dishonour fee amount.
	 *
	 * @return the chequePayerDishonourFeeAmount
	 */
	public double getChequePayerDishonourFeeAmount() {

		return chequePayerDishonourFeeAmount;
	}

	/**
	 * Sets the cheque payer dishonour fee amount.
	 *
	 * @param chequePayerDishonourFeeAmount the chequePayerDishonourFeeAmount to set
	 */
	public void setChequePayerDishonourFeeAmount(double chequePayerDishonourFeeAmount) {

		this.chequePayerDishonourFeeAmount = chequePayerDishonourFeeAmount;
	}

	/**
	 * Checks if is cheque payer dishonour fee pay.
	 *
	 * @return the chequePayerDishonourFeePay
	 */
	public boolean isChequePayerDishonourFeePay() {

		return chequePayerDishonourFeePay;
	}

	/**
	 * Sets the cheque payer dishonour fee pay.
	 *
	 * @param chequePayerDishonourFeePay the chequePayerDishonourFeePay to set
	 */
	public void setChequePayerDishonourFeePay(boolean chequePayerDishonourFeePay) {

		this.chequePayerDishonourFeePay = chequePayerDishonourFeePay;
	}

	/**
	 * Gets the cheque payee dishonour fee ID.
	 *
	 * @return the chequePayeeDishonourFeeID
	 */
	public int getChequePayeeDishonourFeeID() {

		return chequePayeeDishonourFeeID;
	}

	/**
	 * Sets the cheque payee dishonour fee ID.
	 *
	 * @param chequePayeeDishonourFeeID the chequePayeeDishonourFeeID to set
	 */
	public void setChequePayeeDishonourFeeID(int chequePayeeDishonourFeeID) {

		this.chequePayeeDishonourFeeID = chequePayeeDishonourFeeID;
	}

	/**
	 * Gets the cheque payee dishonour fee amount.
	 *
	 * @return the chequePayeeDishonourFeeAmount
	 */
	public double getChequePayeeDishonourFeeAmount() {

		return chequePayeeDishonourFeeAmount;
	}

	/**
	 * Sets the cheque payee dishonour fee amount.
	 *
	 * @param chequePayeeDishonourFeeAmount the chequePayeeDishonourFeeAmount to set
	 */
	public void setChequePayeeDishonourFeeAmount(double chequePayeeDishonourFeeAmount) {

		this.chequePayeeDishonourFeeAmount = chequePayeeDishonourFeeAmount;
	}

	/**
	 * Checks if is cheque payee dishonour fee pay.
	 *
	 * @return the chequePayeeDishonourFeePay
	 */
	public boolean isChequePayeeDishonourFeePay() {

		return chequePayeeDishonourFeePay;
	}

	/**
	 * Sets the cheque payee dishonour fee pay.
	 *
	 * @param chequePayeeDishonourFeePay the chequePayeeDishonourFeePay to set
	 */
	public void setChequePayeeDishonourFeePay(boolean chequePayeeDishonourFeePay) {

		this.chequePayeeDishonourFeePay = chequePayeeDishonourFeePay;
	}

	/**
	 * Gets the cheque print fee ID.
	 *
	 * @return the chequePrintFeeID
	 */
	public int getChequePrintFeeID() {

		return chequePrintFeeID;
	}

	/**
	 * Sets the cheque print fee ID.
	 *
	 * @param chequePrintFeeID the chequePrintFeeID to set
	 */
	public void setChequePrintFeeID(int chequePrintFeeID) {

		this.chequePrintFeeID = chequePrintFeeID;
	}

	/**
	 * Gets the cheque print fee amount.
	 *
	 * @return the chequePrintFeeAmount
	 */
	public double getChequePrintFeeAmount() {

		return chequePrintFeeAmount;
	}

	/**
	 * Sets the cheque print fee amount.
	 *
	 * @param chequePrintFeeAmount the chequePrintFeeAmount to set
	 */
	public void setChequePrintFeeAmount(double chequePrintFeeAmount) {

		this.chequePrintFeeAmount = chequePrintFeeAmount;
	}

	/**
	 * Checks if is cheque print fee pay.
	 *
	 * @return the chequePrintFeePay
	 */
	public boolean isChequePrintFeePay() {

		return chequePrintFeePay;
	}

	/**
	 * Sets the cheque print fee pay.
	 *
	 * @param chequePrintFeePay the chequePrintFeePay to set
	 */
	public void setChequePrintFeePay(boolean chequePrintFeePay) {

		this.chequePrintFeePay = chequePrintFeePay;
	}

	/**
	 * Gets the early payout fee ID.
	 *
	 * @return the earlyPayoutFeeID
	 */
	public int getEarlyPayoutFeeID() {

		return earlyPayoutFeeID;
	}

	/**
	 * Sets the early payout fee ID.
	 *
	 * @param earlyPayoutFeeID the earlyPayoutFeeID to set
	 */
	public void setEarlyPayoutFeeID(int earlyPayoutFeeID) {

		this.earlyPayoutFeeID = earlyPayoutFeeID;
	}

	/**
	 * Gets the early payout fee amount.
	 *
	 * @return the earlyPayoutFeeAmount
	 */
	public double getEarlyPayoutFeeAmount() {

		return earlyPayoutFeeAmount;
	}

	/**
	 * Sets the early payout fee amount.
	 *
	 * @param earlyPayoutFeeAmount the earlyPayoutFeeAmount to set
	 */
	public void setEarlyPayoutFeeAmount(double earlyPayoutFeeAmount) {

		this.earlyPayoutFeeAmount = earlyPayoutFeeAmount;
	}

	/**
	 * Checks if is atm enabled.
	 *
	 * @return the atmEnabled
	 */
	public boolean isAtmEnabled() {

		return atmEnabled;
	}

	/**
	 * Sets the atm enabled.
	 *
	 * @param atmEnabled the atmEnabled to set
	 */
	public void setAtmEnabled(boolean atmEnabled) {

		this.atmEnabled = atmEnabled;
	}

	/**
	 * Gets the atm card limit.
	 *
	 * @return the atmCardLimit
	 */
	public double getAtmCardLimit() {

		return atmCardLimit;
	}

	/**
	 * Sets the atm card limit.
	 *
	 * @param atmCardLimit the atmCardLimit to set
	 */
	public void setAtmCardLimit(double atmCardLimit) {

		this.atmCardLimit = atmCardLimit;
	}

	/**
	 * Gets the atm daily limit.
	 *
	 * @return the atmDailyLimit
	 */
	public double getAtmDailyLimit() {

		return atmDailyLimit;
	}

	/**
	 * Sets the atm daily limit.
	 *
	 * @param atmDailyLimit the atmDailyLimit to set
	 */
	public void setAtmDailyLimit(double atmDailyLimit) {

		this.atmDailyLimit = atmDailyLimit;
	}

	/**
	 * Gets the atm withdrawal fee ID.
	 *
	 * @return the atmWithdrawalFeeID
	 */
	public int getAtmWithdrawalFeeID() {

		return atmWithdrawalFeeID;
	}

	/**
	 * Sets the atm withdrawal fee ID.
	 *
	 * @param atmWithdrawalFeeID the atmWithdrawalFeeID to set
	 */
	public void setAtmWithdrawalFeeID(int atmWithdrawalFeeID) {

		this.atmWithdrawalFeeID = atmWithdrawalFeeID;
	}

	/**
	 * Gets the atm service fee ID.
	 *
	 * @return the atmServiceFeeID
	 */
	public int getAtmServiceFeeID() {

		return atmServiceFeeID;
	}

	/**
	 * Sets the atm service fee ID.
	 *
	 * @param atmServiceFeeID the atmServiceFeeID to set
	 */
	public void setAtmServiceFeeID(int atmServiceFeeID) {

		this.atmServiceFeeID = atmServiceFeeID;
	}

	/**
	 * Gets the atm service fee period num.
	 *
	 * @return the atmServiceFeePeriodNum
	 */
	public int getAtmServiceFeePeriodNum() {

		return atmServiceFeePeriodNum;
	}

	/**
	 * Sets the atm service fee period num.
	 *
	 * @param atmServiceFeePeriodNum the atmServiceFeePeriodNum to set
	 */
	public void setAtmServiceFeePeriodNum(int atmServiceFeePeriodNum) {

		this.atmServiceFeePeriodNum = atmServiceFeePeriodNum;
	}

	/**
	 * Gets the atm service fee period ID.
	 *
	 * @return the atmServiceFeePeriodID
	 */
	public int getAtmServiceFeePeriodID() {

		return atmServiceFeePeriodID;
	}

	/**
	 * Sets the atm service fee period ID.
	 *
	 * @param atmServiceFeePeriodID the atmServiceFeePeriodID to set
	 */
	public void setAtmServiceFeePeriodID(int atmServiceFeePeriodID) {

		this.atmServiceFeePeriodID = atmServiceFeePeriodID;
	}

	/**
	 * Gets the atm service fee last run.
	 *
	 * @return the atmServiceFeeLastRun
	 */
	public Date getAtmServiceFeeLastRun() {

		return atmServiceFeeLastRun;
	}

	/**
	 * Sets the atm service fee last run.
	 *
	 * @param atmServiceFeeLastRun the atmServiceFeeLastRun to set
	 */
	public void setAtmServiceFeeLastRun(Date atmServiceFeeLastRun) {

		this.atmServiceFeeLastRun = atmServiceFeeLastRun;
	}

	/**
	 * Gets the atm min balance.
	 *
	 * @return the atmMinBalance
	 */
	public double getAtmMinBalance() {

		return atmMinBalance;
	}

	/**
	 * Sets the atm min balance.
	 *
	 * @param atmMinBalance the atmMinBalance to set
	 */
	public void setAtmMinBalance(double atmMinBalance) {

		this.atmMinBalance = atmMinBalance;
	}

	/**
	 * Gets the service fee ID 2.
	 *
	 * @return the serviceFeeID2
	 */
	public int getServiceFeeID2() {

		return serviceFeeID2;
	}

	/**
	 * Sets the service fee ID 2.
	 *
	 * @param serviceFeeID2 the serviceFeeID2 to set
	 */
	public void setServiceFeeID2(int serviceFeeID2) {

		this.serviceFeeID2 = serviceFeeID2;
	}

	/**
	 * Gets the service fee period num 2.
	 *
	 * @return the serviceFeePeriodNum2
	 */
	public int getServiceFeePeriodNum2() {

		return serviceFeePeriodNum2;
	}

	/**
	 * Sets the service fee period num 2.
	 *
	 * @param serviceFeePeriodNum2 the serviceFeePeriodNum2 to set
	 */
	public void setServiceFeePeriodNum2(int serviceFeePeriodNum2) {

		this.serviceFeePeriodNum2 = serviceFeePeriodNum2;
	}

	/**
	 * Gets the service fee period ID 2.
	 *
	 * @return the serviceFeePeriodID2
	 */
	public int getServiceFeePeriodID2() {

		return serviceFeePeriodID2;
	}

	/**
	 * Sets the service fee period ID 2.
	 *
	 * @param serviceFeePeriodID2 the serviceFeePeriodID2 to set
	 */
	public void setServiceFeePeriodID2(int serviceFeePeriodID2) {

		this.serviceFeePeriodID2 = serviceFeePeriodID2;
	}

	/**
	 * Checks if is close LOC automatically.
	 *
	 * @return the closeLOCAutomatically
	 */
	public boolean isCloseLOCAutomatically() {

		return closeLOCAutomatically;
	}

	/**
	 * Sets the close LOC automatically.
	 *
	 * @param closeLOCAutomatically the closeLOCAutomatically to set
	 */
	public void setCloseLOCAutomatically(boolean closeLOCAutomatically) {

		this.closeLOCAutomatically = closeLOCAutomatically;
	}

	/**
	 * Checks if is lock provisioning band.
	 *
	 * @return the lockProvisioningBand
	 */
	public boolean isLockProvisioningBand() {

		return lockProvisioningBand;
	}

	/**
	 * Sets the lock provisioning band.
	 *
	 * @param lockProvisioningBand the lockProvisioningBand to set
	 */
	public void setLockProvisioningBand(boolean lockProvisioningBand) {

		this.lockProvisioningBand = lockProvisioningBand;
	}

	/**
	 * Gets the lock provisioning band period num.
	 *
	 * @return the lockProvisioningBandPeriodNum
	 */
	public int getLockProvisioningBandPeriodNum() {

		return lockProvisioningBandPeriodNum;
	}

	/**
	 * Sets the lock provisioning band period num.
	 *
	 * @param lockProvisioningBandPeriodNum the lockProvisioningBandPeriodNum to set
	 */
	public void setLockProvisioningBandPeriodNum(int lockProvisioningBandPeriodNum) {

		this.lockProvisioningBandPeriodNum = lockProvisioningBandPeriodNum;
	}

	/**
	 * Gets the lock provisioning band period ID.
	 *
	 * @return the lockProvisioningBandPeriodID
	 */
	public int getLockProvisioningBandPeriodID() {

		return lockProvisioningBandPeriodID;
	}

	/**
	 * Sets the lock provisioning band period ID.
	 *
	 * @param lockProvisioningBandPeriodID the lockProvisioningBandPeriodID to set
	 */
	public void setLockProvisioningBandPeriodID(int lockProvisioningBandPeriodID) {

		this.lockProvisioningBandPeriodID = lockProvisioningBandPeriodID;
	}

	/**
	 * Gets the interest cap method.
	 *
	 * @return the interestCapMethod
	 */
	public int getInterestCapMethod() {

		return interestCapMethod;
	}

	/**
	 * Sets the interest cap method.
	 *
	 * @param interestCapMethod the interestCapMethod to set
	 */
	public void setInterestCapMethod(int interestCapMethod) {

		this.interestCapMethod = interestCapMethod;
	}

	/**
	 * Checks if is service oldest debt first.
	 *
	 * @return the serviceOldestDebtFirst
	 */
	public boolean isServiceOldestDebtFirst() {

		return serviceOldestDebtFirst;
	}

	/**
	 * Sets the service oldest debt first.
	 *
	 * @param serviceOldestDebtFirst the serviceOldestDebtFirst to set
	 */
	public void setServiceOldestDebtFirst(boolean serviceOldestDebtFirst) {

		this.serviceOldestDebtFirst = serviceOldestDebtFirst;
	}

	/**
	 * Checks if is include interest due in arrears.
	 *
	 * @return the includeInterestDueInArrears
	 */
	public boolean isIncludeInterestDueInArrears() {

		return includeInterestDueInArrears;
	}

	/**
	 * Sets the include interest due in arrears.
	 *
	 * @param includeInterestDueInArrears the includeInterestDueInArrears to set
	 */
	public void setIncludeInterestDueInArrears(boolean includeInterestDueInArrears) {

		this.includeInterestDueInArrears = includeInterestDueInArrears;
	}

	/**
	 * Checks if is include devaluation fee in arrears.
	 *
	 * @return the includeDevaluationFeeInArrears
	 */
	public boolean isIncludeDevaluationFeeInArrears() {

		return includeDevaluationFeeInArrears;
	}

	/**
	 * Sets the include devaluation fee in arrears.
	 *
	 * @param includeDevaluationFeeInArrears the includeDevaluationFeeInArrears to set
	 */
	public void setIncludeDevaluationFeeInArrears(boolean includeDevaluationFeeInArrears) {

		this.includeDevaluationFeeInArrears = includeDevaluationFeeInArrears;
	}

	/**
	 * Checks if is offset prepaid balance against interest due in arrears.
	 *
	 * @return the offsetPrepaidBalanceAgainstInterestDueInArrears
	 */
	public boolean isOffsetPrepaidBalanceAgainstInterestDueInArrears() {

		return offsetPrepaidBalanceAgainstInterestDueInArrears;
	}

	/**
	 * Sets the offset prepaid balance against interest due in arrears.
	 *
	 * @param offsetPrepaidBalanceAgainstInterestDueInArrears the
	 *        offsetPrepaidBalanceAgainstInterestDueInArrears to set
	 */
	public void setOffsetPrepaidBalanceAgainstInterestDueInArrears(
			boolean offsetPrepaidBalanceAgainstInterestDueInArrears) {

		this.offsetPrepaidBalanceAgainstInterestDueInArrears =
				offsetPrepaidBalanceAgainstInterestDueInArrears;
	}

	/**
	 * Checks if is include overdrawn date in arrears.
	 *
	 * @return the includeOverdrawnDateInArrears
	 */
	public boolean isIncludeOverdrawnDateInArrears() {

		return includeOverdrawnDateInArrears;
	}

	/**
	 * Sets the include overdrawn date in arrears.
	 *
	 * @param includeOverdrawnDateInArrears the includeOverdrawnDateInArrears to set
	 */
	public void setIncludeOverdrawnDateInArrears(boolean includeOverdrawnDateInArrears) {

		this.includeOverdrawnDateInArrears = includeOverdrawnDateInArrears;
	}

	/**
	 * Checks if is ignore schedule in arrears.
	 *
	 * @return the ignoreScheduleInArrears
	 */
	public boolean isIgnoreScheduleInArrears() {

		return ignoreScheduleInArrears;
	}

	/**
	 * Sets the ignore schedule in arrears.
	 *
	 * @param ignoreScheduleInArrears the ignoreScheduleInArrears to set
	 */
	public void setIgnoreScheduleInArrears(boolean ignoreScheduleInArrears) {

		this.ignoreScheduleInArrears = ignoreScheduleInArrears;
	}

	/**
	 * Checks if is collect savings during write off.
	 *
	 * @return the collectSavingsDuringWriteOff
	 */
	public boolean isCollectSavingsDuringWriteOff() {

		return collectSavingsDuringWriteOff;
	}

	/**
	 * Sets the collect savings during write off.
	 *
	 * @param collectSavingsDuringWriteOff the collectSavingsDuringWriteOff to set
	 */
	public void setCollectSavingsDuringWriteOff(boolean collectSavingsDuringWriteOff) {

		this.collectSavingsDuringWriteOff = collectSavingsDuringWriteOff;
	}

	/**
	 * Checks if is RPI insurance required.
	 *
	 * @return the isRPIInsuranceRequired
	 */
	public boolean isRPIInsuranceRequired() {

		return isRPIInsuranceRequired;
	}

	/**
	 * Sets the RPI insurance required.
	 *
	 * @param isRPIInsuranceRequired the isRPIInsuranceRequired to set
	 */
	public void setRPIInsuranceRequired(boolean isRPIInsuranceRequired) {

		this.isRPIInsuranceRequired = isRPIInsuranceRequired;
	}

	/**
	 * Checks if is cu account industry code required.
	 *
	 * @return the cuAccountIndustryCodeRequired
	 */
	public boolean isCuAccountIndustryCodeRequired() {

		return cuAccountIndustryCodeRequired;
	}

	/**
	 * Sets the cu account industry code required.
	 *
	 * @param cuAccountIndustryCodeRequired the cuAccountIndustryCodeRequired to set
	 */
	public void setCuAccountIndustryCodeRequired(boolean cuAccountIndustryCodeRequired) {

		this.cuAccountIndustryCodeRequired = cuAccountIndustryCodeRequired;
	}

	/**
	 * Checks if is allow loan issue override.
	 *
	 * @return the allowLoanIssueOverride
	 */
	public boolean isAllowLoanIssueOverride() {

		return allowLoanIssueOverride;
	}

	/**
	 * Sets the allow loan issue override.
	 *
	 * @param allowLoanIssueOverride the allowLoanIssueOverride to set
	 */
	public void setAllowLoanIssueOverride(boolean allowLoanIssueOverride) {

		this.allowLoanIssueOverride = allowLoanIssueOverride;
	}

	/**
	 * Checks if is used.
	 *
	 * @return the used
	 */
	public boolean isUsed() {

		return used;
	}

	/**
	 * Sets the used.
	 *
	 * @param used the used to set
	 */
	public void setUsed(boolean used) {

		this.used = used;
	}

	/**
	 * Checks if is include due in schedule recalc.
	 *
	 * @return the includeDueInScheduleRecalc
	 */
	public boolean isIncludeDueInScheduleRecalc() {

		return includeDueInScheduleRecalc;
	}

	/**
	 * Sets the include due in schedule recalc.
	 *
	 * @param includeDueInScheduleRecalc the includeDueInScheduleRecalc to set
	 */
	public void setIncludeDueInScheduleRecalc(boolean includeDueInScheduleRecalc) {

		this.includeDueInScheduleRecalc = includeDueInScheduleRecalc;
	}

	/**
	 * Checks if is attempt fee external.
	 *
	 * @return the attemptFeeExternal
	 */
	public boolean isAttemptFeeExternal() {

		return attemptFeeExternal;
	}

	/**
	 * Sets the attempt fee external.
	 *
	 * @param attemptFeeExternal the attemptFeeExternal to set
	 */
	public void setAttemptFeeExternal(boolean attemptFeeExternal) {

		this.attemptFeeExternal = attemptFeeExternal;
	}

	/**
	 * Gets the interest refund minimum period num.
	 *
	 * @return the interestRefundMinimumPeriodNum
	 */
	public int getInterestRefundMinimumPeriodNum() {

		return interestRefundMinimumPeriodNum;
	}

	/**
	 * Sets the interest refund minimum period num.
	 *
	 * @param interestRefundMinimumPeriodNum the interestRefundMinimumPeriodNum to set
	 */
	public void setInterestRefundMinimumPeriodNum(int interestRefundMinimumPeriodNum) {

		this.interestRefundMinimumPeriodNum = interestRefundMinimumPeriodNum;
	}

	/**
	 * Gets the interest rate period.
	 *
	 * @return the interestRatePeriod
	 */
	public int getInterestRatePeriod() {

		return interestRatePeriod;
	}

	/**
	 * Sets the interest rate period.
	 *
	 * @param interestRatePeriod the interestRatePeriod to set
	 */
	public void setInterestRatePeriod(int interestRatePeriod) {

		this.interestRatePeriod = interestRatePeriod;
	}

	/**
	 * Gets the interest rate period num.
	 *
	 * @return the interestRatePeriodNum
	 */
	public int getInterestRatePeriodNum() {

		return interestRatePeriodNum;
	}

	/**
	 * Sets the interest rate period num.
	 *
	 * @param interestRatePeriodNum the interestRatePeriodNum to set
	 */
	public void setInterestRatePeriodNum(int interestRatePeriodNum) {

		this.interestRatePeriodNum = interestRatePeriodNum;
	}

	/**
	 * Gets the flat rate interest period.
	 *
	 * @return the flatRateInterestPeriod
	 */
	public int getFlatRateInterestPeriod() {

		return flatRateInterestPeriod;
	}

	/**
	 * Sets the flat rate interest period.
	 *
	 * @param flatRateInterestPeriod the flatRateInterestPeriod to set
	 */
	public void setFlatRateInterestPeriod(int flatRateInterestPeriod) {

		this.flatRateInterestPeriod = flatRateInterestPeriod;
	}

	/**
	 * Gets the flat rate interest period num.
	 *
	 * @return the flatRateInterestPeriodNum
	 */
	public int getFlatRateInterestPeriodNum() {

		return flatRateInterestPeriodNum;
	}

	/**
	 * Sets the flat rate interest period num.
	 *
	 * @param flatRateInterestPeriodNum the flatRateInterestPeriodNum to set
	 */
	public void setFlatRateInterestPeriodNum(int flatRateInterestPeriodNum) {

		this.flatRateInterestPeriodNum = flatRateInterestPeriodNum;
	}

	/**
	 * Gets the flat rate interest rate.
	 *
	 * @return the flatRateInterestRate
	 */
	public double getFlatRateInterestRate() {

		return flatRateInterestRate;
	}

	/**
	 * Sets the flat rate interest rate.
	 *
	 * @param flatRateInterestRate the flatRateInterestRate to set
	 */
	public void setFlatRateInterestRate(double flatRateInterestRate) {

		this.flatRateInterestRate = flatRateInterestRate;
	}

	/**
	 * Checks if is use single loan agreement.
	 *
	 * @return the useSingleLoanAgreement
	 */
	public boolean isUseSingleLoanAgreement() {

		return useSingleLoanAgreement;
	}

	/**
	 * Sets the use single loan agreement.
	 *
	 * @param useSingleLoanAgreement the useSingleLoanAgreement to set
	 */
	public void setUseSingleLoanAgreement(boolean useSingleLoanAgreement) {

		this.useSingleLoanAgreement = useSingleLoanAgreement;
	}

	/**
	 * Checks if is allow payout with interest due.
	 *
	 * @return the allowPayoutWithInterestDue
	 */
	public boolean isAllowPayoutWithInterestDue() {

		return allowPayoutWithInterestDue;
	}

	/**
	 * Sets the allow payout with interest due.
	 *
	 * @param allowPayoutWithInterestDue the allowPayoutWithInterestDue to set
	 */
	public void setAllowPayoutWithInterestDue(boolean allowPayoutWithInterestDue) {

		this.allowPayoutWithInterestDue = allowPayoutWithInterestDue;
	}

	/**
	 * Checks if is allow payout with fees due.
	 *
	 * @return the allowPayoutWithFeesDue
	 */
	public boolean isAllowPayoutWithFeesDue() {

		return allowPayoutWithFeesDue;
	}

	/**
	 * Sets the allow payout with fees due.
	 *
	 * @param allowPayoutWithFeesDue the allowPayoutWithFeesDue to set
	 */
	public void setAllowPayoutWithFeesDue(boolean allowPayoutWithFeesDue) {

		this.allowPayoutWithFeesDue = allowPayoutWithFeesDue;
	}

	/**
	 * Checks if is seasonal loan.
	 *
	 * @return the isSeasonalLoan
	 */
	public boolean isSeasonalLoan() {

		return isSeasonalLoan;
	}

	/**
	 * Sets the seasonal loan.
	 *
	 * @param isSeasonalLoan the isSeasonalLoan to set
	 */
	public void setSeasonalLoan(boolean isSeasonalLoan) {

		this.isSeasonalLoan = isSeasonalLoan;
	}

	/**
	 * Checks if is use holidays and closed days in penalty interest calculations.
	 *
	 * @return the useHolidaysAndClosedDaysInPenaltyInterestCalculations
	 */
	public boolean isUseHolidaysAndClosedDaysInPenaltyInterestCalculations() {

		return useHolidaysAndClosedDaysInPenaltyInterestCalculations;
	}

	/**
	 * Sets the use holidays and closed days in penalty interest calculations.
	 *
	 * @param useHolidaysAndClosedDaysInPenaltyInterestCalculations the
	 *        useHolidaysAndClosedDaysInPenaltyInterestCalculations to set
	 */
	public void setUseHolidaysAndClosedDaysInPenaltyInterestCalculations(
			boolean useHolidaysAndClosedDaysInPenaltyInterestCalculations) {

		this.useHolidaysAndClosedDaysInPenaltyInterestCalculations =
				useHolidaysAndClosedDaysInPenaltyInterestCalculations;
	}

	/**
	 * Gets the deferred period value.
	 *
	 * @return the deferredPeriodValue
	 */
	public int getDeferredPeriodValue() {

		return deferredPeriodValue;
	}

	/**
	 * Sets the deferred period value.
	 *
	 * @param deferredPeriodValue the deferredPeriodValue to set
	 */
	public void setDeferredPeriodValue(int deferredPeriodValue) {

		this.deferredPeriodValue = deferredPeriodValue;
	}

	/**
	 * Gets the deferred period option ID.
	 *
	 * @return the deferredPeriodOptionID
	 */
	public int getDeferredPeriodOptionID() {

		return deferredPeriodOptionID;
	}

	/**
	 * Sets the deferred period option ID.
	 *
	 * @param deferredPeriodOptionID the deferredPeriodOptionID to set
	 */
	public void setDeferredPeriodOptionID(int deferredPeriodOptionID) {

		this.deferredPeriodOptionID = deferredPeriodOptionID;
	}

	/**
	 * Checks if is renew principal and interest.
	 *
	 * @return the renewPrincipalAndInterest
	 */
	public boolean isRenewPrincipalAndInterest() {

		return renewPrincipalAndInterest;
	}

	/**
	 * Sets the renew principal and interest.
	 *
	 * @param renewPrincipalAndInterest the renewPrincipalAndInterest to set
	 */
	public void setRenewPrincipalAndInterest(boolean renewPrincipalAndInterest) {

		this.renewPrincipalAndInterest = renewPrincipalAndInterest;
	}

	/**
	 * Checks if is cal interest accrual daily.
	 *
	 * @return the calInterestAccrualDaily
	 */
	public boolean isCalInterestAccrualDaily() {

		return calInterestAccrualDaily;
	}

	/**
	 * Sets the cal interest accrual daily.
	 *
	 * @param calInterestAccrualDaily the calInterestAccrualDaily to set
	 */
	public void setCalInterestAccrualDaily(boolean calInterestAccrualDaily) {

		this.calInterestAccrualDaily = calInterestAccrualDaily;
	}

	/**
	 * Checks if is separateon schedule 1.
	 *
	 * @return the separateonSchedule1
	 */
	public boolean isSeparateonSchedule1() {

		return separateonSchedule1;
	}

	/**
	 * Sets the separateon schedule 1.
	 *
	 * @param separateonSchedule1 the separateonSchedule1 to set
	 */
	public void setSeparateonSchedule1(boolean separateonSchedule1) {

		this.separateonSchedule1 = separateonSchedule1;
	}

	/**
	 * Gets the fee type 1.
	 *
	 * @return the feeType1
	 */
	public int getFeeType1() {

		return feeType1;
	}

	/**
	 * Sets the fee type 1.
	 *
	 * @param feeType1 the feeType1 to set
	 */
	public void setFeeType1(int feeType1) {

		this.feeType1 = feeType1;
	}

	/**
	 * Gets the fee type 2.
	 *
	 * @return the feeType2
	 */
	public int getFeeType2() {

		return feeType2;
	}

	/**
	 * Sets the fee type 2.
	 *
	 * @param feeType2 the feeType2 to set
	 */
	public void setFeeType2(int feeType2) {

		this.feeType2 = feeType2;
	}

	/**
	 * Checks if is use default interest rate by loan count.
	 *
	 * @return the isUseDefaultInterestRateByLoanCount
	 */
	public boolean isUseDefaultInterestRateByLoanCount() {

		return isUseDefaultInterestRateByLoanCount;
	}

	/**
	 * Sets the use default interest rate by loan count.
	 *
	 * @param isUseDefaultInterestRateByLoanCount the isUseDefaultInterestRateByLoanCount to set
	 */
	public void setUseDefaultInterestRateByLoanCount(boolean isUseDefaultInterestRateByLoanCount) {

		this.isUseDefaultInterestRateByLoanCount = isUseDefaultInterestRateByLoanCount;
	}

	/**
	 * Checks if is withhold due amount.
	 *
	 * @return the withholdDueAmount
	 */
	public boolean isWithholdDueAmount() {

		return withholdDueAmount;
	}

	/**
	 * Sets the withhold due amount.
	 *
	 * @param withholdDueAmount the withholdDueAmount to set
	 */
	public void setWithholdDueAmount(boolean withholdDueAmount) {

		this.withholdDueAmount = withholdDueAmount;
	}

	/**
	 * Checks if is prepay next due only.
	 *
	 * @return the prepayNextDueOnly
	 */
	public boolean isPrepayNextDueOnly() {

		return prepayNextDueOnly;
	}

	/**
	 * Sets the prepay next due only.
	 *
	 * @param prepayNextDueOnly the prepayNextDueOnly to set
	 */
	public void setPrepayNextDueOnly(boolean prepayNextDueOnly) {

		this.prepayNextDueOnly = prepayNextDueOnly;
	}

	/**
	 * Checks if is rescheduleon overpayment.
	 *
	 * @return the rescheduleonOverpayment
	 */
	public boolean isRescheduleonOverpayment() {

		return rescheduleonOverpayment;
	}

	/**
	 * Sets the rescheduleon overpayment.
	 *
	 * @param rescheduleonOverpayment the rescheduleonOverpayment to set
	 */
	public void setRescheduleonOverpayment(boolean rescheduleonOverpayment) {

		this.rescheduleonOverpayment = rescheduleonOverpayment;
	}

	/**
	 * Checks if is penalty per periodic arrears.
	 *
	 * @return the penaltyPerPeriodicArrears
	 */
	public boolean isPenaltyPerPeriodicArrears() {

		return penaltyPerPeriodicArrears;
	}

	/**
	 * Sets the penalty per periodic arrears.
	 *
	 * @param penaltyPerPeriodicArrears the penaltyPerPeriodicArrears to set
	 */
	public void setPenaltyPerPeriodicArrears(boolean penaltyPerPeriodicArrears) {

		this.penaltyPerPeriodicArrears = penaltyPerPeriodicArrears;
	}

	/**
	 * Checks if is transfer accrue interest only.
	 *
	 * @return the transferAccrueInterestOnly
	 */
	public boolean isTransferAccrueInterestOnly() {

		return transferAccrueInterestOnly;
	}

	/**
	 * Sets the transfer accrue interest only.
	 *
	 * @param transferAccrueInterestOnly the transferAccrueInterestOnly to set
	 */
	public void setTransferAccrueInterestOnly(boolean transferAccrueInterestOnly) {

		this.transferAccrueInterestOnly = transferAccrueInterestOnly;
	}

	/**
	 * Checks if is transfer accrue interest only on schedule dates.
	 *
	 * @return the transferAccrueInterestOnlyOnScheduleDates
	 */
	public boolean isTransferAccrueInterestOnlyOnScheduleDates() {

		return transferAccrueInterestOnlyOnScheduleDates;
	}

	/**
	 * Sets the transfer accrue interest only on schedule dates.
	 *
	 * @param transferAccrueInterestOnlyOnScheduleDates the
	 *        transferAccrueInterestOnlyOnScheduleDates to set
	 */
	public void setTransferAccrueInterestOnlyOnScheduleDates(
			boolean transferAccrueInterestOnlyOnScheduleDates) {

		this.transferAccrueInterestOnlyOnScheduleDates = transferAccrueInterestOnlyOnScheduleDates;
	}

	/**
	 * Gets the insurance product ID.
	 *
	 * @return the insuranceProductID
	 */
	public int getInsuranceProductID() {

		return insuranceProductID;
	}

	/**
	 * Sets the insurance product ID.
	 *
	 * @param insuranceProductID the insuranceProductID to set
	 */
	public void setInsuranceProductID(int insuranceProductID) {

		this.insuranceProductID = insuranceProductID;
	}

	/**
	 * Checks if is insurance mandatory.
	 *
	 * @return the isInsuranceMandatory
	 */
	public boolean isInsuranceMandatory() {

		return isInsuranceMandatory;
	}

	/**
	 * Sets the insurance mandatory.
	 *
	 * @param isInsuranceMandatory the isInsuranceMandatory to set
	 */
	public void setInsuranceMandatory(boolean isInsuranceMandatory) {

		this.isInsuranceMandatory = isInsuranceMandatory;
	}

	/**
	 * Checks if is use tiered linked savings payment.
	 *
	 * @return the useTieredLinkedSavingsPayment
	 */
	public boolean isUseTieredLinkedSavingsPayment() {

		return useTieredLinkedSavingsPayment;
	}

	/**
	 * Sets the use tiered linked savings payment.
	 *
	 * @param useTieredLinkedSavingsPayment the useTieredLinkedSavingsPayment to set
	 */
	public void setUseTieredLinkedSavingsPayment(boolean useTieredLinkedSavingsPayment) {

		this.useTieredLinkedSavingsPayment = useTieredLinkedSavingsPayment;
	}

	/**
	 * Gets the number of reviews.
	 *
	 * @return the numberOfReviews
	 */
	public int getNumberOfReviews() {

		return numberOfReviews;
	}

	/**
	 * Sets the number of reviews.
	 *
	 * @param numberOfReviews the numberOfReviews to set
	 */
	public void setNumberOfReviews(int numberOfReviews) {

		this.numberOfReviews = numberOfReviews;
	}

	/**
	 * Checks if is loan district code required.
	 *
	 * @return the loanDistrictCodeRequired
	 */
	public boolean isLoanDistrictCodeRequired() {

		return loanDistrictCodeRequired;
	}

	/**
	 * Sets the loan district code required.
	 *
	 * @param loanDistrictCodeRequired the loanDistrictCodeRequired to set
	 */
	public void setLoanDistrictCodeRequired(boolean loanDistrictCodeRequired) {

		this.loanDistrictCodeRequired = loanDistrictCodeRequired;
	}

	/**
	 * Checks if is use fixed penalty.
	 *
	 * @return the useFixedPenalty
	 */
	public boolean isUseFixedPenalty() {

		return useFixedPenalty;
	}

	/**
	 * Sets the use fixed penalty.
	 *
	 * @param useFixedPenalty the useFixedPenalty to set
	 */
	public void setUseFixedPenalty(boolean useFixedPenalty) {

		this.useFixedPenalty = useFixedPenalty;
	}

	/**
	 * Checks if is use credit scoring limits.
	 *
	 * @return the useCreditScoringLimits
	 */
	public boolean isUseCreditScoringLimits() {

		return useCreditScoringLimits;
	}

	/**
	 * Sets the use credit scoring limits.
	 *
	 * @param useCreditScoringLimits the useCreditScoringLimits to set
	 */
	public void setUseCreditScoringLimits(boolean useCreditScoringLimits) {

		this.useCreditScoringLimits = useCreditScoringLimits;
	}

	/**
	 * Gets the interest rounding adjustment type.
	 *
	 * @return the interestRoundingAdjustmentType
	 */
	public int getInterestRoundingAdjustmentType() {

		return interestRoundingAdjustmentType;
	}

	/**
	 * Sets the interest rounding adjustment type.
	 *
	 * @param interestRoundingAdjustmentType the interestRoundingAdjustmentType to set
	 */
	public void setInterestRoundingAdjustmentType(int interestRoundingAdjustmentType) {

		this.interestRoundingAdjustmentType = interestRoundingAdjustmentType;
	}

	/**
	 * Checks if is declined interest.
	 *
	 * @return the declinedInterest
	 */
	public boolean isDeclinedInterest() {

		return declinedInterest;
	}

	/**
	 * Sets the declined interest.
	 *
	 * @param declinedInterest the declinedInterest to set
	 */
	public void setDeclinedInterest(boolean declinedInterest) {

		this.declinedInterest = declinedInterest;
	}

	/**
	 * Checks if is half balloon payments.
	 *
	 * @return the halfBalloonPayments
	 */
	public boolean isHalfBalloonPayments() {

		return halfBalloonPayments;
	}

	/**
	 * Sets the half balloon payments.
	 *
	 * @param halfBalloonPayments the halfBalloonPayments to set
	 */
	public void setHalfBalloonPayments(boolean halfBalloonPayments) {

		this.halfBalloonPayments = halfBalloonPayments;
	}

	/**
	 * Checks if is prevent change account portfolio on accounts.
	 *
	 * @return the preventChangeAccountPortfolioOnAccounts
	 */
	public boolean isPreventChangeAccountPortfolioOnAccounts() {

		return preventChangeAccountPortfolioOnAccounts;
	}

	/**
	 * Sets the prevent change account portfolio on accounts.
	 *
	 * @param preventChangeAccountPortfolioOnAccounts the preventChangeAccountPortfolioOnAccounts to
	 *        set
	 */
	public void setPreventChangeAccountPortfolioOnAccounts(
			boolean preventChangeAccountPortfolioOnAccounts) {

		this.preventChangeAccountPortfolioOnAccounts = preventChangeAccountPortfolioOnAccounts;
	}

	/**
	 * Gets the credit control ID.
	 *
	 * @return the creditControlID
	 */
	public int getCreditControlID() {

		return creditControlID;
	}

	/**
	 * Sets the credit control ID.
	 *
	 * @param creditControlID the creditControlID to set
	 */
	public void setCreditControlID(int creditControlID) {

		this.creditControlID = creditControlID;
	}

	/**
	 * Gets the credit control woff ID.
	 *
	 * @return the creditControlWoffID
	 */
	public int getCreditControlWoffID() {

		return creditControlWoffID;
	}

	/**
	 * Sets the credit control woff ID.
	 *
	 * @param creditControlWoffID the creditControlWoffID to set
	 */
	public void setCreditControlWoffID(int creditControlWoffID) {

		this.creditControlWoffID = creditControlWoffID;
	}

	/**
	 * Checks if is ignore term and issue during restructure.
	 *
	 * @return the ignoreTermAndIssueDuringRestructure
	 */
	public boolean isIgnoreTermAndIssueDuringRestructure() {

		return ignoreTermAndIssueDuringRestructure;
	}

	/**
	 * Sets the ignore term and issue during restructure.
	 *
	 * @param ignoreTermAndIssueDuringRestructure the ignoreTermAndIssueDuringRestructure to set
	 */
	public void setIgnoreTermAndIssueDuringRestructure(
			boolean ignoreTermAndIssueDuringRestructure) {

		this.ignoreTermAndIssueDuringRestructure = ignoreTermAndIssueDuringRestructure;
	}

	/**
	 * Gets the collect linked savings payment for.
	 *
	 * @return the collectLinkedSavingsPaymentFor
	 */
	public int getCollectLinkedSavingsPaymentFor() {

		return collectLinkedSavingsPaymentFor;
	}

	/**
	 * Sets the collect linked savings payment for.
	 *
	 * @param collectLinkedSavingsPaymentFor the collectLinkedSavingsPaymentFor to set
	 */
	public void setCollectLinkedSavingsPaymentFor(int collectLinkedSavingsPaymentFor) {

		this.collectLinkedSavingsPaymentFor = collectLinkedSavingsPaymentFor;
	}

	/**
	 * Checks if is check percentage of loan amount in linked savings.
	 *
	 * @return the checkPercentageOfLoanAmountInLinkedSavings
	 */
	public boolean isCheckPercentageOfLoanAmountInLinkedSavings() {

		return checkPercentageOfLoanAmountInLinkedSavings;
	}

	/**
	 * Sets the check percentage of loan amount in linked savings.
	 *
	 * @param checkPercentageOfLoanAmountInLinkedSavings the
	 *        checkPercentageOfLoanAmountInLinkedSavings to set
	 */
	public void setCheckPercentageOfLoanAmountInLinkedSavings(
			boolean checkPercentageOfLoanAmountInLinkedSavings) {

		this.checkPercentageOfLoanAmountInLinkedSavings =
				checkPercentageOfLoanAmountInLinkedSavings;
	}

	/**
	 * Gets the linked savings percentage of loan.
	 *
	 * @return the linkedSavingsPercentageOfLoan
	 */
	public double getLinkedSavingsPercentageOfLoan() {

		return linkedSavingsPercentageOfLoan;
	}

	/**
	 * Sets the linked savings percentage of loan.
	 *
	 * @param linkedSavingsPercentageOfLoan the linkedSavingsPercentageOfLoan to set
	 */
	public void setLinkedSavingsPercentageOfLoan(double linkedSavingsPercentageOfLoan) {

		this.linkedSavingsPercentageOfLoan = linkedSavingsPercentageOfLoan;
	}

	/**
	 * Checks if is mandatory account activity.
	 *
	 * @return the mandatoryAccountActivity
	 */
	public boolean isMandatoryAccountActivity() {

		return mandatoryAccountActivity;
	}

	/**
	 * Sets the mandatory account activity.
	 *
	 * @param mandatoryAccountActivity the mandatoryAccountActivity to set
	 */
	public void setMandatoryAccountActivity(boolean mandatoryAccountActivity) {

		this.mandatoryAccountActivity = mandatoryAccountActivity;
	}

	/**
	 * Gets the mandatory account activity period ID.
	 *
	 * @return the mandatoryAccountActivityPeriodID
	 */
	public int getMandatoryAccountActivityPeriodID() {

		return mandatoryAccountActivityPeriodID;
	}

	/**
	 * Sets the mandatory account activity period ID.
	 *
	 * @param mandatoryAccountActivityPeriodID the mandatoryAccountActivityPeriodID to set
	 */
	public void setMandatoryAccountActivityPeriodID(int mandatoryAccountActivityPeriodID) {

		this.mandatoryAccountActivityPeriodID = mandatoryAccountActivityPeriodID;
	}

	/**
	 * Gets the mandatory account activity period num.
	 *
	 * @return the mandatoryAccountActivityPeriodNum
	 */
	public int getMandatoryAccountActivityPeriodNum() {

		return mandatoryAccountActivityPeriodNum;
	}

	/**
	 * Sets the mandatory account activity period num.
	 *
	 * @param mandatoryAccountActivityPeriodNum the mandatoryAccountActivityPeriodNum to set
	 */
	public void setMandatoryAccountActivityPeriodNum(int mandatoryAccountActivityPeriodNum) {

		this.mandatoryAccountActivityPeriodNum = mandatoryAccountActivityPeriodNum;
	}

	/**
	 * Gets the number of transactions.
	 *
	 * @return the numberOfTransactions
	 */
	public int getNumberOfTransactions() {

		return numberOfTransactions;
	}

	/**
	 * Sets the number of transactions.
	 *
	 * @param numberOfTransactions the numberOfTransactions to set
	 */
	public void setNumberOfTransactions(int numberOfTransactions) {

		this.numberOfTransactions = numberOfTransactions;
	}

	/**
	 * Checks if is mandatory savings required.
	 *
	 * @return the mandatorySavingsRequired
	 */
	public boolean isMandatorySavingsRequired() {

		return mandatorySavingsRequired;
	}

	/**
	 * Sets the mandatory savings required.
	 *
	 * @param mandatorySavingsRequired the mandatorySavingsRequired to set
	 */
	public void setMandatorySavingsRequired(boolean mandatorySavingsRequired) {

		this.mandatorySavingsRequired = mandatorySavingsRequired;
	}

	/**
	 * Gets the mandatory savings amount.
	 *
	 * @return the mandatorySavingsAmount
	 */
	public double getMandatorySavingsAmount() {

		return mandatorySavingsAmount;
	}

	/**
	 * Sets the mandatory savings amount.
	 *
	 * @param mandatorySavingsAmount the mandatorySavingsAmount to set
	 */
	public void setMandatorySavingsAmount(double mandatorySavingsAmount) {

		this.mandatorySavingsAmount = mandatorySavingsAmount;
	}

	/**
	 * Checks if is separate installments in schedule.
	 *
	 * @return the isSeparateInstallmentsInSchedule
	 */
	public boolean isSeparateInstallmentsInSchedule() {

		return isSeparateInstallmentsInSchedule;
	}

	/**
	 * Sets the separate installments in schedule.
	 *
	 * @param isSeparateInstallmentsInSchedule the isSeparateInstallmentsInSchedule to set
	 */
	public void setSeparateInstallmentsInSchedule(boolean isSeparateInstallmentsInSchedule) {

		this.isSeparateInstallmentsInSchedule = isSeparateInstallmentsInSchedule;
	}

	/**
	 * Checks if is transfer frm mandatory to linked savings.
	 *
	 * @return the transferFrmMandatoryToLinkedSavings
	 */
	public boolean isTransferFrmMandatoryToLinkedSavings() {

		return transferFrmMandatoryToLinkedSavings;
	}

	/**
	 * Sets the transfer frm mandatory to linked savings.
	 *
	 * @param transferFrmMandatoryToLinkedSavings the transferFrmMandatoryToLinkedSavings to set
	 */
	public void setTransferFrmMandatoryToLinkedSavings(
			boolean transferFrmMandatoryToLinkedSavings) {

		this.transferFrmMandatoryToLinkedSavings = transferFrmMandatoryToLinkedSavings;
	}

	/**
	 * Checks if is calculate payment percentage based on original loan amount.
	 *
	 * @return the calculatePaymentPercentageBasedOnOriginalLoanAmount
	 */
	public boolean isCalculatePaymentPercentageBasedOnOriginalLoanAmount() {

		return calculatePaymentPercentageBasedOnOriginalLoanAmount;
	}

	/**
	 * Sets the calculate payment percentage based on original loan amount.
	 *
	 * @param calculatePaymentPercentageBasedOnOriginalLoanAmount the
	 *        calculatePaymentPercentageBasedOnOriginalLoanAmount to set
	 */
	public void setCalculatePaymentPercentageBasedOnOriginalLoanAmount(
			boolean calculatePaymentPercentageBasedOnOriginalLoanAmount) {

		this.calculatePaymentPercentageBasedOnOriginalLoanAmount =
				calculatePaymentPercentageBasedOnOriginalLoanAmount;
	}

	/**
	 * Checks if is allow mandatory override.
	 *
	 * @return the allowMandatoryOverride
	 */
	public boolean isAllowMandatoryOverride() {

		return allowMandatoryOverride;
	}

	/**
	 * Sets the allow mandatory override.
	 *
	 * @param allowMandatoryOverride the allowMandatoryOverride to set
	 */
	public void setAllowMandatoryOverride(boolean allowMandatoryOverride) {

		this.allowMandatoryOverride = allowMandatoryOverride;
	}

	/**
	 * Gets the nth installment period.
	 *
	 * @return the nthInstallmentPeriod
	 */
	public int getNthInstallmentPeriod() {

		return nthInstallmentPeriod;
	}

	/**
	 * Sets the nth installment period.
	 *
	 * @param nthInstallmentPeriod the nthInstallmentPeriod to set
	 */
	public void setNthInstallmentPeriod(int nthInstallmentPeriod) {

		this.nthInstallmentPeriod = nthInstallmentPeriod;
	}

	/**
	 * Checks if is allow top up in arrears.
	 *
	 * @return the allowTopUpInArrears
	 */
	public boolean isAllowTopUpInArrears() {

		return allowTopUpInArrears;
	}

	/**
	 * Sets the allow top up in arrears.
	 *
	 * @param allowTopUpInArrears the allowTopUpInArrears to set
	 */
	public void setAllowTopUpInArrears(boolean allowTopUpInArrears) {

		this.allowTopUpInArrears = allowTopUpInArrears;
	}

	/**
	 * Checks if is not set same account for linked and mandatory savings.
	 *
	 * @return the notSetSameAccountForLinkedAndMandatorySavings
	 */
	public boolean isNotSetSameAccountForLinkedAndMandatorySavings() {

		return notSetSameAccountForLinkedAndMandatorySavings;
	}

	/**
	 * Sets the not set same account for linked and mandatory savings.
	 *
	 * @param notSetSameAccountForLinkedAndMandatorySavings the
	 *        notSetSameAccountForLinkedAndMandatorySavings to set
	 */
	public void setNotSetSameAccountForLinkedAndMandatorySavings(
			boolean notSetSameAccountForLinkedAndMandatorySavings) {

		this.notSetSameAccountForLinkedAndMandatorySavings =
				notSetSameAccountForLinkedAndMandatorySavings;
	}

	/**
	 * Gets the fixed penalty grace periods.
	 *
	 * @return the fixedPenaltyGracePeriods
	 */
	public int getFixedPenaltyGracePeriods() {

		return fixedPenaltyGracePeriods;
	}

	/**
	 * Sets the fixed penalty grace periods.
	 *
	 * @param fixedPenaltyGracePeriods the fixedPenaltyGracePeriods to set
	 */
	public void setFixedPenaltyGracePeriods(int fixedPenaltyGracePeriods) {

		this.fixedPenaltyGracePeriods = fixedPenaltyGracePeriods;
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
	 * Gets the effective interest rate method ID.
	 *
	 * @return the effectiveInterestRateMethodID
	 */
	public int getEffectiveInterestRateMethodID() {

		return effectiveInterestRateMethodID;
	}

	/**
	 * Sets the effective interest rate method ID.
	 *
	 * @param effectiveInterestRateMethodID the effectiveInterestRateMethodID to set
	 */
	public void setEffectiveInterestRateMethodID(int effectiveInterestRateMethodID) {

		this.effectiveInterestRateMethodID = effectiveInterestRateMethodID;
	}

	/**
	 * Gets the apr calculation method ID.
	 *
	 * @return the aprCalculationMethodID
	 */
	public int getAprCalculationMethodID() {

		return aprCalculationMethodID;
	}

	/**
	 * Sets the apr calculation method ID.
	 *
	 * @param aprCalculationMethodID the aprCalculationMethodID to set
	 */
	public void setAprCalculationMethodID(int aprCalculationMethodID) {

		this.aprCalculationMethodID = aprCalculationMethodID;
	}

	/**
	 * Gets the allow issue more than approved amount.
	 *
	 * @return the allowIssueMoreThanApprovedAmount
	 */
	public int getAllowIssueMoreThanApprovedAmount() {

		return allowIssueMoreThanApprovedAmount;
	}

	/**
	 * Sets the allow issue more than approved amount.
	 *
	 * @param allowIssueMoreThanApprovedAmount the allowIssueMoreThanApprovedAmount to set
	 */
	public void setAllowIssueMoreThanApprovedAmount(int allowIssueMoreThanApprovedAmount) {

		this.allowIssueMoreThanApprovedAmount = allowIssueMoreThanApprovedAmount;
	}

	/**
	 * Gets the allow issue less than approved amount.
	 *
	 * @return the allowIssueLessThanApprovedAmount
	 */
	public int getAllowIssueLessThanApprovedAmount() {

		return allowIssueLessThanApprovedAmount;
	}

	/**
	 * Sets the allow issue less than approved amount.
	 *
	 * @param allowIssueLessThanApprovedAmount the allowIssueLessThanApprovedAmount to set
	 */
	public void setAllowIssueLessThanApprovedAmount(int allowIssueLessThanApprovedAmount) {

		this.allowIssueLessThanApprovedAmount = allowIssueLessThanApprovedAmount;
	}

	/**
	 * Checks if is not tocollect future interest in payout and transfer.
	 *
	 * @return the notTocollectFutureInterestInPayoutAndTransfer
	 */
	public boolean isNotTocollectFutureInterestInPayoutAndTransfer() {

		return notTocollectFutureInterestInPayoutAndTransfer;
	}

	/**
	 * Sets the not tocollect future interest in payout and transfer.
	 *
	 * @param notTocollectFutureInterestInPayoutAndTransfer the
	 *        notTocollectFutureInterestInPayoutAndTransfer to set
	 */
	public void setNotTocollectFutureInterestInPayoutAndTransfer(
			boolean notTocollectFutureInterestInPayoutAndTransfer) {

		this.notTocollectFutureInterestInPayoutAndTransfer =
				notTocollectFutureInterestInPayoutAndTransfer;
	}

	/**
	 * Checks if is product limits by customers branch.
	 *
	 * @return the productLimitsByCustomersBranch
	 */
	public boolean isProductLimitsByCustomersBranch() {

		return productLimitsByCustomersBranch;
	}

	/**
	 * Sets the product limits by customers branch.
	 *
	 * @param productLimitsByCustomersBranch the productLimitsByCustomersBranch to set
	 */
	public void setProductLimitsByCustomersBranch(boolean productLimitsByCustomersBranch) {

		this.productLimitsByCustomersBranch = productLimitsByCustomersBranch;
	}

	/**
	 * Checks if is collect penalty by outstanding period.
	 *
	 * @return the collectPenaltyByOutstandingPeriod
	 */
	public boolean isCollectPenaltyByOutstandingPeriod() {

		return collectPenaltyByOutstandingPeriod;
	}

	/**
	 * Sets the collect penalty by outstanding period.
	 *
	 * @param collectPenaltyByOutstandingPeriod the collectPenaltyByOutstandingPeriod to set
	 */
	public void setCollectPenaltyByOutstandingPeriod(boolean collectPenaltyByOutstandingPeriod) {

		this.collectPenaltyByOutstandingPeriod = collectPenaltyByOutstandingPeriod;
	}

	/**
	 * Checks if is use restructered product control account.
	 *
	 * @return the useRestructeredProductControlAccount
	 */
	public boolean isUseRestructeredProductControlAccount() {

		return useRestructeredProductControlAccount;
	}

	/**
	 * Sets the use restructered product control account.
	 *
	 * @param useRestructeredProductControlAccount the useRestructeredProductControlAccount to set
	 */
	public void setUseRestructeredProductControlAccount(
			boolean useRestructeredProductControlAccount) {

		this.useRestructeredProductControlAccount = useRestructeredProductControlAccount;
	}

	/**
	 * Gets the linked savings product ID.
	 *
	 * @return the linkedSavingsProductID
	 */
	public int getLinkedSavingsProductID() {

		return linkedSavingsProductID;
	}

	/**
	 * Sets the linked savings product ID.
	 *
	 * @param linkedSavingsProductID the linkedSavingsProductID to set
	 */
	public void setLinkedSavingsProductID(int linkedSavingsProductID) {

		this.linkedSavingsProductID = linkedSavingsProductID;
	}

	/**
	 * Checks if is loan reason requried.
	 *
	 * @return the loanReasonRequried
	 */
	public boolean isLoanReasonRequried() {

		return loanReasonRequried;
	}

	/**
	 * Sets the loan reason requried.
	 *
	 * @param loanReasonRequried the loanReasonRequried to set
	 */
	public void setLoanReasonRequried(boolean loanReasonRequried) {

		this.loanReasonRequried = loanReasonRequried;
	}

	/**
	 * Checks if is internet banking.
	 *
	 * @return the isInternetBanking
	 */
	public boolean isInternetBanking() {

		return isInternetBanking;
	}

	/**
	 * Sets the internet banking.
	 *
	 * @param isInternetBanking the isInternetBanking to set
	 */
	public void setInternetBanking(boolean isInternetBanking) {

		this.isInternetBanking = isInternetBanking;
	}

	/**
	 * Checks if is split outstanding interest.
	 *
	 * @return the splitOutstandingInterest
	 */
	public boolean isSplitOutstandingInterest() {

		return splitOutstandingInterest;
	}

	/**
	 * Sets the split outstanding interest.
	 *
	 * @param splitOutstandingInterest the splitOutstandingInterest to set
	 */
	public void setSplitOutstandingInterest(boolean splitOutstandingInterest) {

		this.splitOutstandingInterest = splitOutstandingInterest;
	}

	/**
	 * Checks if is use default day count fraction by loan term.
	 *
	 * @return the useDefaultDayCountFractionByLoanTerm
	 */
	public boolean isUseDefaultDayCountFractionByLoanTerm() {

		return useDefaultDayCountFractionByLoanTerm;
	}

	/**
	 * Sets the use default day count fraction by loan term.
	 *
	 * @param useDefaultDayCountFractionByLoanTerm the useDefaultDayCountFractionByLoanTerm to set
	 */
	public void setUseDefaultDayCountFractionByLoanTerm(
			boolean useDefaultDayCountFractionByLoanTerm) {

		this.useDefaultDayCountFractionByLoanTerm = useDefaultDayCountFractionByLoanTerm;
	}

	/**
	 * Gets the early payout fee type.
	 *
	 * @return the earlyPayoutFeeType
	 */
	public int getEarlyPayoutFeeType() {

		return earlyPayoutFeeType;
	}

	/**
	 * Sets the early payout fee type.
	 *
	 * @param earlyPayoutFeeType the earlyPayoutFeeType to set
	 */
	public void setEarlyPayoutFeeType(int earlyPayoutFeeType) {

		this.earlyPayoutFeeType = earlyPayoutFeeType;
	}

	/**
	 * Gets the early payout fee grace period ID.
	 *
	 * @return the earlyPayoutFeeGracePeriodID
	 */
	public int getEarlyPayoutFeeGracePeriodID() {

		return earlyPayoutFeeGracePeriodID;
	}

	/**
	 * Sets the early payout fee grace period ID.
	 *
	 * @param earlyPayoutFeeGracePeriodID the earlyPayoutFeeGracePeriodID to set
	 */
	public void setEarlyPayoutFeeGracePeriodID(int earlyPayoutFeeGracePeriodID) {

		this.earlyPayoutFeeGracePeriodID = earlyPayoutFeeGracePeriodID;
	}

	/**
	 * Gets the early payout fee grace period num.
	 *
	 * @return the earlyPayoutFeeGracePeriodNum
	 */
	public int getEarlyPayoutFeeGracePeriodNum() {

		return earlyPayoutFeeGracePeriodNum;
	}

	/**
	 * Sets the early payout fee grace period num.
	 *
	 * @param earlyPayoutFeeGracePeriodNum the earlyPayoutFeeGracePeriodNum to set
	 */
	public void setEarlyPayoutFeeGracePeriodNum(int earlyPayoutFeeGracePeriodNum) {

		this.earlyPayoutFeeGracePeriodNum = earlyPayoutFeeGracePeriodNum;
	}

	/**
	 * Gets the provision method.
	 *
	 * @return the provisionMethod
	 */
	public int getProvisionMethod() {

		return provisionMethod;
	}

	/**
	 * Sets the provision method.
	 *
	 * @param provisionMethod the provisionMethod to set
	 */
	public void setProvisionMethod(int provisionMethod) {

		this.provisionMethod = provisionMethod;
	}

	/**
	 * Checks if is source of fund mandatory.
	 *
	 * @return the isSourceOfFundMandatory
	 */
	public boolean isSourceOfFundMandatory() {

		return isSourceOfFundMandatory;
	}

	/**
	 * Sets the source of fund mandatory.
	 *
	 * @param isSourceOfFundMandatory the isSourceOfFundMandatory to set
	 */
	public void setSourceOfFundMandatory(boolean isSourceOfFundMandatory) {

		this.isSourceOfFundMandatory = isSourceOfFundMandatory;
	}

	/**
	 * Checks if is allow cooling period.
	 *
	 * @return the allowCoolingPeriod
	 */
	public boolean isAllowCoolingPeriod() {

		return allowCoolingPeriod;
	}

	/**
	 * Sets the allow cooling period.
	 *
	 * @param allowCoolingPeriod the allowCoolingPeriod to set
	 */
	public void setAllowCoolingPeriod(boolean allowCoolingPeriod) {

		this.allowCoolingPeriod = allowCoolingPeriod;
	}

	/**
	 * Gets the nth cooling period.
	 *
	 * @return the nthCoolingPeriod
	 */
	public int getNthCoolingPeriod() {

		return nthCoolingPeriod;
	}

	/**
	 * Sets the nth cooling period.
	 *
	 * @param nthCoolingPeriod the nthCoolingPeriod to set
	 */
	public void setNthCoolingPeriod(int nthCoolingPeriod) {

		this.nthCoolingPeriod = nthCoolingPeriod;
	}

	/**
	 * Gets the cooling period.
	 *
	 * @return the coolingPeriod
	 */
	public int getCoolingPeriod() {

		return coolingPeriod;
	}

	/**
	 * Sets the cooling period.
	 *
	 * @param coolingPeriod the coolingPeriod to set
	 */
	public void setCoolingPeriod(int coolingPeriod) {

		this.coolingPeriod = coolingPeriod;
	}

	/**
	 * Checks if is ignore closed holidays for flat loans.
	 *
	 * @return the ignoreClosedHolidaysForFlatLoans
	 */
	public boolean isIgnoreClosedHolidaysForFlatLoans() {

		return ignoreClosedHolidaysForFlatLoans;
	}

	/**
	 * Sets the ignore closed holidays for flat loans.
	 *
	 * @param ignoreClosedHolidaysForFlatLoans the ignoreClosedHolidaysForFlatLoans to set
	 */
	public void setIgnoreClosedHolidaysForFlatLoans(boolean ignoreClosedHolidaysForFlatLoans) {

		this.ignoreClosedHolidaysForFlatLoans = ignoreClosedHolidaysForFlatLoans;
	}

	/**
	 * Checks if is use guarantor type restriction.
	 *
	 * @return the useGuarantorTypeRestriction
	 */
	public boolean isUseGuarantorTypeRestriction() {

		return useGuarantorTypeRestriction;
	}

	/**
	 * Sets the use guarantor type restriction.
	 *
	 * @param useGuarantorTypeRestriction the useGuarantorTypeRestriction to set
	 */
	public void setUseGuarantorTypeRestriction(boolean useGuarantorTypeRestriction) {

		this.useGuarantorTypeRestriction = useGuarantorTypeRestriction;
	}

	/**
	 * Checks if is deduct unposted deferred fees in provision.
	 *
	 * @return the deductUnpostedDeferredFeesInProvision
	 */
	public boolean isDeductUnpostedDeferredFeesInProvision() {

		return deductUnpostedDeferredFeesInProvision;
	}

	/**
	 * Sets the deduct unposted deferred fees in provision.
	 *
	 * @param deductUnpostedDeferredFeesInProvision the deductUnpostedDeferredFeesInProvision to set
	 */
	public void setDeductUnpostedDeferredFeesInProvision(
			boolean deductUnpostedDeferredFeesInProvision) {

		this.deductUnpostedDeferredFeesInProvision = deductUnpostedDeferredFeesInProvision;
	}

	/**
	 * Gets the provision against.
	 *
	 * @return the provisionAgainst
	 */
	public int getProvisionAgainst() {

		return provisionAgainst;
	}

	/**
	 * Sets the provision against.
	 *
	 * @param provisionAgainst the provisionAgainst to set
	 */
	public void setProvisionAgainst(int provisionAgainst) {

		this.provisionAgainst = provisionAgainst;
	}

	/**
	 * Checks if is include interest due till the run date for the scheduled interest loans in
	 * provision.
	 *
	 * @return the includeInterestDueTillTheRunDateForTheScheduledInterestLoansInProvision
	 */
	public boolean isIncludeInterestDueTillTheRunDateForTheScheduledInterestLoansInProvision() {

		return includeInterestDueTillTheRunDateForTheScheduledInterestLoansInProvision;
	}

	/**
	 * Sets the include interest due till the run date for the scheduled interest loans in
	 * provision.
	 *
	 * @param includeInterestDueTillTheRunDateForTheScheduledInterestLoansInProvision the
	 *        includeInterestDueTillTheRunDateForTheScheduledInterestLoansInProvision to set
	 */
	public void setIncludeInterestDueTillTheRunDateForTheScheduledInterestLoansInProvision(
			boolean includeInterestDueTillTheRunDateForTheScheduledInterestLoansInProvision) {

		this.includeInterestDueTillTheRunDateForTheScheduledInterestLoansInProvision =
				includeInterestDueTillTheRunDateForTheScheduledInterestLoansInProvision;
	}

	/**
	 * Checks if is exclude payment account balance in provision.
	 *
	 * @return the excludePaymentAccountBalanceInProvision
	 */
	public boolean isExcludePaymentAccountBalanceInProvision() {

		return excludePaymentAccountBalanceInProvision;
	}

	/**
	 * Sets the exclude payment account balance in provision.
	 *
	 * @param excludePaymentAccountBalanceInProvision the excludePaymentAccountBalanceInProvision to
	 *        set
	 */
	public void setExcludePaymentAccountBalanceInProvision(
			boolean excludePaymentAccountBalanceInProvision) {

		this.excludePaymentAccountBalanceInProvision = excludePaymentAccountBalanceInProvision;
	}

	/**
	 * Gets the allow override payout fee.
	 *
	 * @return the allowOverridePayoutFee
	 */
	public int getAllowOverridePayoutFee() {

		return allowOverridePayoutFee;
	}

	/**
	 * Sets the allow override payout fee.
	 *
	 * @param allowOverridePayoutFee the allowOverridePayoutFee to set
	 */
	public void setAllowOverridePayoutFee(int allowOverridePayoutFee) {

		this.allowOverridePayoutFee = allowOverridePayoutFee;
	}

	/**
	 * Gets the ib product details.
	 *
	 * @return the ibProductDetails
	 */
	public Object getIbProductDetails() {

		return ibProductDetails;
	}

	/**
	 * Sets the ib product details.
	 *
	 * @param ibProductDetails the ibProductDetails to set
	 */
	public void setIbProductDetails(Object ibProductDetails) {

		this.ibProductDetails = ibProductDetails;
	}

	/**
	 * Gets the ib deferred period option ID.
	 *
	 * @return the ibDeferredPeriodOptionID
	 */
	public int getIbDeferredPeriodOptionID() {

		return ibDeferredPeriodOptionID;
	}

	/**
	 * Sets the ib deferred period option ID.
	 *
	 * @param ibDeferredPeriodOptionID the ibDeferredPeriodOptionID to set
	 */
	public void setIbDeferredPeriodOptionID(int ibDeferredPeriodOptionID) {

		this.ibDeferredPeriodOptionID = ibDeferredPeriodOptionID;
	}

	/**
	 * Gets the rebate limits.
	 *
	 * @return the rebateLimits
	 */
	public int getRebateLimits() {

		return rebateLimits;
	}

	/**
	 * Sets the rebate limits.
	 *
	 * @param rebateLimits the rebateLimits to set
	 */
	public void setRebateLimits(int rebateLimits) {

		this.rebateLimits = rebateLimits;
	}

	/**
	 * Gets the charge issue fee 1.
	 *
	 * @return the chargeIssueFee1
	 */
	public double getChargeIssueFee1() {

		return chargeIssueFee1;
	}

	/**
	 * Sets the charge issue fee 1.
	 *
	 * @param chargeIssueFee1 the chargeIssueFee1 to set
	 */
	public void setChargeIssueFee1(double chargeIssueFee1) {

		this.chargeIssueFee1 = chargeIssueFee1;
	}

	/**
	 * Gets the issue fee ID 2.
	 *
	 * @return the issueFeeID2
	 */
	public double getIssueFeeID2() {

		return issueFeeID2;
	}

	/**
	 * Sets the issue fee ID 2.
	 *
	 * @param issueFeeID2 the issueFeeID2 to set
	 */
	public void setIssueFeeID2(double issueFeeID2) {

		this.issueFeeID2 = issueFeeID2;
	}

	/**
	 * Gets the charge issue fee 2.
	 *
	 * @return the chargeIssueFee2
	 */
	public double getChargeIssueFee2() {

		return chargeIssueFee2;
	}

	/**
	 * Sets the charge issue fee 2.
	 *
	 * @param chargeIssueFee2 the chargeIssueFee2 to set
	 */
	public void setChargeIssueFee2(double chargeIssueFee2) {

		this.chargeIssueFee2 = chargeIssueFee2;
	}

	/**
	 * Gets the issue fee ID 3.
	 *
	 * @return the issueFeeID3
	 */
	public double getIssueFeeID3() {

		return issueFeeID3;
	}

	/**
	 * Sets the issue fee ID 3.
	 *
	 * @param issueFeeID3 the issueFeeID3 to set
	 */
	public void setIssueFeeID3(double issueFeeID3) {

		this.issueFeeID3 = issueFeeID3;
	}

	/**
	 * Gets the charge issue fee 3.
	 *
	 * @return the chargeIssueFee3
	 */
	public double getChargeIssueFee3() {

		return chargeIssueFee3;
	}

	/**
	 * Sets the charge issue fee 3.
	 *
	 * @param chargeIssueFee3 the chargeIssueFee3 to set
	 */
	public void setChargeIssueFee3(double chargeIssueFee3) {

		this.chargeIssueFee3 = chargeIssueFee3;
	}

	/**
	 * Gets the closed days.
	 *
	 * @return the closedDays
	 */
	public int getClosedDays() {

		return closedDays;
	}

	/**
	 * Sets the closed days.
	 *
	 * @param closedDays the closedDays to set
	 */
	public void setClosedDays(int closedDays) {

		this.closedDays = closedDays;
	}

	/**
	 * Gets the loan agreement id.
	 *
	 * @return the loanAgreementId
	 */
	public int getLoanAgreementId() {

		return loanAgreementId;
	}

	/**
	 * Sets the loan agreement id.
	 *
	 * @param loanAgreementId the loanAgreementId to set
	 */
	public void setLoanAgreementId(int loanAgreementId) {

		this.loanAgreementId = loanAgreementId;
	}

	/**
	 * Gets the use loan repayment date limits.
	 *
	 * @return the useLoanRepaymentDateLimits
	 */
	public Date getUseLoanRepaymentDateLimits() {

		return useLoanRepaymentDateLimits;
	}

	/**
	 * Sets the use loan repayment date limits.
	 *
	 * @param useLoanRepaymentDateLimits the useLoanRepaymentDateLimits to set
	 */
	public void setUseLoanRepaymentDateLimits(Date useLoanRepaymentDateLimits) {

		this.useLoanRepaymentDateLimits = useLoanRepaymentDateLimits;
	}

	/**
	 * Checks if is separateon schedule 2.
	 *
	 * @return the separateonSchedule2
	 */
	public boolean isSeparateonSchedule2() {

		return separateonSchedule2;
	}

	/**
	 * Sets the separateon schedule 2.
	 *
	 * @param separateonSchedule2 the separateonSchedule2 to set
	 */
	public void setSeparateonSchedule2(boolean separateonSchedule2) {

		this.separateonSchedule2 = separateonSchedule2;
	}

	/**
	 * Checks if is allow to override int rate at application level.
	 *
	 * @return the isAllowToOverrideIntRateAtApplicationLevel
	 */
	public boolean isAllowToOverrideIntRateAtApplicationLevel() {

		return isAllowToOverrideIntRateAtApplicationLevel;
	}

	/**
	 * Sets the allow to override int rate at application level.
	 *
	 * @param isAllowToOverrideIntRateAtApplicationLevel the
	 *        isAllowToOverrideIntRateAtApplicationLevel to set
	 */
	public void setAllowToOverrideIntRateAtApplicationLevel(
			boolean isAllowToOverrideIntRateAtApplicationLevel) {

		this.isAllowToOverrideIntRateAtApplicationLevel =
				isAllowToOverrideIntRateAtApplicationLevel;
	}

	/**
	 * Checks if is schedule rounding.
	 *
	 * @return the scheduleRounding
	 */
	public boolean isScheduleRounding() {

		return scheduleRounding;
	}

	/**
	 * Sets the schedule rounding.
	 *
	 * @param scheduleRounding the scheduleRounding to set
	 */
	public void setScheduleRounding(boolean scheduleRounding) {

		this.scheduleRounding = scheduleRounding;
	}

	/**
	 * Checks if is schedule rounding ID.
	 *
	 * @return the scheduleRoundingID
	 */
	public boolean isScheduleRoundingID() {

		return scheduleRoundingID;
	}

	/**
	 * Sets the schedule rounding ID.
	 *
	 * @param scheduleRoundingID the scheduleRoundingID to set
	 */
	public void setScheduleRoundingID(boolean scheduleRoundingID) {

		this.scheduleRoundingID = scheduleRoundingID;
	}

	/**
	 * Checks if is penalty interest per payment.
	 *
	 * @return the penaltyInterestPerPayment
	 */
	public boolean isPenaltyInterestPerPayment() {

		return penaltyInterestPerPayment;
	}

	/**
	 * Sets the penalty interest per payment.
	 *
	 * @param penaltyInterestPerPayment the penaltyInterestPerPayment to set
	 */
	public void setPenaltyInterestPerPayment(boolean penaltyInterestPerPayment) {

		this.penaltyInterestPerPayment = penaltyInterestPerPayment;
	}

	/**
	 * Checks if is allow bank cheque printing.
	 *
	 * @return the allowBankChequePrinting
	 */
	public boolean isAllowBankChequePrinting() {

		return allowBankChequePrinting;
	}

	/**
	 * Sets the allow bank cheque printing.
	 *
	 * @param allowBankChequePrinting the allowBankChequePrinting to set
	 */
	public void setAllowBankChequePrinting(boolean allowBankChequePrinting) {

		this.allowBankChequePrinting = allowBankChequePrinting;
	}

	/**
	 * Checks if is allow change bank account.
	 *
	 * @return the allowChangeBankAccount
	 */
	public boolean isAllowChangeBankAccount() {

		return allowChangeBankAccount;
	}

	/**
	 * Sets the allow change bank account.
	 *
	 * @param allowChangeBankAccount the allowChangeBankAccount to set
	 */
	public void setAllowChangeBankAccount(boolean allowChangeBankAccount) {

		this.allowChangeBankAccount = allowChangeBankAccount;
	}

	/**
	 * Checks if is use default interest rate by tiered settings.
	 *
	 * @return the isUseDefaultInterestRateByTieredSettings
	 */
	public boolean isUseDefaultInterestRateByTieredSettings() {

		return isUseDefaultInterestRateByTieredSettings;
	}

	/**
	 * Sets the use default interest rate by tiered settings.
	 *
	 * @param isUseDefaultInterestRateByTieredSettings the isUseDefaultInterestRateByTieredSettings
	 *        to set
	 */
	public void setUseDefaultInterestRateByTieredSettings(
			boolean isUseDefaultInterestRateByTieredSettings) {

		this.isUseDefaultInterestRateByTieredSettings = isUseDefaultInterestRateByTieredSettings;
	}

	/**
	 * Gets the int rate.
	 *
	 * @return the intRate
	 */
	public double getIntRate() {

		return intRate;
	}

	/**
	 * Sets the int rate.
	 *
	 * @param intRate the intRate to set
	 */
	public void setIntRate(double intRate) {

		this.intRate = intRate;
	}

	/**
	 * Gets the max rate.
	 *
	 * @return the maxRate
	 */
	public double getMaxRate() {

		return maxRate;
	}

	/**
	 * Sets the max rate.
	 *
	 * @param maxRate the maxRate to set
	 */
	public void setMaxRate(double maxRate) {

		this.maxRate = maxRate;
	}

	/**
	 * Gets the min rate.
	 *
	 * @return the minRate
	 */
	public double getMinRate() {

		return minRate;
	}

	/**
	 * Sets the min rate.
	 *
	 * @param minRate the minRate to set
	 */
	public void setMinRate(double minRate) {

		this.minRate = minRate;
	}

	/**
	 * Gets the rate.
	 *
	 * @return the rate
	 */
	public double getRate() {

		return rate;
	}

	/**
	 * Sets the rate.
	 *
	 * @param rate the rate to set
	 */
	public void setRate(double rate) {

		this.rate = rate;
	}

	/**
	 * Gets the default interest rate.
	 *
	 * @return the defaultInterestRate
	 */
	public double getDefaultInterestRate() {

		return defaultInterestRate;
	}

	/**
	 * Sets the default interest rate.
	 *
	 * @param defaultInterestRate the defaultInterestRate to set
	 */
	public void setDefaultInterestRate(double defaultInterestRate) {

		this.defaultInterestRate = defaultInterestRate;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "LoanAbacusAPIModelProductLoan [productId=" + productId + ", code=" + code
				+ ", description=" + description + ", maxAccounts=" + maxAccounts + ", minimumTerm="
				+ minimumTerm + ", maximumTerm=" + maximumTerm + ", maximumMemberAge="
				+ maximumMemberAge + ", minimumMemberAge=" + minimumMemberAge + "]";
	}

}
