/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.model;

/**
 * The {@link MetaDataAPIAbacus} enum.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public enum MetaDataAPIAbacus {

	/** The type. */
	TYPE("type"),

	/** The customer ID. */
	CUSTOMER_ID("customerID"),

	/** The description. */
	DESCRIPTION("description"),

	/** The currency ID. */
	CURRENCY_ID("currencyID"),

	/** The amount. */
	AMOUNT("amount"),

	/** The loan guarantor type ID. */
	LOAN_GUARANTOR_TYPE_ID("loanGuarantorTypeID"),

	/** The loan guarantor. */
	LOAN_GUARANTOR("loanGuarantor"),

	/** The check active. */
	CHECK_ACTIVE("checkActive"),

	/** The product id. */
	PRODUCT_ID("productId"),

	/** The cu account portfolio id. */
	CU_ACCOUNT_PORTFOLIO_ID("cuAccountPortfolioID"),

	/** The cu account industry code id. */
	CU_ACCOUNT_INDUSTRY_CODE_ID("cuAccountIndustryCodeID"),

	/** The is reject udf and analysis. */
	IS_REJECT_UDF_AND_ANALYSIS("IsRejectUDFandAnalysis"),

	/** The use schedule interest. */
	USE_SCHEDULE_INTEREST("useScheduleInterest"),

	/** The interest rate. */
	INTEREST_RATE("interestRate"),

	/** The flat rate interest rate. */
	FLAT_RATE_INTEREST_RATE("flatRateInterestRate"),

	/** The loan amount. */
	LOAN_AMOUNT("loanAmount"),

	/** The issue amount. */
	ISSUE_AMOUNT("issueAmount"),

	/** The refinanced. */
	REFINANCED("refinanced"),

	/** The issue date. */
	ISSUE_DATE("issueDate"),

	/** The level payments. */
	LEVEL_PAYMENTS("levelPayments"),

	/** The initial payment date. */
	INITIAL_PAYMENT_DATE("initialPaymentDate"),

	/** The re payment period num. */
	RE_PAYMENT_PERIOD_NUM("rePaymentPeriodNum"),

	/** The re payment period. */
	RE_PAYMENT_PERIOD("rePaymentPeriod"),

	/** The intpay period num. */
	INTPAY_PERIOD_NUM("intPayPeriodNum"),

	/** The ignore odd days. */
	IGNORE_ODD_DAYS("ignoreOddDays"),

	/** The periods deferred id. */
	PERIODS_DEFERRED_ID("periodsDeferredID"),

	/** The periods deferred. */
	PERIODS_DEFERRED("periodsDeferred"),

	/** The first repayment off set. */
	FIRST_REPAYMENT_OFF_SET("firstRepaymentOffset"),

	/** The grace period. */
	GRACE_PERIOD("gracePeriod"),

	/** The term period num. */
	TERM_PERIOD_NUM("termPeriodNum"),

	/** The term period. */
	TERM_PERIOD("termPeriod"),

	/** The loan calculation mode. */
	LOAN_CALCULATION_MODE("loanCalculationMode"),

	/** The normal payment. */
	NORMAL_PAYMENT("normalPayment"),

	/** The capitalise interest when refinancing. */
	CAPITALISE_INTEREST_WHEN_REFINANCING("capitaliseInterestWhenRefinancing"),

	/** The is reviewed. */
	IS_REVIEWED("isReviewed"),

	/** The loan calculator amount type. */
	LOAN_CALCULATOR_AMOUNT_TYPE("loanCalculatorAmountType"),

	/** The issue fee percentage1. */
	ISSUE_FEE_PERCENTAGE1("issueFeePercentage1"),

	/** The issue fee percentage2. */
	ISSUE_FEE_PERCENTAGE2("issueFeePercentage2"),

	/** The term period id. */
	TERM_PERIOD_ID("termPeriodID"),

	/** The repayment period id. */
	REPAYMENT_PERIOD_ID("repaymentPeriodID"),

	/** The effective int rate. */
	EFFECTIVE_INT_RATE("effectiveIntRate"),

	/** The day count. */
	DAY_COUNT("DayCount"),

	/** The loan part. */
	LOAN_PART("loanPart"),

	/** The loan app. */
	LOAN_APP("loanApp"),

	/** The cancel reason id. */
	CANCEL_REASON_ID("CancelReasonID"),

	/** The note. */
	NOTE("Note"),

	/** The notes. */
	NOTES("Notes"),

	/** The status. */
	STATUS("status"),

	/** The menu key. */
	MENU_KEY("menuKey"),

	/** The cu loan process id. */
	CU_LOAN_PROCESS_ID("cuLoanProcessID"),

	/** The loan approval group. */
	LOAN_APPROVAL_GROUP("loanApprovalGroup"),

	/** The is completed. */
	IS_COMPLETED("isCompleted"),

	/** The reference. */
	REFERENCE("reference"),

	/** The is changed. */
	IS_CHANGED("isChanged"),

	/** The cu loan processes. */
	CU_LOAN_PROCESSES("cuLoanProcesses"),

	/** The loan reason id. */
	LOAN_REASON_ID("loanReasonID"),

	/** The loan source of funds id. */
	LOAN_SOURCE_OF_FUNDS_ID("loanSourceOfFundsID"),

	/** The cu loan guarantor source id. */
	CU_LOAN_GUARANTOR_SOURCE_ID("cuLoanGuarantorSourceID"),

	/** The cu loan district code id. */
	CU_LOAN_DISTRICT_CODE_ID("cuLoanDistrictCodeID"),

	/** The cu loan refinance reason id. */
	CU_LOAN_REFINANCE_REASON_ID("cuLoanRefinanceReasonID"),

	/** The final part date. */
	FINAL_PART_DATE("finalPartDate"),

	/** The surveys. */
	SURVEYS("surveys"),

	/** The community loans. */
	COMMUNITY_LOANS("communityLoans"),

	/** The user defined field group id. */
	USER_DEFINED_FIELD_GROUP_ID("userDefinedFieldGroupID"),

	/** The date. */
	DATE("date"),

	/** The total score. */
	TOTAL_SCORE("totalScore"),

	/** The udf field id. */
	UDF_FIELD_ID("udfFieldID"),

	/** The value. */
	VALUE("value"),

	/** The user defined field list id. */
	USER_DEFINED_FIELD_LIST_ID("userDefinedFieldListID"),

	/** The order. */
	ORDER("order"),

	/** The mask. */
	MASK("mask"),

	/** The mandatory. */
	MANDATORY("mandatory"),

	/** The unique field. */
	UNIQUE_FIELD("uniqueField"),

	/** The validate active udf. */
	VALIDATE_ACTIVE_UDF("validateActiveUDF"),

	/** The udf type. */
	UDF_TYPE("udfType"),

	/** The udf field name. */
	UDF_FIELD_NAME("fieldName"),

	/** The field currency. */
	FIELD_CURRENCY("fieldCurrency"),

	/** The date value. */
	DATE_VALUE("dateValue"),

	/** The customer types. */
	CUSTOMER_TYPES("customerTypes"),

	/** The udf links. */
	UDF_LINKS("udfLinks"),

	/** The customer type. */
	CUSTOMER_TYPE("customerType"),

	/** The branch id. */
	BRANCH_ID("branchID"),

	/** The default cu account portfolio id. */
	DEFAULT_CU_ACCOUNT_PORTFOLIO_ID("defaultCUAccountPortfolioID"),

	/** The date joined. */
	DATE_JOINED("dateJoined"),

	/** The persons. */
	PERSONS("persons"),

	/** The customer address. */
	CUSTOMER_ADDRESS("customerAddress"),

	/** The customer relationships. */
	CUSTOMER_RELATIONSHIPS("customerRelationships"),

	/** The name. */
	NAME("name"),

	/** The alt name. */
	ALT_NAME("altName"),

	/** The correspondence name. */
	CORRESPONDENCE_NAME("correspondenceName"),

	/** The track accs individually. */
	TRACK_ACCS_INDIVIDUALLY("trackAccsIndividually"),

	/** The comments. */
	COMMENTS("comments"),

	/** The community status. */
	COMMUNITY_STATUS("communityStatus"),

	/** The customer role. */
	CUSTOMER_ROLE("customerRole"),

	/** The community customers. */
	COMMUNITY_CUSTOMERS("communityCustomers"),

	/** The title. */
	TITLE("title"),

	/** The person id. */
	PERSON_ID("personID"),

	/** The gender. */
	GENDER("gender"),

	/** The forename part one. */
	FORENAME_PART_ONE("forenamePartOne"),

	/** The forename part two. */
	FORENAME_PART_TWO("forenamePartTwo"),

	/** The forename part three. */
	FORENAME_PART_THREE("forenamePartThree"),

	/** The sur name. */
	SUR_NAME("surname"),

	/** The social security number. */
	SOCIAL_SECURITY_NUMBER("socialSecurityNumber"),

	/** The driving licence number. */
	DRIVING_LICENCE_NUMBER("drivingLicenceNumber"),

	/** The date of birth. */
	DATE_OF_BIRTH("dateOfBirth"),

	/** The telephone 1. */
	TELEPHONE_1("telephone1"),

	/** The telephone 2. */
	TELEPHONE_2("telephone2"),

	/** The telephone 3. */
	TELEPHONE_3("telephone3"),

	/** The email. */
	EMAIL("eMail"),

	/** The employers. */
	EMPLOYERS("employers"),

	/** The finger prints. */
	FINGER_PRINTS("fingerPrints"),

	/** The relationship id. */
	RELATIONSHIP_ID("relationshipID"),

	/** The child customer id. */
	CHILD_CUSTOMER_ID("childCustomerID"),

	/** The relationship list key. */
	RELATIONSHIP_LIST_KEY("relationshipListKey"),

	/** The directional. */
	DIRECTIONAL("directional"),

	/** The active. */
	ACTIVE("active"),

	/** The reversed. */
	REVERSED("reversed"),

	/** The address type id. */
	ADDRESS_TYPE_ID("addressTypeID"),

	/** The date moved in. */
	DATE_MOVED_IN("dateMovedIn"),

	/** The date moved out. */
	DATE_MOVED_OUT("dateMovedOut"),

	/** The is primary. */
	IS_PRIMARY("isPrimary"),

	/** The address 1. */
	ADDRESS_1("address1"),

	/** The address 2. */
	ADDRESS_2("address2"),

	/** The address 3. */
	ADDRESS_3("address3"),

	/** The town city. */
	TOWN_CITY("townCity"),

	/** The county. */
	COUNTY("county"),

	/** The state. */
	STATE("state"),

	/** The postal code. */
	POSTAL_CODE("postalCode"),

	/** The country. */
	COUNTRY("country"),

	/** The region. */
	REGION("region"),

	/** The address 1 id. */
	ADDRESS_1_ID("address1ID"),

	/** The address 2 id. */
	ADDRESS_2_ID("address2ID"),

	/** The address 3 id. */
	ADDRESS_3_ID("address3ID"),

	/** The town city id. */
	TOWN_CITY_ID("townCityID"),

	/** The county id. */
	COUNTY_ID("countyID"),

	/** The state id. */
	STATE_ID("stateID"),

	/** The postal code id. */
	POSTAL_CODE_ID("postalCodeID"),

	/** The country id. */
	COUNTRY_ID("countryID"),

	/** The region id. */
	REGION_ID("regionID"),

	/** The address. */
	ADDRESS("address"),

	/** The organisation. */
	ORGANISATION("organisation"),

	/** The reg number. */
	REG_NUMBER("regNumber"),

	/** The fax. */
	FAX("fax"),

	/** The web site. */
	WEB_SITE("webSite"),

	/** The accounts year end. */
	ACCOUNTS_YEAR_END("accountsYearEnd"),

	/** The APR. */
	APR("APR"),

	/** The Fee Amount 1. */
	FEE_AMOUNT_1("FeeAmt1"),

	/** The Fee Amount 2. */
	FEE_AMOUNT_2("FeeAmt2"),

	/** The cu insurance id. */
	CU_INSURANCE_ID("cuInsuranceID"),

	/** The issue fee. */
	ISSUE_FEE("issueFee"),

	/** The survey id. */
	SURVEY_ID("surveyID"),

	/** The udflink id. */
	UDFLINK_ID("udfLinkID"),

	/** The cu loan collateral id. */
	CU_LOAN_COLLATERAL_ID("cuLoanCollateralID"),
	/** The culoan id. */
	CULOAN_ID("cuLoanID"),
	/** The culoan collateral type id. */
	CULOAN_COLLATERAL_TYPE_ID("cuLoanCollateralTypeID"),
	/** The cuaccount id. */
	CUACCOUNT_ID("cuAccountID"),
	/** The original gross value. */
	ORIGINAL_GROSS_VALUE("originalGrossValue"),
	/** The gross value. */
	GROSS_VALUE("grossValue"),
	/** The realised value. */
	REALISED_VALUE("realisedValue"),
	/** The fixed cost. */
	FIXED_COST("fixedCost"),
	/** The net value. */
	NET_VALUE("netValue"),
	/** The value date. */
	VALUE_DATE("valueDate"),
	/** The expiry date. */
	EXPIRY_DATE("expiryDate"),
	/** The with holding rate. */
	WITH_HOLDING_RATE("withholdingRate"),
	/** The gross value changed. */
	GROSS_VALUE_CHANGED("grossValueChanged"),
	/** The fixed value changed. */
	FIXED_VALUE_CHANGED("fixed_value_changed"),
	/** The realised value changed. */
	REALISED_VALUE_CHANGED("realisedValueChanged"),

	/** The collateral type changed. */
	COLLATERAL_TYPE_CHANGED("CollateralTypeChanged"),

	/** The account changed. */
	ACCOUNT_CHANGED("AccountChanged"),

	/** The withholding rate changed. */
	WITH_HOLDING_RATE_CHANGED("WithholdingRateChanged"),

	/** The valuer changed. */
	VALUER_CHANGED("ValuerChanged"),

	/** The collateral active. */
	COLLATERAL_ACTIVE("CollateralActive"),

	/** The valuer id. */
	VALUER_ID("ValuerID"),

	/** The valuer name. */
	VALUER_NAME("ValuerName"),
	/** The cu loan id. */
	CU_LOAN_ID("cuLoanID"),

	/** The cu loan part id. */
	CU_LOAN_PART_ID("cuLoanPartID"),

	/** The specified initial payement date. */
	SPECIFIED_INITIAL_PAYEMENT_DATE("specifiedInitialPaymentDate"),

	/** The apply date. */
	APPLY_DATE("applyDate"),

	/** The cu account id. */
	CU_ACCOUNT_ID("cuAccountID"),

	/** The apply amount total. */
	APPLY_AMOUNT_TOTAL("applyAmountTotal");

	/** The field name. */
	private String fieldName;

	/**
	 * Instantiates a new meta data API abacus.
	 *
	 * @param fieldName the field name
	 */
	MetaDataAPIAbacus(String fieldName) {

		this.fieldName = fieldName;
	}

	/**
	 * Field name.
	 *
	 * @return the string
	 */
	public String fieldName() {

		return fieldName;
	}
}
