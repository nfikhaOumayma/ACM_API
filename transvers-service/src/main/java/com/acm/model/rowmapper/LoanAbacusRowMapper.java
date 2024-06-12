/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.enums.CustomerType;
import com.acm.utils.models.Loan;

/**
 * {@link LoanAbacusRowMapper} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
public class LoanAbacusRowMapper implements RowMapper<Loan> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public Loan mapRow(ResultSet rs, int rowNum) throws SQLException {

		Loan loan = new Loan();
		loan.setIdLoanExtern(rs.getLong("CULoanID"));
		loan.setPortfolioId(rs.getLong("CUAccountPortfolioID"));
		loan.setAccountNumberExtern(rs.getString("AccountNumber"));
		loan.setIdAccountExtern(rs.getLong("CUAccountID"));
		loan.setProductDescription(rs.getString("LOANPRODUCT_DESCRIPTION"));
		loan.setProductCode(rs.getString("LOANPRODUCT_CODE"));
		loan.setCustomerName(rs.getString("CUSTOMER_NAME"));
		loan.setCurrencySymbol(rs.getString("CURRENCY_SYMBOL"));
		loan.setCurrencyDecimalPlaces(rs.getInt("CURRENCY_DECIMALPLACES"));
		loan.setApplyDate(rs.getDate("APPLYDATE"));
		loan.setPortfolioCode(rs.getString("ACCOUNTPORTFOLIO_CODE"));
		loan.setPortfolioDescription(rs.getString("ACCOUNTPORTFOLIO_DESCRIPTION"));
		loan.setProductId(rs.getInt("ProductID"));
		loan.setCustomerId(rs.getLong("CUSTOMERID"));

		loan.setStatut(rs.getInt("STATUS"));
		loan.setApplyAmountTotal(rs.getBigDecimal("APPLY_AMOUNT_TOTAL"));
		loan.setCreationDate(rs.getDate("CREATION_DATE"));
		loan.setTermPeriodNum(rs.getInt("TERM_PERIOD"));
		loan.setPaymentFreq(rs.getInt("PERIOD_FREQ"));
		loan.setIssueDate(rs.getDate("ISSUE_DATE"));
		loan.setIssueFeeAmount(rs.getBigDecimal("ISSUE_FEE_AMOUNT"));
		loan.setProductRate(rs.getBigDecimal("PRODUCT_RATE"));
		loan.setGracePeriod(rs.getInt("GRACEPERIOD"));
		loan.setIndustryCode(rs.getString("ACCOUNTINDUSTRYCODE_CODE"));
		loan.setIndustryCodeDescription(rs.getString("ACCOUNTINDUSTRYCODE_DESCRIPTION"));
		loan.setLoanReasonCode(rs.getString("PRODUCTLOANREASONS_CODE"));
		loan.setLoanReasonDescription(rs.getString("PRODUCTLOANREASONS_DESCRIPTION"));

		loan.setInitialPaymentDate(rs.getDate("InitialPaymentDate"));
		loan.setNormalPayment(rs.getLong("NormalPayment"));
		loan.setIgnoreOddDays(rs.getBoolean("IgnoreOddDays"));
		loan.setPeriodsDeferred(rs.getInt("PeriodsDeferred"));
		loan.setCalculateInitialPaymentDate(rs.getBoolean("CalculateInitialPaymentDate"));
		loan.setTermPeriodID(rs.getLong("TermPeriodID"));

		loan.setBranchID(rs.getInt("BranchID"));
		loan.setBranchName(rs.getString("BRANCHE_NAME"));
		loan.setBranchDescription(rs.getString("BRANCHE_DESCRIPTION"));

		loan.setCustomerType(CustomerType.typeName(rs.getInt("CUSTOMER_TYPE")));
		loan.setCommunityCULoanID(rs.getLong("CommunityCULoanID"));

		loan.setGuarantorSourceId(rs.getInt("GUARANTOR_SOURCE_ID"));
		loan.setSourceOfFundsID(rs.getInt("SOURCE_OF_FUNDS_ID"));
		loan.setRefinanceReasonId(rs.getInt("REFINANCE_REASON_ID"));
		loan.setDistrictCodeId(rs.getInt("DISTRICT_CODE_ID"));
		loan.setIntPayPeriodNum(rs.getInt("INT_PAY_PERIOD_NUM"));

		loan.setLoanCalculationMode(rs.getInt("loanCalculationMode"));
		loan.setApr(rs.getBigDecimal("APR"));
		loan.setEffectiveIntRate(rs.getBigDecimal("EffectiveIntRate_EIR"));
		return loan;
	}
}
